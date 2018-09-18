#' Calculate Information Value for character or factor columns
#'
#' This function calculates information value for character or factor columns of data frame. 
#'
#' @param df data frame with at least two columns (predictor x and outcome y)
#' @param x column (character or factor) for which Information Value should be calculated
#' @param y column (integer) with binary outcome. y has to be a column in df data frame. It is suggested that y is factor with two levels "bad" and "good" If there are no levels good/bad than the following assumptions are applied - if y is integer, than 0=good and 1=bad. If y is factor than level 2 is assumed to mean bad and 1 good.
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' iv.str(german_data,"purpose","gb")
#' iv.str(german_data,"savings","gb")

iv.str <- function(df,x,y,verbose=FALSE, naomit=TRUE) {
  if (!(class(df)=="data.frame")) {
    stop("Parameter df has to be a data frame.")
  } 
  if (!(is.character(df[, x]) || is.factor(df[, x]))) {
    stop(paste("Input is not a character or factor! Variable:", x))
  } 
  if (!(is.numeric(df[, y]) || is.factor(df[, y]))) {
    stop("Outcome is not a number nor factor!")
  } 
  if (length(unique(df[, y])) != 2) {
    if(verbose) paste(cat(unique(df[,y])),"\n")
    stop("Not a binary outcome")
  }
  if (!(all(sort(unique(df[, y])) == c(0,1))) && is.numeric(df[,y])) {
    stop("Numeric outcome has to be encoded as 0 (good) and 1 (bad). \n")
  }
  
  # Get total bad case number.
  tmp_label = df[, y]

  if (naomit){
    tmp_label = tmp_label[!is.na(df[,x])]
  }

  if (is.factor(tmp_label) && all(levels(tmp_label)[order(levels(tmp_label))]==c("bad","good"))) {
    if (verbose) cat("Assuming good = level 'good' and bad = level 'bad' \n")
    total_1 <- sum(tmp_label=="bad")
  } else if (is.factor(tmp_label)) {
    if (verbose) cat("Factor: Assuming bad = level 2 and good = level 1 \n")
    total_1 <- sum(as.integer(tmp_label)-1)
  } else {
    if (verbose) cat("Numeric: Assuming bad = 1 and good = 0 \n")
    total_1 <-sum(tmp_label)
  }
  total_0 <- length(tmp_label) - total_1      # Total good case number.

  cat(sprintf("Total bad %s, total good %s\n", total_1, total_0)[1])

  outcome_0 <- outcome_1 <- NULL # This is needed to avoid NOTES about not visible binding from R CMD check
  
  if ((!naomit) && (sum(is.na(df[,x])>0))){
    tmp_vector = as.vector(df[, x])
    tmp_vector[is.na(tmp_vector)] = 'NA'  # add 'NA' in vector
    df[, x] = factor(tmp_vector)
  }
  
  # Good and bad cases summary, except <NA>
  iv_data <- data.frame(unclass(table(df[, x], df[, y])))
  
  # Remove NA bin.
  if (naomit){
    iv_data = iv_data[rownames(iv_data) != 'NA',]
  }
  
  if (all(names(iv_data)==c("bad","good"))) {
    iv_data <- iv_data[,c(2,1)]
  }
  
  
  names(iv_data) <- c("outcome_0","outcome_1")
  iv_data <-  within(iv_data, {
    class <- row.names(iv_data)
    variable <- x
    pct_0 <- outcome_0 / total_0
    pct_1 <- outcome_1 / total_1
    odds <-  pct_1 / pct_0
    woe <- log(odds)
    miv <- (pct_1 - pct_0) * woe    
  })

  
  if(is.factor(df[,x])) {
    iv_data$class <- factor(iv_data$class,levels=levels(df[,x]))
  }  
  
  iv_data <- iv_data[c("variable","class","outcome_0","outcome_1","pct_0","pct_1","odds","woe","miv")]

  if(any(iv_data$outcome_0 == 0) | any(iv_data$outcome_1 == 0)) {
    warning("Some group for outcome 0 has zero count. This will result in -Inf or Inf WOE. Replacing - ODDS=1, WoE=0, MIV=0. \n The bin is either too small or suspiciously predictive. \n You should fix this before running any model. It does not make any sense to keep WoE = 0 for such bin.")
    iv_data$woe <- ifelse(is.infinite(iv_data$woe),0,iv_data$woe)
    iv_data$miv <- ifelse(is.infinite(iv_data$miv),0,iv_data$miv)
    iv_data$odds <-ifelse(is.infinite(iv_data$odds),1,iv_data$odds)
  }
  
  rownames(iv_data) <- NULL
  cat (paste("Information Value",round(sum(iv_data$miv),2),"\n"))
  iv_data
}
