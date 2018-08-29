#' Get a summary for function iv.mult result when parameter summary is False.
#' Return a data frame.
#'
#' @iv list result of function iv.mult.
#' iv.str(iv)

iv.summary = function(iv){
    iv_df = Reduce(function(x,y){rbind(x,y)}, iv)
    iv_vector = tapply(iv_df$miv, iv_df$variable, FUN=sum)
    tmp_df = data.frame('iv'=iv_vector, 'variable'=names(iv_vector))
    iv_df = merge(iv_df, tmp_df, by='variable', all=TRUE)
    iv_df$level = '*'
    iv_df[iv_df$iv>=0.02, 'level'] = '**'
    iv_df[iv_df$iv>=0.1, 'level'] = '***'
    iv_df[iv_df$iv>=0.2, 'level'] = '****'
    iv_df[iv_df$iv>=0.5, 'level'] = '*****'
    iv_df[iv_df$iv>=1, 'level'] = '******'
    return(iv_df)
}