#' Function grep.start.mult
#'
#' @description
#' Returns the index (from str_list) of the firt matching string in sub_str
#'
#' @param sub_str vector of strings
#'
#' @param str_list list of strings
#'
#' @return If found, an index value. Otherwise, NA
#'
#' @export
#'
#' @examples
#' l = c('hi','bye','sigh','rye','nigh')
#' sub = c('I','know','what','this','bye','function','sigh','does')
#' grep.start.mult(sub,l)
grep.start.mult <- function ( sub_str, str_list ) {
  indx_sel <- apply( as.data.frame( sub_str, stringsAsFactors = FALSE), 1,
                     function(x) { grep( x, substr( str_list, 1, nchar(x) ) ) } )
  indx_sel <- as.numeric( indx_sel )
  indx_sel <- indx_sel[ is.finite( indx_sel ) ]
  if( length( indx_sel ) > 1 ) indx_sel <- indx_sel[1]
  if( length( indx_sel ) == 0 ) indx_sel <- NA
  return( indx_sel )
}
