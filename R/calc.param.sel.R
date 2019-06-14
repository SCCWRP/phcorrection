# Function calc.param.sel *****************************************************
# Calculates data frame param.sel, including columns:
#     col.name and k.col (the name(s) the column should start and its number)
calc.param.sel <- function( param.list, Param.names, data.col.names ) {
  # browser()
  n.param <- length( param.list )
  param.sel <- data.frame( k.col = rep( NA, n.param ), col.name = character( n.param ),
                           stringsAsFactors = FALSE )
  rownames( param.sel ) <- param.list
  for( k.param.sel in 1:n.param ) {
    k.param <- which( Param.names$Parameter == rownames( param.sel )[ k.param.sel ] )
    if( length( k.param ) == 0 ) {
      param.sel <- NULL    
    } else {
      param.sel$k.col[ k.param.sel ] <- grep.start.mult( Param.names$ParamNameStarts[ k.param ], data.col.names )
      if( is.na( param.sel$k.col[ k.param.sel ] ) ) {
        param.sel$col.name[ k.param.sel ] <- paste( Param.names$ParamNameStarts[ k.param ], collapse = "," ) 
      } else {
        param.sel$col.name[ k.param.sel ] <- paste( data.col.names[ param.sel$k.col[ k.param.sel ] ], collapse = "," )  
      }
    }
  }
  # browser()
  #
  return( param.sel )
}
# End of function calc.param.sel **********************************************
