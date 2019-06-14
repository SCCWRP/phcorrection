calc.valid.data.df <- function( CTD.data ) {
  
  Valid.data <- data.frame( Parameter = colnames( CTD.data ),
                            Samples = NA, Stations = NA, Profiles = NA,
                            Min = NA, Max = NA, stringsAsFactors = FALSE )
  Valid.data$Samples <- apply( !is.na( CTD.data ), 2, sum )
  calc.N.v.stns <- function( CTD.data.column, StnID ) {
    indx.val <- !is.na( CTD.data.column )
    N.v.stns <- length( unique( StnID[ indx.val ] ) )
    return( N.v.stns )
  }
  Valid.data$Stations <- apply( CTD.data, 2, calc.N.v.stns,
                                CTD.data$StnID )
  Valid.data$Profiles <- apply( CTD.data, 2, calc.N.v.stns,
                                CTD.data$Profile )
  Valid.data$Min <- suppressWarnings( apply( CTD.data, 2, min, na.rm = TRUE ) )
  Valid.data$Max <- suppressWarnings( apply( CTD.data, 2, max, na.rm = TRUE ) )
  #
  return( Valid.data ) 
  