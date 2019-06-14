settings.list.create <- function( CTD.param.names, BB.param.names, pH.corr.settings.R ) {
  indx.NA <- grepl( "indx_NA", pH.corr.settings.R$ShortName )
  indx.dTime <- which( grepl( "dTime", pH.corr.settings.R$ShortName ) )
  indx.dZ <- which( grepl( "dZ", pH.corr.settings.R$ShortName ) )
  settings = list(
    CTD.colnames = CTD.param.names[,1:3],
    BB.colnames = BB.param.names[,1:3],
    indx_NA = pH.corr.settings.R$Value[indx.NA],
    dTime = pH.corr.settings.R$Value[indx.dTime[1]],
    dZ = pH.corr.settings.R$Value[indx.dZ[1]]
  )
  # browser()
}