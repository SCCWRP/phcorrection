#' Function density
#'
#' @description
#' Equation of State is from Millero & Poisson (1981) DSR V28: 625-629.
#'
#'   INPUT: Salinity (S) in g/kg or pss.
#'          Temperature (T) in degrees C.
#'
#'   OUTPUT: Density [rho] in g/cc.
#'
#'   DEFINE CONSTANTS FOR EQUATION OF STATE
#'
#' @param S Salinity in g/kg or pss (numeric)
#'
#' @param T_C Temperature in degrees C (numeric)
#'
#' @return Density in g/cc
#'
#' @export
#'
DENSITY <- function( S, T_C) {
  R0 <-  9.99842594E2
  R1 <- 6.793952E-2
  R2 <- -9.095290E-3
  R3 <-  1.001685E-4
  R4 <- -1.120083E-6
  R5 <-  6.536332E-9

  A0 <-  8.24493E-1
  A1 <- -4.0899E-3
  A2 <-  7.6438E-5
  A3 <- -8.2467E-7
  A4 <-  5.3875E-9

  B0 <- -5.72466E-3
  B1 <-  1.0227E-4
  B2 <- -1.6546E-6

  C  <-  4.8314E-4

  # CALCULATE RHO
  SR <- sqrt(S)
  RHO0 <- R0 + T_C * (R1 + T_C * (R2 + T_C * (R3 + T_C *(R4 + T_C * R5))))
  A <- A0 + T_C * (A1 + T_C * (A2 + T_C * (A3 + T_C * A4)))
  B <- B0 + T_C * (B1 + T_C * B2)
  RHO <- RHO0 + S * (A + B * SR + C * S)

  # CONVERT KG/M3 TO g/cc
  DENSITY <- RHO / 1000
  return( DENSITY )
}
