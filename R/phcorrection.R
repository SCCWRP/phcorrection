#' Compute ph correction based on CTD and Bottle data
#'
#' @description
#'   Given CTD and Bottle data, merge the two dataframes together to calculate delta pH and get the corrected pH
#'
#' @details
#'   Get the actual delta pH from the pHinsi function from seacarb
#'   Create a linear model to approximate delta ph for the other depths that dont have measurements
#'
#'
#' @param ctd a data frame with AT LEAST the following information with these headings:
#'
#'    \code{season} - the season;
#'
#'    \code{agency} - the sampling agency;
#'
#'    \code{sampledate} - the date of sample collection;
#'
#'    \code{sampletime} - the time of sample collection;
#'
#'    \code{station} - a code identifying the station;
#'
#'    \code{depth} - An integer representing depth in feet;
#'
#'    \code{fieldrep} - Field replicate;
#'
#'    \code{labrep} - lab replicate;
#'
#'    \code{Latitude} - latitude in decimal degrees.;
#'
#'    \code{Longitude} - longitude in decimal degrees. Make sure there is a negative sign for the Western coordinates;
#'
#'    \code{temperature} - Temperature in Celsius
#'
#'    \code{salinity} - the salinity observed at the location in PSU, ideally at time of sampling.
#'
#'    \code{density} - density;
#'
#'    \code{ph} - ph;
#'
#'
#' @param bottle
#'
#'    \code{season} - the season;
#'
#'    \code{agency} - the sampling agency;
#'
#'    \code{sampledate} - the date of sample collection;
#'
#'    \code{sampletime} - the time of sample collection;
#'
#'    \code{station} - a code identifying the station;
#'
#'    \code{depth} - An integer representing depth in feet;
#'
#'    \code{fieldrep} - Field replicate;
#'
#'    \code{labrep} - lab replicate;
#'
#'    \code{bottle} - a number identifying the bottle
#'
#'    \code{lims} - not sure;
#'
#'    \code{cruise} - not sure;
#'
#'    \code{temperature} - Temperature in Celsius
#'
#'    \code{salinity} - the salinity observed at the location in PSU, ideally at time of sampling.
#'
#'    \code{ta} - alkalinity;
#'
#'    \code{ph} - ph;
#'
#'
#' @importFrom seacarb pHinsi
#' @importFrom oce swPressure
#' @import dplyr
#' @import tidyr
#'
#' @export
ph.omega <- function(
  ctd,
  bottle,
  flag = 8,
  k1k2 = "l",
  kf = "dg",
  ks = "d",
  pHscale = "T",
  b = "l10"
) {

  merged <- dplyr::inner_join(
    ctd,
    bottle,
    by = c('season','agency','sampledate','station','fieldrep','labrep','depth'),
    suffix = c('_ctd','_bottle')
  )

  Pinsi_bar <- 0.1 * swPressure(
    depth = merged$depth,
    latitude = merged$latitude
  )

  merged$bb_pH_insitu <- pHinsi(
    pH = merged$ph_bottle,
    ALK = merged$ta * 1e6,
    Tinsi = merged$temperature_ctd,
    Tlab = merged$temperature_bottle,
    Pinsi = Pinsi_bar,
    S = merged$salinity_bottle,
    k1k2 = k1k2,
    ks = ks,
    pHscale = pHscale,
    b = b
  )
  merged$delta_pH <- merged$bb_pH_insitu - merged$ph_ctd


  lm_coeffs <- merged %>%
    dplyr::group_by(
      season, agency, sampledate, station, fieldrep, labrep
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      intercept = purrr::map(data, function(df){
        lm(formula = delta_pH ~ depth, data = df)$coefficients[1]
      }),
      slope = purrr::map(data, function(df){
        lm(formula = delta_pH ~ depth, data = df)$coefficients[2]
      }),
      rsquared = purrr::map(data, function(df){
        summary(lm(formula = delta_pH ~ depth, data = df))$r.squared
      })
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = c(intercept, slope, rsquared))


  ctd <- ctd %>%
    dplyr::left_join(
      lm_coeffs, by = c('season','agency','station', 'sampledate', 'fieldrep','labrep')) %>%
    dplyr::mutate(
      delta_ph = (slope * depth) + intercept,
      ph_corrected = ph - delta_ph
    )


  ctd$ta <- mean(bottle$ta)

  bottle$T_insitu <- merged$temperature_ctd

  bottle$omega_bottle <- carb(
    flag = flag,
    var1 = bottle$ph,
    var2 = bottle$ta / 1000000,
    k1k2 = k1k2,
    kf = kf,
    ks = ks,
    b = b
  )$OmegaAragonite

  ctd$omega_ctd <- carb(
    flag = flag,
    var1 = ctd$ph_corrected,
    var2 = ctd$ta / 1000000,
    k1k2 = k1k2,
    kf = kf,
    ks = ks,
    b = b
  )$OmegaAragonite

  return(list(ctd = ctd, bottle = bottle))

}














