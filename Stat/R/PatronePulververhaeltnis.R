#Modell Zusammenhang OAL, Pulver, Druck, Geschwindigkeit
library(readr)
library(checkmate)


loadPulverDaten <- function(filename)
{
  assertFileExists(filename)
  pulververhaeltnis =
    read_delim(
      filename,
      ";",
      escape_double = FALSE,
      locale = locale(decimal_mark = ",",
                      grouping_mark = "."),
      trim_ws = TRUE
    )
  return(pulververhaeltnis)
}


gibPulverDaten <- function(pulververhaeltnis, oal, grain) {
  assertDataFrame(pulververhaeltnis)
  assertNumeric(oal)
  assertNumeric(grain)
  
  if (oal > max(pulververhaeltnis$OAL)) oal = max(pulververhaeltnis$OAL)
  if (oal < min(pulververhaeltnis$OAL)) oal = min(pulververhaeltnis$OAL)
  if (grain > max(pulververhaeltnis$Grain)) grain = max(pulververhaeltnis$Grain)
  if (grain < min(pulververhaeltnis$Grain)) grain = min(pulververhaeltnis$Grain)
  
  #sub = subset(pulververhaeltnis, OAL == 28.7  & Grain == 3.4)
  xl = floor(grain * 10) / 10
  xu = ceiling(grain * 10) / 10
  yl = floor(oal * 10) / 10
  yu = ceiling(oal * 10) / 10
  
  sub = subset(pulververhaeltnis, Grain >= xl &
                 Grain <= xu & OAL >= yl & OAL <= yu)
  return(colMeans(sub))
}
