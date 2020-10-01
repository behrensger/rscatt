library(testthat)


ScattUnitTest <- function(fokusfile = 'SCATT Auswertungen/fokus.csv') {
  ScattKwLwZuordnungUnitTest()
  ScattFokusUnitTest(fokusfile)
}
