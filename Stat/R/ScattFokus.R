library(checkmate)
library(testthat)
source("R/ScattAssertions.R", encoding = 'UTF-8')


LoadScattFokusCsv <- function(filename) {
   #assertString(filename, .var.name = 'Filename')
   assertFileExists(filename, access = 'r')

   filename = normalizePath(filename)
   fokus <- read_delim(
      filename,
      ";",
      escape_double = FALSE,
      col_types = cols(
         Von = col_date(format = "%d.%m.%Y"),
         Bis = col_date(format = "%d.%m.%Y")
      ),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
   )


   assertDataFrame(
      fokus,
      min.rows = 2,
      col.names = "named",
      types = c(
         "Date",
         "Date",
         "character",
         "character",
         "character"
      )
      ,
      all.missing = FALSE,
      min.cols = 1,
      null.ok = FALSE
   )

   colnames(fokus) = c(
      "Von",
      "Bis",
      "Hauptfokus",
      "Nebenfokus",
      "Kommentar"
   )

   #remove empty lines
   fokus = subset(fokus, !(
      is.na(Von) &
         is.na(Bis) &
         is.na(Hauptfokus) & is.na(Nebenfokus)
   ))
   
   assertDate(
     fokus$Von,
     lower = as.Date("2017-10-01"),
     upper = Sys.Date() + 1000,
     any.missing = FALSE,
     all.missing = FALSE,
     null.ok = FALSE,
     .var.name = 'Von'
   )
   assertDate(
     fokus$Bis,
     lower = as.Date("2017-10-01"),
     upper = Sys.Date() + 1000,
     any.missing = FALSE,
     all.missing = FALSE,
     null.ok = FALSE,
     .var.name = 'Bis'
   )
   for (x in fokus$Hauptfokus)
     assertString(
       x,
       na.ok = FALSE,
       null.ok = FALSE ,
       pattern = ASSERT_PATTERN_VALUE_TYPE,
       .var.name = 'Hauptfokus'
     )   
   for (x in fokus$Nebenfokus)
     assertString(
       x,
       na.ok = FALSE,
       null.ok = FALSE ,
       pattern = ASSERT_PATTERN_VALUE_TYPE,
       .var.name = 'Nebenfokus'
     )   
   return(fokus)

}


ScattFokusUnitTest <- function(sampleFile = 'rohdaten/WK_Protokoll.csv') {
   test_that("Test LoadScattFokusCsv", {
      expect_error(LoadScattFokusCsv())
      expect_error(LoadScattFokusCsv(NULL))
      expect_error(LoadScattFokusCsv('Keine Existierende Datei'))
      expect_silent(LoadScattFokusCsv(sampleFile))
   })
}

