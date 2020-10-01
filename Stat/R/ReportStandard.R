library(checkmate)
library(testthat)




gibStandardZahlen <- function(x, ...)
   UseMethod("gibStandardZahlen")


gibStandardZahlen.default <- function(x, sd_abstand = 2)
{
   assertNumeric(x)
   assertNumeric(sd_abstand, lower = 0, upper = 10)

   ergebnis = c()
   ergebnis$sd_max = sd_abstand
   ergebnis$mean = mean(x)
   ergebnis$median = median(x)
   ergebnis$sd = sd(x)
   ergebnis$n = length(x)
   ergebnis$se = ergebnis$sd / sqrt(ergebnis$n) #Standardfehler
   ergebnis$sep = ergebnis$se / ergebnis$mean * 100 #Standardfehler in Prozent
   ergebnis$min = min(x)
   ergebnis$max = max(x)
   ergebnis$spread = ergebnis$max - ergebnis$min


   ergebnis$extrem = c(subset(x, x > (
      ergebnis$mean + ergebnis$sd * sd_abstand
   )), subset(x, x < (
      ergebnis$mean - ergebnis$sd * sd_abstand
   )))

   mp_fn = length(ergebnis$extrem)
   ergebnis$failure = ceiling(mp_fn / ergebnis$n * 100)

   ms_vt = table(x) #Datengruppierung
   ergebnis$modal = names(ms_vt[ms_vt == max(ms_vt)])

   return(ergebnis)
}
