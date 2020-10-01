#Meisterpatrone Rohdaten

source("R/ScattDevReport.R", encoding = 'UTF-8')
library(GGally)
library(Hmisc)

PulverDaten = loadPulverDaten("reloading/rohdaten/pulververhaeltnis.csv")

plotMesswerte <- function(messwerte, basiszahlen, einheit) {
  plot(
    messwerte,
    type = 'b',
    col = 'red',
    pch = 16,
    bg = 'blue',
    ylim = c(
      basiszahlen$min - basiszahlen$sd_max * basiszahlen$sd,
      basiszahlen$max + basiszahlen$sd_max * basiszahlen$sd
    ) ,
    xlab = 'Probe',
    ylab = einheit
  )
  points(messwerte,
         type = 'p',
         col = 'blue',
         pch = 16)
}


#Prüfung Messschieber
ms_values = c(9.05,9.04,9.05,9.05,9.06,9.05,9.04,9.04,9.05,9.04,9.05,9.04,9.05,9.05,9.04,9.04,9.05,9.04,9.05,9.04,9.04,9.04,9.05,9.04,9.05,9.04,9.04,9.05,9.05,9.04,9.05)
ms_zahlen = gibStandardZahlen(ms_values, 2) 


#Prüfung Waage
mw_values = c(13.12,13.14,13.12,13.12,13.12,13.12,13.12,13.12,13.10,13.10,13.12,13.12,13.12,13.10,13.10,13.12,13.12,13.12,13.10,13.12,13.10,13.14,13.12,13.14,13.14,13.10,13.14)
mw_zahlen = gibStandardZahlen(mw_values, 2)

#Prüfung Pulverschieber

a = c(8,8,2,0,2,0,0,0,8,2,4,46,6,8,20,8,0,0,6,2,
      2,46,8,2,0,8,46,48,2,0,6,0,0,48,2,2,0,46,0,48,8,
      0,48,46,46,0,44,0,44,8,0,8,0,44,44,0,0,46,48,0,0,34,
      0,48,0,0,44,36,48,0,48,48,48,46,46,46,48,0,0,
      0,0,48,44,0,46,0,0,44,46,46,
      0,44,2,0,0,0,2,6,0,2,0,48,48,2,0,46,0,6,6,0,0,6,0,
      8,2,0,0,2,6,0,2,6,48)
for (i in 1:length(a)) {
  if (a[i] > 10) {
    a[i] = a[i] + 300
  } else {
    a[i] = a[i] + 350
  }
}
mp_values = a / 100
mp_zahlen = gibStandardZahlen(mp_values, 2)


#Test des Pulverschiebers auf Auswirkungen
PulverschieberTest <- numeric(length = length(mp_values))
for (i in seq_along(mp_values)) {
  PulverschieberTest[i] <- gibPulverDaten(PulverDaten, 28.8, mp_values[i])['v0']
}
PulverMengeTest = gibStandardZahlen(PulverschieberTest, 2)


#Prüfung Geschosse
GeschossDaten <- read_delim("reloading/rohdaten/geschosse.csv", 
                            ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%Y-%m-%d")), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)


KG_L145HP_G = gibStandardZahlen(GeschossDaten$Gewicht, 2)
KG_L145HP_L = gibStandardZahlen(GeschossDaten$Laenge, 2)
KG_L145HP_D = gibStandardZahlen(GeschossDaten$Durchmesser, 2)


#Prüfung OAL von erzeugten Patronen
#Original:
pl_values = c(rep(28.6, 12), rep(28.7, 29), rep(28.8, 63), rep(28.9, 38), rep(29, 16),rep(29.1, 9))
#Bereinigte Daten wg. zu hohem Zündhütchen
pl_values = c(rep(28.6, 12), rep(28.7, 29), rep(28.8, 63), rep(28.9, 38 - 9), rep(29, 16 - 9),rep(29.1, 9 - 9))
pl_zahlen = gibStandardZahlen(pl_values, 2)


pl_values2 = c(27.85,27.93,27.88,28.00,28.04,28.10,28.00,28.10,28.00,27.96,27.87,27.99,28.02,28.03,27.98,27.99,
27.94,28.02,28.04,27.96,27.87,27.90,28.03,28.08,27.78,28.01,28.10,27.95,28.03,27.82,27.93,27.91,
27.88,27.98,28.02,27.91,27.81,27.92,27.97,28.04,27.85,27.91,27.91,28.02,28.11,27.89,27.93,27.95,
27.89,28.31,28.06,27.98,27.86,27.84,27.95,27.92,28.00,27.96,28.00,27.94,27.98,28.08,27.81,28.02,
28.01,28.02,27.88,28.11,28.12,27.98,28.06,27.96,28.04,27.96,28.11,27.98,28.06,28.06,28.22,28.18,
27.92,28.02,27.95,27.95,28.10,27.99,27.89,27.99,28.03,28.03,27.86,27.95,28.23,28.08,28.26,28.02,
28.00,27.95,27.88,27.89,
27.96,28.06,27.98,27.96,28.06,28.09,27.80,27.93,27.97,28.02,27.97,28.00,28.11,27.89,27.99,28.12,28.26,27.94,27.77,27.92,28.00,27.93,27.87,28.05,27.96,28.06,28.07,28.10,
28.08,28.18,28.05,28.00,27.98,28.00,28.10,28.01,28.04,27.97,28.15,28.17,28.16,27.94,28.14,28.22,28.17,28.11,28.01,28.00,28.12,28.05,28.10,28.14,28.22,28.07,28.13,28.18,28.12,28.06,28.12,27.99,
28.03,28.06,27.93,28.03,27.96,28.02,27.90,28.01,28.14,28.03,27.98,27.98,28.00,27.95,28.02,28.07,28.12,27.96,27.93,28.08,27.99,28.03,28.17,28.04,28.01,28.01,27.88,28.09,27.86,28.11,28.12,27.82,
28.00,28.20,28.07,28.27,28.23,28.01,27.93,28.01,27.94,28.23,28.15,27.95,28.16,28.13,28.01,28.15,28.15,28.02,28.09,28.19,27.93,28.19,27.91,28.01,27.96,28.14,28.02,28.22,28.00,28.09,28.04,28.19,27.88,28.16,28.08,27.90,27.99,27.98,28.01,28.03,28.20,28.04,27.88,28.13,27.99)
pl_zahlen2 = gibStandardZahlen(pl_values2, 2)

pl_zahlen2
