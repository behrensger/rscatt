#Meisterpatrone Rohdaten

source("R/AllReports.R", encoding = 'UTF-8')

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



#Prüfung Dosiergenauigkeit
a = c(10,10,10,10,08,10,16,16,14,12,10,16,16,24,20,14,14,16,12,10,14,30,16,10,14,22,20,12,24,20,16,12,16,10,20)
for (i in 1:length(a)) {
    a[i] = a[i] + 300
}

mp_values = a / 100
mp_zahlen = gibStandardZahlen(mp_values, 2)
plotMesswerte(mp_values, mp_zahlen, 'gr')
mp_zahlen
hist(mp_values, breaks = seq(3, 3.3, 0.025), col = "blue", xlab = "Pulvermenge in Grain", ylab = "Anzahl", ylim = c(0,15), main = "Histogram Messversuche") 



#Prüfung OAL Varianz bei Speer-Geschossen

oal_value = c(91,94,03,93,11,12,04,93,07,90,04,14,99,00,04,00,01,02,15,99,11,08,00,05,12,04,10,01,91,11,04,99,11,02,00,02,22,00,11,05,02,13,99,84,04,00,96,96,96)
for (i in 1:length(oal_value)) {
  if(oal_value[i] > 50) oal_value[i]=oal_value[i]+2900
  else oal_value[i]=oal_value[i]+3000
}
oal_value = oal_value / 100
oal_value
oal_zahlen = gibStandardZahlen(oal_value, 2)
plotMesswerte(oal_value, oal_zahlen, 'gr')
hist(oal_value, breaks = seq(29.5, 30.5, 0.05), col = "blue", xlab = "Pulvermenge in Grain", ylab = "Anzahl", 
     ylim = c(0,20), 
     main = "Histogram OAL") 

