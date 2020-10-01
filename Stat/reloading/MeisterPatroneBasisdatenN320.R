#Meisterpatrone Rohdaten
#28.6 115gr Hornady HAP
#4.2gr N320

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

oal_value = c(76,60,59,66,63,62,71,59,64,76,71,68,56,75,72,63,69,57,65,59,65,65,69,61,68,66,70,67,80,61,64,62,73,63,76,63,73,69,69,70,64,63,76,65,64,68,73,61,71,77,62,70,69,76,61,61,74,72,63,66,63,62,63,68,
              64,64,60,60,45,64,63,66,68,68,69,62,57,71,59,61,59,63,59,65,63,70,71,60,64,65,65,61,63,61,66,63,72,69,62,67,62,67,64,49,68,62,51,66,64,69,60,60,65,75,62,63,65,67,64,66,64,50,70,67,67,65,73,73,
              74,67,63,63,63,70,62,64,67,68,64,71,67,65,66,62,63,67,61,66,66,71)
for (i in 1:length(oal_value)) {
  oal_value[i]=oal_value[i]+2800
}
oal_value = oal_value / 100
oal_value
oal_zahlen = gibStandardZahlen(oal_value, 2)
plotMesswerte(oal_value, oal_zahlen, 'mm')
hist(oal_value, breaks = seq(28.4, 28.9, 0.01), col = "blue", xlab = "Länge im mm", ylab = "Anzahl", 
     ylim = c(0,20), 
     main = "Histogram OAL") 
oal_zahlen
