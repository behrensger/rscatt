library(ggplot2)


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
a = a / 100
valuesB = a
valuesR = a
dataR = data.frame(value = valuesR)
dataB = data.frame(value = valuesB)

vt = table(valuesB)
names(vt[vt == max(vt)])

vt = as.data.frame(table(a))
vt$Prozent = round(vt$Freq / sum(vt$Freq) * 100)
names(vt) = c('Messwert', 'Haefigkeit','Prozent')

length(valuesB)
xd = mean(valuesB)
sd = sd(valuesB)
sd2 = 1.5*sd
n = length(valuesB)
ausreisser = c(subset(valuesB, valuesB > (xd + sd2)),
           subset(valuesB, valuesB < (xd - sd2)))
ceiling(length(ausreisser) / n * 100)
c(subset(valuesB, valuesB > (xd + sd2)),
               subset(valuesB, valuesB < (xd - sd2)))

vt = table(valuesB) #Datengruppierung
modal = names(vt[vt == max(vt)])
modal



se = sd/sqrt(n)
round(se/xd * 100,1)
h = hist(
  x,
  breaks = seq(3.2, 3.7, 0.04),
  col = "blue",
  xlab = "Gewicht in gr",
  ylab = "Anzahl",
  ylim = c(0, 60),
  main = "Histogram Gewicht"
)
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * 0.04 * length(x)
lines(xfit, yfit, col = "red", lwd = 2)


plot(
  a,
  type = 'b',
  col = 'red',
  pch = 16,
  bg = 'blue',
  main = "Messwerte",
  xlab = "Messung",
  ylab = 'Gewicht in grain',
  ylim = c(3.1, 3.7)
)
points(a, type = 'p', col = 'blue', pch = 16)

round(mean(valuesB), 2) #Mittelwert
round(sd(valuesB), 3) #Sd
length(x) #Anzahl Messungen


d <- density(valuesB, adjust = 1.1) # returns the density data
plot(d,  main = "Kerndichte Patronenlänge")
polygon(d, col = "blue", border = "black")

