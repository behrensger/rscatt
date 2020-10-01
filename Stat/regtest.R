#######################################################################
#Mit diesem Script kann man regressionstests für die NLS-Trendberechnung machen.
#RegTest

#Sourcen Sourcen

setwd('C:\\Users\\Andrey.Behrens\\Desktop\\Stat\\') #set directory
setwd('/Users/abehrens/Desktop/Stat') #set directory

source("R/ScattDevReport.R", encoding = 'UTF-8')


#Daten laden
sourcefile = paste(getwd(), '/SCATT Auswertungen/SCATT Auswertungen.csv', sep = '')
sourcefile = normalizePath(sourcefile)
scatt = loadScattDevCsv(sourcefile)


#Hilfsfunktion für TryCatch-Block
nix <-
  function(param = NULL)
  {
    return(param)
  }   


#Eigentlicher Regressionstest
zusammen = matrix(nrow = 32, ncol = 12)

nrow = 0
for (waffe in c('Ruger', 'Xesse')) {
  for (disziplin in c('Präzision', 'Speed')) {
    for (trendAnschlag in c('Stehend, Beide', 'Stehend, Rechts')) {
      if (waffe == 'Xesse' && trendAnschlag == 'Stehend, Rechts')
        next
      if (waffe == 'Ruger' &&
          trendAnschlag == 'Stehend, Beide')
        next
      for (valueType in c('10.0',
                          '10a0',
                          'Resultat',
                          '10.5',
                          '10a5',
                          'L',
                          'L/250',
                          'Abstand')) {
        nrow = nrow + 1
        
        #print(paste(waffe, disziplin, trendAnschlag, valueType))
        waf_all = subset(scatt, Disziplin ==  disziplin &
                           Waffe ==  waffe & Entf == 25)
        waf_all = subset(waf_all, Anschlag ==  trendAnschlag)
        
        daten = waf_all[, c("Datum", valueType, "Anschlag")]
        names(daten) = c('Datum', 'Value', 'Anschlag')
        daten = subset(daten, !is.na(Value))
        Datum = as.integer(daten$Datum)
        Value = daten$Value
        
        regression = NULL

        zusammen[nrow, 1] = waffe
        zusammen[nrow, 2] = disziplin
        zusammen[nrow, 3] = trendAnschlag
        zusammen[nrow, 4] = valueType
        
        zusammen[nrow, 5] = round(min(Datum), digits = 1)
        zusammen[nrow, 6] = round(median(Datum), digits = 1)
        zusammen[nrow, 7] = round(max(Datum), digits = 1)
        
        if (valueType %in% c('L',  'L/250', 'Abstand')) {
          zusammen[nrow, 8] = 'nnls'
          tryCatch({
            regression = berechneNNLSRegression(Value, Datum)
          }, error = nix())
        } else {
          zusammen[nrow, 8] = 'nls'
          tryCatch({
            regression = berechneNLSRegression(Value, Datum)
          }, error = nix())
        }
        

        pt = paste(valueType, 'FEHLER ===')
        zusammen[nrow, 9] = 'FEHLER'
        if (!is.null(regression)) {
          zusammen[nrow, 9] = 'ok'
          pt = paste(valueType, 'ok')
          #zusammen[nrow, 9] = round(p['theta1'], digits = 1)
          #zusammen[nrow, 10] = round(p['theta2'], digits = 1)
          #zusammen[nrow, 11] = round(p['theta3'], digits = 1)
          #zusammen[nrow, 12] = round(p['theta4'], digits = 1)
          
          #p = regression$m$getAllPars()
          #pt = paste(
          #  'Regression(',
          #  round(p['theta1'], digits = 1),
          #  ',',
          #  round(p['theta2'], digits = 1),
          #  ',',
          #  round(p['theta3'], digits = 1),
          #  ',',
          #  round(p['theta4'], digits = 1),
          #  ')',
          #  sep = ''
          #)
        }
        t = paste(
          paste(waffe, disziplin, trendAnschlag, valueType, '='),
          pt,
          sep = ''
        )
        print(t)
        
      }
    }
  }
}



colnames(zusammen) = c('Waffe','Disziplin', 'Anschlag','Value', 'Min','Med','Max','Richtung','Theta1','Theta2','Theta3','Theta4')
zusammen = zusammen[order(zusammen[,'Richtung'], zusammen[,'Value'], zusammen[,'Waffe'], zusammen[,'Disziplin']),]
kable(zusammen)


waf_all = subset(scatt, Disziplin ==  disziplin & Waffe ==  'Xesse' & Entf == 25)
waf_all = subset(waf_all, Anschlag ==  'Stehend, Beide')
daten = waf_all[, c("Datum", 'L', "Anschlag")]

names(daten) = c('Datum', 'Value', 'Anschlag')
daten = subset(daten, !is.na(Value))
Datum = as.integer(daten$Datum)
Value = daten$Value
source("R/ScattDevReport.R", encoding = 'UTF-8')


plot(Value ~ Datum, ylim=c(min(Value)-400, max(Value)+400))
abline(h = 100)
lines(predict(regression))
lines(-(196 / (1 + exp(( 17494 - Datum ) / 200))) + 20)
theta1 = max(Value)
theta2 = 2
theta3 = 0.1*1
plot(theta1 - theta2 * exp(-theta3*(min(Datum)-Datum)))

min(Value)

predict(regression)
regression
