library(checkmate)
gibLeistungsziele <-
  function(waffentyp, disziplin) {
    assertChoice(waffentyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(disziplin, ASSERT_CHOICE_DISZIPLIN)
    #default
    result = list(
      L1000 = 0,
      L250 = 0,
      P10a0 = 0,
      P10a5 = 0,
      P10_0 = 0,
      P10_5 = 0,
      Abstand = 0,
      Ringe = 0,
      Zeit = 0
    )
    
    
    if (waffentyp == 'KW' && disziplin == 'Präzision') {
      result$L1000 = 225
      result$L250 = 225
      result$P10a0 = 90
      result$P10a5 = 45
      result$P10_0 = 70
      result$P10_5 = 35
      result$Abstand = 15
      result$Ringe = 10.3
      result$Zeit = 8
      return(result)
    }
    #Ziele für Langwaffe
    #Ziel: 290 Ringe Mehrdistanz, 300 Ringe bei 30 Sekunden für Speed
    if ((waffentyp == 'LW' || waffentyp == 'SLF') && disziplin == 'Präzision') {
      result$L1000 = 110 	#hälfte KW, eventuell sog bis 35 runtergehen. Weltspitze läge rechn. bei <20
      result$L250 = 110		#hälfte KW, zuminest nicht größer als L1000
      result$P10a0 = 98	#Zwingend, eigentlich 100% sind aber bei log. Trendberechnung fast nicht möglich.
      result$P10a5 = 70	#70 könnten reichen.
      result$P10_0 = 90	#Zwingend.
      result$P10_5 = 45
      result$Abstand = 10# L plus Abstand sind die zentralen Größen. Alles andere folgt.
      result$Ringe = 10.5
      result$Zeit = 10
      return(result)
    }

    
    if (waffentyp == 'KW' && disziplin == 'Speed') {
      result$L1000 = 450
      result$L250 = 350
      result$P10a0 = 90
      result$P10a5 = 45
      result$P10_0 = 70
      result$P10_5 = 35
      result$Abstand = 30
      result$Ringe = 10.5
      result$Zeit = 2.5
      return(result)
    }
    #Zeit: Für fünf Schuss max. 5 Sekunden. Wenn pro Schuss 0,6 Sek, bleiben für den Erstschuss 2,4 Sek.
    if ((waffentyp == 'LW'||waffentyp == 'SLF') && disziplin == 'Speed') {
      result$L1000 = 300
      result$L250 = 200
      result$P10a0 = 98
      result$P10a5 = 49
      result$P10_0 = 90
      result$P10_5 = 45
      result$Abstand = 20
      result$Ringe = 10.5
      result$Zeit = 2.3
      return(result)
    }
    #Zeit: Für fünf Schuss max. 3.33 Sekunden. Wenn pro Schuss 0,4 Sek, bleiben für den Erstschuss 1,7 Sek.
    #Pro Schuss müssen wg. Zehntelrechnung 10.00 Ringe geschossen werden. Wichtig sind dabei vor allem Fehlschüsse,
    #keine Zehntel.
    if (waffentyp == 'LW' && disziplin == 'Fallscheibe/KK') {
      result$L1000 = 600
      result$L250 = 300
      result$P10a0 = 90
      result$P10a5 = 45
      result$P10_0 = 90
      result$P10_5 = 45 #ist unwichtig
      result$Abstand = 30
      result$Ringe = 10.0
      result$Zeit = 1.7
      return(result)
    }
	
	if (waffentyp == 'SLF' && disziplin == 'Fallscheibe/GK') {
      result$L1000 = 600
      result$L250 = 300
      result$P10a0 = 90
      result$P10a5 = 45
      result$P10_0 = 90
      result$P10_5 = 45 #ist unwichtig
      result$Abstand = 30
      result$Ringe = 10.0
      result$Zeit = 1.7
      return(result)
    }	
    
	if (waffentyp == 'KW' && disziplin == 'Fallscheibe/KK') {
      result$L1000 = 600
      result$L250 = 300
      result$P10a0 = 60
      result$P10a5 = 30 #ist unwichtig
      result$P10_0 = 50 #10=10cm 50=5cm.
      result$P10_5 = 30 #ist unwichtig
      result$Abstand = 50
      result$Ringe = 9.5 #oder reichen sogar 9.0? Scheibe ist 20cm
      result$Zeit = 2.4
      return(result)
    }

	if (waffentyp == 'KW' && disziplin == 'Fallscheibe/GK') {
      result$L1000 = 600
      result$L250 = 300
      result$P10a0 = 60
      result$P10a5 = 30 #ist unwichtig
      result$P10_0 = 50 #10=10cm 50=5cm.
      result$P10_5 = 30 #ist unwichtig
      result$Abstand = 50
      result$Ringe = 9.5 #oder reichen sogar 9.0? Scheibe ist 20cm
      result$Zeit = 2.4
      return(result)
    }
	
    return(result)
    
  }




gibZiel <-
  function(waffenTyp, disziplin) {
    assertChoice(waffenTyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(disziplin, ASSERT_CHOICE_DISZIPLIN)
    return(gibLeistungsziele(waffenTyp, disziplin))
  }

wannLeistungziel <-
  function(regression, ziel) {
    assertChoice(class(regression), ASSERT_CHOICE_TREND_DEV)
    assertNumeric(ziel)
    
    ap = as.double(regression$coefficients[1])
    b = as.numeric(Sys.Date())
    cp = as.double(regression$coefficients[2])
    result = round(((ziel - ap) / cp) - b)
    return(result)
  }

printWannLeistungziel <-
  function(regression, ziel, einheit, richtung) {
    assertChoice(class(regression), ASSERT_CHOICE_TREND_DEV)
    assertNumeric(ziel)
    assertString(einheit)
    assertChoice(richtung, ASSERT_CHOICE_RICHTUNG)
    
    seq = as.numeric(seq(Sys.Date(), Sys.Date() + 250, 1))
    trend = predict(regression, newdata = data.frame(Datum = seq))
    tage = c()
    if (richtung == 'steigt') {
      tage = which(trend > ziel)
    } else {
      tage = which(trend < ziel)
    }
    
    
    result = ''
    if (length(tage) == 0) {
      text1 = 'Ausgehend vom aktuellem Trend kann das Leistungsziel von'
      text2 = 'in den nächsten 250 Tagen *nicht* erreicht werden.'
      result = paste(text1, ziel, einheit, text2)
    } else if (tage[[1]] == 1) {
      text1 = 'Ausgehend vom aktuellem Trend wurde das Leistungsziel von'
      text2 = 'bereits erreicht.'
      result = paste(text1, ziel, einheit, text2)
    }  else {
      text1 = 'Ausgehend vom in der Regressionsgeraden berechneten Trend, kann das Leistungsziel von'
      text2 = 'in'
      text3 = 'Tagen erreicht werden.'
      result = paste(text1, ziel, einheit, text2, tage[[1]], text3)
    }
    return(result)
    
    
    #text1 = 'Ausgehend vom in der Regressionsgeraden berechneten Trend, kann das Leistungsziel von'
    #text2 = 'in'
    #text3 = 'Tagen erreicht werden.'
    
    #war1 = 'Ausgehend vom in der Regressionsgeraden berechneten Trend, wurde das Leistungsziel von'
    #war2 = 'vor'
    #war3 = 'Tagen erreicht.'
    
    #result = ''
    #tage = wannLeistungziel(regression, ziel)
    #if (tage > 0) {
    #  result = paste(text1, ziel, einheit, text2, tage, text3)
    #} else {
    #  result = paste(war1, ziel, einheit, war2, abs(tage), war3)
    #}
    #return(result)
    
  }

gibProzentZielerfuellung <-
  function(waf_all, trendAnschlag, wert, ziel) {
    #regression
    sub  = subset(waf_all, Anschlag == trendAnschlag)
    d = sub[, c("Datum", wert, "Anschlag")]
    names(d) = c('Datum', 'Value', 'Anschlag')
    d = subset(d, !is.na(Value))
    regression = lm(d$Value ~ d$Datum)
    #aktueller Wert
    ap = as.double(regression$coefficients[1])
    b = as.numeric(Sys.Date())
    cp = as.double(regression$coefficients[2])
    aktuell = ap + b * cp
    
    
    #hier muss erst die Richtung bestimmt werden und dann abhängig von der Richtung die Prozente ausrechnen.
    
    if (cp < 0) {
      #wenn CP <= 0 dann sinkt die Leistungskurve und aktuell ist größer Ziel. Macht
      #Prozentrechnung schwierig.
      #      bb = max(d$Value) - aktuell
      #     cc = max(d$Value) - ziel
      bb = (ziel * 10) - aktuell
      cc = (ziel * 10) - ziel
      resultat = round(100 / cc * bb, digits = 1)
      
    } else {
      #wenn CP >= 0 dann steigt die Leistungskurve und aktuell ist kleiner Ziel
      resultat = round(100 / ziel * aktuell, digits = 1)
    }
    
    if (is.na(resultat))
      resultat = 1
    
    #Prozent
    
    if (resultat > 100) {
      return(100)
    } else if (resultat < 0) {
      return(0)
    } else {
      return(resultat)
    }
  }