library(lubridate)
library(checkmate)

source('R/ScattReportDesigns.R')

SCATT_NLS_REGRESSION_MODEL = 'port'

berechneNLSRegression <-
  function(Resultat,
           Datum,
           start = list(
             theta1 = median(Resultat),
             #Skalierung der y-Achse
             theta2 = min(Datum) - 100,
             #verschiebt den Nullpunkt auf x
             theta3 = 50,
             #Macht Kurve steiler oder schwächer
             theta4 = 1 #verschiebt den Nullpunkt auf y
           ),
           lower = list(
             theta1 = min(Resultat) / 2,
             #Skalierung der y-Achse
             theta2 = min(Datum) - 500,
             #verschiebt den Nullpunkt auf x
             theta3 = -100,
             #Macht Kurve steiler oder schwächer
             theta4 = -40 #verschiebt den Nullpunkt auf y
           ),
           upper = list(
             theta1 = max(Resultat) * 2,
             #Skalierung der y-Achse
             theta2 = min(Datum),
             #verschiebt den Nullpunkt auf x
             theta3 = 200,
             #Macht Kurve steiler oder schwächer
             theta4 = 20 #verschiebt den Nullpunkt auf y
           )) {
    assertNumeric(Resultat, lower = 0)
    assertInteger(Datum, lower = ymd("20171101"))
    
    regression = nls(
      Resultat ~ (theta1 / (1 + exp((theta2 - Datum) / theta3
      )))  + theta4,
      #Resultat ~ (theta1 / (1 + exp(( 1 ) / theta3))) + theta4,
      start = start,
      lower = lower,
      upper = upper,
      algorithm = SCATT_NLS_REGRESSION_MODEL,
      control = nls.control(maxiter = 5000)
    )
    
    pars = regression$m$getPars()
    ll = list(
      theta1 = round(pars[1], digits = 3),
      theta2 = round(pars[2], digits = 3),
      theta3 = round(pars[3], digits = 3),
      theta4 = round(pars[4], digits = 3)
    )
    
    regression$expression = substitute(Regression ==   frac(theta1, 1 +
                                                              frac(italic(e) ^ {
                                                                theta2
                                                                - Datum
                                                              }, theta3)) + theta4, ll)
    
    return(regression)
  }


berechneNNLSRegression <-
  function(Resultat,
           Datum,
           start = list(
             theta1 = median(Resultat),
             #Skalierung der y-Achse
             theta2 = min(Datum) - 1,
             #verschiebt den Nullpunkt auf x
             theta3 = 100,
             #Macht Kurve steiler oder schwächer
             theta4 = median(Resultat) * 2 #verschiebt den Nullpunkt auf y
           ),
           lower = list(
             theta1 = min(Resultat),
             #Skalierung der y-Achse
             theta2 = min(Datum) - 500,
             #verschiebt den Nullpunkt auf x
             theta3 = -100,
             #Macht Kurve steiler oder schwächer
             theta4 = min(Resultat) #verschiebt den Nullpunkt auf y
           ),
           upper = list(
             theta1 = max(Resultat) * 3,
             #Skalierung der y-Achse
             theta2 = min(Datum),
             #verschiebt den Nullpunkt auf x
             theta3 = 400,
             #Macht Kurve steiler oder schwächer
             theta4 = max(Resultat) * 2 #verschiebt den Nullpunkt auf y
           )) {
    assertNumeric(Resultat, lower = 0)
    assertInteger(Datum, lower = ymd("20171101"))
    
    regression = nls(
      Resultat ~ -(theta1 / (1 + exp((theta2 - Datum) / theta3
      )))  + theta4,
      start = start,
      lower = lower,
      upper = upper,
      algorithm = SCATT_NLS_REGRESSION_MODEL,
      control = nls.control(maxiter = 1000)
    )
    
    pars = regression$m$getPars()
    ll = list(
      theta1 = round(pars[1], digits = 3)
      ,
      theta2 = round(pars[2], digits = 3)
      ,
      theta3 = round(pars[3], digits = 3)
      ,
      theta4 = round(pars[4], digits = 3)
    )
    
    regression$expression = substitute(Regression ==  -frac(theta1, 1 +
                                                              frac(italic(e) ^ {
                                                                theta2
                                                                - Datum
                                                              }, theta3)) + theta4, ll)
    
    return(regression)
  }





berechneLNRegression <-
  function(Resultat, Datum) {
    assertNumeric(Resultat, lower = 0)
    assertInteger(Datum, lower = ymd("20170101"))
    
    regression = lm(Resultat ~ Datum)
    #regression = lm(Resultat ~ Datum + I(Datum ^ 2) + I(Datum ^ 3))
    #regression = lm(Resultat ~ Datum + I(Datum ^ 2))
    #regression = lm(Resultat ~ poly(Datum,3))
    #regression = lm(Resultat ~ Datum + I(Datum^3))
    #regression = nls(Value ~ SSlogis(Datum, Asym, xmid, scal), data = bar)
    
    return(regression)
  }



addTrend <-
  function(Resultat, Datum, Type, showRange = TRUE, trendColor = COLOR_ORANGE) {
    assertNumeric(Resultat, lower = 0)
    assertDate(Datum, lower = ymd("20170101"))
    assertChoice(Type, ASSERT_CHOICE_TREND_DEV)
    assert_logical(showRange)
    
    Datum = as.integer(Datum)
    regression = NULL
    Type = 'ln'
    if (Type == 'ln') {
      regression = berechneLNRegression(Resultat, Datum)
    } else if (Type == 'nls') {
      regression = berechneNLSRegression(Resultat, Datum)
    } else if (Type == 'nnls') {
      regression = berechneNNLSRegression(Resultat, Datum)
    }
    
    assert(checkClass(regression, "nls"),
           checkClass(regression, "lm"))
    
    if (class(regression) == 'nls') {
      #Prediction
      newdata <-
        data.frame(Datum = seq(min(Datum) - 10, max(Datum) + 50, 1))
      
      pred = predict(regression, newdata = newdata)
      lines(newdata$Datum, pred, lty = 1, col = trendColor)
      
      if (showRange) {
        #standard error of regression
        s = summary(regression)
        lines(newdata$Datum,
              as.numeric(pred) - s$sigma,
              lty = 3,
              col = COLOR_HELLGRUEN)
        lines(newdata$Datum,
              as.numeric(pred) + s$sigma,
              lty = 3,
              col = COLOR_HELLGRUEN)
      }
    } else {
      #Prediction
      newdata <-
        data.frame(Datum = seq(min(Datum) - 10, max(Datum) + 50, 1))
      
      pred = predict(regression,
                     newdata = newdata,
                     interval = c("none"))
      lines(newdata$Datum, pred, lty = 1, col = trendColor)
      
      
      #pred1 = predict(regression, newdata = newdata, interval = c("prediction"))
      #pred2 = predict(regression, newdata = newdata, interval = c("confidence"))
      #lines(newdata$Datum, pred1[,'lwr'], lty = 3, col = COLOR_ORANGE)
      #lines(newdata$Datum, pred1[,'upr'], lty = 3, col = COLOR_ORANGE)
      #lines(newdata$Datum, pred2[,'lwr'], lty = 3, col = COLOR_HELLGRUEN)
      #lines(newdata$Datum, pred2[,'upr'], lty = 3, col = COLOR_HELLGRUEN)
      
      if (showRange) {
        s = summary(regression)
        lines(newdata$Datum,
              as.numeric(pred) - s$sigma,
              lty = 3,
              col = trendColor)
        lines(newdata$Datum,
              as.numeric(pred) + s$sigma,
              lty = 3,
              col = trendColor)
        
      }
      
    }
    
    
    return(regression)
    
  }
