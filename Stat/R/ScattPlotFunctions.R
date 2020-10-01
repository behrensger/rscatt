#library(readr)
#library(knitr)
#library('rmarkdown', verbose = FALSE)
library(plyr)

source("R/ScattPlotComponents.R", encoding = 'UTF-8')
source('R/ScattAssertions.R', encoding = 'UTF-8')




create_Correlation_frame <-
  function(listen) {
    frame = data.frame(
      Resultat = listen$Resultat,
      Schusszeit = listen$Zeit,
      `10.0` = listen$`10.0`,
      `10a0` = listen$`10a0`,
      L = listen$L ,
      `L/250` = listen$`L/250`,
      Abstand = listen$Abstand,
      Q10Abstand = listen$Q10Abstand
    )
    
    return(frame)
  }



calc_Correlation <-
  function(listen) {
    frame = create_Correlation_frame(listen)
    #corr = rcorr(as.matrix(frame), type = "spearman")
    corr = rcorr(as.matrix(frame), type = "pearson")
    return(corr)
  }


GroupValueDaten <-
  function(waf_all, valueType)
  {
    daten = waf_all[, c("Datum", valueType, "Anschlag", "Waffe", "Schuss", "Zyklus")]
    names(daten) = c('Datum', 'Value', 'Anschlag', 'Waffe', "Schuss", "Zyklus")
    daten = subset(daten, !is.na(Value))
    daten$v = daten$Schuss * daten$Value
    x = aggregate(
      daten$v,
      by = list(daten$Zyklus, daten$Anschlag, daten$Waffe),
      FUN = sum
    )
    y = aggregate(
      daten$Schuss,
      by = list(daten$Zyklus, daten$Anschlag, daten$Waffe),
      FUN = sum
    )
    colnames(x) = c("Zyklus", "Anschlag", "Waffe", "v")
    colnames(y) = c("Zyklus", "Anschlag", "Waffe", "v")
    
    x = x[with(x, order(Zyklus, Anschlag, v)),]
    y = y[with(y, order(Zyklus, Anschlag, v)),]
    
    x$Schuss = y$v
    x$Value = x$v / x$Schuss
    x$Datum = x$Zyklus
    return(x)
    
  }



PlotScattAspekt <-
  function(waf_all,
           TrainingsFokus,
           valueType,
           ylab,
           ystep,
           trendAnschlag,
           legendePos,
           einheit,
           trendType,
           leistungsziel = 0,
           title = NULL,
           compressed = TRUE) {
    assert_data_frame(waf_all)
    assertCharacter(valueType, pattern = ASSERT_PATTERN_VALUE_TYPE)
    assertNumeric(ystep, lower = 0)
    assertString(trendAnschlag)
    assertString(legendePos)
    assertString(einheit)
    assertString(title, null.ok = TRUE)
    assertChoice(trendType, ASSERT_CHOICE_TREND_DEV)
    assertNumeric(leistungsziel, lower = 0, upper = 1000)
    assertLogical(compressed)
    assertDataFrame(
      TrainingsFokus,
      min.rows = 2,
      col.names = "named",
      types = c("Date",
                "Date",
                "character",
                "character",
                "character")
      ,
      all.missing = FALSE,
      min.cols = 1,
      null.ok = FALSE
    )
    
    
    daten = NULL
    
    if (compressed == FALSE) {
      daten = GroupValueDaten(waf_all, valueType)
    } else {
      daten = waf_all[, c("Datum", valueType, "Anschlag", "Waffe")]
      names(daten) = c('Datum', 'Value', 'Anschlag', 'Waffe')
      daten = subset(daten, !is.na(Value))
    }
    daten2 = GroupValueDaten(waf_all, valueType)
    
    
    ylim = calcYlimMinMax(waf_all, valueType)
    assertNumeric(ylim, lower = -5, upper = 2000)
    
    
    preparePlot(waf_all,
                TrainingsFokus,
                valueType,
                daten,
                title,
                ylab,
                ylim,
                ystep,
                leistungsziel)
    plotType = 'p'
    regression = NULL
    anschlaege = unique(daten$Anschlag)
    waffen = unique(daten$Waffe)
    for (anschlag in anschlaege) {
      d  = subset(daten, Anschlag == anschlag)
      
      for (waffe in waffen) {
        e  = subset(d, Waffe == waffe)
        #e2  = subset(d2, Waffe == waffe)
        
        if (trendAnschlag == anschlag) {
          lines(
            e$Value ~ e$Datum,
            type = plotType,
            col = ZuordnungWaffe2Color(waffe),
            pch = calcSymbolForAnschlag(anschlag, trendAnschlag)
          )
        } else {
          lines(
            e$Value ~ e$Datum,
            type = plotType,
            col = ZuordnungWaffe2Color(waffe),
            pch = calcSymbolForAnschlag(anschlag, trendAnschlag)
          )
        }
        
      }
      
      if (trendAnschlag == anschlag) {
        d2  = subset(daten2, Anschlag == anschlag)
        
        lines(
          d2$Value ~ d2$Datum,
          type = 'l',
          col = COLOR_HELLGRUEN,
          pch = calcSymbolForAnschlag(anschlag, trendAnschlag),
          lwd = 1
        )
        
        regression = addTrend(d2$Value, d2$Datum, trendType)
        
      }
      
    }
    
    
    
    if (!is.null(regression)) {
      #aktuell = predict(regression, newdata = data.frame(Datum = c(Sys.Date())))
      aktuell = predict(regression, newdata = data.frame(Datum = c(as.numeric(Sys.Date(
        
      )))))
      sigma = summary(regression)$sigma
      varianz = 100 / aktuell * sigma
      
      addLegende(
        position = legendePos,
        anschlaege,
        trendAnschlag,
        trendInfo = paste(round(aktuell, digits = 1), einheit, sep = ''),
        varianz = paste(round(varianz, digits = 1), '%', sep = ''),
        leistungsziel = paste(round(leistungsziel, digits = 1), einheit, sep = '')
      )
      
      if (!is.null(regression$expression))
        title(sub = regression$expression, outer = FALSE)
      #text(
      #  x = min(d$Datum) + 10,
      #  y = 0 + 10,
      #  labels = regression$expression
      #)
    }
    return(regression)
  }



PlotScattRelation <-
  function(waf_all,
           trendAnschlag,
           valueType1,
           valueType2,
           leistungsziel = 0,
           title = NULL,
           updown = TRUE,
           ylim = NULL) {
    assert_data_frame(waf_all)
    assertChoice(trendAnschlag, ASSERT_CHOICE_ANSCHLAEGE)
    assertNumeric(leistungsziel, lower = -100, upper = 100)
    assertString(valueType1)
    assertString(valueType2)
    assertString(title, null.ok = TRUE)
    assertLogical(updown)
    
    
    sub = subset(waf_all, Anschlag == trendAnschlag)
    agg = aggregate(sub, list(sub$Datum), mean)
    #sub = waf_all
    
    #Plot vorbreiten
    if (is.null(ylim)) {
      ylim = max(abs(agg$Relation), na.rm = TRUE) * 1.2
      ylim = c(-ylim, ylim)
      assertNumeric(ylim, lower = -100, upper = 100)
    }
    
    x = max(ylim)
    ystep = 10
    if (x < 10)
      ystep = 1
    else if (x < 25)
      ystep = 5
    else if (x < 50)
      ystep = 10
    else if (x < 100)
      ystep = 10
    else
      ystep = 50
    
    
    prepareRelationPlot(waf_all,
                        title,
                        'Prozentuale Abweichung',
                        ylim,
                        ystep,
                        leistungsziel)
    
    col_up = COLOR_ORANGE
    col_down = COLOR_HELLGRUEN
    if (updown == FALSE) {
      col_up = COLOR_HELLGRUEN
      col_down = COLOR_ORANGE
    }
    
    
    hoch = subset(agg, Relation > 0)
    lines(
      hoch$Relation ~ hoch$Datum,
      type = 'h',
      col = col_up,
      lty = 1,
      #solid
      lwd = 4
    )
    
    tief = subset(agg, Relation < 0)
    lines(
      tief$Relation ~ tief$Datum,
      type = 'h',
      col = col_down,
      lty = 1,
      #solid
      lwd = 4
    )
    
    legend(
      text.col = FG_COLOR,
      x = 'bottomleft',
      legend = c(valueType1,  valueType2, 'Leistungsziel'),
      col = c(col_up, col_down, COLOR_HELLGRUEN),
      lty = c(1, 1, 2),
      inset = .04,
      seg.len = 2,
      bty = 'n',
      lwd = c(4, 4, 1)
    )
    
    return()
  }





PlotScattSessions <-
  function(scatt,
           trainingstyp = 'SCATT',
           leistungsziel = 15,
           title = 'Sessions pro Woche',
           ylim = 0) {
    assert_data_frame(scatt)
    assertNumeric(leistungsziel, lower = 0, upper = 50)
    assertChoice(trainingstyp, ASSERT_CHOICE_TRAININGSTYPE)
    assertString(title, null.ok = FALSE)
    
    
    ystep = 5
    if(ylim == 0){
      ylim = c(0, round_any(leistungsziel*2.5, ystep, f = ceiling))
    } else {
      ylim = c(0, ylim)
    }
    scatt$Relation = 1
    prepareRelationPlot(
      scatt,
      title = title,
      ylim = ylim,
      ylab = '',
      ystep = ystep,
      leistungsziel = leistungsziel
    )

    
        
    text(
      x = min(scatt$Zyklus) - 5,
      y = leistungsziel + 1,
      'Trainingsplan',
      adj = 0,
      cex = 0.5,
      col = COLOR_HELLGRUEN
    )

    if(trainingstyp == 'ALL'){
      daten = scatt
    } else {
      daten = subset(scatt, Trainingstyp == trainingstyp)
    }
    
    
    for (zyklus in sort(unlist(unique(daten$Zyklus)))) {
      y = 0
      sub = subset(daten, Zyklus == zyklus)
      sub = sub[with(sub, order(WaffenId, Disziplin)),]
      ypoint = 0
      for (y in 1:nrow(sub)) {
        line = sub[y,]
        waffe = line$Waffe
        
        ypoint = ypoint + 1
        disziplin = line$Disziplin
        points(
          y = ypoint,
          x = zyklus,
          pch = ZuordnungDisziplin2Id.default(disziplin),
          bg = COLOR_HELLGRUEN,
          col = ZuordnungWaffe2Color(waffe)
        )
      }
    }

    disList = c(
      'Fallscheibe',
      'IPSC',
      'Präzision',
      'Speed',
      'Zielfernrohr',
      'Sonstiges'
    )
    pch = as.numeric( ZuordnungDisziplin2Id(disList)  )  
    par(cex = 0.7)
    legend(
      text.col = COLOR_HELLGRUEN,
      x = 'topleft',
      legend = disList,
      pch = pch,
      col = COLOR_HELLGRUEN,
      bg = COLOR_HELLGRUEN,
      lty = NA_integer_,
      inset = .04,
      seg.len = 2,
      bty = 'n',
      lwd = 1
    )
    
    
    return(0)
  }




printTrendFormel <-
  function(regression)
  {
    return('')
    assertString(class(regression), pattern = 'lm|nls')
    
    p1 = 'Gleichung der Regression ist '
    pAll = ''
    
    if (class(regression) == 'nls') {
      pAll = paste(p1, '$', regression$expression, '$.', sep = '')
    } else if (class(regression) == 'lm') {
      formula = as.character(regression$call$formula)[3]
      pAll = paste(p1, '$f(Datum) = ', formula, '$.', sep = '')
    }
    
    return(pAll)
  }
