library(lubridate)


source('R/ScattReportDesigns.R', encoding = 'UTF-8')
source('R/ScattTrends.R', encoding = 'UTF-8')
source('R/ScattAssertions.R', encoding = 'UTF-8')

#Konstanten
FARBCODE_TREND <- 1
FARBCODE_PREDICTION <- 2
FARBCODE_CONFIDENCE <- 3
FARBCODE_WAFFE <- 4


calcSymbolForAnschlag <-
  function(anschlag, trendAnschlag)
  {
    assertChoice(anschlag, ASSERT_CHOICE_ANSCHLAEGE)
    assertChoice(trendAnschlag, ASSERT_CHOICE_ANSCHLAEGE)
    #pattern nix ist für Sondersachen oder zum Test
    
    if (anschlag == trendAnschlag)
      return(16) #20 == kleiner Kreis, 16 = größerer Kreis
    if (anschlag == 'Stehend, Beide')
      return(1)
    if (anschlag == 'Stehend, Rechts')
      return(2)
    if (anschlag == 'Stehend, links')
      return(3)
    if (anschlag == 'liegend, aufgelegt')
      return(4)
    if (anschlag == 'Kniend, Rechts')
      return(5)
    if (anschlag == 'Kniend, Beidhändig')
      return(6)
    
  }


calcYlimMinMax <-
  function(waf_all, valueType)
  {
    assert_data_frame(waf_all)
    assertCharacter(valueType, pattern = ASSERT_PATTERN_VALUE_TYPE)
    
    if (valueType == 'Resultat')
      return(c(7, 11))
    #if (any(i <- grep(valueType, c('10.0','10.5','10a0','10a5')))) return(c(0,100))
    if (valueType %in% c('10.0', '10.5', '10a0', '10a5'))
      return(c(0, 100))

    if (valueType %in% c('Q10Abstand'))
      return(c(0, 2))
	  
	  
    val = waf_all[valueType]
    max = max(val, na.rm = TRUE)
    assert_data_frame(val)
    assertNumeric(max,  lower = 0, upper = 2000)
    
    if (max < 100) {
      #10er Gruppe
      return(c(0, round((max + 5) / 10) * 10))
    } else {
      #100er
      return(c(0, round((max + 50) / 100) * 100))
    }
  }


preparePlot <-
  function(alldata,
           TrainingsFokus,
           valueType,
           daten,
           title,
           ylab,
           ylim,
           ystep,
           leistungsziel = 0)
  {
    assertDataFrame(alldata)
    assertDataFrame(TrainingsFokus)
    assertCharacter(valueType, pattern = ASSERT_PATTERN_VALUE_TYPE)
    assertNumeric(daten$Value, lower = -5, upper = 20000)
    assertNumeric(ylim, lower = -5)
    assertNumeric(ystep, lower = 0)
    assertDate(daten$Datum, lower = ymd("20171101"))
    assertString(title, null.ok = TRUE)
    
    xlim = c(min(alldata$Datum) - 2, max(alldata$Datum) + 2)
    par(
      bg = BG_COLOR,
      col = FG_COLOR,
      col.axis = FG_COLOR,
      col.lab = FG_COLOR,
      col.main = FG_COLOR,
      col.sub  = FG_COLOR
    )
    plot(
      daten$Value ~ daten$Datum ,
      type = 'n',
      ylim = ylim ,
      xlim = xlim ,
      xlab = NA ,
      ylab = NA ,
      axes = FALSE
    )
    
    #Haupt- und Nebenfokus im Training markieren.
    haupt = subset(TrainingsFokus, Hauptfokus == valueType)
    if (nrow(haupt) > 0) {
      for (i in 1:nrow(haupt)) {
        zeile = haupt[i,]
        rect(
          max(zeile$Von - 1, xlim[1]),
          -50,
          zeile$Bis,
          ylim[2] * 1.2,
          col = BG_COLOR2,
          border = GRID_LINE_COLOR
        )
        text(
          x = max(zeile$Von, xlim[1]) + 1,
          y = ylim[2] * 0.99,
          'Im Hauptfokus',
          adj = 0,
          cex = 0.5,
          col = COLOR_HELLGRUEN
        )
      }
    }
    neben = subset(TrainingsFokus, Nebenfokus == valueType)
    if (nrow(neben) > 0) {
      for (i in 1:nrow(neben)) {
        zeile = neben[i,]
        rect(
          max(zeile$Von - 1, xlim[1]),
          -50,
          zeile$Bis,
          ylim[2] * 1.2,
          col = BG_COLOR3,
          border = GRID_LINE_COLOR
        )
        text(
          x = max(zeile$Von, xlim[1]) + 1,
          y = ylim[2] * 0.99,
          'Im Nebenfokus',
          adj = 0,
          cex = 0.5,
          col = COLOR_HELLGRUEN
        )
      }
    }
    
    
    
    axis(col = FG_COLOR,
         col.ticks = FG_COLOR,
         2,
         seq(min(ylim) - ystep , max(ylim) + ystep, by = ystep))
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      4,
      c(min(ylim) - ystep, max(ylim) + ystep),
      tick = TRUE,
      lwd.ticks = 0,
      labels = FALSE
    )
    
    mseq = seq(
      from = floor_date(min(xlim), "month"),
      to = Sys.Date() %m+% months(1, abbreviate = FALSE),
      by = 'month'
    )
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      1,
      mseq,
      labels = format(mseq, "%y-%m")
    )
    
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      3,
      mseq,
      tick = TRUE,
      lwd.ticks = 0,
      labels = FALSE
    )
    grid(NA, NULL, lty = 3, col = GRID_LINE_COLOR)
    title(title, ylab = ylab)
    
    abline(h = leistungsziel,  lty = 2, col = COLOR_HELLGRUEN)
    #abline(h = leistungsziel)
  }



prepareRelationPlot <-
  function(daten,
           title,
           ylab,
           ylim,
           ystep,
           leistungsziel = 0)
  {
    assertDataFrame(daten)
    assertNumeric(ylim, lower = -100, upper = 1500)
    assertNumeric(ystep, lower = 1, upper = 100)
    assertDate(daten$Datum, lower = ymd("20170101"))
    
    
    
    xlim = c(min(daten$Datum) - 2, max(daten$Datum) + 2)
    ylim = round(ylim) 
    par(
      bg = BG_COLOR,
      col = FG_COLOR,
      col.axis = FG_COLOR,
      col.lab = FG_COLOR,
      col.main = FG_COLOR,
      col.sub  = FG_COLOR
    )
    plot(
      daten$Relation ~ daten$Datum ,
      type = 'n',
      ylim = ylim ,
      xlim = xlim ,
      xlab = NA ,
      ylab = NA ,
      axes = FALSE
    )
    
    axis(col = FG_COLOR,
         col.ticks = FG_COLOR,
         2,
         seq(min(ylim) - ystep , max(ylim) + ystep, by = ystep))
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      4,
      c(min(ylim) - ystep, max(ylim) + ystep),
      tick = TRUE,
      lwd.ticks = 0,
      labels = FALSE
    )
    
    mseq = seq(
      from = floor_date(min(xlim), "month"),
      to = Sys.Date() %m+% months(1, abbreviate = FALSE),
      by = 'month'
    )
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      1,
      mseq,
      labels = format(mseq, "%y-%m")
    )
    
    axis(
      col = FG_COLOR,
      col.ticks = FG_COLOR,
      3,
      mseq,
      tick = TRUE,
      lwd.ticks = 0,
      labels = FALSE
    )
    grid(NA, NULL, lty = 1, col = GRID_LINE_COLOR)
    
    if (leistungsziel != 0) {
      abline(h = leistungsziel,  lty = 2, col = COLOR_HELLGRUEN)
    }

    title(title, ylab = ylab)
    
  }


addLegende <-
  function(position,
           anschlaege,
           trendAnschlag,
           trendInfo,
           varianz,
           leistungsziel) {
    assertString(position, pattern = ASSERT_PATTERN_LEGEND_POS)
    assertCharacter(anschlaege, unique = TRUE)
    assertString(trendAnschlag)
    assertString(trendInfo)
    assertString(varianz)
    assertString(leistungsziel)
    
    regText = paste('Regression (aktuell ', trendInfo, ')', sep = '')
    varText = paste('Standardfehler (', varianz, ')', sep = '')
    #legende = c(regText, paste('Leistungsziel (', leistungsziel, ')', sep = ''), varText, anschlaege)
    legende = c(regText,
                paste('Leistungsziel (', leistungsziel, ')', sep = ''),
                varText)
    lcol = c(COLOR_ORANGE, COLOR_HELLGRUEN, COLOR_ORANGE)
    lpch = c(NA_integer_, NA_integer_, NA_integer_)
    llty = c(1, 2, 3)
    
    #for (anschlag in anschlaege) {
    #  lpch = c(lpch, calcSymbolForAnschlag(anschlag, trendAnschlag))
    #  lcol = c(lcol, FG_COLOR)
    #  llty = c(llty, NA)
    #}
    
    par(cex = 0.7)
    legend(
      text.col = FG_COLOR,
      x = position,
      legend = legende,
      col = lcol,
      pch = lpch,
      lty = llty,
      inset = .04,
      seg.len = 2,
      bty = 'n',
      lwd = 1
    )
    
  }
