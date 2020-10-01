library(checkmate)
library(kableExtra)
library(testthat)
source("R/ScattAssertions.R", encoding = 'UTF-8')
source('R/ReportStandard.R', encoding = 'UTF-8')


LoadWKProtokollCsv <- function(filename) {
  #assertString(filename, .var.name = 'Filename')
  assertFileExists(filename, access = 'r')
  
  filename = normalizePath(filename)
  protokoll <- read_delim(
    filename,
    ";",
    escape_double = FALSE,
    col_types = cols(Datum = col_date(format = "%Y-%m-%d")),
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    trim_ws = TRUE
  )
  
  protokoll = subset(protokoll, !is.na(Datum) & !is.na(WK) &!is.na(Level) &!is.na(Waffe))#Filter leere Zeilen
  
  #return(protokoll)
  assertDataFrame(
    protokoll,
    min.rows = 10,
    col.names = "named",
    types = c(
      "Date",
      "character",
      "character",
      "character",
      "character",
      "integer",
      "numeric",
      "character",
      "character"
    )
    ,
    all.missing = FALSE,
    min.cols = 1,
    null.ok = FALSE
  )
  
  
  colnames(protokoll) = c(
    "Datum",
    "Disziplin",
    "Level",
    "Waffe",
    "Visier",
    "Ringe",
    "Zeit",
    "Veranstaltung",
    "Bemerkung"
  )
  
  
  #remove empty lines
  protokoll = subset(protokoll, !(is.na(Datum)))
  
  assertDate(
    protokoll$Datum,
    lower = as.Date("2017-01-01"),
    upper = Sys.Date() + 10,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    .var.name = 'Datum'
  )
  
  for (x in protokoll$Disziplin)
    assertChoice(x,
                 ASSERT_CHOICE_DISZIPLIN,
                 null.ok = FALSE ,
                 .var.name = 'Disziplin')
  #apply(protokoll$Disziplin, 1, assertChoice(x, ASSERT_CHOICE_DISZIPLIN)
  
  for (x in protokoll$Level)
    assertChoice(x,
                 ASSERT_CHOICE_WK_LEVEL,
                 null.ok = FALSE ,
                 .var.name = 'Level')
  
  for (x in protokoll$Waffe)
    assertChoice(x,
                 ASSERT_CHOICE_WAFFEN,
                 null.ok = FALSE ,
                 .var.name = 'Waffe')
  
  for (x in protokoll$Visier)
    assertChoice(x,
                 ASSERT_CHOICE_SIGHT,
                 null.ok = FALSE ,
                 .var.name = 'Visier')
  
  assertNumeric(
    protokoll$Ringe,
    null.ok = FALSE,
    lower = 0,
    upper = 1500,
    any.missing = FALSE,
    all.missing = FALSE,
    .var.name = 'Ringe'
  )
  
  assertNumeric(
    protokoll$Zeit,
    null.ok = FALSE,
    lower = 0,
    upper = 1500,
    any.missing = FALSE,
    all.missing = FALSE,
    .var.name = 'Zeit'
  )
  
  for (x in protokoll$Veranstaltung)
    assertString(x,
                 min.chars = 5,
                 null.ok = FALSE,
                 .var.name = 'Veranstaltung')
  
  
  #Datenanreicherung
  protokoll$Waffentyp = ZuordnungWaffe2KwLw(protokoll$Waffe)
  protokoll$Zyklus <-
    floor_date(protokoll[["Datum"]],
               unit = "month",
               week_start = getOption("lubridate.month.start", 1))
  
  protokoll$WaffenId = ZuordnungWaffe2Integer.character(protokoll$Waffe)
  
  #preorder the data.frame
  index <-
    with(protokoll, order(Datum, Waffe, Disziplin, Visier, Ringe))
  protokoll = protokoll[index,]
  
  return(protokoll)
  
  
}



CreateWettkampfReport <-
  function(WKProtokolFile,
           fokusfile,
           reportFile,
           outputFile,
           HtmlSelfContained = TRUE) {
    assertFileExists(WKProtokolFile)
    assertFileExists(fokusfile)
    assertFileExists(reportFile)
    assertPathForOutput(outputFile, overwrite = TRUE)
    assertLogical(HtmlSelfContained, null.ok = FALSE)
    
    WKProtokol = LoadWKProtokollCsv(WKProtokolFile)
    TrainingsFokus = LoadScattFokusCsv(fokusfile)
    
    #filter the input data
    
    
    DatenbasisDatei = basename(WKProtokolFile)
    
    fig_dim = 5.0
    render(
      reportFile,
      output_format = html_document(
        toc = FALSE,
        number_sections = FALSE,
        section_divs = FALSE,
        self_contained = HtmlSelfContained,
        smart = TRUE,
        theme = NULL,
        highlight = NULL,
        mathjax = NULL,
        fig_width = fig_dim * 1.5,
        fig_height = fig_dim,
        fig_retina = 2,
        fig_caption = FALSE,
        dev = 'png',
        #dev = 'svg',
        css = 'stylesheet.css'
      ),
      output_file = basename(outputFile),
      output_dir = dirname(outputFile),
      #envir = parent.frame(),
      quiet = TRUE,
      encoding = 'UTF-8'
      
    )
  }




AddLeistungszielLinie <-
  function(Farbe, Text, Ort, lty, Leistungsziel)
  {
    if (Leistungsziel > 0) {
      abline(h = Leistungsziel,  lty = lty, col = Farbe)
      text(
        x = Ort,
        y = Leistungsziel + 2,
        Text,
        adj = 0,
        cex = 0.5,
        col = Farbe
      )
    }
    
  }


PlotWettkampfDisziplin <-
  function(WKProtokol,
           disziplin = 'Mehrdistanz',
           gunType = 'KW')
  {
    assert_data_frame(WKProtokol)
    assertChoice(disziplin, ASSERT_CHOICE_DISZIPLIN)
    assertChoice(gunType, ASSERT_CHOICE_GUN_TYPE)
    
    
    
    WKProtokol$Relation = 1
    daten = subset(WKProtokol, Disziplin == disziplin)
    if(disziplin == 'Mehrdistanz' || disziplin == 'Speed'|| disziplin == 'Präzision'||disziplin == 'Kombi'){
      daten = subset(daten, Ringe > 0)#Filter DQs 
    }
    title = paste('Entwicklung', disziplin, ZuordnungWaffentypName(gunType))
    daten$Relation = daten$Ringe
    
    ylim = c(0, 400)
    ystep = 25
    if (disziplin == 'Speed' && gunType == 'KW') {
      
      daten$Relation = daten$Ringe - daten$Zeit
      ylim = c(000, 300)
      ystep = 50
    } else if (disziplin == 'Speed' && gunType == 'LW') {
      daten$Relation = daten$Ringe - daten$Zeit
      ylim = c(100, 300)
      ystep = 50
    } else if (disziplin == 'Speed' && gunType == 'SLF') {
      daten$Relation = daten$Ringe - daten$Zeit
      ylim = c(000, 200)
      ystep = 50
    } else if (disziplin == 'Mehrdistanz' && gunType == 'KW') {
      ylim = c(200, 400)
      ystep = 50
    } else if (disziplin == 'Mehrdistanz' && gunType == 'LW') {
      ylim = c(200, 300)
      ystep = 25
    } else if (disziplin == 'Mehrdistanz' && gunType == 'SLF') {
      ylim = c(100, 300)
      ystep = 25
    } else if (disziplin == 'Kombi') {
      ylim = c(200, 400)
      ystep = 25
    } else if (disziplin == 'Präzision') {
      ylim = c(100, 200)
      ystep = 25
    } else if (disziplin == 'Fallscheibe') {
      daten$Relation = daten$Zeit
      ylim = c(0, 100)
      ystep = 5
    }
    
    
    
    index <-
      with(daten, order(Zyklus))
    daten = daten[index,]
    
    
    prepareRelationPlot(
      WKProtokol,
      title = title,
      ylim = ylim,
      ylab = '',
      ystep = ystep,
      leistungsziel = 0
    )
    
    ort = min(WKProtokol$Zyklus)
    
    zielDM = ScattWKProtollLeistungsziele(gunType, disziplin)
    qualiDM = ScattWKProtollQualifikationDM(gunType, disziplin)
    qualiLM = ScattWKProtollQualifikationLM(gunType, disziplin)
    if (qualiDM == qualiLM) {
      AddLeistungszielLinie(COLOR_HELLGRUEN,
                            paste('Deutscher Meister (', zielDM, ')', sep = ''),
                            ort ,
                            1,
                            zielDM)
      AddLeistungszielLinie(
        COLOR_HELLGRUEN,
        paste('Qualifikation LM/DM (', qualiDM, ')', sep = ''),
        ort ,
        3,
        qualiDM
      )
    } else {
      AddLeistungszielLinie(COLOR_HELLGRUEN,
                            paste('Deutscher Meister (', zielDM, ')', sep = ''),
                            ort ,
                            1,
                            zielDM)
      AddLeistungszielLinie(COLOR_HELLGRUEN,
                            paste('Qualifikation DM (', qualiDM, ')', sep = ''),
                            ort ,
                            3,
                            qualiDM)
      AddLeistungszielLinie(COLOR_HELLGRUEN,
                            paste('Qualifikation LM (', qualiLM, ')', sep = ''),
                            ort ,
                            2,
                            qualiLM)
    }
    
    #loop ueber waffe
    for (waffe in unique(daten$Waffe)) {
      if (ZuordnungWaffe2KwLw(waffe) != gunType)
        next
      d = subset(daten, Waffe == waffe)
      addTrend(d$Relation,
               d$Zyklus,
               'ln',
               FALSE,
               ZuordnungWaffe2Color(waffe))
      
      #Add Leistungsziele
      
      
      #loop ueber WK-Level
      for (level in unique(d$Level)) {
        lll = subset(d, Level == level)
        #text(
        #  lll$Relation ~ lll$Zyklus,
          #labes = lll$Relation,
          #pos = lll$Zyklus
        #)
        points(
          lll$Relation ~ lll$Zyklus,
          pch = 16,
          col = ZuordnungWaffe2Color(waffe) ,
          cex = as.numeric(ZuordnungWK_Level2Integer(level))
        )
        text((lll$Relation+10) ~ lll$Zyklus, labels = lll$Relation, cex = 0.5,col = ZuordnungWaffe2Color(waffe) )
        
      }
    }
    
    
    lcol = c(COLOR_HELLGRUEN,
             COLOR_HELLGRUEN,
             COLOR_HELLGRUEN,
             COLOR_HELLGRUEN)
    lpch = c(16, 16, 16, 16)
    llty = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    ptcex = as.numeric(ZuordnungWK_Level2Integer(c('VM', 'BM', 'LM', 'DM')))
    legende = c('Verein', 'Bezirk', 'Bundesland', 'Deutschland')
    
    
    par(cex = 0.7)
    legend(
      text.col = FG_COLOR,
      x = 'bottomright',
      legend = legende,
      col = lcol,
      pch = lpch,
      lty = llty,
      inset = .04,
      seg.len = 2,
      pt.cex = ptcex,
      bty = 'n',
      title = 'Wettkampf-Level',
      lwd = 1
    )
    
    return(daten)
  }




PlotWettkampfVerteilung <-
  function(WKProtokol,
           leistungsziel = 2,
           title = 'Wettkämpfe pro Monat') {
    assert_data_frame(WKProtokol)
    assertNumeric(leistungsziel, lower = 0, upper = 10)
    assertString(title, null.ok = TRUE)
    
    
    
    index <-
      with(WKProtokol,
           order(Zyklus, WaffenId, ZuordnungWK_Level2Integer(Level)))
    protokoll = WKProtokol[index,]
    
    counts2 = t(table(data.frame(WKProtokol$Waffe, WKProtokol$Zyklus)))
    ystep = 5
    ylim = c(0, round_any(max(rowSums(counts2)) + 1, ystep, f = ceiling))
    df = as.data.frame.matrix(counts2)
    df$Monat = date(rownames(df))
    daten = WKProtokol
    daten$Relation = 1
    
    prepareRelationPlot(
      daten,
      title = title,
      ylim = ylim,
      ylab = '',
      ystep = ystep,
      leistungsziel = leistungsziel
    )
    
    
    text(
      x = min(WKProtokol$Datum) - 5,
      y = leistungsziel + 0.5,
      'Wettkampfplan',
      adj = 0,
      cex = 0.5,
      col = COLOR_HELLGRUEN
    )
    
    
    
    
    prevZyklus = 0
    for (proLoop in 1:nrow(protokoll)) {
      if (prevZyklus != protokoll[proLoop, 'Zyklus']) {
        counter = 1
        prevZyklus = protokoll[proLoop, 'Zyklus']
      }
      points(
        y = counter,
        x = protokoll[proLoop, 'Zyklus'],
        pch = 16,
        col = ZuordnungWaffe2Color(protokoll[[proLoop, 'Waffe']]),
        cex = as.numeric(ZuordnungWK_Level2Integer(protokoll[[proLoop, 'Level']]))
      )
      #print( )
      counter = counter + 1
    }
    
    
    lcol = c(COLOR_HELLGRUEN,
             COLOR_HELLGRUEN,
             COLOR_HELLGRUEN,
             COLOR_HELLGRUEN)
    lpch = c(16, 16, 16, 16)
    llty = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    ptcex = as.numeric(ZuordnungWK_Level2Integer(c('VM', 'BM', 'LM', 'DM')))
    legende = c('Verein', 'Bezirk', 'Bundesland', 'Deutschland')
    
    
    par(cex = 0.7)
    legend(
      text.col = FG_COLOR,
      x = 'topleft',
      legend = legende,
      col = lcol,
      pch = lpch,
      lty = llty,
      inset = .04,
      seg.len = 2,
      pt.cex = ptcex,
      bty = 'n',
      title = 'Wettkampf-Level',
      lwd = 1
    )
    
    
    
    return()
  }



ZuordnungWK_Level2Integer <- function(x, ...)
  UseMethod("ZuordnungWK_Level2Integer")


ZuordnungWK_Level2Integer.character <-
  function(liste) {
    assertCharacter(liste)
    
    for (i in 1:length(liste)) {
      liste[i] = ZuordnungWK_Level2Integer.default(liste[i])
    }
    
    return(liste)
    
  }

ZuordnungWK_Level2Integer.default <-
  function(WK_Level) {
    assert_choice(WK_Level, ASSERT_CHOICE_WK_LEVEL, .var.name = 'WK_Level')
    if (WK_Level == 'VM') {
      return(1.4)
    }
    if (WK_Level == 'BM') {
      return(1.7)
    }
    if (WK_Level == 'LM') {
      return(2.0)
    }
    if (WK_Level == 'DM') {
      return(2.3)
    }
    if (WK_Level == 'EM') {
      return(2.6)
    }
    if (WK_Level == 'WM') {
      return(2.9)
    }
    return(0)
  }



ZuordnungWaffe2Integer <- function(x, ...)
  UseMethod("ZuordnungWaffe2Integer")


ZuordnungWaffe2Integer.character <-
  function(liste) {
    assertCharacter(liste)
    
    for (i in 1:length(liste)) {
      liste[i] = ZuordnungWaffe2Integer.default(liste[i])
    }
    
    return(liste)
    
  }

ZuordnungWaffe2Integer.default <-
  function(Waffe) {
    assertChoice(Waffe, ASSERT_CHOICE_WAFFEN)
    
    #KW
    if (Waffe == 'Xesse')
      return(003)
    if (Waffe == 'HPS')
      return(001)
    if (Waffe == 'PPQ')
      return(002)
    
    #LW
    if (Waffe == 'Ruger')
      return(022)
    if (Waffe == 'Triarii')
      return(023)
    if (Waffe == 'F12')
      return(021)
    if (Waffe == 'DAR15')
      return(024)
    
    
  }


ScattWKProtollLeistungsziele <-
  function(Waffentyp, Disziplin) {
    assertChoice(Waffentyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(Disziplin, ASSERT_CHOICE_DISZIPLIN)
    
    if (Disziplin == 'Präzision') {
      if (Waffentyp == 'KW') {
        return(197)
      } else {
        return(0)
      }
      
    } else if (Disziplin == 'Kombi') {
      if (Waffentyp == 'KW') {
        return(380)
      } else {
        return(0)
      }
    } else if (Disziplin == 'Mehrdistanz') {
      if (Waffentyp == 'KW') {
        return(385)
      } else if (Waffentyp == 'LW') {
        return(290)
      } else if (Waffentyp == 'SLF') {
        #Disziplin 4505
        return(290)
      } else {
        return(0)
      }
    } else if (Disziplin == 'Speed') {
      if (Waffentyp == 'KW') {
        return(240)
      } else if (Waffentyp == 'LW') {
        return(265)
      } else if (Waffentyp == 'SLF') {
        #Disziplin 4302
        return(160)
      } else {
        return(0)
      }
    }
    
    return(0)
  }


ScattWKProtollQualifikationLM <-
  function(Waffentyp, Disziplin) {
    assertChoice(Waffentyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(Disziplin, ASSERT_CHOICE_DISZIPLIN)
    
    if (Disziplin == 'Präzision') {
      if (Waffentyp == 'KW') {
        return(170)
      } else {
        return(0)
      }
      
    } else if (Disziplin == 'Kombi') {
      if (Waffentyp == 'KW') {
        return(300)
      } else {
        return(0)
      }
    } else if (Disziplin == 'Mehrdistanz') {
      if (Waffentyp == 'KW') {
        return(320)
      } else if (Waffentyp == 'LW') {
        return(270)
      } else if (Waffentyp == 'SLF') {
        return(210)
      } else {
        return(0)
      }
      
    } else if (Disziplin == 'Speed') {
      if (Waffentyp == 'KW') {
        return(150)
      } else if (Waffentyp == 'LW') {
        return(225)
      } else if (Waffentyp == 'SLF') {
        return(90)
      } else {
        return(0)
      }
      
    }
    return(0)
  }

ScattWKProtollQualifikationDM <-
  function(Waffentyp, Disziplin) {
    assertChoice(Waffentyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(Disziplin, ASSERT_CHOICE_DISZIPLIN)
    
    if (Disziplin == 'Präzision') {
      if (Waffentyp == 'KW') {
        return(184)
      } else {
        return(0)
      }
      
    } else if (Disziplin == 'Kombi') {
      if (Waffentyp == 'KW') {
        return(346)
      } else {
        return(0)
      }
    } else if (Disziplin == 'Mehrdistanz') {
      if (Waffentyp == 'KW') {
        return(360)
      } else if (Waffentyp == 'LW') {
        return(270)
      } else if (Waffentyp == 'SLF') {
        return(225)
      } else {
        return(0)
      }
      
    } else if (Disziplin == 'Speed') {
      if (Waffentyp == 'KW') {
        return(180)
      } else if (Waffentyp == 'LW') {
        return(241)
      } else if (Waffentyp == 'SLF') {
        return(120)
      } else {
        return(0)
      }
      
    }
    
    return(0)
  }


ErzeugeLeistungszielTabelle <-
  function() {
    XOP = ScattWKProtollLeistungsziele('KW', 'Präzision')
    XOK = ScattWKProtollLeistungsziele('KW', 'Kombi')
    XOM = ScattWKProtollLeistungsziele('KW', 'Mehrdistanz')
    XOS = ScattWKProtollLeistungsziele('KW', 'Speed')
    XOF = ScattWKProtollLeistungsziele('KW', 'Fallscheibe')
    XOI = ScattWKProtollLeistungsziele('KW', 'IPSC')
    
    HOP = ScattWKProtollLeistungsziele('LW', 'Präzision')
    HOK = ScattWKProtollLeistungsziele('LW', 'Kombi')
    HOM = ScattWKProtollLeistungsziele('LW', 'Mehrdistanz')
    HOS = ScattWKProtollLeistungsziele('LW', 'Speed')
    HOF = ScattWKProtollLeistungsziele('LW', 'Fallscheibe')
    HOI = ScattWKProtollLeistungsziele('LW', 'IPSC')
    
    SOP = ScattWKProtollLeistungsziele('SLF', 'Präzision')
    SOK = ScattWKProtollLeistungsziele('SLF', 'Kombi')
    SOM = ScattWKProtollLeistungsziele('SLF', 'Mehrdistanz')
    SOS = ScattWKProtollLeistungsziele('SLF', 'Speed')
    SOF = ScattWKProtollLeistungsziele('SLF', 'Fallscheibe')
    SOI = ScattWKProtollLeistungsziele('SLF', 'IPSC')
    
    prae = c(XOP, HOP, SOP)
    komb = c(XOK, HOK, SOK)
    mehr = c(XOM, HOM, SOM)
    spee = c(XOS, HOS, SOS)
    fall = c(XOF, HOF, SOF)
    ipsc = c(XOI, HOI, SOI)
    
    
    leistungsziele = data.frame(prae, komb, mehr, spee, fall, ipsc)
    colnames(leistungsziele) = c('Präzision',
                                 'Kombi',
                                 'Mehrdistanz',
                                 'Speed',
                                 'Fallscheibe',
                                 'IPSC')
    rownames(leistungsziele) = c('Kurzwaffe', 'Langwaffe', 'Selbstlade-Flinte')
    return(leistungsziele)
    
  }



ErzeugeQualifikationDMTabelle <-
  function() {
    XOP = ScattWKProtollQualifikationDM('KW', 'Präzision')
    XOK = ScattWKProtollQualifikationDM('KW', 'Kombi')
    XOM = ScattWKProtollQualifikationDM('KW', 'Mehrdistanz')
    XOS = ScattWKProtollQualifikationDM('KW', 'Speed')
    XOF = ScattWKProtollQualifikationDM('KW', 'Fallscheibe')
    XOI = ScattWKProtollQualifikationDM('KW', 'IPSC')
    
    
    HOP = ScattWKProtollQualifikationDM('LW', 'Präzision')
    HOK = ScattWKProtollQualifikationDM('LW', 'Kombi')
    HOM = ScattWKProtollQualifikationDM('LW', 'Mehrdistanz')
    HOS = ScattWKProtollQualifikationDM('LW', 'Speed')
    HOF = ScattWKProtollQualifikationDM('LW', 'Fallscheibe')
    HOI = ScattWKProtollQualifikationDM('LW', 'IPSC')
    
    SOP = ScattWKProtollQualifikationDM('SLF', 'Präzision')
    SOK = ScattWKProtollQualifikationDM('SLF', 'Kombi')
    SOM = ScattWKProtollQualifikationDM('SLF', 'Mehrdistanz')
    SOS = ScattWKProtollQualifikationDM('SLF', 'Speed')
    SOF = ScattWKProtollQualifikationDM('SLF', 'Fallscheibe')
    SOI = ScattWKProtollQualifikationDM('SLF', 'IPSC')
    
    prae = c(XOP, HOP, SOP)
    komb = c(XOK, HOK, SOK)
    mehr = c(XOM, HOM, SOM)
    spee = c(XOS, HOS, SOS)
    fall = c(XOF, HOF, SOF)
    ipsc = c(XOI, HOI, SOI)
    
    
    
    leistungsziele = data.frame(prae, komb, mehr, spee, fall, ipsc)
    colnames(leistungsziele) = c('Präzision',
                                 'Kombi',
                                 'Mehrdistanz',
                                 'Speed',
                                 'Fallscheibe',
                                 'IPSC')
    rownames(leistungsziele) = c('Kurzwaffe', 'Langwaffe', 'Selbstlade-Flinte')
    return(leistungsziele)
    
  }



ErzeugeQualifikationLMTabelle <-
  function() {
    XOP = ScattWKProtollQualifikationLM('KW', 'Präzision')
    XOK = ScattWKProtollQualifikationLM('KW', 'Kombi')
    XOM = ScattWKProtollQualifikationLM('KW', 'Mehrdistanz')
    XOS = ScattWKProtollQualifikationLM('KW', 'Speed')
    XOF = ScattWKProtollQualifikationLM('KW', 'Fallscheibe')
    XOI = ScattWKProtollQualifikationLM('KW', 'IPSC')
    
    HOP = ScattWKProtollQualifikationLM('LW', 'Präzision')
    HOK = ScattWKProtollQualifikationLM('LW', 'Kombi')
    HOM = ScattWKProtollQualifikationLM('LW', 'Mehrdistanz')
    HOS = ScattWKProtollQualifikationLM('LW', 'Speed')
    HOF = ScattWKProtollQualifikationLM('LW', 'Fallscheibe')
    HOI = ScattWKProtollQualifikationLM('LW', 'IPSC')
    
    SOP = ScattWKProtollQualifikationLM('SLF', 'Präzision')
    SOK = ScattWKProtollQualifikationLM('SLF', 'Kombi')
    SOM = ScattWKProtollQualifikationLM('SLF', 'Mehrdistanz')
    SOS = ScattWKProtollQualifikationLM('SLF', 'Speed')
    SOF = ScattWKProtollQualifikationLM('SLF', 'Fallscheibe')
    SOI = ScattWKProtollQualifikationLM('SLF', 'IPSC')
    
    prae = c(XOP, HOP, SOP)
    komb = c(XOK, HOK, SOK)
    mehr = c(XOM, HOM, SOM)
    spee = c(XOS, HOS, SOS)
    fall = c(XOF, HOF, SOF)
    ipsc = c(XOI, HOI, SOI)
    
    
    leistungsziele = data.frame(prae, komb, mehr, spee, fall, ipsc)
    colnames(leistungsziele) = c('Präzision',
                                 'Kombi',
                                 'Mehrdistanz',
                                 'Speed',
                                 'Fallscheibe',
                                 'IPSC')
    rownames(leistungsziele) = c('Kurzwaffe', 'Langwaffe', 'Selbstlade-Flinte')
    return(leistungsziele)
    
    
  }


ScattWKProtokollUnitTest <-
  function(sampleFile = 'rohdaten/WK_Protokoll.csv') {
    test_that("Test LoadWKProtokollCsv", {
      expect_error(LoadWKProtokollCsv())
      expect_error(LoadWKProtokollCsv(NULL))
      expect_error(LoadWKProtokollCsv('Keine Existierende Datei'))
      expect_silent(LoadWKProtokollCsv(sampleFile))
    })
  }
