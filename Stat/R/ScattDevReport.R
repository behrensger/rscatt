library(readr, verbose = FALSE)
library(knitr, verbose = FALSE)
library(rmarkdown, verbose = FALSE)
library(Hmisc, verbose = FALSE)
library(GGally, verbose = FALSE)
library(lubridate)
library(igraph)
library(checkmate)
library(fmsb)
library(kableExtra)
library(checkmate)
#library(dplyr)


source("R/ScattPlotFunctions.R", encoding = 'UTF-8')
source("R/ScattPlotComponents.R", encoding = 'UTF-8')
source("R/ScattLeistungsziele.R", encoding = 'UTF-8')
source("R/ScattKwLwZuordnung.R", encoding = 'UTF-8')
source("R/ScattTrends.R", encoding = 'UTF-8')
source("R/ScattFokus.R", encoding = 'UTF-8')
source("R/ScattWKProtokol.R", encoding = 'UTF-8')
source("R/ScattUnitTest.R", encoding = 'UTF-8')
source('R/ReportStandard.R', encoding = 'UTF-8')
source('R/PatronePulververhaeltnis.R', encoding = 'UTF-8')





loadScattDevCsv <-
  function(filename)
  {
    assertFileExists(filename, access = "r")
    #read stuff
    filename = normalizePath(filename)
    scatt <- read_delim(
      filename,
      ";",
      escape_double = FALSE,
      col_types = cols(Datum = col_date(format = "%d.%m.%Y")),
      locale = locale(decimal_mark = ",", grouping_mark = "."),
      trim_ws = TRUE
    )
    
    
    assertDataFrame(
      scatt,
      min.rows = 50,
      col.names = "named",
      types = c(
        "character",
        "date",
        "character",
        "integer",
        "character",
        "character",
        "integer",
        "character",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double",
        "character"
      )
      ,
      all.missing = TRUE,
      min.cols = 18,
      null.ok = FALSE
    )
    
    
    colnames(scatt) = c(
      "Trainingstyp",
      "Datum",
      "Disziplin",
      "Schuss",
      "Waffe",
      "Visier",
      "Entf",
      'Anschlag',
      "Resultat",
      "Zeit",
      "10.0",
      "10.5",
      "10a0",
      "10a5",
      "L",
      "L/250",
      "Abstand",
      "Kommentar"
    )
    
    
    #remove empty lines
    scatt = subset(scatt, !(
      is.na(Datum) &
        is.na(Waffe) &
        is.na(Disziplin)
    ))
    
    assertDate(
      scatt$Datum,
      lower = as.Date("2017-10-01"),
      upper = Sys.Date(),
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE,
      .var.name = 'Datum'
    )
    
    for (x in scatt$Disziplin)
      assertChoice(x,
                   ASSERT_CHOICE_DISZIPLIN,
                   null.ok = FALSE ,
                   .var.name = 'Disziplin')
    for (x in scatt$Kommentar)
      assertString(x,
                   na.ok = TRUE,
                   null.ok = FALSE,
                   .var.name = 'Kommentar')
    for (x in scatt$Waffe)
      assertChoice(x, ASSERT_CHOICE_GUN,  .var.name = 'Waffe')
    for (x in scatt$Visier)
      assertChoice(x,
                   ASSERT_CHOICE_SIGHT,
                   null.ok = FALSE ,
                   .var.name = 'Visier')
    for (x in scatt$Anschlag)
      assertChoice(x,
                   ASSERT_CHOICE_ANSCHLAEGE,
                   null.ok = FALSE ,
                   .var.name = 'Anschlag')
    assertNumeric(
      scatt$Schuss,
      null.ok = FALSE,
      lower = 0,
      upper = 500,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'Schuss'
    )
    assertNumeric(
      scatt$Entf,
      null.ok = FALSE,
      lower = 10,
      upper = 100,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'Entf'
    )
    assertNumeric(
      scatt$Resultat,
      null.ok = TRUE,
      lower = 0,
      upper = 11.9,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = 'Resultat'
    )
    assertNumeric(
      scatt$Zeit,
      null.ok = FALSE,
      lower = 0.5,
      upper = 30,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = 'Zeit'
    )
    
    assertNumeric(
      scatt$`10.0`,
      null.ok = FALSE,
      lower = 0.5,
      upper = 100,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = '10.0'
    )
    assertNumeric(
      scatt$`10.5`,
      null.ok = FALSE,
      lower = 0.5,
      upper = 100,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = '10.5'
    )
    assertNumeric(
      scatt$`10a0`,
      null.ok = FALSE,
      lower = 0.5,
      upper = 100,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = '10a0'
    )
    assertNumeric(
      scatt$`10a5`,
      null.ok = FALSE,
      lower = 0.5,
      upper = 100,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = '10a5'
    )
    assertNumeric(
      scatt$L,
      null.ok = FALSE,
      lower = 1,
      upper = 1600,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = 'L'
    )
    assertNumeric(
      scatt$`L/250`,
      null.ok = FALSE,
      lower = 1,
      upper = 1300,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = 'L/250'
    )
    assertNumeric(
      scatt$Abstand,
      null.ok = FALSE,
      lower = 1,
      upper = 150,
      any.missing = TRUE,
      all.missing = FALSE,
      .var.name = 'Abstand'
    )
    for (x in scatt$Trainingstyp)
      assertChoice(x,
                   ASSERT_CHOICE_TRAININGSTYPE,
                   null.ok = FALSE ,
                   .var.name = 'Trainingstyp')

    
    
    for (i in nrow(scatt)) {
      if (scatt[1,]$Trainingstyp == 'DRY') {
        assert(!checkNull(scatt[1,]$Resultat))
      }else if (scatt[1,]$Trainingstyp == 'LIVE') {
      }else if (scatt[1,]$Trainingstyp == 'DRY') {
        assert(checkNull(scatt[1,]$Resultat))
      }
    }
    
    
    #add new column 'Waffentyp'
    scatt$Waffentyp = ZuordnungWaffe2KwLw(scatt$Waffe)
    scatt$WaffenId = ZuordnungWaffe2Integer(scatt$Waffe)

    
    #add a new column Woche
    #scatt$Woche <- c(strftime(scatt[["Datum"]], format = "%V"))
    #scatt$Jahr <- c(strftime(scatt[["Datum"]], format = "%V"))
    scatt$Zyklus <-
      floor_date(scatt[["Datum"]],
                 unit = "week",
                 week_start = getOption("lubridate.week.start", 1))

    #add new column '10.0/Abstand'
    scatt$Q10Abstand =  (1 / (scatt$`10.0` / scatt$Abstand))

    #preorder the data.frame
    index <-
      with(scatt, order(Datum, WaffenId, Disziplin, Visier, Anschlag))
    scatt = scatt[index,]
    
    #give it back
    return(scatt)
    
  }

createScattTitle <-
  function(waf_all,
           waffe = 'Xesse',
           disziplin = 'Präzision',
           entfernung = 25) {
    return(paste(
      'Scatt Dev -- ',
      format(Sys.Date() - 15, "%Y-%m"),
      ' -- ',
      waffe,
      ' -- ',
      median(waf_all$Entf),
      'm ',
      disziplin,
      sep = ''
    ))
    
  }


createWaf_All <-
  function(scatt, waffentyp, disziplin, visier, entfernung)
  {
    waf_all = scatt
    if (!is.null(waffentyp))
      waf_all = subset(scatt, Waffentyp %in% waffentyp)
    if (!is.null(disziplin))
      waf_all = subset(waf_all, Disziplin == disziplin)
    if (!is.null(visier))
      waf_all = subset(waf_all, Visier == visier)
    if (!is.null(entfernung)) {
      waf_all = subset(waf_all, Entf == entfernung)
    } else {
      waf_all = subset(waf_all, Entf == median(waf_all$Entf))
    }
    return(waf_all)
  }    



CreateScattReport <-
  function(filename,
           fokusfile,
           wbkprotokolFile,
           reportFile,
           outputFile,
           waffentyp = 'Xesse',
           disziplin = 'Präzision',
           entfernung = NULL,
           visier = NULL,
           HtmlSelfContained = TRUE) {
    assertFileExists(filename)
    assertFileExists(fokusfile)
    assertFileExists(reportFile)
    assertPathForOutput(outputFile, overwrite = TRUE)
    assertLogical(HtmlSelfContained, null.ok = FALSE)
    
    
    assertChoice(disziplin,
                 ASSERT_CHOICE_DISZIPLIN,
                 null.ok = FALSE ,
                 .var.name = 'Disziplin')
    assertChoice(waffentyp,
                 ASSERT_CHOICE_GUN_TYPE,
                 null.ok = FALSE ,
                 .var.name = 'Waffentyp')
    
    assertChoice(visier,
                 ASSERT_CHOICE_SIGHT,
                 null.ok = TRUE ,
                 .var.name = 'Visier')
    
    assertInteger(
      entfernung,
      null.ok = TRUE,
      lower = 10,
      upper = 100,
      .var.name = 'Entf'
    )
    
    scatt = loadScattDevCsv(filename)
    TrainingsFokus = LoadScattFokusCsv(fokusfile)
    
    #filter the input data
    
    waf_all = NULL
    if (waffentyp == 'KW') {
      waf_all = createWaf_All(scatt, c('KW'), disziplin, visier, entfernung)
    } else {
      waf_all = createWaf_All(scatt, c('LW', 'SLF'), disziplin, visier, entfernung)
    }

    
    DatenbasisDatei = basename(filename)
    
    dir_output = dirname(outputFile)
    dir_tmp  = paste(dir_output, '/tmp/', sep ='')
    
    
    fig_dim = 7
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
        fig_width = fig_dim * 1.7,
        fig_height = fig_dim,
        fig_retina = 2,
        fig_caption = FALSE,
        dev = 'png',
        #dev = 'jpeg',
        #dev = 'svg',
        css = 'stylesheet.css'
      ),
      output_file = basename(outputFile),
      output_dir = dir_output,
      #intermediates_dir = dir_tmp,
      #envir = parent.frame(),
      quiet = TRUE,
      clean = TRUE,
      encoding = 'UTF-8'
    )
    remove(dir_tmp)
  }

getTrendAnschlag <- function(waf_all) {
  t = table(waf_all$Anschlag)
  t = sort(t)
  trendAnschlag = names(t[length(t)])
  return(trendAnschlag)
}

fixMe <-
  function(wert)
  {
    if (is.numeric(wert) && !is.na(wert) && !is.nan(wert) && !is.infinite(wert)) return(wert)
    return(0)
  }


CreateDatenbasisTabelle <-
  function(scatt, waf_all, waffentyp, disziplin, trendAnschlag) {
    assertDataFrame(scatt)
    assertDataFrame(waf_all)
    #assertChoice(waffentyp, ASSERT_CHOICE_GUN_TYPE)
    assertChoice(disziplin, ASSERT_CHOICE_DISZIPLIN)
    assertChoice(trendAnschlag, ASSERT_CHOICE_ANSCHLAEGE)
    
    
    
    dr =  subset(scatt, Trainingstyp == 'DRY')
    lv =  subset(scatt, Trainingstyp == 'LIVE')
    sc =  subset(scatt, Trainingstyp == 'SCATT')
    tr0 =  subset(scatt, Waffentyp %in% waffentyp)
    tr1 =  waf_all
    tr2 =  subset(waf_all, Anschlag == trendAnschlag)
    sc18 = subset(scatt, Datum >= as.Date("2018-01-01") & Datum <= as.Date("2018-12-31"))
    sc19 = subset(scatt, Datum >= as.Date("2019-01-01") & Datum <= as.Date("2019-12-31"))
    gs = scatt
    
    
    
    s_dr = fixMe(round_any(sum(dr$Schuss), 100))
    s_lv = fixMe(round_any(sum(lv$Schuss), 100))
    s_sc = fixMe(round_any(sum(sc$Schuss), 100))
    s_tr0 = fixMe(round_any(sum(tr0$Schuss), 100))
    s_tr1 = fixMe(round_any(sum(tr1$Schuss), 100))
    s_tr2 = fixMe(round_any(sum(tr2$Schuss), 100))
    s_sc18 = fixMe(round_any(sum(sc18$Schuss), 100))
    s_sc19 = fixMe(round_any(sum(sc19$Schuss), 100))
    s_gs = s_dr + s_lv + s_sc

    w_dr = ceiling(as.numeric( difftime(max(dr$Datum), min(dr$Datum),units = "weeks")))
    w_lv = ceiling(as.numeric( difftime(max(lv$Datum), min(lv$Datum),units = "weeks")))
    w_sc = ceiling(as.numeric( difftime(max(sc$Datum), min(sc$Datum),units = "weeks")))
    w_tr0 = ceiling(as.numeric( difftime(max(tr0$Datum), min(tr0$Datum),units = "weeks")))
    w_tr1 = ceiling(as.numeric( difftime(max(tr1$Datum), min(tr1$Datum),units = "weeks")))
    w_tr2 = ceiling(as.numeric( difftime(max(tr2$Datum), min(tr2$Datum),units = "weeks")))
    w_sc18 = ceiling(as.numeric( difftime(max(sc18$Datum), min(sc18$Datum),units = "weeks")))
    w_sc19 = ceiling(as.numeric( difftime(max(sc19$Datum), min(sc19$Datum),units = "weeks")))
    w_gs = ceiling(as.numeric( difftime(max(gs$Datum), min(gs$Datum),units = "weeks")))
    
    sp_dr = fixMe(round(s_dr / w_dr))
    sp_lv = fixMe(round(s_lv / w_lv))
    sp_sc = fixMe(round(s_sc / w_sc))
    sp_tr0 = fixMe(round(s_tr1 / w_tr0))
    sp_tr1 = fixMe(round(s_tr1 / w_tr1))
    sp_tr2 = fixMe(round(s_tr2 / w_tr2))
    sp_sc18 = fixMe(round(s_sc18 / w_sc18))
    sp_sc19 = fixMe(round(s_sc19 / w_sc19))
    sp_gs = fixMe(round(s_gs / w_gs))
    
        
    ss_dr = nrow(dr)
    ss_lv = nrow(lv)
    ss_sc = nrow(sc)
    ss_tr0 = nrow(tr0)
    ss_tr1 = nrow(tr1)
    ss_tr2 = nrow(tr2)
    ss_sc18 = nrow(sc18)
    ss_sc19 = nrow(sc19)
    ss_gs = ss_dr + ss_lv + ss_sc
    
    ssp_dr = fixMe(round(ss_dr / w_dr, digits = 1))
    ssp_lv = fixMe(round(ss_lv / w_lv, digits = 1))
    ssp_sc = fixMe(round(ss_sc / w_sc, digits = 1))
    ssp_tr0 = fixMe(round(ss_tr0 / w_tr0, digits = 1))
    ssp_tr1 = fixMe(round(ss_tr1 / w_tr1, digits = 1))
    ssp_tr2 = fixMe(round(ss_tr2 / w_tr2, digits = 1))
    ssp_sc18 = fixMe(round(ss_sc18 / w_sc18, digits = 1))
    ssp_sc19 = fixMe(round(ss_sc19 / w_sc19, digits = 1))
    ssp_gs = ssp_dr + ssp_lv + ssp_sc
    

    Von = c(min(dr$Datum), min(lv$Datum), min(sc$Datum),min(tr0$Datum), min(tr1$Datum), min(tr2$Datum),min(sc18$Datum),min(sc19$Datum), min(gs$Datum))
    Bis = c(max(dr$Datum), max(lv$Datum), max(sc$Datum), max(tr0$Datum), max(tr1$Datum), max(tr2$Datum),max(sc18$Datum),max(sc19$Datum), max(gs$Datum))
    Zeitraum = paste(Von, '-', Bis)
    Weeks = c(w_dr, w_lv, w_sc, w_tr0, w_tr1, w_tr2, w_sc18, w_sc19, w_gs)
    Schuss = c(s_dr, s_lv, s_sc, s_tr0, s_tr1, s_tr2, s_sc18, s_sc19, s_gs)
    ProWoche1 = c(sp_dr, sp_lv, sp_sc, sp_tr0, sp_tr1, sp_tr2, sp_sc18, sp_sc19, sp_gs)
    Session = c(ss_dr, ss_lv, ss_sc, ss_tr0, ss_tr1, ss_tr2, ss_sc18, ss_sc19, ss_gs)
    ProWoche2 = c(ssp_dr, ssp_lv, ssp_sc, ssp_tr0, ssp_tr1, ssp_tr2, ssp_sc18, ssp_sc19, ssp_gs)

    x = data.frame(Zeitraum,Weeks, Schuss, ProWoche1, Session, ProWoche2)
    colnames(x) = c('Von - Bis', 'Wochen', 'Schuss', 'Pro Woche', 'Einheiten', 'Pro Woche')
    rownames(x) = c('Trocken', 'Live', 'SCATT', paste('davon', waffentyp, sep = ' '),paste('davon', disziplin, sep = ' '), paste('davon', trendAnschlag, sep = ' '),'2018', '2019', 'Gesamt')

    #mutate_all(funs(replace(., is.na(.), 0)))
    return(x)
    
  }
           
createLeistungszielDatafram <-
  function(waf_all, trendAnschlag, waffentyp, disziplin)
{
  
  
  c = c(
    gibProzentZielerfuellung(waf_all, trendAnschlag, 'L', gibZiel(waffentyp,disziplin)$L1000),
    gibProzentZielerfuellung(waf_all, trendAnschlag, 'L/250', gibZiel(waffentyp,disziplin)$L250),
    gibProzentZielerfuellung(waf_all, trendAnschlag, '10a0', gibZiel(waffentyp,disziplin)$P10a0),
    gibProzentZielerfuellung(waf_all, trendAnschlag, '10a5', gibZiel(waffentyp,disziplin)$P10a5),
    gibProzentZielerfuellung(waf_all, trendAnschlag, '10.0', gibZiel(waffentyp,disziplin)$P10_0),
    gibProzentZielerfuellung(waf_all, trendAnschlag, '10.5', gibZiel(waffentyp,disziplin)$P10_5),
    gibProzentZielerfuellung(waf_all, trendAnschlag, 'Abstand', gibZiel(waffentyp,disziplin)$Abstand),
    #gibProzentZielerfuellung(waf_all, trendAnschlag, 'Zeit', gibZiel(waffentyp,disziplin)$Zeit),
    gibProzentZielerfuellung(waf_all, trendAnschlag, 'Resultat', gibZiel(waffentyp,disziplin)$Ringe)
    
  )
  
  maxval = 100
  minval = 00
  data  = as.data.frame(t(matrix(c)))
  data = rbind(rep(maxval,length(c)) , rep(minval,length(c)) , data)
  colnames(data) = c("L" , "L/250" , "10a0" , "10a5" , "10.0", "10.5", "Abstand" , "Ergebnis")
  
  return(data)
  }


simplifiedScattReport <-
  function(waffentyp, disziplin)
  {
    sourcefile = 'rohdaten/SCATT_Protokoll.csv'
    fokusfile = 'rohdaten/SCATT_Fokus.csv'
    WKProtokolFile = 'rohdaten/WK_Protokoll.csv'
    reportFile = 'report/scatt.Rmd'
    outputFile = paste('output/',tolower(substr(waffentyp,1,1)),'w',tolower(substr(disziplin,1,1)),'.html', sep='')
    CreateScattReport(sourcefile, fokusfile, WKProtokolFile, reportFile, outputFile,waffentyp, disziplin)
  }
    