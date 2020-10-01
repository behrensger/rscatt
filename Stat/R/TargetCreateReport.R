library('shotGroups')
library('yaml')
library('knitr')
library('measurements')
library('robustbase')
library('rmarkdown')
library(checkmate)

source("R/TargetSupportReport.R", encoding = 'UTF-8')
source("R/TargetReportSpeed.R", encoding = 'UTF-8')
source("R/TargetReportPrae.R", encoding = 'UTF-8')
source("R/TargetReportFallscheibe.R", encoding = 'UTF-8')
source("R/TargetConversions.R", encoding = 'UTF-8')
source("R/TargetDrawGroupColour.R", encoding = 'UTF-8')
source("R/TargetGetGroupColourCode.R", encoding = 'UTF-8')
source('R/ReportStandard.R', encoding = 'UTF-8')

loadYamlConfig <-
  function(configFile)
  {
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    configFile = normalizePath(configFile)
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    
    connection = file(
      description = configFile,
      open = "r",
      blocking = FALSE,
      encoding = "UTF-8"
    )
    
    config = yaml.load_file(connection)
    close(connection)
    
    return(config)
    
  }


loadWeaponConfigFile <-
  function(configFile) {
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    configFile = normalizePath(configFile)
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    
    config = loadYamlConfig(configFile)
    
    
    assertString(config$weapon, min.chars = 3, .var.name = 'weapon: Name der Waffenkonfiguration')
    assertNumeric(
      config$caliber,
      lower = 0,
      upper = 999,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'caliber: Kaliberangabe'
    )
    assertChoice(config$caliberUnit, ASSERT_CHOICE_CALIBER_UNIT, .var.name = 'caliberUnit: Maßeinheit des Kalibers')
    assertNumeric(
      config$click.x,
      lower = 0,
      upper = 999,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'click.x: Visierverstellung'
    )
    assertNumeric(
      config$click.y,
      lower = 0,
      upper = 999,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'click.y: Visierverstellung'
    )
    assertChoice(config$click.Unit, ASSERT_CHOICE_CALIBER_UNIT, .var.name = 'caliber.Unit: Maßeinheit des Kalibers')
    assertNumeric(
      config$click.Distance,
      lower = 0,
      upper = 9999,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'click.Distance: Visierverstellung'
    )
    assertChoice(config$click.DistanceUnit,
                 ASSERT_CHOICE_DISTANCE_UNIT,
                 .var.name = 'click.DistanceUnit: Visierverstellung')
    assertNumeric(
      config$goal.plate.time,
      lower = 20,
      upper = 999,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'goal.plate.time: Zielzeit bei Fallscheibe'
    )
    assertNumeric(
      config$goal.speed.time,
      lower = 30,
      upper = 180,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'goal.speed.time: Zielzeit bei Speed'
    )
    assertNumeric(
      config$goal.speed.rings,
      lower = 100,
      upper = 300,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'goal.speed.rings: Zielsetzung bei Speed-Ringen'
    )
    assertNumeric(
      config$goal.precision,
      lower = 100,
      upper = 200,
      any.missing = FALSE,
      all.missing = FALSE,
      .var.name = 'goal.precision: Zielsetzung bei Präzision'
    )
    assertChoice(config$fallscheibe.type,
                 ASSERT_CHOICE_FALLSCHEIBE_TYPE,
                 .var.name = 'fallscheibe.type: Fallscheiben-Typ')
    
    
    return(config)
  }


loadPointDataConfigFile <-
  function(configFile) {
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    config = loadYamlConfig(configFile)
    
    assertDirectory(config$pointData.Path, .var.name = 'loadPointDataConfigFile: pointData.Path')
    assertString(config$pointData.Pattern,
                 null.ok = FALSE,
                 .var.name = 'loadPointDataConfigFile: pointData.Pattern')
    assertString(
      config$pointData.Type,
      null.ok = FALSE,
      pattern = 'OT2',
      .var.name = 'loadPointDataConfigFile: pointData.Type'
    )
    assertLogical(config$pointData.XyTopLeft,
                  null.ok = FALSE,
                  .var.name = 'loadPointDataConfigFile: pointData.XyTopLeft')
    assertLogical(config$pointData.RelPOA,
                  null.ok = FALSE,
                  .var.name = 'loadPointDataConfigFile: pointData.RelPOA')
    assertString(
      config$pointData.Unit,
      null.ok = FALSE,
      pattern = 'cm|mm|inch',
      .var.name = 'loadPointDataConfigFile: pointData.Unit'
    )
    
    
    return(config)
  }

loadReportConfigFile <-
  function(configFile) {
    assertFileExists(configFile, access = 'r',  extension = 'yaml')
    config = loadYamlConfig(configFile)
    
    assertFileExists(config$report.File, .var.name = 'loadReportConfigFile: report.File')
    assertDirectory(config$report.OutputDir, .var.name = 'loadReportConfigFile: report.OutputDir')
    assertString(config$report.description,
                 null.ok = FALSE,
                 .var.name = 'loadReportConfigFile: report.description')
    assertString(config$report.Target,
                 null.ok = FALSE,
                 .var.name = 'loadReportConfigFile: report.Target')
    #assertNumeric(config$report.Distance, null.ok = TRUE, lower = 0, .var.name = 'loadReportConfigFile: report.Distance')
    assertString(
      config$report.DistanceUnit,
      null.ok = FALSE,
      pattern = 'm|yd',
      .var.name = 'loadReportConfigFile: report.DistanceUnit'
    )
    assertLogical(config$report.Robust,
                  null.ok = FALSE,
                  .var.name = 'loadReportConfigFile: report.Robust')
    assertString(
      config$report.TargetPlotUnit,
      null.ok = FALSE,
      pattern = 'cm|mm|inch',
      .var.name = 'loadReportConfigFile: report.TargetPlotUnit'
    )
    
    
    return(config)
  }




createReport <-
  function(PointDataFile ,
           PointDataConfigFile,
           WeaponConfigFile,
           ReportConfigFile,
           PointDataDir = NULL,
           TrainingDate = Sys.Date(),
           TrainingComments = NULL,
           IndividualComments = '',
           ScaledDistance = NULL,
           HtmlSelfContained = TRUE)
  {
    if (!is.null(PointDataDir))
      assertDirectoryExists(PointDataDir, access = 'r')
    assertFileExists(PointDataConfigFile,
                     access = 'r',
                     extension = 'yaml')
    assertFileExists(WeaponConfigFile, access = 'r',  extension = 'yaml')
    assertFileExists(ReportConfigFile, access = 'r',  extension = 'yaml')
    assertDate(TrainingDate, lower = Sys.Date() - 1000, upper = Sys.Date())
    assertString(TrainingComments, null.ok = TRUE)
    assertString(IndividualComments, null.ok = TRUE)
    assertLogical(HtmlSelfContained, null.ok = FALSE)
    assertNumeric(ScaledDistance, null.ok = TRUE)
    
    #load Configs
    pointDataConfig = loadPointDataConfigFile(PointDataConfigFile)
    weaponConfig = loadWeaponConfigFile(WeaponConfigFile)
    reportConfig = loadReportConfigFile(ReportConfigFile)
    
    
    #TrainingDate
    TrainingDateString = format(TrainingDate, "%Y-%m-%d")
    
    #load PointData and rescale pointdata
    if (is.null(PointDataDir))
      pointDataConfig$pointData.Path = normalizePath(pointDataConfig$pointData.Path)
    else
      pointDataConfig$pointData.Path = normalizePath(PointDataDir)
    assertDirectoryExists(pointDataConfig$pointData.Path, access = 'r')
    
    pointGroups =  loadPointData(
      paste(pointDataConfig$pointData.Path, PointDataFile, sep = '/'),
      pointDataType = pointDataConfig$pointData.Type
    )
    reportConfig$OriginalDistance = median(pointGroups[, 'distance'])
    reportConfig$ScaledDistance = reportConfig$OriginalDistance
    if (!is.null(ScaledDistance)
        && is.numeric(ScaledDistance)
        && (ScaledDistance >= 1)) {
      pointGroups = rescalePointGroup(pointGroups, ScaledDistance)
      reportConfig$ScaledDistance = reportConfig$OriginalDistance
    } else if (!is.null(reportConfig$report.Distance)
               && is.numeric(reportConfig$report.Distance)
               && (reportConfig$report.Distance >= 1)) {
      pointGroups = rescalePointGroup(pointGroups, reportConfig$report.Distance)
      reportConfig$ScaledDistance = reportConfig$report.Distance
    }
    
    
    #render report
    outputFile = tools::file_path_sans_ext(basename(PointDataFile))
    outputFile = paste(reportConfig$report.OutputDir,
                       '/',
                       outputFile,
                       '.html',
                       sep = '')
    
    fig_dim = 7
    render(
      reportConfig$report.File,
      output_format = html_document(
        toc = FALSE,
        number_sections = FALSE,
        section_divs = FALSE,
        self_contained = HtmlSelfContained,
        smart = TRUE,
        theme = NULL,
        highlight = NULL,
        mathjax = NULL,
        fig_width = fig_dim,
        fig_height = fig_dim,
        fig_retina = 1,
        fig_caption = FALSE,
        #dev = 'svgz'
        dev = 'png'
      ),
      output_file = basename(outputFile),
      output_dir = dirname(outputFile),
      quiet = TRUE,
      encoding = 'UTF-8'
      
    )
    
    return(outputFile)
  }


batchCreateReports <-
  function(PointDataConfigFile,
           WeaponConfigFile,
           ReportConfigFile,
           RecentFiles = 1,
           PointDataDir = NULL,
           TrainingDate = Sys.Date(),
           TrainingComments = NULL,
           IndividualComments = '',
           ScaledDistance = NULL,
           HtmlSelfContained = TRUE)
  {
    if (!is.null(PointDataDir))
      assertDirectoryExists(PointDataDir, access = 'r')
    assertFileExists(PointDataConfigFile,
                     access = 'r',
                     extension = 'yaml')
    assertFileExists(WeaponConfigFile, access = 'r',  extension = 'yaml')
    assertFileExists(ReportConfigFile, access = 'r',  extension = 'yaml')
    assertNumeric(RecentFiles, lower = 1, null.ok = FALSE)
    assertDate(TrainingDate, lower = Sys.Date() - 1000, upper = Sys.Date())
    assertString(TrainingComments, null.ok = TRUE)
    assertString(IndividualComments, null.ok = TRUE)
    assertLogical(HtmlSelfContained, null.ok = FALSE)
    assertNumeric(ScaledDistance, null.ok = TRUE)
    
    pointDataConfig = loadYamlConfig(PointDataConfigFile)
    
    details = NULL
    path = NULL
    if (is.null(PointDataDir)) {
      path = normalizePath(pointDataConfig$pointData.Path)
    } else {
      path = normalizePath(PointDataDir)
    }
    
    details = file.info(
      list.files(
        path,
        pattern = pointDataConfig$pointData.Pattern,
        ignore.case = TRUE,
        full.names = TRUE
      )
    )
    
    
    details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)),]
    
    num = 1
    
    for (datei in rownames(details)) {
      datei = normalizePath(datei)
      if (!is.null(RecentFiles) && num > RecentFiles) {
        #process the latest only
        next
      } else  {
        print(paste("Process ", num, ". file: ", datei, sep = ''))
        num = num + 1
      }
      
      createReport(
        PointDataFile = basename(datei),
        PointDataDir = dirname(datei),
        PointDataConfigFile = PointDataConfigFile,
        WeaponConfigFile = WeaponConfigFile,
        ReportConfigFile = ReportConfigFile,
        TrainingDate = TrainingDate,
        TrainingComments = TrainingComments,
        ScaledDistance = ScaledDistance,
        IndividualComments = IndividualComments,
        HtmlSelfContained = HtmlSelfContained
      )
      
    }
  }
