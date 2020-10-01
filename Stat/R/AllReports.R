

library(GGally)
library(Hmisc)
library(shotGroups)
library(yaml)
library(rmarkdown)
library(markdown)

source("r/TargetCreateReport.R", encoding = 'UTF-8')
source("R/ScattDevReport.R", encoding = 'UTF-8')


#simpleBatchReport('xesse_eisen','25m_Prae', 25)
simpleBatchReport <-
  function(waffe,
           diszplin,
           ScaledDistance = NULL,
           RecentFiles = 1)
  {
    assertString(waffe)
    assertString(diszplin)
    assertNumeric(ScaledDistance, null.ok = TRUE)
    assertNumeric(RecentFiles, null.ok = TRUE)
    PointDataConfigFile = 'config/pointdata_ot2.yaml'
    WeaponConfigFile = paste('config/conf_', waffe, '.yaml', sep = '')
    PointDataDir = 'xxx'
    ReportConfigFile = paste('config/disziplin_', diszplin, '.yaml', sep = '')
    IndividualComments = ''
    
    
    if(PointDataDir == 'xxx') {
      switch(Sys.info()[['sysname']],
             Windows = {PointDataDir = 'C:/Users/Andrey.Behrens/Desktop/Stat/input'},
             Linux  = {PointDataDir = '/Users/abehrens/Downloads'},
             Darwin = {PointDataDir = '/Users/abehrens/Downloads'})
      
    }
    
    
    
    
    batchCreateReports (
      PointDataConfigFile = PointDataConfigFile,
      WeaponConfigFile = WeaponConfigFile,
      ReportConfigFile = ReportConfigFile,
      RecentFiles = RecentFiles,
      PointDataDir = PointDataDir,
      TrainingDate = Sys.Date(),
      TrainingComments = NULL,
      IndividualComments = '',
      ScaledDistance = ScaledDistance,
      HtmlSelfContained = TRUE
    )
  }
