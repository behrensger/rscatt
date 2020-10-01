

setwd('/Users/abehrens/Desktop/Stat') #set directory
setwd('C:\\Users\\Andrey.Behrens\\Desktop\\Stat\\') #set directory
source("R/AllReports.R", encoding = 'UTF-8')


simpleBatchReport('xesse_eisen','25m_Prae', 5)
simpleBatchReport('xesse_eisen','25m_Prae', 10)
simpleBatchReport('xesse_eisen','25m_Prae', 15)
simpleBatchReport('xesse_eisen','25m_Prae', 25, 1)

simpleBatchReport('ppq_rd','25m_Prae', 5)
simpleBatchReport('ppq_rd','25m_Prae', 10)
simpleBatchReport('ppq_rd','25m_Prae', 25, 1)

simpleBatchReport('hps_eisen','25m_Prae', 5)
simpleBatchReport('hps_eisen','25m_Prae', 10)
simpleBatchReport('hps_eisen','25m_Prae', 15)
simpleBatchReport('hps_eisen','25m_Prae', 20)
simpleBatchReport('hps_eisen','25m_Prae', 25, 8)
simpleBatchReport('hps_eisen','25m_Prae', 25,1)



simpleBatchReport('xesse_eisen','25m_speed_kw', 25, 1)
simpleBatchReport('hps_eisen','25m_speed_kw', 25, 1)
simpleBatchReport('ppq_eisen','25m_speed_kw', 25, 1)

simpleBatchReport('ruger_sf','25m_speed_lw')
simpleBatchReport('ruger_sf','25m_Prae')
simpleBatchReport('ruger_sf','25m_Prae',25,3)
simpleBatchReport('f12_rd','25m_speed_lw')
simpleBatchReport('dar15_zf','25m_speed_lw',25,2)
simpleBatchReport('dar15_zf','25m_Prae',25,2)
simpleBatchReport('dar15_zf','100m_Prae_ZF',100,1)
simpleBatchReport('f12_rd','25m_Prae')



foo = createReport(	PointDataFile = 's9mm.csv',
              PointDataConfigFile = 'config/pointdata_ot2.yaml',
              #WeaponConfigFile = 'config/conf_xesse_rd.yaml',
              WeaponConfigFile = 'config/conf_ppq_eisen.yaml',
              #WeaponConfigFile = 'config/conf_xesse_eisen.yaml',
              #ReportConfigFile = 'config/disziplin_10m_LG.yaml',
              #ReportConfigFile = 'config/disziplin_25m_Prae.yaml',
              #ReportConfigFile = 'config/disziplin_25m_speed_lw.yaml',
              #ReportConfigFile = 'config/disziplin_25m_speed_kw.yaml',
              ReportConfigFile = 'config/disziplin_25m_fallscheibe.yaml',
              #ReportConfigFile = 'config/disziplin_md_kw.yaml',
              #ReportConfigFile = 'config/disziplin_md_lw.yaml',
              TrainingDate = Sys.Date() - 1,
              IndividualComments = '',
              HtmlSelfContained = TRUE
)



#Execute Batch Processing
batchCreateReports( PointDataConfigFile = 'config/pointdata_ot2.yaml',
                    WeaponConfigFile = 'config/conf_ruger_sf.yaml',
                    #WeaponConfigFile = 'config/conf_ruger_air_rifle.yaml',
                    #WeaponConfigFile = 'config/conf_f12_rd.yaml',
                    #WeaponConfigFile = 'config/conf_ppq_eisen.yaml',
                    #WeaponConfigFile = 'config/conf_xesse_rd.yaml',
                    #WeaponConfigFile = 'config/conf_xesse_eisen.yaml',
                    #WeaponConfigFile = 'config/conf_xesse_air_pistol.yaml',
                    #WeaponConfigFile = 'config/conf_hps_eisen.yaml',
                    RecentFiles = 3,
                    PointDataDir = '/Users/abehrens/Downloads',
                    #ReportConfigFile = 'config/disziplin_10m_LG.yaml',
                    #ReportConfigFile = 'config/disziplin_10m_Pistole.yaml',
                    #ReportConfigFile = 'config/disziplin_25m_Prae.yaml',
                    #ReportConfigFile = 'config/disziplin_25m_fallscheibe.yaml',
                    #ReportConfigFile = 'config/disziplin_25m_fallscheibe.yaml',
                    #ReportConfigFile = 'config/disziplin_25m_speed_kw.yaml',
                    #ReportConfigFile = 'config/disziplin_25m_speed_lw.yaml',
                    #ReportConfigFile = 'config/disziplin_md_kw.yaml',
                    ReportConfigFile = 'config/disziplin_md_lw.yaml',              
                    TrainingDate = Sys.Date() ,
                    #ScaledDistance = 10,
                    IndividualComments = ''
)


#################################
#Open GUI
runGUI("analyze")



#################################
#Some Tests
pointDataConfig = loadYamlConfig('config/pointdata_ot2.yaml')
weaponConfig = loadYamlConfig('config/conf_xesse_eisen.yaml')
reportConfig = loadYamlConfig('config/disziplin_25m_Prae.yaml')
PointDataFile = 'x5v5.csv'
pointGroups =  loadPointData('/Users/abehrens/Downloads/ich 06-11-18 00-21-34.csv',pointDataType = pointDataConfig$pointData.Type)

xy = getXYMatrixCorrected(pointGroups, pointDataConfig, weaponConfig, reportConfig)

simRingCount_BDSSpeed(xy, speedTarget,9)
buildSpeedSummaryTable(pointGroups, pointDataConfig, weaponConfig, reportConfig, NULL)
simRingCount(xy, target = speedTarget, caliber = 9, 
             unit = 'mm') 

x = xy[,'x']
y = xy[,'y']
xd = mean(x)
sd(x)
sd(x)/sqrt(length(x))
median(y)
sd(y)
sd(y)/sqrt(length(y))
boxplot(y)


ms_values = c(9.05,9.04,9.05,9.05,9.06,9.05,9.04,9.04,9.05,9.04,9.05,9.04,9.05,9.05,9.04,9.04,9.05,9.04,9.05,9.04,9.04,9.04,9.05,9.04,9.05,9.04,9.04,9.05,9.05,9.04,9.05)
mm = 2
c(subset(x, x > (xd + sd(x) * mm)), subset(x, x < (xd - sd(x) * mm)))
source('R/ReportStandard.R')
gibStandardZahlen(ms_values)
as.list(x)
as.double(as.list(x))
subset(x, x > (0))
x

ergebnis = c()
ergebnis$foo = 0
ergebnis
source("R/TargetCreateReport.R", encoding = 'UTF-8')

buildFallscheibeSummaryTable(pointGroups, pointDataConfig, weaponConfig, reportConfig, 'KK', NULL)
aggregate(pointGroups, list(pointGroups$group), nrow)
scatt

weaponConfig$goal.speed.time

goal.speed.time
scatt$Waffe


reportConfig

reportConfig$report.Robust

if (reportConfig$report.Robust == TRUE) {print('ein')} else {print('kein')}

round(10.12, 1)

geschosse <- read_delim("reloading/rohdaten/geschosse.csv", 
                        ";", escape_double = FALSE, col_types = cols(Datum = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE)
geschosse$Gewicht
geschosse$Durchmesser
geschosse$Laenge


kable(rcorr(as.matrix(geschosse[3:5]), type = "pearson")$r)
corr$P


round_any(135, 9)

conv_unit(1, 'yd', 'm')


source("R/AllReports.R", encoding = 'UTF-8')
config = loadYamlConfig('config/conf_xesse_eisen.yaml')
weaponConfig