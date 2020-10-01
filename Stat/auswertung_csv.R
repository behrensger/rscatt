#Auswertung CSV

setwd('C:\\Users\\Andrey.Behrens\\Desktop\\Stat\\') #set directory
setwd('/Users/abehrens/Desktop/Stat') #set directory

sourcefile = 'rohdaten/SCATT_Protokoll.csv'
fokusfile = 'rohdaten/SCATT_Fokus.csv'
WKProtokolFile = 'rohdaten/WK_Protokoll.csv'


source("R/AllReports.R", encoding = 'UTF-8')
simplifiedScattReport('KW', 'Speed')
simplifiedScattReport('KW', 'Präzision')
simplifiedScattReport('LW', 'Speed')
simplifiedScattReport('LW', 'Präzision')
simplifiedScattReport('SLF', 'Speed')
simplifiedScattReport('SLF', 'Präzision')
CreateWettkampfReport(WKProtokolFile, fokusfile, 'report/wk_statistik.Rmd', 'output/wk.html',)
ScattUnitTest(fokusfile)


scatt = loadScattDevCsv(sourcefile)
TrainingsFokus = LoadScattFokusCsv(fokusfile)
WKProtokol = LoadWKProtokollCsv(WKProtokolFile)
waffentyp = 'KW'
disziplin = 'Präzision'
visier = 'Offen'
entfernung = '25'
waf_all = createWaf_All(scatt, waffentyp, disziplin, visier, entfernung)
trendAnschlag = getTrendAnschlag(waf_all)
scatt
Sys.setenv(TZ="Europe/Berlin") 

MeinTest <- function()
{
  for (i in 1:100) {
    WKProtokol = LoadWKProtokollCsv(WKProtokolFile)  
    #print(i)
  }
  
}
MeinTest()
system.time(MeinTest()) 

source('R/ScattWKProtokol.R')
WKProtokol = LoadWKProtokollCsv(WKProtokolFile)  
apply(as.matrix(WKProtokol$Ringe), 1, assertInteger(lower = 0, upper = 400))
      
as.matrix(WKProtokol$Ringe)
source("R/ScattDevReport.R", encoding = 'UTF-8')
WKProtokol = LoadWKProtokollCsv(WKProtokolFile)
View(WKProtokol)
    
x=-2.5
y=-2.7

x+3-y
5-x+y
-x+y+8
-6+x-y-x

as.Date("2018-01-01")

waf_all = subset(scatt, Waffentyp %in% waffentyp)
subset(scatt, Datum >= as.Date("2018-01-01") & Datum <= as.Date("2018-12-31"))
scatt

foo = data.frame(format(scatt$Datum, format = '%Y'), gsub('SCATT', 'DRY',scatt$Trainingstyp), scatt$Schuss, scatt$Waffe)
colnames(foo)=c('Datum', 'Typ', 'Schuss', 'Waffe')
table(foo$Schuss, foo$Waffe)
tabulate(foo)
foo
summary(foo)
format(scatt$Datum, format = '%Y')
aggregate(x = foo, by = list(Waffen), FUN = 'sum')



