library("XML")
library("methods")
library("HTML")
library(digest)
library(shiny)


con = newXMLNode('div')
newXMLNode(parent = con, 'h1', "2018-01-30: 25m Speed – Hammerli XEsse Amazon RedDot")
newXMLNode(parent = con, 'h2', "Individuelle Notizen")
newXMLNode(parent = con, 'h2', "Überblick – Schussgruppen")
newXMLNode(parent = con, 'h3', "Ohne Visier-Korrektur")
newXMLNode(parent = con, 'h3', "Nach angenommener Korrektur des Visiers")
newXMLNode(parent = con, 'h3', "Nach angenommener Korrektur des Visiers")


con = div(
  h1("2018-01-30: 25m Speed – Hammerli XEsse Amazon RedDot"),
  h2("Individuelle Notizen"),
  h2("Überblick – Schussgruppen"),
  h3("Ohne Visier-Korrektur"),
  h3("Nach angenommener Korrektur des Visiers"),
  div('')
)
class(content)
newXMLNode('en-note',content)

foo = buildSpeedSummaryTable(pointGroups, weaponConfig, reportConfig)
htmlTable::htmlTable(foo, useViewer=FALSE)
bar


as.character(content)
class(en_export)
en_export

con

########################################
source('R/evernote.R', encoding = 'UTF-8')

#create exportWrapper
en_export = createEnExport(title = 'title', author = 'author', source = 'desktop.win', sourceUrl =  'file://foo.csv', content = con)
doc = createEvernoteDoc(en_export)
doc
saveEvernoteDoc('evernote.enex', doc = doc, doctype = createEvernoteDocType())
######################
#Doctype-Test
doctype = Doctype(name = 'en-export', system = 'http://xml.evernote.com/pub/evernote-export2.dtd')
is(doctype, "Doctype")
xmlName(xmlRoot(doc))
doc = newXMLDoc(dtd = 'http://xml.evernote.com/pub/evernote-export2.dtd', namespaces = NULL, addFinalizer = TRUE, 
                name = 'en-export', node = con, isHTML = FALSE)
saveXML(doc, doctype = doctype, indent=TRUE)
doc
