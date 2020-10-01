library("XML")
library("methods")


#evernote support XML stuff


createEnExport <-
  function(title, author, source, sourceUrl, content) {
    #the export frame
    en_export = newXMLNode('en-export',
                           attrs = c(
                             'export-date' = format(Sys.time(), "%Y%m%dT%H%M%SZ"),
                             application = 'Evernote/Windows',
                             version = '6.x'
                           ))
    note = newXMLNode('note', parent = en_export)
    newXMLNode('title', title, parent = note)
    newXMLNode('created', format(Sys.time(), "%Y%m%dT%H%M%SZ"), parent = note)
    newXMLNode('updated', format(Sys.time(), "%Y%m%dT%H%M%SZ"), parent = note)
    
    #some attributes
    noteattr = newXMLNode('note-attributes', parent = note)
    newXMLNode('author', author, parent = noteattr)
    newXMLNode('source', source, parent = noteattr)
    newXMLNode('source-url', sourceUrl, parent = noteattr)
    newXMLNode('source-application', 'evernote.win32', parent = noteattr)
    

    #a content node for notes
    cont_node = newXMLNode('content', parent = note)
    
    
    #a note itself
    note = newXMLNode('en-note')
    addChildren(note, content)
    
    noteDoc = newXMLDoc(dtd = "", namespaces = NULL, addFinalizer = TRUE, 
                    name = 'en-note', node = note, isHTML = FALSE)
    noteDoctype = Doctype(system = 'http://xml.evernote.com/pub/enml2.dtd')
    newXMLCDataNode(saveXML(noteDoc, doctype = noteDoctype, indent=FALSE), parent = cont_node)
    
    
    
    
    return(en_export)
    
  }

createEvernoteDoc <-
  function(enExport) {
    doc = newXMLDoc(dtd = "", namespaces = NULL, addFinalizer = TRUE, 
                    name = 'en-export', node = enExport, isHTML = FALSE)
    return(doc)
    
  }

createEvernoteDocType <-
  function() {
    doctype = Doctype(name = 'en-export', system = 'http://xml.evernote.com/pub/evernote-export2.dtd')
    return(doctype)
  }

saveEvernoteDoc <-
  function(filename, doc, doctype) {
    saveXML(doc, file = filename, doctype = doctype, indent=FALSE)

  }
