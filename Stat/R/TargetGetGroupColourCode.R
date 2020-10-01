library("grDevices")
library('base64enc', verbose = FALSE)
library('checkmate')

get_GroupColourCode <-
  function(groupno, alpha) {
    assertNumeric(groupno, lower = 1)
    assertNumeric(alpha, lower = 0, upper = 1)
    
    #initial checks
    
    colorval = 0.60
    
    foo =   groupno %% 6
    if (foo == 0) {
      return(rgb(colorval, colorval, colorval, alpha))
    } else if (foo == 1) {
      return(rgb(colorval, 1, 1, alpha))
    } else if (foo == 2) {
      return(rgb(1, colorval, 1, alpha))
    } else if (foo == 3) {
      return(rgb(1, 1, colorval, alpha))
    } else if (foo == 4) {
      return(rgb(colorval, 1, colorval, alpha))
    } else if (foo == 5) {
      return(rgb(colorval, colorval, 1, alpha))
    } else {
      return(rgb(1, colorval, colorval, alpha))
    }
  }

get_png_circle <-
  function(colorno) {
    assertNumeric(colorno)
    
    # create a temp file
    tempFile = tempfile(pattern = "file",
                        tmpdir = tempdir(),
                        fileext = ".png")
    dim = 15
    
    #draw an circle and store it in the file
    #plot.new()
    png(
      filename = tempFile,
      bg = "transparent",
      width = dim,
      height = dim
    )
    par(mar = c(0, 0, 0, 0))
    plot(
      0,
      0,
      pch = 21,
      cex = dim / 6,
      col = "black",
      bg = get_GroupColourCode(colorno, 1),
      axes = FALSE,
      mar = c(0, 0, 0, 0),
      ann = FALSE
    )
    dev.off()
    
    #build the result string
    base = base64encode(tempFile, linewidth = NULL)
    
    #remove the temp file
    if (file.exists(tempFile))
      file.remove(tempFile)
    
    a = paste('<img title = "Color for ',
              colorno,
              '. Group" width="',
              dim,
              '" height="',
              dim,
              '"',
              sep = '')
    b = paste('alt="Embedded Image"')
    c = paste('src="data:image/png;base64,', base, sep = '')
    d = paste('" /> ')
    
    result = paste(a, b, c, d)
    return(result)
  }

get_svg_circle <-
  function(colorno) {
    assertInteger(colorno)
    
    #anf = '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20">'
    anf = '<svg width="20" height="20">'
    bod = paste(
      '<circle cx="9" cy="12" r="7" stroke="black" stroke-width="1.5" fill="',
      get_GroupColourCode(colorno, 1),
      '" />',
      sep = ''
    )
    end = '</svg>'
    return(paste(anf, bod, end, sep = ''))
  }

get_GroupColouredName <-
  function(groupno, graphicType = 1) {
    assertNumeric(graphicType, lower = 1, upper = 3)
    assertCharacter(groupno)
    
    if (!is.numeric(groupno)) {
      groupno = as.integer(groupno)
    }
    assertInteger(groupno, lower = 1)
    
    if (graphicType == 1) {
      a = get_png_circle(groupno)
      b = '<b>&nbsp;'
      c = groupno
      d = '. Group</b>'
      return(paste(a, b, c, d, sep = ''))
    } else if (graphicType == 2) {
      a = get_svg_circle(groupno)
      b = '<b>&nbsp;'
      c = groupno
      d = '. Group</b>'
      return(paste(a, b, c, d, sep = ''))
    } else if (graphicType == 3) {
      return(
        paste(
          '<span style="color: black; background-color: ',
          get_GroupColourCode(groupno, 1),
          ';-evernote-highlight:true;"><b>&nbsp;&#x25CB;&nbsp;</b></span><b>&nbsp;',
          groupno,
          '. Group</b>',
          '',
          sep = ''
        )
      )
    } else {
      stop(c('Graphic Type is valid for 1..3 only'))
    }
  }
