#
library(checkmate)
library(testthat)
library(base64enc)

ZuordnungWaffentypName <-
  function(waffentyp) {
    
    #assertChoice(waffentyp, ASSERT_CHOICE_GUN_TYPE)
    
    if ('KW' %in% waffentyp)
      return('Kurzwaffe')
    else
      return('Langwaffe')
  }



ZuordnungDisziplin2Id <- function(x, ...)
  UseMethod("ZuordnungDisziplin2Id")


ZuordnungDisziplin2Id.character <-
  function(disziplinen) {
    assertCharacter(disziplinen)
    
    for (i in 1:length(disziplinen)) {
      disziplinen[i] = ZuordnungDisziplin2Id.default(disziplinen[i])
    }
    
    return(disziplinen)
    
  }



ZuordnungDisziplin2Id.default <-
  function(disziplin) {
    FOO_CHOICE = c(ASSERT_CHOICE_DISZIPLIN, 'Sonstiges')
    assertChoice(disziplin, FOO_CHOICE)

    if (1 == 2)
      return(8)
    else if (disziplin == 'Fallscheibe/KK')
      return(15)
    else if (disziplin == 'Fallscheibe')
      return(15)
    else if (disziplin == 'IPSC')
      return(17)
    else if (disziplin == 'Präzision')
      return(18)
    else if (disziplin == 'Speed')
      return(19)
    else if (disziplin == 'Zielfernrohr')
      return(20)
    else
      return(8)
    
  }




ZuordnungWaffe2KwLw <- function(x, ...)
  UseMethod("ZuordnungWaffe2KwLw")


ZuordnungWaffe2KwLw.character <-
  function(waffenliste) {
    assertCharacter(waffenliste)
    
    for (i in 1:length(waffenliste)) {
      waffenliste[i] = ZuordnungWaffe2KwLw.default(waffenliste[i])
    }
    
    return(waffenliste)
    
  }

ZuordnungWaffe2KwLw.default <-
  function(waffe) {
    assertChoice(waffe, ASSERT_CHOICE_GUN)
    
    if (waffe %in% ASSERT_CHOICE_LW) {
      return('LW')
    } else if (waffe %in% ASSERT_CHOICE_KW) {
      return('KW')
    } else if (waffe %in% ASSERT_CHOICE_SLF) {
      return('SLF')
    } else {
      stop(c('cannot find waffe', waffe))
    }
  }

ZuordnungKwLw2Waffe <-
  function(waffentyp) {
    assertChoice(waffentyp, ASSERT_CHOICE_GUN_TYPE)
    
    if (waffentyp == 'LW') {
      return(ASSERT_CHOICE_LW)
    } else if (waffentyp == 'KW') {
      return(ASSERT_CHOICE_KW)
    } else {
      return(ASSERT_CHOICE_SLF)
    }
  }



ZuordnungWaffe2Color <-
  function(waffe) {
    assertChoice(waffe, ASSERT_CHOICE_GUN)
    
    if (waffe == 'Xesse')
      return('#088A85')
    else if (waffe == 'PPQ')
      return('#868A08')
    else if (waffe == 'LuPi')
      return('#868A08')
    else if (waffe == 'HPS')
      return('#6600ff')
    else if (waffe == 'Ruger')
      return('#0073e6')
    else if (waffe == 'Triarii')
      return('#868A08')
    else if (waffe == 'DAR15')
      return('#868A08')
    else if (waffe == 'F12')
      return('#009999')
    return(NULL)
  }


GibFarbkodiertenKreis <-
  function(waffe) {
    assertChoice(waffe, ASSERT_CHOICE_GUN)
    
    farbe = ZuordnungWaffe2Color(waffe)
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
      bg = farbe,
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
    
    a = paste('<img title = "Farbcode ',
              waffe,
              '" width="',
              dim,
              '" height="',
              dim,
              '"',
              sep = '')
    b = paste('alt="Farbcode', waffe, '"')
    c = paste('src="data:image/png;base64,', base, sep = '')
    d = paste('" /> ')
    
    result = paste(a, b, c, d)
    return(result)
  }


GibFarbkodiertesSymbol <-
  function(symbol, farbe = FG_COLOR) {
    #assertNumeric(symbol, lower = 0, upper = 25, .var.name = 'Symbol-Nummer')
    assertString(farbe,  .var.name = 'Farbcode')
    
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
      pch = symbol,
      cex = dim / 10,
      col = farbe,
      #bg = farbe,
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
    
    a = paste('<img title = "Symbolcode ',
              symbol,
              '" width="',
              dim,
              '" height="',
              dim,
              '"',
              sep = '')
    b = paste('alt="Farbcode', symbol, '"')
    c = paste('src="data:image/png;base64,', base, sep = '')
    d = paste('" /> ')
    
    result = paste(a, b, c, d)
    return(result)
  }



ScattKwLwZuordnungUnitTest <- function() {
  test_that("Test ZuordnungWaffe2KwLw", {
    expect_error(ZuordnungWaffe2KwLw())
    expect_error(ZuordnungWaffe2KwLw(NULL))
    expect_error(ZuordnungWaffe2KwLw('Kein Sinnvoller Wert'))
    
    expect_equal(ZuordnungWaffe2KwLw('PPQ'), 'KW')
    expect_equal(ZuordnungWaffe2KwLw('Ruger'), 'LW')
  })
  
  
  test_that("Test ZuordnungKwLw2Waffe", {
    expect_error(ZuordnungKwLw2Waffe())
    expect_error(ZuordnungKwLw2Waffe(NULL))
    expect_error(ZuordnungKwLw2Waffe('xix'))
    
    expect_equal(ZuordnungKwLw2Waffe('LW'), ASSERT_CHOICE_LW)
    expect_equal(ZuordnungKwLw2Waffe('KW'), ASSERT_CHOICE_KW)
  })
  
  
  test_that("Test ZuordnungWaffe2KwLw", {
    expect_error(ZuordnungWaffe2KwLw())
    expect_error(ZuordnungWaffe2KwLw(NULL))
    expect_error(ZuordnungWaffe2KwLw('Kein Sinnvoller Wert'))
    
    expect_equal(ZuordnungWaffe2KwLw(c("PPQ", "Ruger")), c('KW', 'LW'))
  })
  
  test_that("Test ZuordnungWaffentypName", {
    expect_error(ZuordnungWaffentypName())
    #expect_error(ZuordnungWaffentypName(NULL))
    #expect_error(ZuordnungWaffentypName('Kein Sinnvoller Wert'))
    
    expect_equal(ZuordnungWaffentypName(c('LW')), 'Langwaffe')
    expect_equal(ZuordnungWaffentypName(c('KW')), 'Kurzwaffe')
  })
}

ScattKwLwZuordnungUnitTest()