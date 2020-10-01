library('shotGroups', verbose = FALSE)
library('yaml', verbose = FALSE)
library('knitr', verbose = FALSE)
library('measurements', verbose = FALSE)
library('robustbase', verbose = FALSE)
library('rmarkdown', verbose = FALSE)
library(checkmate)




source("R/TargetConversions.R", encoding = 'UTF-8')
source("R/TargetDrawGroupColour.R", encoding = 'UTF-8')


## determine unit of (x,y)-coords from conversion string
getUnits <-
  function(x = "m2cm", first = TRUE) {
    assertString(x, null.ok = FALSE, fixed = '2')
    assertLogical(first, null.ok = FALSE)
    
    units    <-
      strsplit(x, "2")        # first and second part of string
    
    assertLogical(lengths(units) == 2L)
    if (!all(lengths(units) == 2L)) {
      # check that there are two parts
      stop("Unit not recognized - input must have form like m2cm")
    }
    
    knownUnits <-
      c("km",
        "m",
        "cm",
        "mm",
        "yd",
        "yard",
        "ft",
        "foot",
        "feet",
        "in",
        "inch")
    isKnown    <-
      vapply(units, function(x) {
        all(x %in% knownUnits)
      }, logical(1))
    if (!all(isKnown)) {
      stop(c(
        "Unit not recognized - needs to be one of\n",
        paste(knownUnits, collapse = " ")
      ))
    }
    
    ## replace feet with ft, yard with yd, inch with in
    replaceUnit <- function(x) {
      x <- gsub("yard", "yd", x)
      x <- gsub("foot", "ft", x)
      x <- gsub("feet", "ft", x)
      gsub("inch", "in", x)
    }
    
    units <- lapply(units, replaceUnit)
    if (first) {
      vapply(units, head, FUN.VALUE = character(1), n = 1)
    } else {
      vapply(units, tail, FUN.VALUE = character(1), n = 1)
    }
  }


loadPointData <-
  function(pointDataFile,
           pointDataType = 'OT2') {
    #check arguments
    assertFileExists(pointDataFile, access = 'r',  extension = 'csv')
    assertString(pointDataType, pattern = 'OT2')
    
    #seperate arguments
    bname = basename(pointDataFile)
    dame = dirname(pointDataFile)
    
      result = readDataOT2(
        fPath = dame,
        fNames = bname,
        combine = TRUE,
        fPat = ''
      )

    result = subset(result,
                    !(
                      point.x == 0
                      & point.y == 0
                      & aim.x == 0
                      & aim.y == 0
                      & center.x == 0
                      & center.y == 0
                    ))
    
    assertDataFrame(result, null.ok = FALSE)
    assertNumeric(nrow(result), lower = 1, null.ok = FALSE)
    assertNumeric(result[['distance']], lower = 1, null.ok = FALSE)
    assertNumeric(result[['aim.x']],  null.ok = FALSE)
    assertNumeric(result[['aim.y']],  null.ok = FALSE)
    assertNumeric(result[['center.x']], null.ok = FALSE)
    assertNumeric(result[['center.y']], null.ok = FALSE)
    assertNumeric(result[['point.x']],  null.ok = FALSE)
    assertNumeric(result[['point.y']], null.ok = FALSE)
    assertNumeric(result[['velocity']], lower = 0, null.ok = FALSE)
    assertFactor(result[['group']], null.ok = FALSE)
    
    for (x in unique(result$group)) {
      assertInteger(nrow(subset(result, group == x)), lower = 2, .var.name = paste('Test Group size for group', x))
    }
    
    
    return(result)
  }


calculateSightCorrection <-
  function(pointGroups,
           pointDataConfig,
           reportConfig,
           weaponConfig) {
    if (missing(pointGroups)) {
      stop("pointGroups argument required")
    }
    if (missing(reportConfig)) {
      stop("reportConfig argument required")
    }
    if (missing(weaponConfig)) {
      stop("weaponConfig argument required")
    }
    
    distance = median(pointGroups[, 'distance'])
    
    xymat = getXYmat(
      pointGroups,
      xyTopLeft = pointDataConfig$pointData.XyTopLeft,
      relPOA = pointDataConfig$pointData.RelPOA
    )
    
    report.UnitConversion <-
      paste(reportConfig$report.DistanceUnit,
            '2',
            pointDataConfig$pointData.Unit,
            sep = '')
    
    groupLoc <- groupLocation(
      xymat,
      level = 0.95,
      plots = FALSE,
      bootCI = c('basic'),
      dstTarget = distance,
      conversion = report.UnitConversion
    )
    
    if (reportConfig$report.Robust == TRUE) {
      click.corrections = groupLoc$ctrRob
    } else {
      click.corrections = groupLoc$ctr
    }
    
    
    #calculate the sight correction clicks
    click.result = click.corrections
    click.result['x'] = 0
    click.result['y'] = 0
    
    #get config
    click.unit = conv_unit(1,
                           weaponConfig$click.Unit,
                           pointDataConfig$pointData.Unit)
    distance.unit = conv_unit(1,
                              weaponConfig$click.DistanceUnit,
                              reportConfig$report.DistanceUnit)
    click.distance = weaponConfig$click.Distance * distance.unit
    
    
    #get click-value
    #0.25 * clickFator * 9/10)
    click.x = weaponConfig$click.x * click.unit / click.distance * distance
    click.y = weaponConfig$click.y * click.unit / click.distance * distance
    
    if (is.nan(click.x) || is.infinite(click.x)) {
      click.x = 0
    }
    if (is.nan(click.y) || is.infinite(click.y)) {
      click.y = 0
    }
    
    #calc clicks
    click.result['x'] = click.corrections['x'] / click.x
    click.result['y'] = click.corrections['y'] / click.y
    if (is.nan(click.result['x']) ||
        is.infinite(click.result['x']))
      click.result['x'] = 0
    if (is.nan(click.result['y']) ||
        is.infinite(click.result['y']))
      click.result['y'] = 0
    
    click.result = round(click.result * -1)
    
    return(click.result)
  }


rescalePointGroup <-
  function(pointGroups, newDistance) {
    assertDataFrame(pointGroups)
    assertNumeric(nrow(pointGroups), lower = 1)
    assertNumeric(newDistance, lower = 5)
    
    oldDistance = median(pointGroups[, 'distance'])
    pointGroups[, 'distance'] = pointGroups[, 'distance'] / oldDistance * newDistance
    pointGroups[, 'aim.x'] = pointGroups[, 'aim.x'] / oldDistance * newDistance
    pointGroups[, 'aim.y'] = pointGroups[, 'aim.y'] / oldDistance * newDistance
    pointGroups[, 'center.x'] = pointGroups[, 'center.x'] / oldDistance * newDistance
    pointGroups[, 'center.y'] = pointGroups[, 'center.y'] / oldDistance * newDistance
    pointGroups[, 'point.x'] = pointGroups[, 'point.x'] / oldDistance * newDistance
    pointGroups[, 'point.y'] = pointGroups[, 'point.y'] / oldDistance * newDistance
    
    return(pointGroups)
  }



getXYMatrix <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig) {
    xymat = getXYmat(
      pointGroups,
      xyTopLeft = pointDataConfig$pointData.XyTopLeft,
      relPOA = pointDataConfig$pointData.RelPOA
    )
    return(xymat)
  }

getXYMatrixCorrected <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig,
           correctionClicks = NULL) {
    xymat = getXYmat(
      pointGroups,
      xyTopLeft = pointDataConfig$pointData.XyTopLeft,
      relPOA = pointDataConfig$pointData.RelPOA
    )
    if (is.null(correctionClicks)) {
      return(xymat)
    }
    
    #get config
    click.unit = conv_unit(1,
                           weaponConfig$click.Unit,
                           pointDataConfig$pointData.Unit)
    distance.unit = conv_unit(1,
                              weaponConfig$click.DistanceUnit,
                              reportConfig$report.DistanceUnit)
    click.distance = weaponConfig$click.Distance * distance.unit
    
    
    #get click-value
    #0.25 * clickFator * 9/10)
    click.x = weaponConfig$click.x * click.unit / click.distance * reportConfig$ScaledDistance
    click.y = weaponConfig$click.y * click.unit / click.distance * reportConfig$ScaledDistance
    
    if (is.nan(click.x) || is.infinite(click.x))
      click.x = 0
    if (is.nan(click.y) || is.infinite(click.y))
      click.y = 0
    
    if (click.x != 0)
      xymat[, 1] = xymat[, 1] + correctionClicks['x'] * click.x
    if (click.y != 0)
      xymat[, 2] = xymat[, 2] + correctionClicks['y'] * click.y
    
    return(xymat)
  }




buildSightSettingsTable <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig) {
    x = 'x (horizontal)'
    y = 'y (vertical)'
    colname = c(x, y)
    
    pointData.Unit <-
      paste('Group Center', pointDataConfig$pointData.Unit, sep = ' ')
    n_ctr1 = paste('Distance to Group Center in',
                   pointDataConfig$pointData.Unit,
                   sep = ' ')
    n_scr = 'Sight Correction in Clicks'
    n_ctr2 = paste('Distance after Correction in',
                   pointDataConfig$pointData.Unit,
                   sep = ' ')
    rowname = c(n_ctr1, n_scr , n_ctr2)
    B = matrix(
      nrow = length(rowname),
      ncol = length(colname),
      dimnames = list(rowname, colname)
    )
    
    
    xymat = getXYMatrix(pointGroups, pointDataConfig, weaponConfig, reportConfig)
    correctionClicks = calculateSightCorrection(pointGroups, pointDataConfig, reportConfig, weaponConfig)
    xymatCorr = getXYMatrixCorrected(pointGroups,
                                     pointDataConfig,
                                     weaponConfig,
                                     reportConfig,
                                     correctionClicks)
    
    report.UnitConversion <-
      paste(reportConfig$report.DistanceUnit,
            '2',
            pointDataConfig$pointData.Unit,
            sep = '')
    
    distance = median(pointGroups[, 'distance'])
    
    grp1 <- groupLocation(
      xymat,
      level = 0.95,
      plots = FALSE,
      bootCI = c('basic'),
      dstTarget = distance,
      conversion = report.UnitConversion
    )
    grp2 <- groupLocation(
      xymatCorr,
      level = 0.95,
      plots = FALSE,
      bootCI = c('basic'),
      dstTarget = distance,
      conversion = report.UnitConversion
    )
    
    if (reportConfig$report.Robust == TRUE) {
      ctr1 = grp1$ctrRob
      ctr2 = grp2$ctrRob
    } else {
      ctr1 = grp1$ctr
      ctr2 = grp2$ctr
    }
    
    B[n_ctr1, x] = round(ctr1[['x']], digits = 1)
    B[n_ctr1, y] = round(ctr1[['y']], digits = 1)
    B[n_scr, x] = round(correctionClicks[['x']], digits = 0)
    B[n_scr, y] = round(correctionClicks[['y']], digits = 0)
    B[n_ctr2, x] = round(ctr2[['x']], digits = 1)
    B[n_ctr2, y] = round(ctr2[['y']], digits = 1)
    
    return(B)
  }


#calc a significance level from hotelling T^2 test from group center calculation.
#result is a level as a string
calcHotellingSigni <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig) {
    #
    report.UnitConversion = paste(reportConfig$report.DistanceUnit,
                                  '2',
                                  pointDataConfig$pointData.Unit,
                                  sep = '')
    
    resultGroupLocation <-
      groupLocation(
        pointGroups,
        level = 0.95,
        plots = FALSE,
        bootCI = c('basic'),
        dstTarget = reportConfig$ScaledDistance,
        conversion = report.UnitConversion
      )
    
    
    prop = resultGroupLocation$Hotelling$'Pr(>F)'[1]
    x <- prop * 1000
    if (x < 1) {
      return('very high')
    } else if (x < 5) {
      return('high')
    } else if (x < 50) {
      return('average')
    } else if (x < 500) {
      return('low')
    } else {
      return('very low')
    }
    
  }



calcHitProb4Range <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig,
           correctionClicks) {
    report.UnitConversion = paste(reportConfig$report.DistanceUnit,
                                  '2',
                                  pointDataConfig$pointData.Unit,
                                  sep = '')
    
    hitSizes = c(5 / 2, 10 / 2, 12.5 / 2, 15 / 2, 17.5 /
                   2, 20 / 2, 30 / 2, 40 / 2)
    
    hit = getHitProb(
      pointGroups,
      r = hitSizes,
      unit = 'cm',
      dstTarget = reportConfig$report.DistanceUnit,
      doRob = reportConfig$report.Robust,
      conversion =
        report.UnitConversion,
      center = FALSE,
      accuracy = FALSE,
      type = 'Rayleigh'
    )
    
    hit = round(hit * 100, digits = 1)
    hit = unname(hit)
    hitSizes = hitSizes * 2
    
    prob = cbind('Target Diameter' = hitSizes,
                 'Hit Probability' = hit)
    
    #ergebnis zusammenhabuen.
    result = list()
    result$Probability = prob
    
    return(result)
    
  }
