source("R/TargetGetGroupColourCode.R", encoding = 'UTF-8')
library(checkmate)



simRingCount_BDS9_Zehntel <-
  function(xy, caliber, unit = 'cm')
  {
    assertMatrix(xy,any.missing = FALSE, all.missing = FALSE, min.rows = 1, ncols = 2, null.ok = FALSE)
    assertNumeric(caliber, lower = 0, upper = 100)
    assertString(unit, pattern = 'mm|cm|in')
    
    result = list()

    dstPOA <- abs(sqrt(rowSums(xy ^ 2)) - caliber / 2)
    #dstPOA = dstPOA * getConvFac(paste(unit,'2mm', sep = ''))

    b = c(seq(0, 250.0, 2.5), Inf)
    l = c(seq(10.9, 1.0, -0.1), 0)
    result$rings = cut(dstPOA, breaks = b, labels = l)
    
    result$max = nrow(xy)*10.9
    result$count = sum(as.numeric(levels(result$rings)[result$rings]))
    result$mean = mean(as.numeric(levels(result$rings)[result$rings]))
    result$median = median(as.numeric(levels(result$rings)[result$rings]))    

    return(result)
  }


  
buildShotGroupSummaryTable <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig,
           correctionClicks = NULL) {
    #create Summary Table for Shotgroups
    x = table(pointGroups['group'])
    pg = pointGroups
    
    y = c()
    for (n in names(x)) {
      y = c(y,  get_GroupColouredName(n))
    }
    
    c_sigma = paste('Sigma (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_dist = paste('Dist POA (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_circle = paste('Circle (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_allShots = '<b>All Shots</b>'
    c_20Shots = '<b>20 Shots</b>'
    c_30Shots = '<b>30 Shots</b>'
    c_40Shots = '<b>40 Shots</b>'
    
    c_shot = 'Shots'
    c_ring = 'Rings'
    c_max = 'Max'
    c_mean = 'Mean'
    c_median = 'Median'
    c_tenth = 'BDS9-Tenth'
    
    colname = c(c_shot,
                c_ring,
                c_max,
                c_mean,
                c_median,
                c_tenth,
                c_sigma,
                c_dist,
                c_circle)
    
    rowname = c(y, c_allShots, c_20Shots, c_30Shots, c_40Shots)
    B = matrix(
      nrow = length(rowname),
      ncol = length(colname),
      dimnames = list(rowname, colname)
    )
    
    
    for (n in names(x)) {
      name = get_GroupColouredName(n)
      xymat = getXYMatrixCorrected(
        subset(pg, group == n),
        pointDataConfig,
        weaponConfig,
        reportConfig,
        correctionClicks
      )
      ConvertedCaliber = getConvFac(paste(
        weaponConfig$caliberUnit,
        pointDataConfig$pointData.Unit,
        sep = '2'
      )) * weaponConfig$caliber
      ConvertedUnit = pointDataConfig$pointData.Unit
      
      resultRingCount <- simRingCount(
        xymat,
        target = reportConfig$report.Target,
        caliber = ConvertedCaliber,
        unit = ConvertedUnit
      )
      resultRingCount10 <- simRingCount_BDS9_Zehntel(
        xymat,
        caliber = ConvertedCaliber,
        unit = ConvertedUnit
      )      
      report.UnitConversion = paste(reportConfig$report.DistanceUnit,
                                    '2',
                                    pointDataConfig$pointData.Unit,
                                    sep = '')
      
      resultGroupLocation <- groupLocation(
        xymat,
        level = 0.95,
        plots = FALSE,
        bootCI = c('basic'),
        dstTarget = median(subset(pg, group == n)[["distance"]]),
        conversion = report.UnitConversion
      )
      
      
      
      if (reportConfig$report.Robust == TRUE) {
        B[name, c_dist] = round(resultGroupLocation$distPOArob[['unit']], digits = 1)
      } else {
        B[name, c_dist] = round(resultGroupLocation$distPOA[['unit']], digits = 1)
      }
      
      
      if (nrow(xymat) < 2)
        B[name, c_circle] = 0
      else
        B[name, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
      B[name, c_shot] = nrow(xymat)
      B[name, c_ring] = resultRingCount$count
      B[name, c_max] = resultRingCount$max
      B[name, c_mean] = round(B[name, c_ring] / B[name, c_shot], digits = 1)
      B[name, c_median] = median(as.numeric(levels(resultRingCount$rings)[resultRingCount$rings]))
      B[name, c_tenth] = round(resultRingCount10$mean, digits = 1)
      B[name, c_sigma] = round(getRayParam(
        xymat,
        doRob = reportConfig$report.Robust,
        mu = c(x = 0, y = 0)
      )$sigma["sigma"],
      digits = 0)
      
    }
    
    xymat = getXYMatrixCorrected (pg,
                                  pointDataConfig,
                                  weaponConfig,
                                  reportConfig,
                                  correctionClicks)
    
    ConvertedCaliber = getConvFac(paste(
      weaponConfig$caliberUnit,
      pointDataConfig$pointData.Unit,
      sep = '2'
    )) * weaponConfig$caliber
    ConvertedUnit = pointDataConfig$pointData.Unit
    resultRingCount <- simRingCount(
      xymat,
      target = reportConfig$report.Target,
      caliber = ConvertedCaliber,
      unit = ConvertedUnit
    )
    resultRingCount10 <- simRingCount_BDS9_Zehntel(
      xymat,
      caliber = ConvertedCaliber,
      unit = ConvertedUnit
    )     
    report.UnitConversion = paste(reportConfig$report.DistanceUnit,
                                  '2',
                                  pointDataConfig$pointData.Unit,
                                  sep = '')
    
    resultGroupLocation <- groupLocation(
      xymat,
      level = 0.95,
      plots = FALSE,
      bootCI = c('basic'),
      dstTarget = median(subset(pg, group == n)[["distance"]]),
      conversion = report.UnitConversion
    )
    
    
    
    if (reportConfig$report.Robust == TRUE) {
      B[c_allShots, c_dist] = round(resultGroupLocation$distPOArob[['unit']], digits = 1)
    } else {
      B[c_allShots, c_dist] = round(resultGroupLocation$distPOA[['unit']], digits = 1)
    }
    
    
    if (nrow(xymat) < 2)
      B[c_allShots, c_circle] = 0
    else
      B[c_allShots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    
    B[c_allShots, c_shot] = nrow(xymat)
    B[c_allShots, c_ring] = resultRingCount$count
    B[c_allShots, c_tenth] = round(resultRingCount10$mean, digits = 1)
    B[c_allShots, c_max] = resultRingCount$max
    B[c_allShots, c_mean] = round(B[c_allShots, c_ring] / B[c_allShots, c_shot], digits = 1)
    B[c_allShots, c_median] = median(as.numeric(levels(resultRingCount$rings)[resultRingCount$rings]))
    B[c_allShots, c_sigma] = round(getRayParam(
      xymat,
      doRob = reportConfig$report.Robust,
      mu = c(x = 0, y = 0)
    )$sigma["sigma"],
    digits = 0)
    #create a match line
    B[c_20Shots, c_dist] = B[c_allShots, c_dist]
    if (nrow(xymat) < 2)
      B[c_20Shots, c_circle] = 0
    else
      B[c_20Shots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    
    B[c_20Shots, c_mean] = B[c_allShots, c_mean]
    B[c_20Shots, c_median] = B[c_allShots, c_median]
    B[c_20Shots, c_sigma] =  B[c_allShots, c_sigma]
    B[c_20Shots, c_shot] = 20
    B[c_20Shots, c_ring] = round(B[c_allShots, c_ring] /  B[c_allShots, c_shot] * B[c_20Shots, c_shot], digits = 0)
    B[c_20Shots, c_tenth] = B[c_allShots, c_tenth]
    B[c_20Shots, c_max] = B[c_20Shots, c_shot] * 10
    B[c_20Shots, c_sigma] = B[c_allShots, c_sigma]

    
    B[c_30Shots, c_dist] = B[c_allShots, c_dist]
    if (nrow(xymat) < 2)
      B[c_30Shots, c_circle] = 0
    else
      B[c_30Shots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    
    B[c_30Shots, c_mean] = B[c_allShots, c_mean]
    B[c_30Shots, c_median] = B[c_allShots, c_median]
    B[c_30Shots, c_sigma] =  B[c_allShots, c_sigma]
    B[c_30Shots, c_shot] = 30
    B[c_30Shots, c_ring] = round(B[c_allShots, c_ring] /  B[c_allShots, c_shot] * B[c_30Shots, c_shot], digits = 0)
    B[c_30Shots, c_tenth] = B[c_allShots, c_tenth]
    B[c_30Shots, c_max] = B[c_30Shots, c_shot] * 10
    B[c_30Shots, c_sigma] = B[c_allShots, c_sigma]
    
    
    B[c_40Shots, c_dist] = B[c_allShots, c_dist]
    if (nrow(xymat) < 2)
      B[c_40Shots, c_circle] = 0
    else
      B[c_40Shots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    B[c_40Shots, c_mean] = B[c_allShots, c_mean]
    B[c_40Shots, c_median] = B[c_allShots, c_median]
    B[c_40Shots, c_sigma] =  B[c_allShots, c_sigma]
    B[c_40Shots, c_shot] = 40
    B[c_40Shots, c_ring] = round(B[c_allShots, c_ring] /  B[c_allShots, c_shot] * B[c_40Shots, c_shot], digits = 0)
    B[c_40Shots, c_tenth] = B[c_allShots, c_tenth]
    B[c_40Shots, c_max] = B[c_40Shots, c_shot] * 10
    B[c_40Shots, c_sigma] = B[c_allShots, c_sigma]
    
    return(B)
  }
