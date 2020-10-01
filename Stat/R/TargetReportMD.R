source("R/TargetGetGroupColourCode.R", encoding = 'UTF-8')

md_result_table <-
  function(pointGroups,
           weaponConfig,
           reportConfig,
           correctionClicks = NULL) {
    realDistance = median(pointGroups[, 'distance'])
    
    rowname = c()
    colname = c('Shots',
                'Rings',
                'Max Rings',
                'Mean Ring',
                'Median Ring',
                'Dist POA',
                'Sigma')
    B = matrix(
      nrow = length(rowname),
      ncol = length(colname),
      dimnames = list(rowname, colname)
    )
    
    names = c()
    for (dist in c(20, 15, 10, 5)) {
      if (realDistance == dist) {
        c_txt = paste(dist, 'Meter')
      } else {
        c_txt = paste(dist, 'Meter (projected from', realDistance, 'Meter)')
      }
      B = rbind(B, 1)
      rowname = c(rowname, c_txt)
      rownames(B) = rowname
      
      xy = getXYMatrixCorrected(
        rescalePointGroup(pointGroups, dist),
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
      rc = simRingCount(xy,
                        target = 'BDS9',
                        caliber = ConvertedCaliber,
                        unit = ConvertedUnit)
      
      
      report.UnitConversion = paste(reportConfig$report.DistanceUnit,
                                    '2',
                                    reportConfig$pointData.Unit,
                                    sep = '')
      
      gl = groupLocation(
        xy,
        level = 0.95,
        plots = FALSE,
        bootCI = c('basic'),
        dstTarget = dist,
        conversion = report.UnitConversion
      )
      
      
      B[c_txt, 'Shots'] = nrow(xy)
      B[c_txt, 'Rings'] = rc$count
      B[c_txt, 'Max Rings'] = rc$max
      B[c_txt, 'Mean Ring'] = round(B[c_txt, 'Rings'] / B[c_txt, 'Shots'], digits = 1)
      B[c_txt, 'Median Ring'] = median(as.numeric(levels(rc$rings)[rc$rings]))
      B[c_txt, 'Dist POA'] = round(gl$distPOA[['unit']], digits = 1)
      B[c_txt, 'Sigma'] = round(getRayParam(
        xy
        ,
        doRob = reportConfig$report.Robust,
        mu = c(x = 0, y = 0)
      )$sigma["sigma"],
      digits = 0)
      
    }
    
    return(B)
    
  }
