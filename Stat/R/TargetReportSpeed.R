source("R/TargetGetGroupColourCode.R", encoding = 'UTF-8')


simRingCount_BDSSpeed <-
  function(xy, target, calSize)
  {
    ## get distance of inner edge of bullet hole to point of aim (0,0)
    ## negative difference -> distance from other side
    dstPOA <- abs(sqrt(rowSums(xy ^ 2)) - calSize / 2)
    
    ## cut with breaks = ring radii
    rings <-
      if (!is.null(target$countMouche) && target$countMouche) {
        ## with 1st ring (mouche, ring 10 inner sub-division)
        maxAll <- with(target, maxVal + 1)
        with(target$inUnit,
             cut(
               dstPOA,
               breaks = c(0, ringR, Inf),
               labels = c((target$maxVal + 1):(target$maxVal -
                                                 target$nRings + 1), 0)
             ),
             include.lowest = TRUE)
      } else {
        ## except 1st ring (mouche, ring 10 inner sub-division)
        maxAll <- with(target, maxVal)
        with(target$inUnit,
             cut(
               dstPOA,
               breaks = c(0, 50, 100, Inf),
               labels = c(10, 7, 0)
             ),
             
             #cut(
             #   dstPOA,
             #   breaks = c(0, ringR[-1], Inf),
             #  labels = c(target$maxVal:(target$maxVal -
             #                               target$nRings + 1), 0)
             #),
             include.lowest = TRUE)
      }
    
    ## convert factor labels to numeric, then sum
    ringCount <-
      sum(as.numeric(as.character(rings)))  # observed ring count
    ringMax   <-
      maxAll * nrow(xy)                     # maximum possible
    
    shots <- nrow(xy)
    
    return(list(
      count = ringCount,
      max = ringMax,
      rings = rings,
      shots = shots
    ))
  }


speedTarget = list(
  name = c("BDS 25m Speed"),
  unitTarget = "mm",
  nRings = 3,
  maxVal = 10,
  unitConv = "cm",
  cols = c("OldLace", "grey20"),
  colsTxt = c("grey20", "OldLace"),
  inUnit = list(
    ringD10 = 10,
    ringD10i = 10,
    ringW = NULL,
    ringR = c(5, 10)
  ),
  convert = list(
    ringD10 = 100,
    ringD10i = 100,
    ringW = NULL,
    ringR = c(50, 100)
  ),
  simRingCount = simRingCount_BDSSpeed
  
)


simRingCount_Speed_Zehntel <-
  function(xy, caliber, unit = 'cm')
  {
    assertMatrix(
      xy,
      any.missing = FALSE,
      all.missing = FALSE,
      min.rows = 1,
      ncols = 2,
      null.ok = FALSE
    )
    assertNumeric(caliber, lower = 0, upper = 100)
    assertString(unit, pattern = 'mm|cm|in')
    
    result = list()
    
    dstPOA <- abs(sqrt(rowSums(xy ^ 2)) - caliber / 2)
    #dstPOA = dstPOA * getConvFac(paste(unit,'2mm', sep = ''))
    
    b = c(seq(0, 500.0, 5.0), Inf)
    l = c(seq(10.9, 1.0, -0.1), 0)
    result$rings = cut(dstPOA, breaks = b, labels = l)
    
    result$max = nrow(xy) * 10.9
    result$count = sum(as.numeric(levels(result$rings)[result$rings]))
    result$mean = mean(as.numeric(levels(result$rings)[result$rings]))
    result$median = median(as.numeric(levels(result$rings)[result$rings]))
    
    return(result)
  }





buildSpeedSummaryTable <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig,
           correctionClicks = NULL) {
    #rescale distance
    pg = pointGroups
    
    #set shoot-Time
    
    c_allShots = '<b>All Shots</b>'
    c_30Shots = '<b>30 Shot Match</b>'
    c_20Shots = '<b>20 Shot Match</b>'
    
    c_time = weaponConfig$goal.speed.time / 30 #const for shoot time
    
    #create Summary Table for Shotgroups
    x = table(pg['group'])
    
    y = c()
    for (n in names(x)) {
      y = c(y,  get_GroupColouredName(n))
    }
    
    c_shot = 'Shots'
    c_ring = 'Rings'
    c_max = 'Max'
    c_mean = 'Mean'
    c_median = 'Median'
    c_point = 'Speed Points'
    c_sigma = paste('Sigma (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_dist = paste('Dist POA (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_circle = paste('Circle (', pointDataConfig$pointData.Unit, ')', sep = '')
    c_tenth = 'Tenth'
    
    
    colname = c(c_shot,
                c_ring,
                c_max,
                c_mean,
                c_median,
                c_point,
                c_tenth,
                c_sigma,
                c_dist,
                c_circle)
    rowname = c(y, c_allShots, c_30Shots, c_20Shots)
    B = matrix(
      nrow = length(rowname),
      ncol = length(colname),
      dimnames = list(rowname, colname)
    )
    
    
    #loop for groups
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
      
      resultRingCount10 <- simRingCount_Speed_Zehntel(xymat,
                                                      caliber = ConvertedCaliber,
                                                      unit = ConvertedUnit)
      
      #resultRingCount <- simRingCount(xymat,
      #target = speedTarget,
      #caliber = ConvertedCaliber,
      #unit = ConvertedUnit)
      
      resultRingCount <-
        simRingCount_BDSSpeed(xymat,
                              target = speedTarget,
                              calSize = ConvertedCaliber)
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
      B[name, c_point] =  floor(B[name, c_ring] - (c_time  * B[name, c_shot]))
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
    
    resultRingCount10 <- simRingCount_Speed_Zehntel(xymat,
                                                    caliber = ConvertedCaliber,
                                                    unit = ConvertedUnit)
    #resultRingCount <- simRingCount(xymat,
    #target = speedTarget,
    #caliber = ConvertedCaliber,
    #unit = ConvertedUnit)
    
    resultRingCount <-
      simRingCount_BDSSpeed(xymat,
                            target = speedTarget,
                            calSize = ConvertedCaliber)
    
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
    
    
    
    #create a summary column
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
    B[c_allShots, c_max] = resultRingCount$max
    B[c_allShots, c_mean] = round(B[c_allShots, c_ring] / B[c_allShots, c_shot], digits = 1)
    B[c_allShots, c_median] = median(as.numeric(levels(resultRingCount$rings)[resultRingCount$rings]))
    B[c_allShots, c_point] =  floor(B[c_allShots, c_ring] - (c_time  * B[c_allShots, c_shot]))
    B[c_allShots, c_tenth] = round(resultRingCount10$mean, digits = 1)
    B[c_allShots, c_sigma] = round(getRayParam(
      xymat,
      doRob = reportConfig$report.Robust,
      mu = c(x = 0, y = 0)
    )$sigma["sigma"],
    digits = 0)
    
    
    #create a match line
    B[c_30Shots, c_dist] = round(resultGroupLocation$distPOA[['unit']], digits = 1)
    if (nrow(xymat) < 2)
      B[c_30Shots, c_circle] = 0
    else
      B[c_30Shots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    B[c_30Shots, c_mean] = B[c_allShots, c_mean]
    B[c_30Shots, c_median] = B[c_allShots, c_median]
    B[c_30Shots, c_sigma] =  B[c_allShots, c_sigma]
    B[c_30Shots, c_shot] = 30
    B[c_30Shots, c_ring] = round(B[c_allShots, c_ring] /  B[c_allShots, c_shot] * 30, digits = 0)
    B[c_30Shots, c_max] = 300
    B[c_30Shots, c_tenth] = B[c_allShots, c_tenth]
    B[c_30Shots, c_point] =  floor(B[c_30Shots, c_ring] - (c_time  * B[c_30Shots, c_shot]))
    

    #create a match line
    B[c_20Shots, c_dist] = round(resultGroupLocation$distPOA[['unit']], digits = 1)
    if (nrow(xymat) < 2)
      B[c_20Shots, c_circle] = 0
    else
      B[c_20Shots, c_circle] = round(getMinCircle(xymat)$rad * 2, digits = 0)
    B[c_20Shots, c_mean] = B[c_allShots, c_mean]
    B[c_20Shots, c_median] = B[c_allShots, c_median]
    B[c_20Shots, c_sigma] =  B[c_allShots, c_sigma]
    B[c_20Shots, c_shot] = 20
    B[c_20Shots, c_ring] = round(B[c_allShots, c_ring] /  B[c_allShots, c_shot] * 20, digits = 0)
    B[c_20Shots, c_max] = 200
    B[c_20Shots, c_tenth] = B[c_allShots, c_tenth]
    B[c_20Shots, c_point] =  floor(B[c_20Shots, c_ring] - (c_time  * B[c_20Shots, c_shot]))
    
        
    return(B)
  }



drawTarget_BDS_Speed <-
  function(x, cex = par("cex")) {
    with(
      x$inUnit,
      symbols(
        rep(0, length(ringR)),
        rep(0, length(ringR)),
        add = TRUE,
        circles = rev(ringR),
        bg = rev(x$cols),
        fg = rev(x$colsTxt),
        inches = FALSE
      )
    )
    # abline(v=0, h=0, col="lightgray")    # add point of aim
    
    ## add ring numbers except for bullseye (ring number maxVal)
    ## bullseye has inner sub-division -> start numbers on ring 3
    rings1 <-
      with(x,
           seq(
             from = maxVal - nRings + 1,
             to = maxVal - 1,
             length.out = nRings - 1
           )) # left side of center
    rings2 <-
      c(rings1, rev(rings1))                         # both sides
    pos1   <-
      with(x$inUnit, ringR[3:length(ringR)] - (ringW / 2)) # right side of center
    pos2   <-
      c(-rev(pos1), pos1)                            # both sides
    cols1  <-
      with(x$inUnit, x$colsTxt[3:length(ringR)])     # right side of center
    cols2  <-
      c(rev(cols1), cols1)                           # both sides
    
  }