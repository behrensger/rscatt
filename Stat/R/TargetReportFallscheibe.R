source("R/TargetGetGroupColourCode.R", encoding = 'UTF-8')
source("R/TargetAssertion.R", encoding = 'UTF-8')

library(checkmate)


simRingCount_Fallscheibe <-
  function(xy, target, calSize, gunType)
  {
    assertChoice(gunType, ASSERT_CHOICE_FALLSCHEIBE_TYPE)
    
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
        bb = c(0, 100, Inf)
        ll = c(10, 0)
        
        if (gunType == 'KK') {
          bb = c(0, 75, Inf)
          ll = c(10, 0)
        }
        
        ## except 1st ring (mouche, ring 10 inner sub-division)
        maxAll <- with(target, maxVal)
        with(target$inUnit,
             cut(dstPOA,
                 breaks = bb,
                 labels = ll),
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


fallscheibeKKTarget = list(
  name = c("BDS 25m Fallscheibe KK"),
  unitTarget = "mm",
  nRings = 2,
  maxVal = 10,
  unitConv = "cm",
  cols = c("OldLace", "grey20"),
  colsTxt = c("grey20", "OldLace"),
  inUnit = list(
    ringD10 = 10,
    ringD10i = 10,
    ringW = NULL,
    ringR = c(10)
  ),
  convert = list(
    ringD10 = 75,
    ringD10i = 75,
    ringW = NULL,
    ringR = c(75)
  ),
  simRingCount = simRingCount_Fallscheibe
  
)



fallscheibeGKTarget = list(
  name = c("BDS 25m Fallscheibe GK"),
  unitTarget = "mm",
  nRings = 2,
  maxVal = 10,
  unitConv = "cm",
  cols = c("OldLace", "grey20"),
  colsTxt = c("grey20", "OldLace"),
  inUnit = list(
    ringD10 = 10,
    ringD10i = 10,
    ringW = NULL,
    ringR = c(10)
  ),
  convert = list(
    ringD10 = 100,
    ringD10i = 100,
    ringW = NULL,
    ringR = c(100)
  ),
  simRingCount = simRingCount_Fallscheibe
  
)


simRingCount_Fallscheibe_Zehntel <-
  function(xy, caliber, unit = 'cm', gunType)
  {
    assertChoice(gunType, ASSERT_CHOICE_FALLSCHEIBE_TYPE)
    
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
    
    b = c(seq(0, 100.0, 10.0), Inf)
    l = c(seq(10.9, 10.0, -0.1), 0)
    if (gunType == 'KK') {
      b = c(seq(0, 75.0, 7.5), Inf)
      l = c(seq(10.9, 10.0, -0.1), 0)
    }
    result$rings = cut(dstPOA, breaks = b, labels = l)
    
    result$max = nrow(xy) * 10.9
    result$count = sum(as.numeric(levels(result$rings)[result$rings]))
    result$mean = mean(as.numeric(levels(result$rings)[result$rings]))
    result$median = median(as.numeric(levels(result$rings)[result$rings]))
    
    return(result)
  }




#Zielzeiten
#9mm 6.8
#kk 28" 4.6
#KK/b 19"


buildFallscheibeSummaryTable <-
  function(pointGroups,
           pointDataConfig,
           weaponConfig,
           reportConfig,
           gunType,
           correctionClicks = NULL) {
    #rescale distance
    pg = pointGroups
    assert_choice(gunType, ASSERT_CHOICE_FALLSCHEIBE_TYPE)
    
    #set shoot-Time
    
    c_allShots = '<b>All Shots</b>'
    c_30Shots = '<b>30 Shot Match</b>'
    
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
    c_point = 'Fallscheibe Points'
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
    rowname = c(y, c_allShots, c_30Shots)
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
      
      
      target = NULL
      if (gunType == 'KK') {
        target = fallscheibeKKTarget
      } else {
        target = fallscheibeGKTarget
        
      }
      resultRingCount10 <- simRingCount_Fallscheibe_Zehntel(xymat,
                                                            caliber = ConvertedCaliber,
                                                            unit = ConvertedUnit,
                                                            gunType)
      
        
        resultRingCount <- simRingCount_Fallscheibe(xymat,
                                        target = target,
                                        calSize = ConvertedCaliber,
                                        gunType = gunType)
        
      
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
    
   
    target = NULL
    if (gunType == 'KK') {
      target = fallscheibeKKTarget
    } else {
      target = fallscheibeGKTarget
      
    }
    resultRingCount10 <- simRingCount_Fallscheibe_Zehntel(xymat,
                                                          caliber = ConvertedCaliber,
                                                          unit = ConvertedUnit,
                                                          gunType)
    
    
    resultRingCount <- simRingCount_Fallscheibe(xymat,
                                                target = target,
                                                calSize = ConvertedCaliber,
                                                gunType = gunType)    
    
    
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
    
    
    return(B)
  }
