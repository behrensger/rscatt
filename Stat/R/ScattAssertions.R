#
library(checkmate)

ASSERT_PATTERN_VALUE_TYPE = 'L|L/250|Resultat|Abstand|10.0|10.5|10a0|10a5|Q10Abstand'
ASSERT_PATTERN_LEGEND_POS = 'bottomright|bottom|bottomleft|left|topleft|top|topright|right|center'

ASSERT_CHOICE_TREND_DEV = c('lm','nls','nnls','ln')

ASSERT_CHOICE_WAFFEN = c('Xesse','HPS','PPQ','Ruger','Triarii','F12','DAR15')
ASSERT_CHOICE_KW = c('Xesse','PPQ','HPS')
ASSERT_CHOICE_LW = c('Ruger','Triarii','DAR15')
ASSERT_CHOICE_SLF = c('F12')
ASSERT_CHOICE_GUN = c(ASSERT_CHOICE_KW, ASSERT_CHOICE_LW, ASSERT_CHOICE_SLF)
ASSERT_CHOICE_RICHTUNG = c('steigt','sinkt')
ASSERT_CHOICE_GUN_TYPE = c('KW','LW','SLF')
ASSERT_CHOICE_DISZIPLIN = c('Speed','Präzision','Fallscheibe/KK','Fallscheibe','Zielfernrohr','Steel Challenge','Speed-Steel', 'IPSC','Kombi','Mehrdistanz')
ASSERT_CHOICE_ANSCHLAEGE = c('Stehend, Beide','Stehend, Rechts','Stehend, links','liegend, aufgelegt','Kniend, Rechts','Kniend, Beidhändig','Kniend, Rechts','Kniend, Links','Sitzend, Links','Sitzend, Rechts','nix','liegend, Frei')
ASSERT_CHOICE_SIGHT = c('Offen','RedDot','UTG')
ASSERT_CHOICE_WK_LEVEL = c('VM','BM','LM','DM','EM','WM')
ASSERT_CHOICE_TRAININGSTYPE = c('SCATT','DRY','LIVE','ALL')
ASSERT_CHOICE_CALIBER_UNIT = c('mm', 'inch','cm')
ASSERT_CHOICE_DISTANCE_UNIT = c('m', 'yd')
ASSERT_CHOICE_FALLSCHEIBE_TYPE = c('KK','GK','FLINTE')

