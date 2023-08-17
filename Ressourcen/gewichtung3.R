#' Videoserie Gewichtung mit R
#' Teil 3/6
#' Wo finden wir Gewichte in bekannten Surveys?"
#' Dr. Uwe Remer



#### Allbus ####

## Daten einlesen

library(foreign)
allbus <- read.spss("./Daten/Allbus2021/ZA5280_v2-0-0.sav",
                    to.data.frame = TRUE,
                    use.value.labels = FALSE,
                    reencode = TRUE)

names(allbus)

## Variablen des Stichprobendesigns
# Sampling  Points (Cluster der Stichprobe)
# xs11
# Variablen für das stratifizierte Sampling
# land, bil, gkpl

allbus[, c("xs11", "land", "bik", "gkpol")]



## Gewichtungsvariablen
# wghtpew - Designgewicht für Analysen auf Personenebene (gleicht Oversampling Ost aus)
# wghtht - Transformationsgewicht für Analysen auf Haushaltsebene 
# wghthew - Designgewicht für Analysen auf Haushaltsebene 
# wghthtew - Kombiniertes Design- und Transformationsgewicht für Analysen auf Haushaltsebene  

hist(allbus$wghtpew)



#### ESS ####

ess.i <- read.spss("./Daten/ESS10/ESS10.sav",
                   to.data.frame = TRUE,
                   use.value.labels = FALSE,
                   reencode = TRUE)
ess.sc <- read.spss("./Daten/ESS10/ESS10SC.sav",
                    to.data.frame = TRUE,
                    use.value.labels = FALSE,
                    reencode = TRUE)

# Zusammenführen
vars <- names(ess.i)[names(ess.i) %in% names(ess.sc)]
ess <- rbind(ess.sc[,vars], ess.i[,vars])


names(ess)
hist(ess$anweight)
