#' Videoserie Gewichtung mit R
#' Teil 6/6
#' Wie gewichten wir komplexe Stichproben in R?
#' Dr. Uwe Remer



#### Daten importieren ####


# Allbus
library(foreign)
allbus <- read.spss("./Daten/Allbus2021/ZA5280_v2-0-0.sav",
                    to.data.frame = TRUE,
                    use.value.labels = FALSE,
                    reencode = TRUE)


# ESS
library(foreign)
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

ess <- ess[!(ess$cntry %in% c("IL", "RS")),]




library(survey)
?svydesign



#### komplexe Stichprobendesign definieren ####

ess_svy <- svydesign(ids=~psu,
                     strata = ~stratum,
                     #probs=prob, 
                     weight=~anweight,
                     nest=TRUE,
                     data = ess)


# Einzele PSU pro Schicht erlauben
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="average")


# Das dauert ein paar Sekunden...
svymean(~stfdem, 
        design=ess_svy, 
        na.rm = T)

# warnings()
# Die Warnungen weisen auf einzelne PSU pro Schicht hin 




#### Ein Vergleich von verschiedenen Spezifikationen #### 


# Ungewichtet
ess_svy_0 <- svydesign(ids = ~1,
                       data = ess)

# Nur Gewichtung
ess_svy_1 <- svydesign(ids = ~1, 
                       weights = ~anweight,
                       data = ess)

# Gewichtung mit Berücksichtigung des komplexen Samplings
ess_svy_2 <- svydesign(ids=~psu, 
                       strata = ~stratum,
                       nest=TRUE,
                       weights = ~anweight,
                       data = ess)


# Gruppierte Mittelwerte nach Land
# Zuerst für ungewichtete Date ohne komplexes SP
svyby(~atcherp, by=~cntry, design=ess_svy_0, FUN=svymean, na.rm=T)

mw0 <- svyby(~atcherp, by=~cntry, design=ess_svy_0, FUN=svymean, na.rm=T) 
mw0$Modell <- "1. ohne Gewichtung" 
mw0$ci_hi <- mw0$atcherp + (1.96*mw0$se) 
mw0$ci_lo <- mw0$atcherp - (1.96*mw0$se)


# Für gewichtete Daten
mw1 <- svyby(~atcherp, by=~cntry, design=ess_svy_1, FUN=svymean, na.rm=T)
mw1$Modell <- "2. nur Gewichtung"
mw1$ci_hi <- mw1$atcherp + (1.96*mw1$se)
mw1$ci_lo <- mw1$atcherp - (1.96*mw1$se)

# Für gewichtete Daten mit komplexen SP Design
mw2 <- svyby(~atcherp, by=~cntry, design=ess_svy_2, FUN=svymean, na.rm=T)
mw2$Modell <- "3. Gewichtung mit Sampling Design"
mw2$ci_hi <- mw2$atcherp + (1.96*mw2$se)
mw2$ci_lo <- mw2$atcherp - (1.96*mw2$se)


# Datensätze zusammenführen
df <- rbind(mw0, mw1, mw2)
# Länder für Grafik als Faktorvariable sortieren
df$cntry <- forcats::fct_reorder(df$cntry, df$atcherp)


# Jetzt Grafik erstellen
library(ggplot2)
ggplot(df, aes(x=cntry, y=atcherp, 
               ymin=ci_lo, ymax=ci_hi,
               color=Modell)) +
  geom_pointrange(position = position_dodge(width = .5)) +
  geom_hline(yintercept=mean(df$atcherp)) +
  theme_bw() +
  theme(legend.position="bottom")


# SEs ins Verhältnis setzen
barplot(sort(mw2$se/mw0$se))
abline(h=1, col="red")





#### Regressions-Beispiel mit dem Allbus #### 

# abh. Variable vorbereiten (drehen)
allbus$ps03

allbus$demzufr <- abs(allbus$ps03-6) 
#Neue Skala reicht nun von 0 bis 5.

# pt03 - Vertrauen Bundestag
# isced97 - Bildung
# incc - Nettoeinkommen
# age  - Alter 
# eastwest  - Landesteil
# pt03:eastwest




allb_svy_2 <- svydesign(ids=~xs11, 
                       strata = ~ eastwest + land + bik + gkpol,
                       nest=TRUE,
                       weights = ~wghtpew,
                       data = allbus)

# Fehlermeldung aufgrund fehlender Werte
sum(is.na(allbus$land))
sum(is.na(allbus$bik))
sum(is.na(allbus$gkpol))
allbus <- allbus[complete.cases(allbus[,c("eastwest","land","bik","gkpol")]),]

# Gewichtung und komplexes SP-Design
allb_svy_2 <- svydesign(ids=~xs11, 
                       strata = ~ eastwest + land + bik + gkpol,
                       nest=TRUE,
                       weights = ~wghtpew,
                       data = allbus)


# Nur mit Gewichtung
allb_svy_1 <- svydesign(ids = ~1, 
                       weights = ~wghtpew,
                       data = allbus)


# Fehlende Fälle ausschließen für vergleichbare Fallzahlen
variablen <- c("demzufr", "pt03", "isced97", "incc", "age", "eastwest")
allbus <- allbus[complete.cases(allbus[,variablen]),]

# Regressionen schätzen
fit1 <- svyglm(demzufr ~ pt03 + isced97 + incc + age + eastwest + pt03:eastwest, 
               design=allb_svy_1,
               family=stats::gaussian())

fit2 <- svyglm(demzufr ~ pt03 + isced97 + incc + age + eastwest + pt03:eastwest,  
               design=allb_svy_2,
               family=stats::gaussian())

#summary(fit1)
#summary(fit2)
texreg::screenreg(list(fit1, fit2))


# Beispiel mit reduzierter Fallzahl

set.seed(99) #Reproduzierbarkeit des Zufalls-Samplings
# 1000 zufällige Fälle aus dem Allbus ziehen
allbus_small <- allbus[sample(1:nrow(allbus), 1000),]

# Designs definieren
allb_s_svy_1 <- svydesign(ids = ~1, 
                       weights = ~wghtpew,
                       data = allbus_small)
allb_s_svy_2 <- svydesign(ids=~xs11, 
                       strata = ~land + bik + gkpol,
                       nest=TRUE,
                       weights = ~wghtpew,
                       data = allbus_small)

# Regressionen schätzen 
fit1.s <- svyglm(ps03 ~ pt03 + isced97 + incc + age + eastwest + pt03:eastwest, 
               design=allb_s_svy_1,
               family=stats::gaussian())

fit2.s <- svyglm(ps03 ~ pt03 + isced97 + incc + age + eastwest+ pt03:eastwest,  
               design=allb_s_svy_2,
               family=stats::gaussian())

texreg::screenreg(list(fit1.s, fit2.s))


