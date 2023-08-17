#' Videoserie Gewichtung mit R
#' Teil 5/6
#' Wie nutzen wir einfache Gewichte in R? Das survey Paket
#' Dr. Uwe Remer



#### Daten importieren ####


# Allbus
library(foreign)
allbus <- read.spss("./Daten/Allbus2021/ZA5280_v2-0-0.sav",
                    to.data.frame = TRUE,
                    use.value.labels = FALSE,
                    reencode = TRUE)

# ESS
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

# Ausschluss
ess <- ess[!(ess$cntry %in% c("IL", "RS")),]



#### Das survey Paket #### 

install.packages("survey")
library(survey)

#svydesign()

# Das survey-Design Objekt definieren
allbus_svy <- svydesign(ids=~1, # Definiert die Cluster. Wenn ~1 keine Cluster
                        weights=~wghtpew, # Gewichtungsvariable
                        data=allbus) # Datensatz


# Häufigkeitstabelle
svytable(~pv01, design=allbus_svy)


# Fallzahl standardisieren und Runden
svytable(~pv01, 
         design=allbus_svy,
         Ntotal=sum(!is.na(allbus$pv01)),
         round=T)


# Relative Häufigkeiten in Prozent
svytable(~pv01, 
         design=allbus_svy,
         Ntotal=100)

prop.table(svytable(~pv01, design=allbus_svy))*100


# Kreuztabelle
svytable(~pv01+sex, design=allbus_svy)


# survey-Design Objekt ohne Gewichtung (nur aus didaktischen Gründen)
allbus_svy_ohne_weight <- svydesign(ids=~1,
                                    data=allbus)
prop.table(svytable(~pv01, design=allbus_svy_ohne_weight))



# Gewichteter Mittelwert
svymean(~rb07, allbus_svy, na.rm=T)


# t-Test für gewichtete Daten
# Religiöistät in Bayern vs. Rest von Deutschland

# Bayern ist Merklamsausprägung 90
attr(allbus$land,"value.labels")

# Gruppierungsvariable vorbereiten:
allbus$bayern <- car::recode(allbus$land,
                             "10:80 = FALSE;
                             90 = TRUE;
                             100:160 = FALSE;
                             else = NA")

# Produziert Fehler
#svyttest(rb07~bayern, allbus_svy, na.rm=T)


# survey-Design Obekt erst aktualisieren, 
# damit neue Variable 'bayern' enthalten ist
allbus_svy <- svydesign(ids=~1, 
                        weights=~wghtpew, 
                        data=allbus) 
svyttest(rb07~bayern, allbus_svy, na.rm=T)



#### Gewichtete Regression ####

# Mit base-R
fit.lm <- lm(ps03 ~ pt03 + isced97 + incc + age + eastwest, 
             data=allbus,
             weight=wghtpew)
summary(fit.lm)


# Mit dem survey Paket
fit <- svyglm(ps03 ~ pt03 + isced97 + incc + age + eastwest,
              design=allbus_svy)
summary(fit)

# R^2 berechnen
performance::r2(fit)


?svyglm
