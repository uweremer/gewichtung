# Videoserie Mehrebenenregression mit R 
# Teil 2/6 
# Wann ist eine Mehrebenenregression angebracht?
# Dr. Uwe Remer



#### Einelesen und Vorbereiten der Daten ####

# Einlesen des Datensatzes
library(foreign)
ess <- read.spss("./Daten/ESS9e02.sav", 
                 use.value.labels = FALSE,
                 to.data.frame = TRUE,
                 reencode = TRUE)



# Operationalisierung der abh. Variable 
# "Politisches Vertrauen"
# Mittelwertindex aus drei Items:

idx_vars <- c("trstprl","trstplt","trstprt")
ess$pol_vertrauen <- rowMeans(ess[,idx_vars], 
                              na.rm = F)

table(ess$pol_vertrauen, useNA = "always")
DescTools::Desc(ess$pol_vertrauen)


# Operationalisierung der Gruppierungsvariable
# Länder 
ess$cntry
table(ess$cntry)




#### Das Nullmodell ####

library(lme4)
library(lmerTest) #Eigentlich genügt lmerTest, da das automatisch auch lme4 lädt

mreg.0 <- lmer(pol_vertrauen ~ 1 + (1 | cntry),
               data = ess)

summary(mreg.0)

#install.packages("performance")
library(performance)
icc(mreg.0)

