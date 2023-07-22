# Videoserie Mehrebenenregression mit R 
# Teil 3/6 
# Wie werden die Daten für eine Mehrebenregression vorbereitet?
# Dr. Uwe Remer



#### Operationalisierung ####

# Importieren des Datensatz
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
#table(ess$pol_vertrauen, useNA = "always")
library(DescTools)
Desc(ess$pol_vertrauen)



# Operationalisierung der unabh. Variable 
# Bildung
ess$bildung <- ess$eisced 

# NAs definieren:
# `0` und `55` auf `NA` setzten
ess$bildung[ess$bildung %in% c(0,55)] <- NA

Desc(ess$bildung)



# Operationalisierung der unabh. Variable 
# Wahrgenommene politische Responsivität 
# Mittelwertindex aus zwei Items 

idx_vars <- c("psppsgva","psppipla")
ess$responsivitaet <- rowMeans(ess[,idx_vars], 
                               na.rm = F)

Desc(ess$responsivitaet)



# Operationalisierung der unabh. Variable 
# "Zufriedenheit Wirtschaftslage"

ess$zufr_wirtschaft <- ess$stfeco
Desc(ess$zufr_wirtschaft)




# Operationalisierung der unabh. Variable 
# Soziales Vertrauen

idx_vars <- c("ppltrst", "pplfair", "pplhlp")
ess$soz_vertrauen <- rowMeans(ess[,idx_vars], 
                              na.rm = F)
Desc(ess$soz_vertrauen)




#### Makrodaten  ####

# Einlesen Makrodaten einlesen und mit merge() hinzuspielen
# ggf. Daetipfad anpassen
ess_macro <- read.table("./Daten/ess2018_macro.csv",
                        sep=";", header=T)
#View(ess_macro)

ess_micro <- ess

# Datensätze zusammenspielen
ess <- merge(ess_micro, ess_macro, # Die beiden Datensätze die zusammengespielt werden
             by = "cntry", # Die Schlüsselvariable
             all.x = T) # Das alle Fälle des x-Datensatzes (des erstgenannten) behalten werden



# Missing Values ausschließen
variablen_im_modell <- c("pol_vertrauen",
                         "bildung",      
                         "responsivitaet",                         
                         "zufr_wirtschaft",
                         "soz_vertrauen", 
                         "cntry",
                         "c_ticpi_2018")
ess <- na.omit(ess[,variablen_im_modell])



# Zentrieren der Ebene 1 x-Variablen
# Diese sind an 2. bis 5. und 7. Stelle im Datensatz
names(ess)
ess[,c(2:5)]
scale(ess[,c(2:5)], scale=F)

ess_centered <- as.data.frame(scale(ess[,c(2:5)], scale=F))
ess[,c(2:5)] <- ess_centered

