# Videoserie Mehrebenenregression mit R 
# Teil 4/6 
# Wie schätzt man eine Mehrebenenregression in R?
# Dr. Uwe Remer


# R Skript aus Video 3 ausführen lassen, damit Daten und
# vorbereitete Variablen vorhanden sind:
source("./Ressourcen/RScript_mreg3.R") 




#### Schätzen der Mehrebenenmodelle ####

#### Variierende Intercepts mit L1 Prädiktoren ####
library(lmerTest) 
mreg1 <- lmer(pol_vertrauen ~ 1 + bildung + 
                responsivitaet + zufr_wirtschaft + soz_vertrauen + 
                (1 | cntry),  
              data=ess)

summary(mreg1)

#Zeigt die Random Effects
ranef(mreg1)

#Zeigt die Fixed Effects
fixef(mreg1)


# Zum Vergleich: das Nullmodell
mreg0 <- lmer(pol_vertrauen ~ 1 + (1 | cntry),  
              data=ess)

summary(mreg0)
summary(mreg1)

# Gütemaße 
library(performance)
r2(mreg1)

model_performance(mreg1)


#### Variierende Intercepts mit L1 und L2 Prädiktoren ####

# Drehen der Makrovariable "c_ticpi_2018" (Korruptionsindex)
ess$korruption <- abs(ess$c_ticpi_2018-100)

# Deskriptive Grafik zur gedrehten Variable "korruption"
library(ggplot2)
ggplot(ess, aes(x=cntry, y=korruption)) +
  geom_bar(stat = "summary", fun="mean") +
  scale_y_continuous(limits=c(0,100)) +
  theme(axis.text=element_text(size=6))



# Schätzen des neuen Mehrebenenmodells mit Prädiktor 'korruption' auf Ebene 2
mreg2 <- lmer(pol_vertrauen ~ 1 + bildung + 
                responsivitaet + zufr_wirtschaft + soz_vertrauen + 
                korruption + 
                (1 | cntry),  
              data=ess)

summary(mreg2)

model_performance(mreg2)

