#' Videoserie Gewichtung mit R
#' Teil 4/6
#' Wie nutzen wir einfache Gewichte in R?
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

# Ausschluss
ess <- ess[!(ess$cntry %in% c("IL", "RS")),]






#### Das sjmisc Paket #### 

install.packages("sjmisc")
library(sjmisc)


## Tabellen

# Ungewichtet
frq(allbus$pv01)

# Gewichtet nach wghtpew
frq(allbus$pv01,
    weights = allbus$wghtpew)



# Objekt speichern für Grafik
tabelle <- frq(allbus$pv01,
               weights = allbus$wghtpew)
str(tabelle)
tabelle[[1]]


tabelle_df <- tabelle[[1]]
tabelle_df <- tabelle_df[-9,] #Zeile mit dem NA eintrag ausschließen
tabelle_df$label <- rev(names(attr(allbus$pv01, "value.labels")))
tabelle_df$label <- as.factor(tabelle_df$label)
tabelle_df$label <- forcats::fct_reorder(tabelle_df$label, tabelle_df$valid.prc)

ggplot(tabelle_df, aes(x=label,
                       y=valid.prc, 
                       label=valid.prc,
                       fill=label)) +
  geom_bar(stat="identity") +
  geom_label() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Sonntagsfrage",
       subtitle = "Anteil Zweitstimmen in Prozent",
       caption="Quelle: Allbus 2021, Daten gewichtet mit Ost-West Personengewicht (wghtpew)")


## Deskriptive Statistik

descr(allbus$rb07)

descr(allbus, 
      rb07,
      weights = wghtpew) 
# Diese schreibweise funktioniert nicht:
# descr(allbus$rb07,
#      weights = allbus$wghtpew) 


weighted.mean(allbus$rb07, allbus$wghtpew, na.rm=T)



#### Das weights Paket ####


#install.packages("weights")
library(weights)


## Tabellen


prop.table(table(ess$cntry))
wpct(ess$cntry,
     weight=ess$anweight, 
     na.rm=TRUE)

cntry_df <- data.frame(Land = rep(names(prop.table(table(ess$cntry))),2),
                       Anteil = c(prop.table(table(ess$cntry)),
                                  wpct(ess$cntry, weight=ess$anweight,na.rm=TRUE)),
                       Modell = c(rep("ungewichtet", 29),
                                  rep("gewichtet", 29)))
ggplot(cntry_df, aes(x=Land, y=Anteil, 
                     group=Modell, fill=Modell)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "bottom")


## Bivariate Statistik

### Chi2 test

wtd.chi.sq(ess$edulvlb, ess$cntry,  
     weight=ess$anweight, 
     na.rm=TRUE)


### Korrelation

cor.test(allbus$rb07, allbus$age)

wtd.cor(allbus$rb07, allbus$age,  
     weight=allbus$wghtpew)



### t-Test

# t-test für ungabhängige Stichproben

# Variable allbus$dh16 Kinder im eigenen Haushalt: 
# 0: Keine Kinder im HH,  nicht-0: Kinder im HH

t.test(allbus$hs04[allbus$dh16!=0],
       allbus$hs04[allbus$dh16==0])

wtd.t.test(allbus$hs04[allbus$dh16!=0], 
           allbus$hs04[allbus$dh16==0],  
           weight=allbus$wghtpew[allbus$dh16!=0],
           weighty=allbus$wghtpew[allbus$dh16==0])




