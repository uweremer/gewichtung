# Videoserie Mehrebenenregression mit R 
# Teil 1/6 
# Was ist eine Mehrebenenregression?
# Dr. Uwe Remer

# Dieses RScript zu Video 1 enthält nur den RCode zu den im Video genutzten Grafiken.
# Dieser Code ist nicht Gegenstand der Lernziele und daher nicht didaktisch aufbereitet.
# Der Vollständigkeit wegen wird dieser Code dennoch dokumentiert.



library(foreign)
ess <- read.spss("./Daten/ESS9e02.sav", 
                 use.value.labels = FALSE,
                 to.data.frame = TRUE,
                 reencode = TRUE)


library(ggplot2)
ggplot(ess, aes(x=cntry)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), 
            col="white",
            hjust=1,
            angle=90) +
  theme_minimal() + 
  scale_x_discrete("Länder") +
  theme(axis.text=element_text(size=6))



ggplot(ess, aes(x=cntry, color=region)) +
  geom_bar(position = "stack", show.legend = F) +
  theme_minimal() + 
  scale_x_discrete("Länder mit Regionen") +
  theme(axis.text=element_text(size=6))
