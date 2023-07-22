# Videoserie Mehrebenenregression mit R 
# Teil 6/6 
# Wie lassen sich die Ergebnisse einer Mehrebenregression berichten?
# Dr. Uwe Remer


# Ausführen des Scriptes aus Video 5
# Dieses bereitet die Daten vor
source("./Ressourcen/RScript_mreg5.R") # Pfad zur Datei ggf. anpassen


#### Tabellen #### 


library(texreg)
screenreg(list(mreg3,mreg4))


wordreg(list(mreg3,mreg4),
         file = "./Docs/tabelle.doc")




# Wenn Sie RMarkdown nutzen:
#htmlreg(list(mreg3,mreg4))


# Markdown Code-Chunks

```{r}
#Vorbereitung
source("./RScript_mreg5.R")
library(texreg)
```

```{r, results='asis'}
# Der eigentliche Tabellenaufruf
htmlreg(list(mreg3,mreg4))
#texreg(list(mreg3,mreg4))
```



library(sjPlot)
#?tab_model
# Gibt Tabelle im RStudio Viewer aus
tab_model(mreg0, mreg3, mreg4, 
          p.style = "stars",
          use.viewer = T)

tab_model(mreg0, mreg3, mreg4, 
          p.style = "stars",
          use.viewer = F,
          file = "./Docs/tabelle.html")



#### Grafiken ####


library(sjPlot)
plot_model(mreg4,
           show.values=T)


library(sjPlot)
plot_models(mreg3, mreg4,
           show.values=T)


plot_model(mreg4, type="re")

plot_model(mreg4, type="re", sort.est = "bildung")
plot_model(mreg4, type="re", sort.est = "(Intercept)")



plot_model(mreg4, type="int")
