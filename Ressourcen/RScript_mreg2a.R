# Videoserie Mehrebenenregression mit R 
# Teil 2/6 (erste Hälfte des Videos)
# Was ist eine Mehrebenenregression?
# Dr. Uwe Remer



# Dieses erste RScript zu Video 2 "RScript_mreg2a.R" enthält nur den RCode zu den 
# Grafiken, die in der ersten Hälfte des Videos gezeigt wurden.
# Dieser Code ist nicht Gegenstand der Lernziele und daher nicht didaktisch aufbereitet.
# Der Vollständigkeit wegen, wird dieser Code dennoch dokumentiert.

# Der RCode, welcher in der zweiten Hälfte des Videos in R gezeigt wird befindet sich
# in der Datei "RScript_mreg2b.R"


set.seed(2022)

library(foreign)
ess <- read.spss("./Daten/ESS9e02.sav", 
                 use.value.labels = FALSE,
                 to.data.frame = TRUE,
                 reencode = TRUE)
idx_vars <- c("trstprl","trstplt","trstprt")
ess$pol_vertrauen <- rowMeans(ess[,idx_vars], 
                              na.rm = F)
ess$zufr_wirtschaft <- ess$stfeco
# Subset an Ländern, da sonst zu viele Daten
laender <- c("BG","CY","DE","NO","SE","FR")
ess_c6 <- ess[ess$cntry %in% laender, ]
# Gruppierungsvariable ohne Gruppen (complete pooling)
ess$no_countries <- "alle Befragte"
ess_c6$no_countries <- "alle Befragte"
# Nur Gültige
ess_c6 <- na.omit(ess_c6[,c("pol_vertrauen",
                            "zufr_wirtschaft",
                            "cntry", 
                            "no_countries")])
# Mittelwert
means_df <- aggregate(list(pol_vertrauen = ess_c6$pol_vertrauen),
                           by=list(cntry = ess_c6$cntry),
                           FUN=mean, na.rm=T)
grand_mean <- mean(means_df[,2])

library(ggplot2)
library(ggtext)
library(grid)
#devtools::install_github("wilkelab/ungeviz")
library(ungeviz)
ggplot(ess_c6, aes(x=pol_vertrauen, y=no_countries)) +
  geom_point() +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,2,0,2), "cm"))


ggplot(ess, aes(pol_vertrauen, no_countries)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=.5, alpha =.3, shape=".") +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,2,0,2), "cm"))


l <- length(ess_c6[,1])
ess_c6 <- ess_c6[sample(1:l,l*0.15),]
ggplot(ess_c6, aes(pol_vertrauen, no_countries)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1) +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,2,0,2), "cm"))

ggplot(ess_c6, aes(pol_vertrauen, no_countries)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1) +
  stat_summary(fun=mean, geom="vpline", 
               size=1.2, height = 1, show.legend=FALSE) +  
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +    
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,2,0,2), "cm"))


farben <- c("#00966E", "#D57800", "#FFCC00", "#002654","#BA0C2F", "#006AA7")
ggplot(ess_c6, 
       aes(pol_vertrauen, no_countries, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =1) +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,2), "cm")) +
  guides(colour = guide_legend(override.aes = list(size=2)))

flags <- c("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Bulgaria.svg/320px-Flag_of_Bulgaria.svg.png' alt='Bulgarien', title='Bulgarien'  width='20' /><br>",
           "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Flag_of_Cyprus.svg/320px-Flag_of_Cyprus.svg.png'  width='20' /><br>",
           "<img src='https://upload.wikimedia.org/wikipedia/en/thumb/b/ba/Flag_of_Germany.svg/320px-Flag_of_Germany.svg.png'  width='20' /><br>",
           "<img src='https://upload.wikimedia.org/wikipedia/en/thumb/c/c3/Flag_of_France.svg/320px-Flag_of_France.svg.png'  width='20' /><br>",
           "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/320px-Flag_of_Norway.svg.png'  width='20' /><br>",
           "<img src='https://upload.wikimedia.org/wikipedia/en/thumb/4/4c/Flag_of_Sweden.svg/320px-Flag_of_Sweden.svg.png'  width='20' /><br>")
ggplot(ess_c6, 
       aes(pol_vertrauen, cntry, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =1) +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  scale_y_discrete(labels = flags) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,2), "cm"),
        axis.text.x  = element_markdown(color = "black", size = 7)) +
  guides(colour = guide_legend(override.aes = list(size=2)))

ggplot(ess_c6, 
       aes(pol_vertrauen, cntry, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =.5) +
  stat_summary(fun=mean, geom="vpline", 
               size=1.2, height = 1, show.legend=FALSE) +  
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  scale_y_discrete(labels = flags) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,2), "cm"),
        axis.text.x  = element_markdown(color = "black", size = 7)) +
  guides(colour = guide_legend(override.aes = list(size=2)))


ggplot(ess_c6, 
       aes(pol_vertrauen, cntry, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =.5) +
  stat_summary(fun=mean, geom="vpline", 
               size=1.2, height = 1, show.legend=FALSE) +  
  geom_vline(xintercept=grand_mean, col="black", size=1) +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  scale_y_discrete(labels = flags) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,2), "cm"),
        axis.text.x  = element_markdown(color = "black", size = 7)) +
  guides(colour = guide_legend(override.aes = list(size=2)))


V2_1_8 <- ggplot(ess_c6, 
       aes(pol_vertrauen, cntry, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =.5, show.legend = F) +
  stat_summary(fun=mean, geom="vpline", 
               size=1.2, height = 1, show.legend=FALSE) +  
  geom_vline(xintercept=grand_mean, col="black", size=1) +
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10, by=2)) +  
  scale_y_discrete(labels = flags) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="Pol. Vertrauen") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        axis.text.x  = element_markdown(color = "black", size = 2)) 
# Zentrieren um den Gruppenmittelwert
sdf <- split(ess_c6, ess_c6$cntry)
ess_c6$pol_vertrauen_gc <- do.call("c", sapply(sdf, function(x) x$pol_vertrauen - mean(x$pol_vertrauen, na.rm=T)))
means_df_gc <- aggregate(list(pol_vertrauen_gc = ess_c6$pol_vertrauen_gc),
                           by=list(cntry = ess_c6$cntry),
                           FUN=mean, na.rm=T)
grand_mean_gc <- mean(means_df_gc[,2])
V2_1_9 <- ggplot(ess_c6, 
       aes(pol_vertrauen_gc, cntry, color=cntry)) +
  geom_jitter(position = position_jitter(width=.3, height = .3, seed = 2022), 
              size=1, alpha =.5, show.legend= F) +
  geom_vline(xintercept=grand_mean_gc, col="black", size=1) +
  scale_x_continuous(limits = c(-10,10), breaks=seq(-10,10, by=2)) +  
  scale_y_discrete(labels = flags) +  
  scale_colour_manual(values = farben) +
  coord_flip() +
  labs(y="Länder", x="") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        plot.margin=unit(c(0,.3,0,0), "cm"),
        axis.text.x  = element_markdown(color = "black", size = 2)) 
library(gridExtra)
grid.arrange(V2_1_8, V2_1_9, nrow=1)


gesamtvarianz <- var(ess_c6$pol_vertrauen, na.rm=T)
varianz_L1 <- var(ess_c6$pol_vertrauen_gc, na.rm=T)
varianz_L2 <- gesamtvarianz - varianz_L1
varianz_L2/gesamtvarianz
