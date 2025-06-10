install.packages("forecast")
library(forecast)

# Série utilisée: 11743, STATISTIQUES D'EXPLOITATION ET FINANCIÈRES DES PRINCIPAUX TRANSPORTEURS AÉRIENS CANADIENS, MENSUEL

avion_raw = read.csv("C:/_Code/courses/metho_prevision/devoir3/avion1.txt",header=TRUE, dec=".",fill=TRUE)

plot(avion_raw$V11743, main = "Série complète")
avion = avion_raw[349:456,]

plot(avion$V11743, main = "Nombre de passagers des principaux transporteurs
     canadiens par mois", xlab = "Mois", ylab = "Milliers de passagers", xaxt="n")

axis(1, at=seq(1,108, by=12), labels = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))


avion_time_series = ts(data=avion$V11743, frequency = 12, start = c(2010, 1))
avion_time_series
plot.ts(avion_time_series)


components = decompose(avion_time_series)
components$seasonal
components$trend
plot(components)



# Application de la différentiation

avion_diff1 = diff(avion_time_series, differences = 1)
plot(avion_diff1, main = "Première différence")
# On voit qu'avec une différentiation de degré 1, la série semble avoir 
# une moyenne constante.
# On applique donc une différentiation de degré 1. 

diff2 = diff(avion_time_series, differences = 2)
plot(diff2)


# ACF
acf(avion_diff1, plot=FALSE, lag.max = 48)
acf(avion_diff1, lag.max = 48)
# Avec ACF on constate qu'il y a présence d'autocorrélation significative entre
# chaque mois et le même mois des années précédentes. Par exemple, le nombre de
# passagers du mois de juin 2015 sera corrélé avec le nombre de passagers en 
# juin 2014, juin 2013, etc, et de moins en moins pour les années plus éloignées.
# La corrélation n'est pas significative avec les autres mois de l'année.

# PACF
acf(avion_diff1, type = "partial", lag.max = 48)



fit = auto.arima(avion_time_series)

arimaorder(fit)

# SARIMA (0,1,0)(0, 1, 1)[12]


# Performance prévisionnelle
checkresiduals(fit)





pred = forecast(fit, h=12)
pred
pred_2019 = pred$mean


avion_2019= avion_raw[457:468,]
months = c(2019, 2019+1/12, 2019+2/12, 2019+3/12, 2019+4/12, 2019+5/12, 2019+6/12, 2019+7/12, 2019+8/12, 2019+9/12, 2019+10/12, 2019+11/12)
months_names=c("Jan", "Fev", "Mar", "Avr", "Mai", "Juin", "Juil", "Aout", "Sept", "Oct", "Nov", "Dec")


plot(forecast(fit), xlim = c(2019, 2019+11/12), 
     main="Comparaison des prévisions pour 2019 avec les valeurs réelles",
      xaxt="n")
axis(1, at=months, labels = months_names )
lines(months, 
      avion_2019$V11743,
      col = "red")
legend(x = "topleft",legend=c("Prévision","Bandes de prévision", "Valeurs réelles"), fill=c("royalblue","grey", "red"))


# Erreur
100*mean(abs((pred_2019-avion_2019$V11743)/avion_2019$V11743))

