installed.packages("TTR", "quantmod") ##intallo pacchetti necessari 
library(quantmod)
library(TTR)

#scarichiamo dati storici petrolio WTI
getSymbols("WTISPLC", src = "FRED", from= "01-01-2000", auto.assign = FALSE )  # Prezzi spot WTI da FRED, from= "2000-01-01", auto.assign = FALSE)
# stiamo dicendo cosa, da dove, da quando scaricare il petrolio 
#FALSE fondamentale altrimenti caricherebbe una nuova variabile 
##ho dovuto cambiare yahoo causa non funzionamento con fred. 

#estraiamo i prezzi di chiusura  della serie storica del petrolio 
close_prices <- na.omit(Cl(oil)) 

#calcolo medie mobili 
sma50 <- SMA(close_prices, 50) ##media mobile 50 giorni
sma200 <- SMA(close_prices, 200) ##media mobile 200 giorni 

#fondamentale unione delle serie, prezzo petrolio e medie mobili
oil_clean <- na.omit(merge(close_prices, sma50, sma200)) ##forza r ad omettere NA
colnames(oil_clean) <- c("Close", "SMA50", "SMA200")
##  na.omit per togliere le serie vuote, col names per ordinare per leggibilità 

# preparo incontro fra sma50 e sma200 (goldern cross e death cross)
golden_cross <- which(diff(oil_clean$SMA50 > oil_clean$SMA200) == 1) + 1 ##golden cross (segnale bullish) quando SMA50 > SMA200
#golden cross bullish
death_cross <- which(diff(oil_clean$SMA200 < oil_clean$SMA50) == 1) + 1 ##death cross (segnale bearish) quando SMA200 > SMA50
#death cross bearish 
## +1 perchè diff accorcia la serie di 1 

## ora affinchè il codice funzioni occorre trasformare la serie in (formato) numerico
dates <- index(oil_clean)
close <- as.numeric(oil_clean$Close)
sma50 <- as.numeric(oil_clean$SMA50)
sma200 <- as.numeric(oil_clean$SMA200)

# siccome l'output della legenda è troppo grande devi spostarlo verso destra
# per spostare a destra si usa la funzione par che sposta il contenuto fuori dal grafico prima di crearlo
# è fondameale mettere xpd=true perchè se fosse false  li metterebbe all'interno
par(xpd= TRUE, mar = c(5, 4, 4, 6))

#legenda
plot(dates, close, type = "l", col = "black", lwd = 2,
     main = "Prezzo WTI con SMA50, SMA200, Golden & Death Cross",
     xlab = "Data", ylab = "Prezzo")

# linee delle medie mobili sul grafico, quella a 200gg più spessa perchè più rilevante
lines(dates, sma50, col="green", lwd=1)
lines(dates, sma200, col="orange", lwd=1.5)

## imposto i punti nel grafico dei miei cross
points(dates[golden_cross], close[golden_cross], col = "blue", pch = 16, cex = 1)
points(dates[death_cross],  close[death_cross],  col = "red", pch = 17, cex = 1)

# Coordinata di partenza in alto a destra del grafico
x0 <- as.Date("2023-01-01")
y0 <- 140

# Distanze verticali
dy <- 5

# Scritte e simboli
labels <- c("Prezzo", "SMA50", "SMA200", "Golden", "Death")
colors <- c("black", "green", "orange", "blue", "red")
types <- c("line", "line", "line", "point", "point")

# Loop per costruire la legenda riga per riga, for perchè appunto loop infinito sulla mia serie storica 
for (i in seq_along(labels)) {
  y <- y0 - (i - 1) * dy
  
  if (types[i] == "line") {
    segments(x0 - 100, y, x0 - 50, y, col = colors[i], lwd = ifelse(i == 3, 1.5, 1))
  } else {
    points(x0 - 75, y, col = colors[i], pch = ifelse(i == 4, 16, 17), cex = 0.8)
  }
  
  text(x0 - 40, y, labels[i], adj = 0, cex = 0.6)
}



