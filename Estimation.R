#===============================================================================
#                     ESTIMATION DE PROCESSUS ARMA(p,q)
#===============================================================================

# Installation des bibliothèques
# install.packages("quantmod")
# install.packages("forecast") # Essayer également le package récent "fable" 
# install.packages("tseries")
# install.packages("tsibble")
# install.packages("fable")
# install.packages("feasts")
# install.packages("ggplot2")

# Chargement des bibliothèques
library(quantmod)          # Pour le téléchargement des données financières
library(forecast)          # Pour la fonction auto.arima()
library(tseries)           # Pour la fonction arma()
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)


# Données
## Téléchargement des données
ticker = "GOOG"            # Symbole de l'action (exemple : Google)
start_date = "2020-01-01"  # Date de début
end_date = "2025-03-24"    # Date de fin

data = getSymbols(
  Symbols = ticker,
  src = "yahoo",           # https://finance.yahoo.com/markets/
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

head(data)

## Extraire le cours ajusté à la clôture
adjclose_prices <- Ad(data) 

## Calcul des log-rentabilités
log_returns <- diff(log(adjclose_prices))  # Log-rentabilités
log_returns <- na.omit(log_returns)        # Supprimer les valeurs manquantes

head(log_returns)

## Renommer le processus stochastique à modéliser
y = log_returns

# Estimation des paramètres du modèle ARMA(p,q) sélectionné

## Estimation avec la fonction auto.arima() du la bibliothèque "forecast"
fit = auto.arima(y, 
                 d = 0,        # Fixe la différenciation à 0 (donc modèle ARMA, pas ARIMA)
                 max.p = 5,    # Ordre AR maximum à tester
                 max.q = 5,    # Ordre MA maximum à tester
                 ic = "bic",   # Critère d'information de Schwarz
                 stepwise = FALSE)  # Pour une recherche exhaustive (plus précise mais plus lente)

p <- fit$arma[1]
q <- fit$arma[2]

summary(fit)

### Validation
checkresiduals(fit)                           # Visualisation des résidus
acf(residuals(fit))                           # ACF des résidus
Box.test(residuals(fit), type = "Ljung-Box")  # Test de blancheur

## Estimation avec la fonction arima() du la bibliothèque "stats" chargée par défaut
fit2 <- arima(y, order = c(p, 0, q))

summary(fit2)

## Estimation avec la fonction arma() du la bibliothèque "tseries"
fit3 <- arma(y, order = c(p, q))

summary(fit3)

## Estimation avec la bibliothèque "fable"

### Convertir en tsibble (format compatible fable)
ts_data <- tibble(
  index = 1:length(y),
  value = y
) %>%
  as_tsibble(index = index)

head(ts_data)

### Ajuster un modèle ARIMA
fit <- ts_data %>%
  model(ARIMA(value ~ pdq(p, 0, q)))

report(fit)

### Diagnostiquer les résidus
fit %>%
  gg_tsresiduals()

### Test de Ljung-Box sur les résidus
augment(fit) %>%
  features(.resid, ljung_box, lag = 10)




