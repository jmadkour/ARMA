#===============================================================================
#                     IDENTIFICATION DE PROCESSUS ARMA(p,q)
#===============================================================================

# Installation des bibliothèques
# install.packages("quantmod")
# install.packages("TSA")
# install.packages("forecast") # Essayer également le package récent "fable" 

# Chargement des bibliothèques
library(quantmod)          # Pour le téléchargement des données financières
library(TSA)               # Pour la fonction eacf()
library(forecast)          # Pour la fonction auto.arima()

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

# Fonctions d'autocorrélation

## Fonction d'autocorrélation (ACF)
print(acf(log_returns, lag.max = 5, main = ticker))

## Fonction d'autocorrélation partielle (PACF)
print(pacf(log_returns, lag.max = 5, main = ticker))

## Fonction d'autocorrélation étendue (EACF)
eacf_result <- eacf(log_returns, ar.max = 5, ma.max = 5)

# Sélection automatique à l'aide d'un critère d'information

## Critère d'information d'Akaike
fit = auto.arima(log_returns, 
                 d = 0,        # Fixe la différenciation à 0 (donc modèle ARMA, pas ARIMA)
                 max.p = 5,    # Ordre AR maximum à tester
                 max.q = 5,    # Ordre MA maximum à tester
                 ic = "aic",   # Critère d'information d'Akaike
                 stepwise = FALSE)  # Pour une recherche exhaustive (plus précise mais plus lente)

### Extraire les valeurs de p et q :
p <- fit$arma[1]
q <- fit$arma[2]
cat("Modèle sélectionné : ARMA(", p, ",", q, ")\n", sep = "")

## Critère d'information de Schwarz
fit = auto.arima(log_returns, 
                 d = 0,        # Fixe la différenciation à 0 (donc modèle ARMA, pas ARIMA)
                 max.p = 5,    # Ordre AR maximum à tester
                 max.q = 5,    # Ordre MA maximum à tester
                 ic = "bic",   # Critère d'information de Schwarz
                 stepwise = FALSE)  # Pour une recherche exhaustive (plus précise mais plus lente)

### Extraire les valeurs de p et q :
p <- fit$arma[1]
q <- fit$arma[2]
cat("Modèle sélectionné : ARMA(", p, ",", q, ")\n", sep = "")


