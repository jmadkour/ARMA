#===============================================================================
#                     SIMULATION DE PROCESSUS ARMA(p,q)
#===============================================================================

# Fixer la graine pour la reproductibilité
set.seed(123)  

# Définir les paramètres du modèle ARMA(p,q)
modele = list( ar = c(0.2, 0.1), # Coefficients de la composante AR
               ma = c(2, 0.5) )  # Coefficients de la composante MA

# Simuler le processus stochastique y
y = arima.sim(model = modele,   # Coefficients du modèle ARMA(p,q)
              n = 1000,         # Nombres de valeurs à simuler
              sd = 1 )          # Écart-type du terme d'erreur

# Afficher les premières valeurs simulées
head(y)

# Retrouver les ordres p et q du modèle ARMA
p = length(modele$ar)
q = length(modele$ma)

# Visualiser la série temporelle simulée
plot(y, main = paste0("Simulation de données à partir d'un modèle ARMA(",p,",",q,")"),
     xlab = "Temps",
     ylab = "Données simulées", 
     col = "blue", 
     type = "l")
