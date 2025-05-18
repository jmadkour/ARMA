# Paramètres du modèle AR(1)
theta <- 1
alpha1 <- 0.5
sigma_eps <- 1

# Nombre de simulations
nsim <- 100000

# Tailles d'échantillons
tailles <- seq(50, 500, by = 50)

# Initialisation du tableau de résultats rempli de NA
resultats <- data.frame(
  n = c(tailles, "Théorique"),
  moy_moy = rep(NA, length(tailles) + 1),
  moy_var = rep(NA, length(tailles) + 1),
  moy_acf1 = rep(NA, length(tailles) + 1),
  moy_acf2 = rep(NA, length(tailles) + 1),
  moy_acf3 = rep(NA, length(tailles) + 1),
  moy_acf4 = rep(NA, length(tailles) + 1),
  moy_acf5 = rep(NA, length(tailles) + 1)
)

# Affichage du tableau vide (rempli de NA)
print("Tableau initial rempli de NA :")
print(resultats)

# Boucle sur les tailles d'échantillons
for (i in seq_along(tailles)) {
  n <- tailles[i]
  
  # Vecteurs pour stocker les résultats de chaque simulation
  moyennes <- numeric(nsim)
  variances <- numeric(nsim)
  acf1 <- acf2 <- acf3 <- acf4 <- acf5 <- numeric(nsim)
  
  # Boucle de simulation
  for (sim in 1:nsim) {
    y <- numeric(n)
    epsilon <- rnorm(n, mean = 0, sd = sigma_eps)
    
    # Initialisation stationnaire
    y[1] <- rnorm(1, mean = theta / (1 - alpha1), sd = sqrt(sigma_eps^2 / (1 - alpha1^2)))
    
    # Simulation AR(1)
    for (t in 2:n) {
      y[t] <- theta + alpha1 * y[t - 1] + epsilon[t]
    }
    
    # Sauvegarder moyenne et variance
    moyennes[sim] <- mean(y)
    variances[sim] <- var(y)
    
    # Estimer les autocorrélations jusqu'au lag 5
    acf_est <- acf(y, lag.max = 5, plot = FALSE)$acf[-1]  # Exclure lag 0
    
    acf1[sim] <- acf_est[1]
    acf2[sim] <- acf_est[2]
    acf3[sim] <- acf_est[3]
    acf4[sim] <- acf_est[4]
    acf5[sim] <- acf_est[5]
  }
  
  # Mise à jour du tableau progressivement
  resultats$moy_moy[i] <- round(mean(moyennes), 2)
  resultats$moy_var[i] <- round(mean(variances), 2)
  resultats$moy_acf1[i] <- round(mean(acf1), 2)
  resultats$moy_acf2[i] <- round(mean(acf2), 2)
  resultats$moy_acf3[i] <- round(mean(acf3), 2)
  resultats$moy_acf4[i] <- round(mean(acf4), 2)
  resultats$moy_acf5[i] <- round(mean(acf5), 2)
  
  # Affichage du tableau mis à jour après chaque itération
  print(paste("Mise à jour pour n =", n))
  print(resultats)
}

# Calcul des valeurs théoriques
mu_theorique <- theta / (1 - alpha1)
var_theorique <- sigma_eps^2 / (1 - alpha1^2)
acf_theorique <- sapply(1:5, function(k) alpha1^k)

# Ajout des valeurs théoriques dans la dernière ligne
resultats$moy_moy[length(tailles) + 1] <- round(mu_theorique, 2)
resultats$moy_var[length(tailles) + 1] <- round(var_theorique, 2)
resultats$moy_acf1[length(tailles) + 1] <- round(acf_theorique[1], 2)
resultats$moy_acf2[length(tailles) + 1] <- round(acf_theorique[2], 2)
resultats$moy_acf3[length(tailles) + 1] <- round(acf_theorique[3], 2)
resultats$moy_acf4[length(tailles) + 1] <- round(acf_theorique[4], 2)
resultats$moy_acf5[length(tailles) + 1] <- round(acf_theorique[5], 2)

# Affichage final du tableau complet
print("Tableau final avec valeurs théoriques :")
print(resultats)

# write.csv(resultats, "resultats_simulations_AR1.csv", row.names = FALSE)