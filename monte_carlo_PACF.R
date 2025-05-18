# Paramètres du modèle MA(1)
theta <- 1        # constante
beta1 <- 0.5      # coefficient MA(1)
sigma_eps <- 1    # écart-type du bruit blanc

# Nombre de simulations
nsim <- 100000

# Tailles d'échantillons
tailles <- seq(50, 500, by = 50)

# Initialisation du tableau de résultats (seulement pour PACF + moyenne + variance)
resultats <- data.frame(
  n = c(tailles, "Théorique"),
  moy_moy = rep(NA, length(tailles) + 1),
  moy_var = rep(NA, length(tailles) + 1),
  moy_pacf1 = rep(NA, length(tailles) + 1),
  moy_pacf2 = rep(NA, length(tailles) + 1),
  moy_pacf3 = rep(NA, length(tailles) + 1),
  moy_pacf4 = rep(NA, length(tailles) + 1),
  moy_pacf5 = rep(NA, length(tailles) + 1)
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
  pacf1 <- pacf2 <- pacf3 <- pacf4 <- pacf5 <- numeric(nsim)
  
  # Boucle de simulation
  for (sim in 1:nsim) {
    epsilon <- rnorm(n, mean = 0, sd = sigma_eps)
    y <- numeric(n)
    
    # Simulation MA(1)
    for (t in 1:n) {
      if (t == 1) {
        y[t] <- theta + epsilon[t]  # Pas de terme passé au premier instant
      } else {
        y[t] <- theta + epsilon[t] + beta1 * epsilon[t - 1]
      }
    }
    
    # Sauvegarder moyenne et variance
    moyennes[sim] <- mean(y)
    variances[sim] <- var(y)
    
    # Estimer les autocorrélations partielles (PACF) jusqu'au lag 5
    pacf_est <- tryCatch({
      pacf(y, lag.max = 5, plot = FALSE)$acf
    }, error = function(e) {
      rep(NA, 5)
    })
    
    if (!is.null(pacf_est) && length(pacf_est) == 5) {
      pacf1[sim] <- pacf_est[1]
      pacf2[sim] <- pacf_est[2]
      pacf3[sim] <- pacf_est[3]
      pacf4[sim] <- pacf_est[4]
      pacf5[sim] <- pacf_est[5]
    }
  }
  
  # Mise à jour du tableau progressivement
  resultats$moy_moy[i] <- round(mean(moyennes), 2)
  resultats$moy_var[i] <- round(mean(variances), 2)
  resultats$moy_pacf1[i] <- round(mean(pacf1, na.rm = TRUE), 2)
  resultats$moy_pacf2[i] <- round(mean(pacf2, na.rm = TRUE), 2)
  resultats$moy_pacf3[i] <- round(mean(pacf3, na.rm = TRUE), 2)
  resultats$moy_pacf4[i] <- round(mean(pacf4, na.rm = TRUE), 2)
  resultats$moy_pacf5[i] <- round(mean(pacf5, na.rm = TRUE), 2)
  
  # Affichage du tableau mis à jour après chaque itération
  print(paste("Mise à jour pour n =", n))
  print(resultats)
}

# Calcul des valeurs théoriques
mu_theorique <- theta
var_theorique <- sigma_eps^2 * (1 + beta1^2)

# Estimation approximative de la PACF théorique via simulation très précise
set.seed(1)
n_grand <- 100000
epsilon_grand <- rnorm(n_grand, 0, sigma_eps)
y_grand <- numeric(n_grand)

for (t in 1:n_grand) {
  if (t == 1) {
    y_grand[t] <- theta + epsilon_grand[t]
  } else {
    y_grand[t] <- theta + epsilon_grand[t] + beta1 * epsilon_grand[t - 1]
  }
}

# Calcul de la PACF théorique approchée
pacf_th <- pacf(y_grand, lag.max = 5, plot = FALSE)$acf

# Ajout des valeurs théoriques dans la dernière ligne
resultats$moy_moy[length(tailles) + 1] <- round(mu_theorique, 2)
resultats$moy_var[length(tailles) + 1] <- round(var_theorique, 2)
resultats$moy_pacf1[length(tailles) + 1] <- round(pacf_th[1], 2)
resultats$moy_pacf2[length(tailles) + 1] <- round(pacf_th[2], 2)
resultats$moy_pacf3[length(tailles) + 1] <- round(pacf_th[3], 2)
resultats$moy_pacf4[length(tailles) + 1] <- round(pacf_th[4], 2)
resultats$moy_pacf5[length(tailles) + 1] <- round(pacf_th[5], 2)

# Affichage final du tableau complet
print("Tableau final avec valeurs théoriques (PACF seulement - MA(1)) :")
print(resultats)

# Option : Exportation des résultats dans un fichier CSV
# write.csv(resultats, "resultats_simulations_MA1_PACF_seulement.csv", row.names = FALSE)