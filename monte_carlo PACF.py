import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import pacf
import warnings

# Paramètres du modèle MA(1)
theta = 1         # constante
beta1 = 0.5       # coefficient MA(1)
sigma_eps = 1     # écart-type du bruit blanc

# Nombre de simulations
nsim = 100_000

# Tailles d'échantillons
tailles = list(range(50, 501, 50))  # De 50 à 500 par pas de 50

# Initialisation du tableau de résultats (PACF + moyenne + variance)
resultats = pd.DataFrame({
    'n': tailles + ['Théorique'],
    'moy_moy': [np.nan] * (len(tailles) + 1),
    'moy_var': [np.nan] * (len(tailles) + 1),
    'moy_pacf1': [np.nan] * (len(tailles) + 1),
    'moy_pacf2': [np.nan] * (len(tailles) + 1),
    'moy_pacf3': [np.nan] * (len(tailles) + 1),
    'moy_pacf4': [np.nan] * (len(tailles) + 1),
    'moy_pacf5': [np.nan] * (len(tailles) + 1)
})

print("Tableau initial rempli de NaN :")
print(resultats)

# Boucle sur les tailles d'échantillons
for i, n in enumerate(tailles):
    print(f"Simulation pour n = {n}")
    
    # Tableaux pour stocker les résultats de chaque simulation
    moyennes = []
    variances = []
    pacf1 = pacf2 = pacf3 = pacf4 = pacf5 = []

    for _ in range(nsim):
        epsilon = np.random.normal(0, sigma_eps, size=n)
        y = np.zeros(n)

        # Simulation MA(1)
        for t in range(n):
            if t == 0:
                y[t] = theta + epsilon[t]
            else:
                y[t] = theta + epsilon[t] + beta1 * epsilon[t - 1]

        # Sauvegarder moyenne et variance
        moyennes.append(np.mean(y))
        variances.append(np.var(y))

        # Calculer la PACF
        try:
            pacf_est = pacf(y, nlags=5)[1:]  # Ignorer le lag 0
            pacf1.append(pacf_est[0])
            pacf2.append(pacf_est[1])
            pacf3.append(pacf_est[2])
            pacf4.append(pacf_est[3])
            pacf5.append(pacf_est[4])
        except Exception as e:
            # En cas d'erreur, on ajoute des NaN
            pacf1.append(np.nan)
            pacf2.append(np.nan)
            pacf3.append(np.nan)
            pacf4.append(np.nan)
            pacf5.append(np.nan)

    # Mise à jour du tableau
    resultats.loc[i, 'moy_moy'] = round(np.mean(moyennes), 2)
    resultats.loc[i, 'moy_var'] = round(np.mean(variances), 2)
    resultats.loc[i, 'moy_pacf1'] = round(np.nanmean(pacf1), 2)
    resultats.loc[i, 'moy_pacf2'] = round(np.nanmean(pacf2), 2)
    resultats.loc[i, 'moy_pacf3'] = round(np.nanmean(pacf3), 2)
    resultats.loc[i, 'moy_pacf4'] = round(np.nanmean(pacf4), 2)
    resultats.loc[i, 'moy_pacf5'] = round(np.nanmean(pacf5), 2)

    print(f"Mis à jour pour n = {n}:")
    print(resultats.iloc[:i+1])

# Calcul des valeurs théoriques
mu_theorique = theta
var_theorique = sigma_eps**2 * (1 + beta1**2)

# Estimation approchée de la PACF théorique avec un grand échantillon
n_grand = 100_000
epsilon_grand = np.random.normal(0, sigma_eps, size=n_grand)
y_grand = np.zeros(n_grand)

for t in range(n_grand):
    if t == 0:
        y_grand[t] = theta + epsilon_grand[t]
    else:
        y_grand[t] = theta + epsilon_grand[t] + beta1 * epsilon_grand[t - 1]

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    pacf_th = pacf(y_grand, nlags=5)[1:]  # Ignorer le lag 0

# Ajout des valeurs théoriques dans la dernière ligne
last_idx = len(tailles)
resultats.loc[last_idx, 'moy_moy'] = round(mu_theorique, 2)
resultats.loc[last_idx, 'moy_var'] = round(var_theorique, 2)
resultats.loc[last_idx, 'moy_pacf1'] = round(pacf_th[0], 2)
resultats.loc[last_idx, 'moy_pacf2'] = round(pacf_th[1], 2)
resultats.loc[last_idx, 'moy_pacf3'] = round(pacf_th[2], 2)
resultats.loc[last_idx, 'moy_pacf4'] = round(pacf_th[3], 2)
resultats.loc[last_idx, 'moy_pacf5'] = round(pacf_th[4], 2)

# Affichage final
print("\nTableau final avec valeurs théoriques (PACF seulement - MA(1)) :")
print(resultats)

# Option : Exportation des résultats dans un fichier CSV
# resultats.to_csv("resultats_simulations_MA1_PACF_seulement.csv", index=False)