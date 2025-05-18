import numpy as np
import pandas as pd
from statsmodels.tsa.stattools import acf

# Paramètres du modèle AR(1)
theta = 1         # constante
alpha1 = 0.5      # coefficient AR(1)
sigma_eps = 1     # écart-type du bruit blanc

# Nombre de simulations
nsim = 100_000

# Tailles d'échantillons
tailles = list(range(50, 501, 50))  # De 50 à 500 par pas de 50

# Initialisation du tableau de résultats rempli de NaN
resultats = pd.DataFrame({
    'n': tailles + ['Théorique'],
    'moy_moy': [np.nan] * (len(tailles) + 1),
    'moy_var': [np.nan] * (len(tailles) + 1),
    'moy_acf1': [np.nan] * (len(tailles) + 1),
    'moy_acf2': [np.nan] * (len(tailles) + 1),
    'moy_acf3': [np.nan] * (len(tailles) + 1),
    'moy_acf4': [np.nan] * (len(tailles) + 1),
    'moy_acf5': [np.nan] * (len(tailles) + 1)
})

print("Tableau initial rempli de NaN :")
print(resultats)

# Boucle sur les tailles d'échantillons
for i, n in enumerate(tailles):
    print(f"Simulation pour n = {n}")
    
    # Tableaux pour stocker les résultats de chaque simulation
    moyennes = []
    variances = []
    acf1 = acf2 = acf3 = acf4 = acf5 = []

    for _ in range(nsim):
        y = np.zeros(n)
        epsilon = np.random.normal(0, sigma_eps, size=n)

        # Initialisation stationnaire
        mu_y0 = theta / (1 - alpha1)
        sigma_y0 = np.sqrt(sigma_eps**2 / (1 - alpha1**2))
        y[0] = np.random.normal(mu_y0, sigma_y0)

        # Simulation AR(1)
        for t in range(1, n):
            y[t] = theta + alpha1 * y[t - 1] + epsilon[t]

        # Sauvegarder moyenne et variance
        moyennes.append(np.mean(y))
        variances.append(np.var(y))

        # Estimation des autocorrélations jusqu'au lag 5
        try:
            acf_est = acf(y, nlags=5)[1:]  # Exclure le lag 0
            acf1.append(acf_est[0])
            acf2.append(acf_est[1])
            acf3.append(acf_est[2])
            acf4.append(acf_est[3])
            acf5.append(acf_est[4])
        except Exception as e:
            # En cas d'erreur, on ajoute des NaN
            acf1.append(np.nan)
            acf2.append(np.nan)
            acf3.append(np.nan)
            acf4.append(np.nan)
            acf5.append(np.nan)

    # Mise à jour du tableau progressivement
    resultats.loc[i, 'moy_moy'] = round(np.mean(moyennes), 2)
    resultats.loc[i, 'moy_var'] = round(np.mean(variances), 2)
    resultats.loc[i, 'moy_acf1'] = round(np.nanmean(acf1), 2)
    resultats.loc[i, 'moy_acf2'] = round(np.nanmean(acf2), 2)
    resultats.loc[i, 'moy_acf3'] = round(np.nanmean(acf3), 2)
    resultats.loc[i, 'moy_acf4'] = round(np.nanmean(acf4), 2)
    resultats.loc[i, 'moy_acf5'] = round(np.nanmean(acf5), 2)

    print(f"Mis à jour pour n = {n}:")
    print(resultats.iloc[:i+1])

# Calcul des valeurs théoriques
mu_theorique = theta / (1 - alpha1)
var_theorique = sigma_eps**2 / (1 - alpha1**2)
acf_theorique = [alpha1**k for k in range(1, 6)]

# Ajout des valeurs théoriques dans la dernière ligne
last_idx = len(tailles)
resultats.loc[last_idx, 'moy_moy'] = round(mu_theorique, 2)
resultats.loc[last_idx, 'moy_var'] = round(var_theorique, 2)
resultats.loc[last_idx, 'moy_acf1'] = round(acf_theorique[0], 2)
resultats.loc[last_idx, 'moy_acf2'] = round(acf_theorique[1], 2)
resultats.loc[last_idx, 'moy_acf3'] = round(acf_theorique[2], 2)
resultats.loc[last_idx, 'moy_acf4'] = round(acf_theorique[3], 2)
resultats.loc[last_idx, 'moy_acf5'] = round(acf_theorique[4], 2)

# Affichage final
print("\nTableau final avec valeurs théoriques :")
print(resultats)

# Option : Exportation des résultats dans un fichier CSV
# resultats.to_csv("resultats_simulations_AR1.csv", index=False)