# ANOVA TWO-WAY SANS RÉPLICATION
# Notation : Lignes (r) x Colonnes (c)

#--- SAISIE DES DIMENSIONS ---
  r     <- as.integer(readline(prompt = "Combien de lignes (niveaux facteur A) ? "))
  c_val <- as.integer(readline(prompt = "Combien de colonnes (niveaux facteur B) ? "))
  
  # --- SAISIE DES NOMS DES LIGNES ---
  noms_lignes <- c()
  for (i in 1:r) {
    nom <- readline(prompt = sprintf("Nom de la ligne %d : ", i))
    noms_lignes <- c(noms_lignes, nom)
  }
  ss
  # --- SAISIE DES NOMS DES COLONNES ---
  noms_colonnes <- c()
  for (j in 1:c_val) {
    nom <- readline(prompt = sprintf("Nom de la colonne %d : ", j))
    noms_colonnes <- c(noms_colonnes, nom)
  }
  
  # --- SAISIE DES VALEURS (1 par cellule) ---
  X <- matrix(NA, nrow = r, ncol = c_val,
              dimnames = list(noms_lignes, noms_colonnes))
  
  for (i in 1:r) {
    for (j in 1:c_val) {
      saisie <- readline(prompt = sprintf(
        "Valeur [%s , %s] : ", noms_lignes[i], noms_colonnes[j]))
      valeur <- as.numeric(saisie)
      
      if (is.na(valeur)) {
        cat(" Valeur invalide. Relancez.\n"); stop()
      }
      
      X[i, j] <- valeur
    }
  }
  #afficher le tableau
  cat("\n=== TABLEAU DES DONNÉES SAISI ===\n")
  print(X)
  
# STATISTIQUES DESCRIPTIVES

cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")

Ti_point <- apply(X, 1, sum)   # Totaux lignes
T_point_j <- apply(X, 2, sum)  # Totaux colonnes
T <- sum(X)                     # Total général

cat("\nTotaux par ligne (Ti.) :\n")
print(Ti_point)

cat("\nTotaux par colonne (T.j) :\n")
print(T_point_j)

cat(sprintf("\nMoyennes par %s :\n", noms_lignes))
print(apply(X, 1, mean))

cat(sprintf("\nMoyennes par %s :\n", noms_colonnes))
print(apply(X, 2, mean))

cat(sprintf("\nTotal général T = %.4f\n", T))
cat(sprintf("Moyenne générale = %.4f\n", T / (r * c_val)))

# CALCUL 

cat("\n=== DÉCOMPOSITION DE LA VARIANCE ===\n")
cat("SST = SSC + SSR + SSE\n\n")

# Facteur de correction
CF <- T^2 / (r * c_val)
cat(sprintf("Facteur de correction CF = T²/(r×c) = %.4f²/(%d×%d) = %.4f\n",
            T, r, c_val, CF))

# SST
SST <- sum(X^2) - CF
cat(sprintf("\nSST = ΣΣXij² - CF = %.4f - %.4f = %.4f\n",
            sum(X^2), CF, SST))

# SSC — Effet colonnes
SSC <- (1/r) * sum(T_point_j^2) - CF
cat(sprintf("\nSSC = (1/r)×ΣT.j² - CF\n"))
cat(sprintf("    = (1/%d)×%.4f - %.4f = %.4f\n",
            r, sum(T_point_j^2), CF, SSC))

# SSR — Effet lignes
SSR <- (1/c_val) * sum(Ti_point^2) - CF
cat(sprintf("\nSSR = (1/c)×ΣTi.² - CF\n"))
cat(sprintf("    = (1/%d)×%.4f - %.4f = %.4f\n",
            c_val, sum(Ti_point^2), CF, SSR))

# SSE — Erreur
SSE <- SST - SSC - SSR
cat(sprintf("\nSSE = SST - SSC - SSR\n"))
cat(sprintf("    = %.4f - %.4f - %.4f = %.4f\n",
            SST, SSC, SSR, SSE))

# Degrés de liberté
df_C <- c_val - 1
df_R <- r - 1
df_E <- (r - 1) * (c_val - 1)
df_T <- r * c_val - 1

# Carrés moyens
MSC <- SSC / df_C
MSR <- SSR / df_R
MSE <- SSE / df_E

# Statistiques F
F_C <- MSC / MSE
F_R <- MSR / MSE
p_C <- pf(F_C, df_C, df_E, lower.tail = FALSE)
p_R <- pf(F_R, df_R, df_E, lower.tail = FALSE)

# Valeurs critiques
F_crit_C <- qf(0.95, df_C, df_E)
F_crit_R <- qf(0.95, df_R, df_E)

cat(sprintf("\nVérification : SSC+SSR+SSE = %.4f %s SST\n",
            SSC+SSR+SSE,
            ifelse(abs((SSC+SSR+SSE)-SST) < 1e-8, "✅ =", "❌ ≠")))


# ============================================================
# TABLE ANOVA COMPLÈTE
# ============================================================

cat("\n=== TABLE ANOVA TWO-WAY ===\n")
cat(sprintf("%-25s %10s %5s %10s %10s %10s %10s\n",
            "Source", "SS", "ddl", "MS", "F obs", "F crit", "p-value"))
cat(strrep("-", 82), "\n")
cat(sprintf("%-25s %10.4f %5d %10.4f %10.4f %10.4f %10.4f\n",
            paste0("Colonnes-", colonnes_nom),
            SSC, df_C, MSC, F_C, F_crit_C, p_C))
cat(sprintf("%-25s %10.4f %5d %10.4f %10.4f %10.4f %10.4f\n",
            paste0("Lignes-", lignes_nom),
            SSR, df_R, MSR, F_R, F_crit_R, p_R))
cat(sprintf("%-25s %10.4f %5d %10.4f\n",
            "Erreur (SSE)", SSE, df_E, MSE))
cat(sprintf("%-25s %10.4f %5d\n",
            "Total (SST)", SST, df_T))
cat(strrep("-", 82), "\n")

cat(sprintf("\n→ F_colonnes(%d,%d) : F_obs=%.4f %s F_crit=%.4f → %s\n",
            df_C, df_E, F_C,
            ifelse(F_C > F_crit_C, ">", "<"), F_crit_C,
            ifelse(p_C < 0.05, "✅ Rejet H0", "❌ Non rejet H0")))

cat(sprintf("→ F_lignes  (%d,%d) : F_obs=%.4f %s F_crit=%.4f → %s\n",
            df_R, df_E, F_R,
            ifelse(F_R > F_crit_R, ">", "<"), F_crit_R,
            ifelse(p_R < 0.05, "✅ Rejet H0", "❌ Non rejet H0")))


# ============================================================
# DÉCISION ET POST-HOC
# ============================================================

cat("\n=== DÉCISION ===\n")

donnees_long <- data.frame(
  valeur   = as.vector(X),
  lignes   = factor(rep(noms_lignes, times = c_val)),
  colonnes = factor(rep(noms_colonnes, each = r))
)

modele <- aov(valeur ~ lignes + colonnes, data = donnees_long)

if (p_C < 0.05) {
  cat(sprintf("\n=== POST-HOC Tukey — %s (Colonnes) ===\n", noms_colonnes))
  print(TukeyHSD(modele, which = "colonnes"))
} else {
  cat(sprintf("\n→ %s : Aucune différence significative.\n", noms_colonnes))
}

if (p_R < 0.05) {
  cat(sprintf("\n=== POST-HOC Tukey — %s (Lignes) ===\n", noms_lignes))
  print(TukeyHSD(modele, which = "lignes"))
} else {
  cat(sprintf("\n→ %s : Aucune différence significative.\n", noms_lignes))
}


# ============================================================
# VISUALISATION
# ============================================================

par(mfrow = c(2, 2))

# 1. Interaction plot
interaction.plot(
  x.factor     = donnees_long$colonnes,
  trace.factor  = donnees_long$lignes,
  response      = donnees_long$valeur,
  col           = rainbow(r), lwd = 2,
  pch = 19, type = "b",
  main          = "Profils des moyennes",
  xlab          = noms_colonnes,
  ylab          = "Moyenne",
  trace.label   = lignes_nom
)

# 2. Boxplot Colonnes
boxplot(valeur ~ colonnes, data = donnees_long,
        col  = rainbow(c_val),
        main = paste("Effet", noms_colonnes),
        xlab = noms_colonnes, ylab = "Valeurs", las = 1)

# 3. Boxplot Lignes
boxplot(valeur ~ lignes, data = donnees_long,
        col  = rainbow(r),
        main = paste("Effet",noms_lignes),
        xlab = lignes_nom, ylab = "Valeurs", las = 1)

# 4. QQ-Plot résidus
plot(modele, which = 2,
     main = "QQ-Plot des résidus",
     col  = "steelblue", pch = 16)

par(mfrow = c(1, 1))


# ============================================================
# CONCLUSION
# ============================================================

cat("\n=== CONCLUSION ===\n")
cat(strrep("=", 55), "\n")

cat(sprintf("Facteur COLONNES : %s\n", noms_colonnes))
cat(sprintf("  F(%d,%d) obs=%.4f | crit=%.4f | p=%.6f → %s\n",
            df_C, df_E, F_C, F_crit_C, p_C,
            ifelse(p_C < 0.05,
                   "effet significatif — On rejette H0",
                   "Pas d'effet — On ne rejette pas H0")))

cat(sprintf("\nFacteur LIGNES : %s\n", noms_lignes))
cat(sprintf("  F(%d,%d) obs=%.4f | crit=%.4f | p=%.6f → %s\n",
            df_R, df_E, F_R, F_crit_R, p_R,
            ifelse(p_R < 0.05,
                   "Effet significatif — On rejette H0",
                   "Pas d'effet — On ne rejette pas H0")))

cat(strrep("=", 55), "\n")