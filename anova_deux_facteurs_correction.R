# ==============================================================================
# ANOVA À 2 FACTEURS AVEC RÉPLICATION — VERSION FINALE ET VALIDÉE
# ==============================================================================

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 0 : CHARGEMENT DES PACKAGES
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 0 : CHARGEMENT DES PACKAGES ==========\n")

packages <- c("ggplot2", "dplyr", "emmeans", "car")
manquants <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

if (length(manquants) > 0) {
  cat("Installation des packages manquants :", paste(manquants, collapse = ", "), "\n")
  install.packages(manquants, repos = "https://cloud.r-project.org/")
}

for (pkg in packages) {
  library(pkg, character.only = TRUE)
}
cat("✓ Tous les packages chargés avec succès\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 1 : CONSTRUCTION DES DONNÉES (3 × 3 × 4 = 36 obs)
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 1 : CONSTRUCTION DES DONNÉES ==========\n")

set.seed(123)

# Structure factorielle complète
temperature <- factor(rep(c("Basse", "Moyenne", "Haute"), each = 12))
fertilisant <- factor(rep(rep(c("Org", "Min", "Sans"), each = 4), times = 3))

# Rendement : entrée interactive des valeurs (ou utiliser valeurs par défaut)
cat("\n--- Entrée des données de rendement ---\n")
cat("Appuyez sur Entrée pour utiliser les valeurs par défaut, ou entrez vos propres valeurs.\n")

valeurs_par_defaut <- c(
  42, 44, 43, 41,   # Basse - Organique
  52, 54, 53, 55,   # Basse - Minéral  
  28, 30, 29, 27,   # Basse - Sans
  58, 60, 59, 61,   # Moyenne - Organique
  72, 74, 73, 75,   # Moyenne - Minéral
  38, 40, 39, 37,   # Moyenne - Sans
  45, 47, 46, 44,   # Haute - Organique
  55, 57, 56, 54,   # Haute - Minéral
  22, 24, 23, 21    # Haute - Sans
)

cat("Valeurs par défaut : ", paste(valeurs_par_defaut, collapse = ", "), "\n")
reponse <- readline("Utiliser les valeurs par défaut ? (o/n) : ")

if (tolower(reponse) == "n") {
  cat("\nEntrez les 36 valeurs de rendement séparées par des virgules :\n")
  entree <- readline("Valeurs : ")
  rendement <- as.numeric(strsplit(entree, ",")[[1]])
  if (length(rendement) != 36) {
    cat("Erreur : 36 valeurs requises. Utilisation des valeurs par défaut.\n")
    rendement <- valeurs_par_defaut
  }
} else {
  rendement <- valeurs_par_defaut
  cat("✓ Utilisation des valeurs par défaut\n")
}

donnees <- data.frame(
  Temperature = temperature,
  Fertilisant = fertilisant,
  Rendement = rendement
)

cat("Structure des données:\n")
print(str(donnees))
cat("\nAperçu des 12 premières lignes:\n")
print(head(donnees, 12))
cat("\n✓ Données créées :", nrow(donnees), "observations\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 2 : STATISTIQUES DESCRIPTIVES
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 2 : STATISTIQUES DESCRIPTIVES ==========\n")

stats_desc <- donnees %>%
  group_by(Temperature, Fertilisant) %>%
  summarise(
    N = n(),
    Moyenne = round(mean(Rendement), 2),
    SD = round(sd(Rendement), 2),
    Min = min(Rendement),
    Max = max(Rendement),
    .groups = "drop"
  )

print(as.data.frame(stats_desc))

# Vérification du plan équilibré
cat("\nVérification du plan équilibré:\n")
table_verif <- table(donnees$Temperature, donnees$Fertilisant)
print(table_verif)
cat("✓ Plan équilibré :", all(table_verif == 4), "(4 observations par cellule)\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 3 : CONFIGURATION CRUCIALE DES CONTRASTES (Type III)
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 3 : CONFIGURATION DES CONTRASTES ==========\n")

# SAUVEGARDER les contrastes par défaut
contraste_avant <- options()$contrasts
cat("Contrastes avant modification :", contraste_avant[1], "\n")

# CORRECTION ESSENTIELLE : contrastes sum-to-zero pour Type III valide
options(contrasts = c("contr.sum", "contr.poly"))
cat("Contrastes après modification :", options()$contrasts[1], "\n")
cat("✓ Contrastes configurés pour sommes de carrés de Type III\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 4 : AJUSTEMENT DU MODÈLE ANOVA
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 4 : AJUSTEMENT DU MODÈLE ==========\n")

modele <- aov(Rendement ~ Temperature * Fertilisant, data = donnees)
cat("Modèle ajusté : Rendement ~ Temperature * Fertilisant\n")
cat("✓ Modèle ANOVA créé\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 5 : VÉRIFICATION DES HYPOTHÈSES
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 5 : VÉRIFICATION DES HYPOTHÈSES ==========\n")

# 5A. Normalité des résidus
residus <- residuals(modele)
shapiro <- shapiro.test(residus)
cat("\n5A. Test de Shapiro-Wilk (Normalité):\n")
cat(sprintf("   W = %.4f, p-value = %.4f\n", shapiro$statistic, shapiro$p.value))
if (shapiro$p.value > 0.05) {
  cat("   ✓ Résidus normalement distribués (p > 0.05)\n")
} else {
  cat("   ⚠ Résidus NON normaux (p ≤ 0.05)\n")
}

# 5B. Homogénéité des variances
levene <- leveneTest(Rendement ~ Temperature * Fertilisant, data = donnees, center = mean)
cat("\n5B. Test de Levene (Homogénéité):\n")
p_levene <- levene$`Pr(>F)`[1]
cat(sprintf("   F = %.3f, p-value = %.4f\n", levene$`F value`[1], p_levene))
if (p_levene > 0.05) {
  cat("   ✓ Variances homogènes (p > 0.05)\n")
} else {
  cat("   ⚠ Variances hétérogènes (p ≤ 0.05)\n")
}

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 6 : TABLE ANOVA TYPE III (RÉSULTATS PRINCIPAUX)
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 6 : TABLE ANOVA TYPE III ==========\n")

cat("\n>>> TABLE DE L'ANOVA (Type III) <<<\n")
anova_result <- Anova(modele, type = "III")
print(anova_result)

# Extraction et interprétation
cat("\n>>> INTERPRÉTATION DES EFFETS <<<\n")

# Récupération des valeurs p
p_temp <- anova_result["Temperature", "Pr(>F)"]
p_fert <- anova_result["Fertilisant", "Pr(>F)"]
p_inter <- anova_result["Temperature:Fertilisant", "Pr(>F)"]

# Fonction d'interprétation
interpretation <- function(nom, p, f_val) {
  etoiles <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns")))
  sig <- ifelse(p < 0.05, "SIGNIFICATIF", "NON significatif")
  cat(sprintf("%-30s F=%6.2f, p=%.2e  %s  (%s)\n", nom, f_val, p, etoiles, sig))
}

f_temp <- anova_result["Temperature", "F value"]
f_fert <- anova_result["Fertilisant", "F value"]
f_inter <- anova_result["Temperature:Fertilisant", "F value"]

interpretation("Effet Température", p_temp, f_temp)
interpretation("Effet Fertilisant", p_fert, f_fert)
interpretation("Interaction TxF", p_inter, f_inter)

cat("\nLégende: *** p<0.001 | ** p<0.01 | * p<0.05 | ns p≥0.05\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 7 : TAILLE DES EFFETS (η² partiel)
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 7 : TAILLE DES EFFETS ==========\n")

ss <- anova_result[, "Sum Sq"]
ss_resid <- ss["Residuals"]

eta2 <- function(ss_eff) {
  round(ss_eff / (ss_eff + ss_resid), 4)
}

eta_temp <- eta2(ss["Temperature"])
eta_fert <- eta2(ss["Fertilisant"])
eta_inter <- eta2(ss["Temperature:Fertilisant"])

cat(sprintf("η²p Température  : %.4f (explique %.1f%% de la variance)\n", eta_temp, eta_temp*100))
cat(sprintf("η²p Fertilisant  : %.4f (explique %.1f%% de la variance)\n", eta_fert, eta_fert*100))
cat(sprintf("η²p Interaction  : %.4f (explique %.1f%% de la variance)\n", eta_inter, eta_inter*100))
cat("\nRéférence Cohen: petit ≥ 0.01 | moyen ≥ 0.06 | grand ≥ 0.14\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 8 : COMPARAISONS POST-HOC (TUKEY)
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 8 : COMPARAISONS POST-HOC ==========\n")

# Moyennes marginales
emm <- emmeans(modele, ~ Temperature * Fertilisant)

cat("\n>>> MOYENNES MARGINALES ESTIMÉES <<<")
print(emm)

# Toutes les comparaisons
cat("\n>>> COMPARAISONS Toutes les paires (Tukey) <<<")
comp_toutes <- pairs(emm, adjust = "tukey")
print(comp_toutes)

# Comparaisons simples
cat("\n>>> EFFETS SIMPLES : Température par Fertilisant <<<")
comp_simple <- emmeans(modele, ~ Temperature | Fertilisant)
print(pairs(comp_simple, adjust = "tukey"))

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 9 : VISUALISATIONS
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 9 : GRAPHIQUES ==========\n")

# 9A. Graphique d'interaction
emm_df <- as.data.frame(emm)

p1 <- ggplot(emm_df, aes(x = Temperature, y = emmean, 
                          color = Fertilisant, group = Fertilisant)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, linewidth = 1) +
  scale_color_manual(values = c("Org" = "#228B22", "Min" = "#4169E1", "Sans" = "#DC143C"),
                     labels = c("Organique", "Minéral", "Sans")) +
  labs(title = "Interaction Température × Fertilisant",
       subtitle = "Moyennes marginales estimées ± IC 95%",
       y = "Rendement (kg/ha)",
       color = "Fertilisant") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1)
cat("✓ Graphique d'interaction généré\n")

# 9B. Boxplot
p2 <- ggplot(donnees, aes(x = Temperature, y = Rendement, fill = Fertilisant)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Org" = "#90EE90", "Min" = "#87CEEB", "Sans" = "#FFB6C1"),
                    labels = c("Organique", "Minéral", "Sans")) +
  labs(title = "Distribution du Rendement",
       y = "Rendement (kg/ha)",
       fill = "Fertilisant") +
  theme_minimal(base_size = 12)

print(p2)
cat("✓ Boxplot généré\n")

# 9C. Diagnostics
par(mfrow = c(2, 2))
plot(modele)
par(mfrow = c(1, 1))
cat("✓ Graphiques de diagnostics générés\n")

# ═════════════════════════════════════════════════════════════════════════════
# ÉTAPE 10 : SYNTHÈSE FINALE
# ═════════════════════════════════════════════════════════════════════════════

cat("\n========== ÉTAPE 10 : SYNTHÈSE FINALE ==========\n")

cat("\n╔════════════════════════════════════════════════════════════════╗")
cat("\n║              RÉSULTATS DE L'ANOVA À 2 FACTEURS                 ║")
cat("\n╠════════════════════════════════════════════════════════════════╣")

cat(sprintf("\n║  EFFET TEMPÉRATURE   : F=%6.2f, p=%.2e  %s              ║", 
            f_temp, p_temp, ifelse(p_temp < 0.05, "✓ SIG", "  ns ")))
cat(sprintf("\n║  EFFET FERTILISANT : F=%6.2f, p=%.2e  %s              ║", 
            f_fert, p_fert, ifelse(p_fert < 0.05, "✓ SIG", "  ns ")))
cat(sprintf("\n║  INTERACTION T×F   : F=%6.2f, p=%.2e  %s              ║", 
            f_inter, p_inter, ifelse(p_inter < 0.05, "✓ SIG", "  ns ")))

cat("\n╠════════════════════════════════════════════════════════════════╣")
cat("\n║  CONCLUSION STATISTIQUE (α = 0.05)                            ║")

# Conclusion pour Température
cat(sprintf("\n║  TEMPÉRATURE :                                               ║"))
if (p_temp < 0.05) {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  la température a un effet SIGNIFICATIF sur le rendement.      ║"))
} else {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  la température n'a PAS d'effet significatif sur le rendement. ║"))
}

# Conclusion pour Fertilisant
cat(sprintf("\n║                                                                ║"))
cat(sprintf("\n║  FERTILISANT :                                               ║"))
if (p_fert < 0.05) {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  le type de fertilisant a un effet SIGNIFICATIF sur le rendement.║"))
} else {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  le type de fertilisant n'a PAS d'effet significatif.          ║"))
}

# Conclusion pour Interaction
cat(sprintf("\n║                                                                ║"))
cat(sprintf("\n║  INTERACTION :                                                ║"))
if (p_inter < 0.05) {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  l'interaction Température×Fertilisant est SIGNIFICATIVE.      ║"))
  cat(sprintf("\n║  L'effet du fertilisant dépend du niveau de température.        ║"))
} else {
  cat(sprintf("\n║  À risque de se tromper de 0.05, nous pouvons conclure que     ║"))
  cat(sprintf("\n║  l'interaction n'est PAS significative.                        ║"))
  cat(sprintf("\n║  Les effets principaux sont interprétables indépendamment.      ║"))
}

cat("\n╚════════════════════════════════════════════════════════════════╝\n")

# Restaurer les contrastes par défaut
options(contrasts = contraste_avant)
cat("\n✓ Contrastes par défaut restaurés\n")
cat("✓ ANALYSE TERMINÉE AVEC SUCCÈS\n")
