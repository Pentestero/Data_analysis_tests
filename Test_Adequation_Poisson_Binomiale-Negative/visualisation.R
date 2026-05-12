# ================================================================
#  visualisation.R — Classe Visualisation (R5 / Reference Class)
#  Responsabilités :
#    - Construire les 4 graphiques ggplot2 à partir des résultats
#      des tests (objets passés par la classe TestAdequation)
#    - Assembler la grille et sauvegarder / afficher
#
#  Dépendances : ggplot2, gridExtra
# ================================================================

library(ggplot2)
library(gridExtra)

Visualisation <- setRefClass("Visualisation",
                             
                             # ------ Champs ------
                             fields = list(
                               couleurs = "character",   # palette nommée pour les 3 séries
                               types    = "character"    # types de lignes nommés
                             ),
                             
                             # ------ Méthodes ------
                             methods = list(
                               
                               # ---- Constructeur ----
                               initialize = function() {
                                 couleurs <<- c(
                                   "Observé"            = "#2C3E50",
                                   "Poisson"            = "#E74C3C",
                                   "Binomiale_Négative" = "#27AE60"
                                 )
                                 types <<- c(
                                   "Observé"            = "solid",
                                   "Poisson"            = "dashed",
                                   "Binomiale_Négative" = "dotdash"
                                 )
                               },
                               
                               # ----------------------------------------------------------------
                               # generer(ctx, fichier_png)
                               #
                               #   ctx       : liste produite par TestAdequation$executer()
                               #               Champs attendus :
                               #                 counts, n, k_max, observed_orig,
                               #                 lambda_est, chi2_pois, pv_pois, rejet_pois,
                               #                 resultat_nb (NULL ou list(size,mu,chi2,df,p_value)),
                               #                 rejet_nb (NA ou logical)
                               #   fichier_png : chemin PNG (NULL = affichage seul)
                               # ----------------------------------------------------------------
                               generer = function(ctx, fichier_png = NULL) {
                                 
                                 k_vals        <- 0:ctx$k_max
                                 observed_orig <- ctx$observed_orig
                                 n             <- ctx$n
                                 lambda_est    <- ctx$lambda_est
                                 resultat_nb   <- ctx$resultat_nb
                                 
                                 # Extraire les paramètres NB si disponibles
                                 has_nb    <- !is.null(resultat_nb)
                                 size_est  <- if (has_nb) resultat_nb$size else NULL
                                 mu_est    <- if (has_nb) resultat_nb$mu   else NULL
                                 
                                 # ---- Données communes pour G1 ----
                                 df_plot <- data.frame(
                                   k       = k_vals,
                                   Observé = observed_orig / n,
                                   Poisson = dpois(k_vals, lambda = lambda_est) /
                                     sum(dpois(k_vals, lambda = lambda_est))
                                 )
                                 if (has_nb) {
                                   df_plot$Binomiale_Négative <-
                                     dnbinom(k_vals, size = size_est, mu = mu_est) /
                                     sum(dnbinom(k_vals, size = size_est, mu = mu_est))
                                 }
                                 
                                 df_long <- reshape(df_plot,
                                                    varying   = names(df_plot)[-1],
                                                    v.names   = "Fréquence",
                                                    timevar   = "Modèle",
                                                    times     = names(df_plot)[-1],
                                                    direction = "long"
                                 )
                                 
                                 # ==============================================================
                                 # G1 — Courbes de fréquences relatives
                                 # ==============================================================
                                 g1 <- ggplot(df_long,
                                              aes(x = k, y = Fréquence,
                                                  color = Modèle, linetype = Modèle)) +
                                   geom_line(linewidth = 1.1) +
                                   geom_point(size = 2.5) +
                                   scale_color_manual(values = couleurs) +
                                   scale_linetype_manual(values = types) +
                                   labs(
                                     title    = "Comparaison des distributions observée vs théoriques",
                                     subtitle = paste0(
                                       "n = ", n, " | λ_Poisson = ", round(lambda_est, 3),
                                       if (has_nb)
                                         paste0(" | size_NB = ", round(size_est, 3),
                                                ", mu_NB = ", round(mu_est, 3))
                                       else ""
                                     ),
                                     x = "Nombre d'événements k",
                                     y = "Fréquence relative"
                                   ) +
                                   theme_minimal(base_size = 13) +
                                   theme(legend.position  = "bottom",
                                         plot.title       = element_text(face = "bold"))
                                 
                                 # ==============================================================
                                 # G2 — Histogramme + courbes théoriques superposées
                                 # ==============================================================
                                 g2 <- ggplot(data.frame(x = ctx$counts), aes(x = x)) +
                                   geom_histogram(
                                     aes(y = after_stat(density)),
                                     binwidth = 1, fill = "#BDC3C7",
                                     color = "white", boundary = -0.5
                                   ) +
                                   stat_function(
                                     fun = function(x)
                                       dpois(round(x), lambda_est) /
                                       sum(dpois(0:ctx$k_max, lambda_est)),
                                     aes(color = "Poisson"),
                                     linewidth = 1.2, n = ctx$k_max + 1
                                   ) +
                                   {
                                     if (has_nb)
                                       stat_function(
                                         fun = function(x)
                                           dnbinom(round(x), size = size_est, mu = mu_est) /
                                           sum(dnbinom(0:ctx$k_max, size = size_est, mu = mu_est)),
                                         aes(color = "Binomiale_Négative"),
                                         linewidth = 1.2, n = ctx$k_max + 1
                                       )
                                   } +
                                   scale_color_manual(
                                     name   = "Modèle",
                                     values = c("Poisson"            = "#E74C3C",
                                                "Binomiale_Négative" = "#27AE60")
                                   ) +
                                   labs(
                                     title    = "Histogramme des données avec courbes théoriques",
                                     subtitle = "Densité normalisée sur 0:k_max",
                                     x = "Valeur observée", y = "Densité"
                                   ) +
                                   theme_minimal(base_size = 13) +
                                   theme(legend.position = "bottom",
                                         plot.title      = element_text(face = "bold"))
                                 
                                 # ==============================================================
                                 # G3 — Résidus de Pearson
                                 # ==============================================================
                                 prob_p_norm <- dpois(k_vals, lambda_est) /
                                   sum(dpois(k_vals, lambda_est))
                                 resid_pois  <- (observed_orig - n * prob_p_norm) /
                                   sqrt(n * prob_p_norm + 1e-9)
                                 
                                 df_resid <- data.frame(
                                   k      = k_vals,
                                   Résidu = resid_pois,
                                   Modèle = "Poisson"
                                 )
                                 
                                 if (has_nb) {
                                   prob_nb_norm <- dnbinom(k_vals, size = size_est, mu = mu_est) /
                                     sum(dnbinom(k_vals, size = size_est, mu = mu_est))
                                   resid_nb     <- (observed_orig - n * prob_nb_norm) /
                                     sqrt(n * prob_nb_norm + 1e-9)
                                   df_resid <- rbind(
                                     df_resid,
                                     data.frame(k = k_vals, Résidu = resid_nb,
                                                Modèle = "Binomiale_Négative")
                                   )
                                 }
                                 
                                 g3 <- ggplot(df_resid, aes(x = k, y = Résidu, fill = Modèle)) +
                                   geom_col(position = "dodge", alpha = 0.8) +
                                   geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                                              color = "black", linewidth = 0.7) +
                                   scale_fill_manual(values = c("Poisson"            = "#E74C3C",
                                                                "Binomiale_Négative" = "#27AE60")) +
                                   labs(
                                     title    = "Résidus de Pearson par classe",
                                     subtitle = "Lignes pointillées : seuils ±2 (écarts significatifs)",
                                     x = "k", y = "Résidu de Pearson"
                                   ) +
                                   theme_minimal(base_size = 13) +
                                   theme(legend.position = "bottom",
                                         plot.title      = element_text(face = "bold"))
                                 
                                 # ==============================================================
                                 # G4 — Comparaison des p-values
                                 # ==============================================================
                                 labels_test <- paste0(
                                   "Poisson\np = ",
                                   format(ctx$pv_pois, digits = 3, scientific = TRUE),
                                   "\n", ifelse(ctx$rejet_pois, "REJETÉ", "Acceptable")
                                 )
                                 pvaleurs   <- ctx$pv_pois
                                 modeles_pv <- "Poisson"
                                 
                                 if (has_nb) {
                                   pv_nb <- resultat_nb$p_value
                                   labels_test <- c(
                                     labels_test,
                                     paste0("Bin. Négative\np = ",
                                            format(pv_nb, digits = 3, scientific = TRUE),
                                            "\n", ifelse(ctx$rejet_nb, "REJETÉ", "Acceptable"))
                                   )
                                   pvaleurs   <- c(pvaleurs, pv_nb)
                                   modeles_pv <- c(modeles_pv, "Bin. Négative")
                                 }
                                 
                                 df_pv <- data.frame(
                                   Modèle  = factor(modeles_pv, levels = modeles_pv),
                                   p_value = pvaleurs,
                                   label   = labels_test
                                 )
                                 
                                 g4 <- ggplot(df_pv,
                                              aes(x = Modèle, y = pmin(p_value, 0.5), fill = Modèle)) +
                                   geom_col(alpha = 0.85, width = 0.4) +
                                   geom_hline(yintercept = 0.05, linetype = "dashed",
                                              color = "black", linewidth = 0.9) +
                                   annotate("text", x = 0.5, y = 0.058,
                                            label = "alpha = 0.05", hjust = 0, size = 3.8) +
                                   geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
                                   scale_fill_manual(
                                     values = c("Poisson" = "#E74C3C", "Bin. Négative" = "#27AE60"),
                                     guide  = "none"
                                   ) +
                                   scale_y_continuous(limits = c(0, max(pvaleurs, 0.15) * 1.5)) +
                                   labs(
                                     title = "Comparaison des p-values des tests",
                                     x = NULL, y = "p-value (plafonnée à 0.5)"
                                   ) +
                                   theme_minimal(base_size = 13) +
                                   theme(plot.title = element_text(face = "bold"))
                                 
                                 # ==============================================================
                                 # Assemblage et sortie
                                 # ==============================================================
                                 grille <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol = 2)
                                 
                                 if (!is.null(fichier_png)) {
                                   ggplot2::ggsave(fichier_png, plot = grille,
                                                   width = 14, height = 11, dpi = 150)
                                   cat(sprintf("[Graphiques sauvegardés dans '%s']\n", fichier_png))
                                 } else {
                                   cat("[Graphiques affichés (non sauvegardés)]\n")
                                 }
                                 
                                 invisible(grille)
                               }
                             )
)