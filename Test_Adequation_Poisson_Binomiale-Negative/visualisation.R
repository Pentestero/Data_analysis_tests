# ================================================================
#  visualisation.R — Classe Visualisation (R5 / Reference Class)
#
#  Responsabilités :
#    - Construire les 4 graphiques ggplot2 à partir du contexte ctx
#    - Retourner chaque graphique individuellement (pour Shiny)
#    - Assembler et sauvegarder en PNG si demandé
#
#  Dépendances : ggplot2, gridExtra, scales
# ================================================================

library(ggplot2)
library(gridExtra)

Visualisation <- setRefClass("Visualisation",
                             
                             fields = list(
                               couleurs = "character",   # palette nommée pour les 3 séries
                               types    = "character",   # types de lignes nommés
                               theme_gui = "ANY"         # thème ggplot commun
                             ),
                             
                             methods = list(
                               
                               # ---- Constructeur ----
                               initialize = function() {
                                 couleurs <<- c(
                                   "Observe"            = "#2C3E50",
                                   "Poisson"            = "#E74C3C",
                                   "Binomiale_Negative" = "#27AE60"
                                 )
                                 types <<- c(
                                   "Observe"            = "solid",
                                   "Poisson"            = "dashed",
                                   "Binomiale_Negative" = "dotdash"
                                 )
                                 theme_gui <<- theme_minimal(base_size = 13) +
                                   theme(
                                     plot.title        = element_text(face = "bold", size = 13, color = "#1B2A4A"),
                                     plot.subtitle     = element_text(size = 10, color = "#475569"),
                                     legend.position   = "bottom",
                                     legend.title      = element_blank(),
                                     panel.grid.minor  = element_blank(),
                                     plot.background   = element_rect(fill = "white", color = NA),
                                     panel.background  = element_rect(fill = "white", color = NA)
                                   )
                               },
                               
                               # ----------------------------------------------------------------
                               # graphique_frequences(ctx)
                               #   G1 — Courbes de fréquences relatives : observé vs théoriques.
                               #   Retourne un objet ggplot.
                               # ----------------------------------------------------------------
                               graphique_frequences = function(ctx) {
                                 k_vals       <- 0:ctx$k_max
                                 n            <- ctx$n
                                 lambda_est   <- ctx$lambda_est
                                 resultat_nb  <- ctx$resultat_nb
                                 has_nb       <- !is.null(resultat_nb)
                                 observed_orig<- ctx$observed_orig
                                 
                                 df_plot <- data.frame(
                                   k        = k_vals,
                                   Observe  = observed_orig / n,
                                   Poisson  = {
                                     p <- dpois(k_vals, lambda = lambda_est)
                                     p / sum(p)
                                   }
                                 )
                                 
                                 if (has_nb) {
                                   size_est <- resultat_nb$size
                                   mu_est   <- resultat_nb$mu
                                   p_nb     <- dnbinom(k_vals, size = size_est, mu = mu_est)
                                   df_plot$Binomiale_Negative <- p_nb / sum(p_nb)
                                 }
                                 
                                 noms_series <- names(df_plot)[-1]
                                 df_long <- reshape(df_plot,
                                                    varying   = noms_series,
                                                    v.names   = "Frequence",
                                                    timevar   = "Modele",
                                                    times     = noms_series,
                                                    direction = "long")
                                 
                                 sous_titre <- paste0("n = ", n,
                                                      " | \u03bb = ", round(lambda_est, 3),
                                                      if (has_nb) paste0(" | size_NB = ", round(resultat_nb$size, 3),
                                                                         ", \u03bc_NB = ", round(resultat_nb$mu, 3)) else "")
                                 
                                 cols_actifs <- couleurs[noms_series]
                                 types_actifs <- types[noms_series]
                                 
                                 ggplot(df_long, aes(x = k, y = Frequence, color = Modele, linetype = Modele)) +
                                   geom_line(linewidth = 1.2) +
                                   geom_point(size = 3) +
                                   scale_color_manual(values = cols_actifs,
                                                      labels = gsub("_", " ", noms_series)) +
                                   scale_linetype_manual(values = types_actifs,
                                                         labels = gsub("_", " ", noms_series)) +
                                   labs(title    = "Comparaison des distributions",
                                        subtitle = sous_titre,
                                        x        = "Nombre d'\u00e9v\u00e9nements k",
                                        y        = "Fr\u00e9quence relative") +
                                   theme_gui
                               },
                               
                               # ----------------------------------------------------------------
                               # graphique_histogramme(ctx)
                               #   G2 — Histogramme des données + courbes théoriques.
                               #   Retourne un objet ggplot.
                               # ----------------------------------------------------------------
                               graphique_histogramme = function(ctx) {
                                 k_vals      <- 0:ctx$k_max
                                 lambda_est  <- ctx$lambda_est
                                 resultat_nb <- ctx$resultat_nb
                                 has_nb      <- !is.null(resultat_nb)
                                 
                                 g <- ggplot(data.frame(x = ctx$counts), aes(x = x)) +
                                   geom_histogram(
                                     aes(y = after_stat(density)),
                                     binwidth = 1, fill = "#BDC3C7",
                                     color = "white", boundary = -0.5
                                   ) +
                                   stat_function(
                                     fun = function(x) {
                                       dpois(round(x), lambda_est) /
                                         sum(dpois(0:ctx$k_max, lambda_est))
                                     },
                                     aes(color = "Poisson"),
                                     linewidth = 1.3, n = ctx$k_max + 1
                                   )
                                 
                                 if (has_nb) {
                                   size_est <- resultat_nb$size
                                   mu_est   <- resultat_nb$mu
                                   g <- g + stat_function(
                                     fun = function(x) {
                                       dnbinom(round(x), size = size_est, mu = mu_est) /
                                         sum(dnbinom(0:ctx$k_max, size = size_est, mu = mu_est))
                                     },
                                     aes(color = "Binomiale_Negative"),
                                     linewidth = 1.3, n = ctx$k_max + 1
                                   )
                                 }
                                 
                                 g +
                                   scale_color_manual(
                                     name   = "Mod\u00e8le",
                                     values = c("Poisson" = "#E74C3C", "Binomiale_Negative" = "#27AE60"),
                                     labels = c("Poisson" = "Poisson", "Binomiale_Negative" = "Binomiale N\u00e9gative")
                                   ) +
                                   labs(title    = "Histogramme avec courbes th\u00e9oriques",
                                        subtitle = "Densit\u00e9 normalis\u00e9e sur [0, k_max]",
                                        x        = "Valeur observ\u00e9e",
                                        y        = "Densit\u00e9") +
                                   theme_gui
                               },
                               
                               # ----------------------------------------------------------------
                               # graphique_residus(ctx)
                               #   G3 — Résidus de Pearson par classe.
                               #   Retourne un objet ggplot.
                               # ----------------------------------------------------------------
                               graphique_residus = function(ctx) {
                                 k_vals       <- 0:ctx$k_max
                                 n            <- ctx$n
                                 lambda_est   <- ctx$lambda_est
                                 resultat_nb  <- ctx$resultat_nb
                                 has_nb       <- !is.null(resultat_nb)
                                 observed_orig<- ctx$observed_orig
                                 
                                 prob_p   <- dpois(k_vals, lambda_est) / sum(dpois(k_vals, lambda_est))
                                 resid_p  <- (observed_orig - n * prob_p) / sqrt(n * prob_p + 1e-9)
                                 
                                 df_resid <- data.frame(k = k_vals, Residu = resid_p, Modele = "Poisson")
                                 
                                 if (has_nb) {
                                   size_est <- resultat_nb$size
                                   mu_est   <- resultat_nb$mu
                                   prob_nb  <- dnbinom(k_vals, size = size_est, mu = mu_est)
                                   prob_nb  <- prob_nb / sum(prob_nb)
                                   resid_nb <- (observed_orig - n * prob_nb) / sqrt(n * prob_nb + 1e-9)
                                   df_resid <- rbind(df_resid,
                                                     data.frame(k = k_vals, Residu = resid_nb,
                                                                Modele = "Binomiale_Negative"))
                                 }
                                 
                                 ggplot(df_resid, aes(x = k, y = Residu, fill = Modele)) +
                                   geom_col(position = "dodge", alpha = 0.82, width = 0.7) +
                                   geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                                              color = "#1B2A4A", linewidth = 0.8) +
                                   annotate("text", x = max(k_vals), y = 2.15,
                                            label = "+2", color = "#1B2A4A", size = 3.2, hjust = 1) +
                                   annotate("text", x = max(k_vals), y = -2.15,
                                            label = "-2", color = "#1B2A4A", size = 3.2, hjust = 1) +
                                   scale_fill_manual(
                                     values = c("Poisson" = "#E74C3C", "Binomiale_Negative" = "#27AE60"),
                                     labels = c("Poisson" = "Poisson", "Binomiale_Negative" = "Binomiale N\u00e9gative")
                                   ) +
                                   labs(title    = "R\u00e9sidus de Pearson par classe",
                                        subtitle = "Valeurs hors [\u00b12] : classes mal ajust\u00e9es",
                                        x        = "k",
                                        y        = "R\u00e9sidu de Pearson") +
                                   theme_gui
                               },
                               
                               # ----------------------------------------------------------------
                               # graphique_pvaleurs(ctx)
                               #   G4 — Comparaison des p-valeurs avec seuil alpha.
                               #   Retourne un objet ggplot.
                               # ----------------------------------------------------------------
                               graphique_pvaleurs = function(ctx) {
                                 resultat_nb <- ctx$resultat_nb
                                 has_nb      <- !is.null(resultat_nb)
                                 
                                 labels_test <- paste0("Poisson\np = ",
                                                       format(ctx$pv_pois, digits = 3, scientific = TRUE),
                                                       "\n", ifelse(ctx$rejet_pois, "REJET\u00c9", "Acceptable"))
                                 pvaleurs   <- ctx$pv_pois
                                 modeles_pv <- "Poisson"
                                 
                                 if (has_nb) {
                                   pv_nb <- resultat_nb$p_value
                                   labels_test <- c(labels_test,
                                                    paste0("Bin. N\u00e9gative\np = ",
                                                           format(pv_nb, digits = 3, scientific = TRUE),
                                                           "\n", ifelse(ctx$rejet_nb, "REJET\u00c9", "Acceptable")))
                                   pvaleurs   <- c(pvaleurs, pv_nb)
                                   modeles_pv <- c(modeles_pv, "Bin. N\u00e9gative")
                                 }
                                 
                                 df_pv <- data.frame(
                                   Modele  = factor(modeles_pv, levels = modeles_pv),
                                   p_value = pvaleurs,
                                   label   = labels_test
                                 )
                                 
                                 ggplot(df_pv, aes(x = Modele, y = pmin(p_value, 0.5), fill = Modele)) +
                                   geom_col(alpha = 0.85, width = 0.45) +
                                   geom_hline(yintercept = 0.05, linetype = "dashed",
                                              color = "#1B2A4A", linewidth = 1) +
                                   annotate("text", x = 0.5, y = 0.06,
                                            label = "\u03b1 = 0.05", hjust = 0, size = 4, color = "#1B2A4A") +
                                   geom_text(aes(label = label), vjust = -0.4, size = 3.5, color = "#1B2A4A") +
                                   scale_fill_manual(
                                     values = c("Poisson" = "#E74C3C", "Bin. N\u00e9gative" = "#27AE60"),
                                     guide  = "none"
                                   ) +
                                   scale_y_continuous(limits = c(0, max(pvaleurs, 0.15) * 1.7)) +
                                   labs(title = "Comparaison des p-valeurs",
                                        x     = NULL,
                                        y     = "p-valeur (plafond\u00e9e \u00e0 0.5)") +
                                   theme_gui +
                                   theme(legend.position = "none")
                               },
                               
                               # ----------------------------------------------------------------
                               # generer(ctx, fichier_png)
                               #   Assemble les 4 graphiques et sauvegarde si fichier_png != NULL.
                               #   Retourne une liste nommée des 4 plots (pour Shiny).
                               # ----------------------------------------------------------------
                               generer = function(ctx, fichier_png = NULL) {
                                 g1 <- graphique_frequences(ctx)
                                 g2 <- graphique_histogramme(ctx)
                                 g3 <- graphique_residus(ctx)
                                 g4 <- graphique_pvaleurs(ctx)
                                 
                                 if (!is.null(fichier_png)) {
                                   grille <- gridExtra::arrangeGrob(g1, g2, g3, g4, ncol = 2)
                                   ggplot2::ggsave(fichier_png, plot = grille,
                                                   width = 14, height = 11, dpi = 150)
                                   message(sprintf("[Graphiques sauvegardés : '%s']", fichier_png))
                                 }
                                 
                                 invisible(list(g1 = g1, g2 = g2, g3 = g3, g4 = g4))
                               }
                             )
)
