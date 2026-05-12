# ================================================================
#  main.R — Classes TestAdequation & Dialogue (R5 / Reference Class)
#
#  Architecture :
#    Dialogue          →  orchestre les échanges console
#      └── TestAdequation  →  exécute les deux tests khi-deux
#            ├── UtilsTest      (utils.R)
#            └── Visualisation  (visualisation.R)
#
#  Point d'entrée : source("main.R")  → dialogue interactif
#
# Pour exécuter (lancer l'interprétation) le script il faut exécuter
# il faut exécuter la commande source("main.R") dans la console
# ================================================================

library(MASS)
source("utils.R")
source("visualisation.R")


# ================================================================
#  Classe TestAdequation
#  Responsabilités :
#    - Recevoir les données validées et le niveau alpha
#    - Calculer le test Poisson et le test Binomiale Négative
#    - Afficher les résultats dans la console
#    - Déléguer les graphiques à Visualisation
#    - Retourner un contexte (liste) exploitable par Dialogue
# ================================================================
TestAdequation <- setRefClass("TestAdequation",
                              
                              fields = list(
                                utils  = "ANY",   # instance de UtilsTest
                                visu   = "ANY"    # instance de Visualisation
                              ),
                              
                              methods = list(
                                
                                # ---- Constructeur ----
                                initialize = function() {
                                  utils <<- UtilsTest$new(seuil = 5)
                                  visu  <<- Visualisation$new()
                                },
                                
                                # ----------------------------------------------------------------
                                # executer(counts, alpha, fichier_png)
                                #   Lance les deux tests et les graphiques.
                                #   Retourne la liste ctx (contexte complet des résultats).
                                # ----------------------------------------------------------------
                                executer = function(counts, alpha = 0.05, fichier_png = NULL) {
                                  
                                  # Validation
                                  counts <- utils$valider_counts(counts)
                                  n      <- length(counts)
                                  k_max  <- max(counts)
                                  
                                  obs_freq      <- table(factor(counts, levels = 0:k_max))
                                  observed_orig <- as.numeric(obs_freq)
                                  
                                  .afficher_entete(n, k_max, obs_freq)
                                  
                                  # ---- Test Poisson ----
                                  res_pois <- .test_poisson(counts, observed_orig, n, k_max, alpha)
                                  
                                  # ---- Test Binomiale Négative ----
                                  res_nb   <- .test_negbin(counts, observed_orig, n, k_max, alpha)
                                  
                                  # ---- Recommandation ----
                                  .afficher_recommandation(res_pois, res_nb, alpha)
                                  
                                  # ---- Contexte pour la visualisation ----
                                  ctx <- list(
                                    counts        = counts,
                                    n             = n,
                                    k_max         = k_max,
                                    observed_orig = observed_orig,
                                    lambda_est    = res_pois$lambda,
                                    chi2_pois     = res_pois$chi2,
                                    pv_pois       = res_pois$p_value,
                                    rejet_pois    = res_pois$rejet,
                                    resultat_nb   = if (!is.null(res_nb)) list(
                                      size    = res_nb$size,
                                      mu      = res_nb$mu,
                                      chi2    = res_nb$chi2,
                                      df      = res_nb$df,
                                      p_value = res_nb$p_value
                                    ) else NULL,
                                    rejet_nb      = if (!is.null(res_nb)) res_nb$rejet else NA
                                  )
                                  
                                  # ---- Graphiques (délégués à Visualisation) ----
                                  cat("\n[Génération des graphiques...]\n")
                                  visu$generer(ctx, fichier_png)
                                  
                                  invisible(ctx)
                                },
                                
                                # ----------------------------------------------------------------
                                # MÉTHODES PRIVÉES (préfixe ".")
                                # ----------------------------------------------------------------
                                
                                .afficher_entete = function(n, k_max, obs_freq) {
                                  cat("=============================================================\n")
                                  cat("  TEST D'AJUSTEMENT AU KHI-DEUX POUR DONNÉES DE COMPTAGE\n")
                                  cat("=============================================================\n")
                                  cat("Nombre d'observations :", n, "\n")
                                  cat("Valeurs de 0 à", k_max, ":\n")
                                  print(obs_freq)
                                },
                                
                                # Retourne list(lambda, chi2, df, p_value, rejet)
                                .test_poisson = function(counts, observed_orig, n, k_max, alpha) {
                                  lambda_est    <- mean(counts)
                                  probs         <- dpois(0:k_max, lambda = lambda_est)
                                  probs         <- probs / sum(probs)
                                  expected      <- n * probs
                                  
                                  rg     <- utils$regrouper(observed_orig, expected)
                                  obs_r  <- rg$observed
                                  exp_r  <- rg$expected
                                  
                                  chi2   <- sum((obs_r - exp_r)^2 / exp_r, na.rm = TRUE)
                                  df     <- length(obs_r) - 2          # -1 contrainte, -1 lambda
                                  vc     <- qchisq(1 - alpha, df = max(df, 1))
                                  pv     <- pchisq(chi2, df = max(df, 1), lower.tail = FALSE)
                                  rejet  <- chi2 > vc
                                  
                                  cat("\n-------------------------------------------------------------\n")
                                  cat("1. TEST D'AJUSTEMENT À LA LOI DE POISSON\n")
                                  cat("-------------------------------------------------------------\n")
                                  cat("H0 : Poisson(lambda =", round(lambda_est, 4), ")\n")
                                  cat("H1 : Pas une loi de Poisson\n\n")
                                  cat("Nombre de classes après regroupement :", length(obs_r), "\n")
                                  cat("Statistique chi2   :", round(chi2, 4), "\n")
                                  cat("Degrés de liberté  :", df, "\n")
                                  cat("Valeur critique    :", round(vc, 4), "(alpha =", alpha, ")\n")
                                  cat("p-value            :", format(pv, digits = 4, scientific = TRUE), "\n\n")
                                  
                                  if (rejet) {
                                    cat("Décision : REJETER H0\n")
                                    cat("Les données ne suivent PAS une loi de Poisson",
                                        "au seuil de", alpha * 100, "%.\n")
                                  } else {
                                    cat("Décision : NE PAS REJETER H0\n")
                                    cat("Les données sont compatibles avec une loi de Poisson",
                                        "au seuil de", alpha * 100, "%.\n")
                                  }
                                  
                                  list(lambda = lambda_est, chi2 = chi2, df = df,
                                       p_value = pv, rejet = rejet)
                                },
                                
                                # Retourne list(size, mu, chi2, df, p_value, rejet) ou NULL
                                .test_negbin = function(counts, observed_orig, n, k_max, alpha) {
                                  cat("\n-------------------------------------------------------------\n")
                                  cat("2. TEST D'AJUSTEMENT À LA LOI BINOMIALE NÉGATIVE\n")
                                  cat("-------------------------------------------------------------\n")
                                  
                                  fit_nb <- tryCatch(
                                    MASS::fitdistr(counts, densfun = "negative binomial"),
                                    error = function(e) {
                                      cat("AVERTISSEMENT : Estimation NB impossible :", e$message, "\n")
                                      NULL
                                    }
                                  )
                                  
                                  if (is.null(fit_nb)) {
                                    cat("Test Binomiale Négative non réalisable.\n")
                                    return(NULL)
                                  }
                                  
                                  size_est <- fit_nb$estimate["size"]
                                  mu_est   <- fit_nb$estimate["mu"]
                                  
                                  probs    <- dnbinom(0:k_max, size = size_est, mu = mu_est)
                                  probs    <- probs / sum(probs)
                                  expected <- n * probs
                                  
                                  rg     <- utils$regrouper(observed_orig, expected)
                                  obs_r  <- rg$observed
                                  exp_r  <- rg$expected
                                  
                                  chi2   <- sum((obs_r - exp_r)^2 / exp_r, na.rm = TRUE)
                                  df     <- length(obs_r) - 3          # -1 contrainte, -2 paramètres
                                  vc     <- qchisq(1 - alpha, df = max(df, 1))
                                  pv     <- pchisq(chi2, df = max(df, 1), lower.tail = FALSE)
                                  rejet  <- chi2 > vc
                                  
                                  cat("H0 : BN(size =", round(size_est, 4), ", mu =",
                                      round(mu_est, 4), ")\n")
                                  cat("H1 : Pas une loi Binomiale Négative\n\n")
                                  cat("Nombre de classes après regroupement :", length(obs_r), "\n")
                                  cat("Statistique chi2   :", round(chi2, 4), "\n")
                                  cat("Degrés de liberté  :", df, "\n")
                                  cat("Valeur critique    :", round(vc, 4), "(alpha =", alpha, ")\n")
                                  cat("p-value            :", format(pv, digits = 4, scientific = TRUE), "\n\n")
                                  
                                  if (rejet) {
                                    cat("Décision : REJETER H0\n")
                                    cat("Les données ne suivent PAS une loi Binomiale Négative",
                                        "au seuil de", alpha * 100, "%.\n")
                                  } else {
                                    cat("Décision : NE PAS REJETER H0\n")
                                    cat("Les données sont compatibles avec une loi Binomiale Négative",
                                        "au seuil de", alpha * 100, "%.\n")
                                  }
                                  
                                  list(size = size_est, mu = mu_est, chi2 = chi2,
                                       df = df, p_value = pv, rejet = rejet)
                                },
                                
                                .afficher_recommandation = function(res_pois, res_nb, alpha) {
                                  cat("\n=============================================================\n")
                                  cat("  RECOMMANDATION FINALE\n")
                                  cat("=============================================================\n")
                                  
                                  if (!res_pois$rejet) {
                                    cat("-> Loi de POISSON acceptable (p =",
                                        format(res_pois$p_value, digits = 3), ").\n")
                                    cat("   Modèle recommandé : Poisson\n")
                                  } else if (!is.null(res_nb) && !isTRUE(res_nb$rejet)) {
                                    cat("-> Poisson rejetée. Loi BINOMIALE NÉGATIVE acceptable",
                                        "(p =", format(res_nb$p_value, digits = 3), ").\n")
                                    cat("   Modèle recommandé : Binomiale Négative\n")
                                    cat("   (Présence probable de surdispersion)\n")
                                  } else {
                                    cat("-> Les deux lois rejetées au seuil de", alpha * 100, "%.\n")
                                    cat("   Envisagez : ZIP, ZINB, mélange de lois.\n")
                                  }
                                }
                              )
)


# ================================================================
#  Classe Dialogue
#  Responsabilités :
#    - Orchestrer l'interaction console en 5 étapes
#    - Collecter données, alpha, chemin PNG
#    - Déléguer l'exécution à TestAdequation
#    - Boucler jusqu'à la sortie
# ================================================================
Dialogue <- setRefClass("Dialogue",
                        
                        fields = list(
                          utils = "ANY",   # instance de UtilsTest
                          test  = "ANY"    # instance de TestAdequation
                        ),
                        
                        methods = list(
                          
                          initialize = function() {
                            utils <<- UtilsTest$new()
                            test  <<- TestAdequation$new()
                          },
                          
                          # ---- Point d'entrée public ----
                          lancer = function() {
                            cat("\n")
                            cat("╔══════════════════════════════════════════════════════════════╗\n")
                            cat("║   TEST D'AJUSTEMENT KHI-DEUX — Poisson & Binomiale Négative ║\n")
                            cat("╚══════════════════════════════════════════════════════════════╝\n")
                            cat("  Tapez 'q' ou 'quitter' à n'importe quelle invite pour sortir.\n\n")
                            
                            continuer <- TRUE
                            while (continuer) {
                              counts     <- .etape_source()
                              if (is.null(counts)) break
                              
                              alpha      <- .etape_alpha()
                              if (is.null(alpha)) break
                              
                              fichier    <- .etape_sauvegarde()
                              # NULL = pas de sauvegarde (réponse "n"), NA = quitter
                              if (identical(fichier, NA_character_)) break
                              
                              cat("\n─────────────────────────────────────────────────────────────\n")
                              cat("ÉTAPE 4 — Exécution du test\n")
                              cat("─────────────────────────────────────────────────────────────\n\n")
                              test$executer(counts, alpha = alpha, fichier_png = fichier)
                              
                              cat("\n─────────────────────────────────────────────────────────────\n")
                              suite <- utils$lire_choix(
                                "Effectuer un autre test ? [o/n] : ",
                                c("o", "n", "q", "quitter")
                              )
                              continuer <- (suite == "o")
                              cat("\n")
                            }
                            
                            cat("Merci d'avoir utilisé le script. Au revoir !\n")
                            invisible(NULL)
                          },
                          
                          # ----------------------------------------------------------------
                          # ÉTAPES PRIVÉES
                          # ----------------------------------------------------------------
                          
                          # Retourne un vecteur d'entiers ou NULL (quitter)
                          .etape_source = function() {
                            cat("─────────────────────────────────────────────────────────────\n")
                            cat("ÉTAPE 1 — Source des données\n")
                            cat("─────────────────────────────────────────────────────────────\n")
                            cat("  [1] Saisie manuelle\n")
                            cat("  [2] Simulation (Poisson / BN / Mélange)\n")
                            cat("  [3] Fichier CSV\n")
                            rep <- utils$lire_choix("Votre choix [1/2/3] : ",
                                                    c("1", "2", "3", "q", "quitter"))
                            if (rep %in% c("q", "quitter")) return(NULL)
                            
                            if (rep == "1") return(.saisie_manuelle())
                            if (rep == "2") return(.simulation())
                            if (rep == "3") return(.charger_csv())
                          },
                          
                          .saisie_manuelle = function() {
                            repeat {
                              saisie <- readline(prompt = "Valeurs (ex: 0,1,1,2,3) : ")
                              if (trimws(tolower(saisie)) %in% c("q", "quitter")) return(NULL)
                              counts <- utils$parser_vecteur_entiers(saisie)
                              if (!is.null(counts) && all(counts >= 0)) {
                                cat(sprintf("  -> %d valeurs lues. Min=%d Max=%d Moy=%.3f\n",
                                            length(counts), min(counts), max(counts), mean(counts)))
                                return(counts)
                              }
                              cat("  [!] Données invalides (entiers >= 0 attendus).\n")
                            }
                          },
                          
                          .simulation = function() {
                            cat("\n  [a] Poisson  [b] Binomiale Négative  [c] Mélange\n")
                            sim <- utils$lire_choix("  Type [a/b/c] : ",
                                                    c("a", "b", "c", "q", "quitter"))
                            if (sim %in% c("q", "quitter")) return(NULL)
                            
                            n_sim <- utils$lire_entier_positif("  Nombre d'observations : ")
                            if (is.null(n_sim)) return(NULL)
                            
                            seed_rep <- utils$lire_choix(
                              "  Fixer la graine ? [o/n] : ", c("o", "n", "q", "quitter"))
                            if (seed_rep %in% c("q", "quitter")) return(NULL)
                            if (seed_rep == "o") {
                              g <- utils$lire_entier_positif("  Graine : ")
                              if (is.null(g)) return(NULL)
                              set.seed(g)
                              cat(sprintf("  -> Graine = %d\n", g))
                            }
                            
                            if (sim == "a") {
                              lam <- utils$lire_reel_intervalle("  Lambda (> 0) : ", 1e-4, 1e6)
                              if (is.null(lam)) return(NULL)
                              counts <- rpois(n_sim, lambda = lam)
                              cat(sprintf("  -> %d obs Poisson(%.3f)\n", n_sim, lam))
                              
                            } else if (sim == "b") {
                              mu   <- utils$lire_reel_intervalle("  Mu (> 0) : ", 1e-4, 1e6)
                              if (is.null(mu)) return(NULL)
                              size <- utils$lire_reel_intervalle("  Size (> 0) : ", 1e-4, 1e6)
                              if (is.null(size)) return(NULL)
                              counts <- rnbinom(n_sim, mu = mu, size = size)
                              cat(sprintf("  -> %d obs BN(mu=%.3f, size=%.3f)\n", n_sim, mu, size))
                              
                            } else {
                              prop <- utils$lire_reel_intervalle(
                                "  Proportion Poisson [0.01-0.99] : ", 0.01, 0.99)
                              if (is.null(prop)) return(NULL)
                              lam  <- utils$lire_reel_intervalle("  Lambda Poisson : ", 1e-4, 1e6)
                              if (is.null(lam)) return(NULL)
                              mu   <- utils$lire_reel_intervalle("  Mu BN : ", 1e-4, 1e6)
                              if (is.null(mu)) return(NULL)
                              size <- utils$lire_reel_intervalle("  Size BN : ", 1e-4, 1e6)
                              if (is.null(size)) return(NULL)
                              n_p    <- round(n_sim * prop)
                              n_nb   <- n_sim - n_p
                              counts <- c(rpois(n_p, lam), rnbinom(n_nb, mu = mu, size = size))
                              cat(sprintf("  -> Mélange : %d Poisson + %d BN\n", n_p, n_nb))
                            }
                            
                            counts
                          },
                          
                          .charger_csv = function() {
                            repeat {
                              chemin <- readline(prompt = "  Chemin CSV : ")
                              if (trimws(tolower(chemin)) %in% c("q", "quitter")) return(NULL)
                              if (!file.exists(chemin)) { cat("  [!] Fichier introuvable.\n"); next }
                              
                              df_csv <- tryCatch(read.csv(chemin), error = function(e) NULL)
                              if (is.null(df_csv)) { cat("  [!] Lecture échouée.\n"); next }
                              
                              cat(sprintf("  -> %d lignes, %d colonne(s).\n",
                                          nrow(df_csv), ncol(df_csv)))
                              
                              if (ncol(df_csv) > 1) {
                                cat("  Colonnes :", paste(names(df_csv), collapse = ", "), "\n")
                                col <- readline(prompt = "  Colonne à utiliser : ")
                                if (!(col %in% names(df_csv))) { cat("  [!] Colonne introuvable.\n"); next }
                                col_data <- df_csv[[col]]
                              } else {
                                col_data <- df_csv[[1]]
                              }
                              
                              vals <- suppressWarnings(as.integer(col_data))
                              if (any(is.na(vals)) || any(vals < 0)) {
                                cat("  [!] Valeurs non entières ou négatives détectées.\n"); next
                              }
                              cat(sprintf("  -> %d valeurs chargées.\n", length(vals)))
                              return(vals)
                            }
                          },
                          
                          # Retourne alpha (numeric) ou NULL (quitter)
                          .etape_alpha = function() {
                            cat("\n─────────────────────────────────────────────────────────────\n")
                            cat("ÉTAPE 2 — Niveau de signification alpha\n")
                            cat("─────────────────────────────────────────────────────────────\n")
                            rep <- utils$lire_choix(
                              "  [1] 0.01  [2] 0.05  [3] 0.10  [4] Personnalisé : ",
                              c("1", "2", "3", "4", "q", "quitter")
                            )
                            if (rep %in% c("q", "quitter")) return(NULL)
                            
                            alpha <- switch(rep,
                                            "1" = 0.01,
                                            "2" = 0.05,
                                            "3" = 0.10,
                                            "4" = utils$lire_reel_intervalle(
                                              "  Alpha [0.001 - 0.20] : ", 0.001, 0.20)
                            )
                            if (is.null(alpha)) return(NULL)
                            cat(sprintf("  -> alpha = %g\n", alpha))
                            alpha
                          },
                          
                          # Retourne : chemin (string) | NULL (pas de sauvegarde) | NA (quitter)
                          .etape_sauvegarde = function() {
                            cat("\n─────────────────────────────────────────────────────────────\n")
                            cat("ÉTAPE 3 — Sauvegarde des graphiques\n")
                            cat("─────────────────────────────────────────────────────────────\n")
                            rep <- utils$lire_choix(
                              "  Sauvegarder en PNG ? [o/n] : ",
                              c("o", "n", "q", "quitter")
                            )
                            if (rep %in% c("q", "quitter")) return(NA_character_)
                            if (rep == "n") return(NULL)
                            
                            nom <- readline(
                              prompt = "  Nom du fichier (sans extension, Entrée = défaut) : ")
                            nom <- trimws(nom)
                            if (trimws(tolower(nom)) %in% c("q", "quitter")) return(NA_character_)
                            if (nchar(nom) == 0) nom <- "test_adequation_plots"
                            paste0(nom, ".png")
                          }
                        )
)


# ================================================================
#  POINT D'ENTRÉE
# ================================================================
dialogue <- Dialogue$new()
dialogue$lancer()