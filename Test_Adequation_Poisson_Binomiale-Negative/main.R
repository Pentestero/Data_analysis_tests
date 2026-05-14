# ================================================================
#  main.R — Classe TestAdequation (R5 / Reference Class)
#
#  Architecture :
#    TestAdequation  →  exécute les deux tests khi-deux
#      ├── UtilsTest      (utils.R)
#      └── Visualisation  (visualisation.R)
#
#  Ce script est un moteur pur : elle ne contient aucun
#  appel console (cat/readline). Toute l'interaction passe par
#  l'interface Shiny (app.R).
#
#  Point d'entrée GUI : shiny::runApp("app.R")
# ================================================================

library(MASS)
source("utils.R")
source("visualisation.R")


# ================================================================
#  Classe TestAdequation
#  Responsabilités :
#    - Recevoir les données validées et le niveau alpha
#    - Calculer le test Poisson (khi-deux)
#    - Calculer le test Binomiale Négative (khi-deux)
#    - Construire et retourner le contexte complet (ctx)
#    - Déléguer les graphiques à Visualisation
# ================================================================
TestAdequation <- setRefClass("TestAdequation",
                              
                              fields = list(
                                utils = "ANY",   # instance de UtilsTest
                                visu  = "ANY"    # instance de Visualisation
                              ),
                              
                              methods = list(
                                
                                # ---- Constructeur ----
                                initialize = function() {
                                  utils <<- UtilsTest$new(seuil = 5)
                                  visu  <<- Visualisation$new()
                                },
                                
                                # ----------------------------------------------------------------
                                # executer(counts, alpha, fichier_png)
                                #
                                #   Paramètres :
                                #     counts      — vecteur d'entiers >= 0
                                #     alpha       — seuil de signification (défaut 0.05)
                                #     fichier_png — chemin PNG pour sauvegarder (NULL = pas de fichier)
                                #
                                #   Retourne : liste ctx contenant
                                #     $stats       — statistiques descriptives (UtilsTest$resume_descriptif)
                                #     $poisson     — résultats test Poisson
                                #     $negbin      — résultats test BN (NULL si impossible)
                                #     $recommandation — texte de recommandation finale
                                #     $plots       — liste des 4 objets ggplot (g1..g4)
                                #     + champs bruts : counts, n, k_max, observed_orig,
                                #                      lambda_est, chi2_pois, pv_pois, rejet_pois,
                                #                      resultat_nb, rejet_nb
                                # ----------------------------------------------------------------
                                executer = function(counts, alpha = 0.05, fichier_png = NULL) {
                                  
                                  # ── Validation ────────────────────────────────────────────────
                                  counts <- utils$valider_counts(counts)
                                  n      <- length(counts)
                                  k_max  <- max(counts)
                                  
                                  obs_freq      <- table(factor(counts, levels = 0:k_max))
                                  observed_orig <- as.numeric(obs_freq)
                                  
                                  # ── Statistiques descriptives ─────────────────────────────────
                                  stats <- utils$resume_descriptif(counts)
                                  
                                  # ── Test Poisson ──────────────────────────────────────────────
                                  res_pois <- .test_poisson(observed_orig, n, k_max, alpha)
                                  
                                  # ── Test Binomiale Négative ───────────────────────────────────
                                  res_nb <- .test_negbin(counts, observed_orig, n, k_max, alpha)
                                  
                                  # ── Recommandation ────────────────────────────────────────────
                                  recommandation <- .recommandation(res_pois, res_nb, alpha)
                                  
                                  # ── Contexte complet ─────────────────────────────────────────
                                  ctx <- list(
                                    # — données brutes —
                                    counts        = counts,
                                    n             = n,
                                    k_max         = k_max,
                                    observed_orig = observed_orig,
                                    # — Poisson —
                                    lambda_est    = res_pois$lambda,
                                    chi2_pois     = res_pois$chi2,
                                    pv_pois       = res_pois$p_value,
                                    rejet_pois    = res_pois$rejet,
                                    # — BN —
                                    resultat_nb   = if (!is.null(res_nb)) list(
                                      size    = res_nb$size,
                                      mu      = res_nb$mu,
                                      chi2    = res_nb$chi2,
                                      df      = res_nb$df,
                                      p_value = res_nb$p_value
                                    ) else NULL,
                                    rejet_nb      = if (!is.null(res_nb)) res_nb$rejet else NA,
                                    # — objets structurés —
                                    stats          = stats,
                                    poisson        = res_pois,
                                    negbin         = res_nb,
                                    recommandation = recommandation
                                  )
                                  
                                  # ── Graphiques ────────────────────────────────────────────────
                                  ctx$plots <- visu$generer(ctx, fichier_png)
                                  
                                  invisible(ctx)
                                },
                                
                                # ================================================================
                                #  MÉTHODES PRIVÉES (préfixe ".")
                                # ================================================================
                                
                                # ----------------------------------------------------------------
                                # .test_poisson(observed_orig, n, k_max, alpha)
                                #   Retourne list(lambda, chi2, df, vc, p_value, rejet,
                                #                 tableau_obs, tableau_exp, nb_classes)
                                # ----------------------------------------------------------------
                                .test_poisson = function(observed_orig, n, k_max, alpha) {
                                  
                                  lambda_est <- sum((0:k_max) * observed_orig) / n   # = mean(counts)
                                  probs      <- dpois(0:k_max, lambda = lambda_est)
                                  probs      <- probs / sum(probs)
                                  expected   <- n * probs
                                  
                                  rg     <- utils$regrouper(observed_orig, expected)
                                  obs_r  <- rg$observed
                                  exp_r  <- rg$expected
                                  
                                  chi2   <- sum((obs_r - exp_r)^2 / exp_r, na.rm = TRUE)
                                  df     <- max(length(obs_r) - 2, 1)   # -1 contrainte, -1 lambda estimé
                                  vc     <- qchisq(1 - alpha, df = df)
                                  pv     <- pchisq(chi2, df = df, lower.tail = FALSE)
                                  rejet  <- chi2 > vc
                                  
                                  list(
                                    lambda     = lambda_est,
                                    chi2       = chi2,
                                    df         = df,
                                    vc         = vc,
                                    p_value    = pv,
                                    rejet      = rejet,
                                    obs_r      = obs_r,
                                    exp_r      = exp_r,
                                    nb_classes = length(obs_r)
                                  )
                                },
                                
                                # ----------------------------------------------------------------
                                # .test_negbin(counts, observed_orig, n, k_max, alpha)
                                #   Retourne list(size, mu, chi2, df, vc, p_value, rejet,
                                #                 obs_r, exp_r, nb_classes)  ou  NULL si échec.
                                # ----------------------------------------------------------------
                                .test_negbin = function(counts, observed_orig, n, k_max, alpha) {
                                  
                                  fit_nb <- tryCatch(
                                    MASS::fitdistr(counts, densfun = "negative binomial"),
                                    error   = function(e) NULL,
                                    warning = function(w) NULL
                                  )
                                  
                                  if (is.null(fit_nb)) return(NULL)
                                  
                                  size_est <- fit_nb$estimate["size"]
                                  mu_est   <- fit_nb$estimate["mu"]
                                  
                                  probs    <- dnbinom(0:k_max, size = size_est, mu = mu_est)
                                  probs    <- probs / sum(probs)
                                  expected <- n * probs
                                  
                                  rg     <- utils$regrouper(observed_orig, expected)
                                  obs_r  <- rg$observed
                                  exp_r  <- rg$expected
                                  
                                  chi2   <- sum((obs_r - exp_r)^2 / exp_r, na.rm = TRUE)
                                  df     <- max(length(obs_r) - 3, 1)   # -1 contrainte, -2 paramètres estimés
                                  vc     <- qchisq(1 - alpha, df = df)
                                  pv     <- pchisq(chi2, df = df, lower.tail = FALSE)
                                  rejet  <- chi2 > vc
                                  
                                  list(
                                    size       = size_est,
                                    mu         = mu_est,
                                    chi2       = chi2,
                                    df         = df,
                                    vc         = vc,
                                    p_value    = pv,
                                    rejet      = rejet,
                                    obs_r      = obs_r,
                                    exp_r      = exp_r,
                                    nb_classes = length(obs_r)
                                  )
                                },
                                
                                # ----------------------------------------------------------------
                                # .recommandation(res_pois, res_nb, alpha)
                                #   Retourne une liste structurée (texte + niveau de sévérité).
                                # ----------------------------------------------------------------
                                .recommandation = function(res_pois, res_nb, alpha) {
                                  pct <- alpha * 100
                                  
                                  if (!res_pois$rejet) {
                                    list(
                                      modele  = "Poisson",
                                      niveau  = "success",
                                      message = sprintf(
                                        "La loi de Poisson est acceptable au seuil de %g %% (p = %.4f).\n\nModèle recommandé : Poisson(\u03bb = %.4f).",
                                        pct, res_pois$p_value, res_pois$lambda
                                      )
                                    )
                                  } else if (!is.null(res_nb) && !isTRUE(res_nb$rejet)) {
                                    list(
                                      modele  = "Binomiale Négative",
                                      niveau  = "warning",
                                      message = sprintf(
                                        "La loi de Poisson est REJETÉE (p = %.4f).\nLa loi Binomiale Négative est acceptable (p = %.4f).\n\nModèle recommandé : BN(size = %.4f, \u03bc = %.4f).\nSurdispersion probable (variance > moyenne).",
                                        res_pois$p_value, res_nb$p_value, res_nb$size, res_nb$mu
                                      )
                                    )
                                  } else {
                                    list(
                                      modele  = "Aucun",
                                      niveau  = "danger",
                                      message = sprintf(
                                        "Les deux lois sont REJETÉES au seuil de %g %%.\n\nEnvisagez des modèles alternatifs :\n  — ZIP (Zero-Inflated Poisson)\n  — ZINB (Zero-Inflated Binomiale Négative)\n  — Mélange de lois",
                                        pct
                                      )
                                    )
                                  }
                                }
                              )
)


# ================================================================
#  Classe SourceDonnees  (nouvelle — remplace Dialogue pour la GUI)
#  Responsabilités :
#    - Générer des données simulées (Poisson / BN / Mélange)
#    - Parser une saisie manuelle
#    - Charger un fichier CSV
#  Ces méthodes sont appelées directement par l'app Shiny.
# ================================================================
SourceDonnees <- setRefClass("SourceDonnees",
                             
                             fields = list(
                               utils = "ANY"   # instance de UtilsTest
                             ),
                             
                             methods = list(
                               
                               initialize = function() {
                                 utils <<- UtilsTest$new()
                               },
                               
                               # ----------------------------------------------------------------
                               # depuis_saisie(chaine)
                               #   Parse une chaîne de caractères et retourne le vecteur.
                               #   Lève une erreur si la chaîne est invalide.
                               # ----------------------------------------------------------------
                               depuis_saisie = function(chaine) {
                                 counts <- utils$parser_vecteur_entiers(chaine)
                                 if (is.null(counts))
                                   stop("Format invalide. Entrez des entiers >= 0 séparés par des virgules ou des espaces.")
                                 utils$valider_counts(counts)
                               },
                               
                               # ----------------------------------------------------------------
                               # depuis_simulation(type, n, seed, ...)
                               #   type  : "poisson" | "negbin" | "melange"
                               #   n     : nombre d'observations
                               #   seed  : graine (NA = pas de graine)
                               #   ...   : lambda / mu / size / prop selon le type
                               # ----------------------------------------------------------------
                               depuis_simulation = function(type, n, seed = NA,
                                                            lambda = NULL, mu = NULL,
                                                            size = NULL, prop = NULL) {
                                 if (!is.na(seed)) set.seed(seed)
                                 
                                 counts <- switch(type,
                                                  "poisson" = {
                                                    if (is.null(lambda) || lambda <= 0)
                                                      stop("Lambda doit être > 0 pour la loi de Poisson.")
                                                    rpois(n, lambda = lambda)
                                                  },
                                                  "negbin" = {
                                                    if (is.null(mu)   || mu   <= 0) stop("Mu doit être > 0.")
                                                    if (is.null(size) || size <= 0) stop("Size doit être > 0.")
                                                    rnbinom(n, mu = mu, size = size)
                                                  },
                                                  "melange" = {
                                                    if (is.null(prop)   || prop <= 0 || prop >= 1)
                                                      stop("La proportion doit être dans ]0, 1[.")
                                                    if (is.null(lambda) || lambda <= 0) stop("Lambda doit être > 0.")
                                                    if (is.null(mu)     || mu     <= 0) stop("Mu doit être > 0.")
                                                    if (is.null(size)   || size   <= 0) stop("Size doit être > 0.")
                                                    n_p  <- round(n * prop)
                                                    n_nb <- n - n_p
                                                    c(rpois(n_p, lambda = lambda),
                                                      rnbinom(n_nb, mu = mu, size = size))
                                                  },
                                                  stop("Type inconnu : ", type)
                                 )
                                 
                                 utils$valider_counts(counts)
                               },
                               
                               # ----------------------------------------------------------------
                               # depuis_csv(chemin, colonne)
                               #   Charge un fichier CSV et retourne la colonne choisie.
                               #   colonne : nom (string) ou NULL pour prendre la 1ère colonne.
                               # ----------------------------------------------------------------
                               depuis_csv = function(chemin, colonne = NULL) {
                                 if (!file.exists(chemin))
                                   stop("Fichier introuvable : ", chemin)
                                 df <- tryCatch(read.csv(chemin, stringsAsFactors = FALSE),
                                                error = function(e) stop("Lecture CSV impossible : ", e$message))
                                 if (nrow(df) == 0) stop("Le fichier CSV est vide.")
                                 
                                 col_data <- if (!is.null(colonne) && nchar(colonne) > 0) {
                                   if (!(colonne %in% names(df)))
                                     stop("Colonne '", colonne, "' introuvable. Colonnes disponibles : ",
                                          paste(names(df), collapse = ", "))
                                   df[[colonne]]
                                 } else {
                                   df[[1]]
                                 }
                                 
                                 vals <- suppressWarnings(as.integer(col_data))
                                 if (any(is.na(vals)))
                                   stop("La colonne contient des valeurs non entières.")
                                 utils$valider_counts(vals)
                               }
                             )
)
