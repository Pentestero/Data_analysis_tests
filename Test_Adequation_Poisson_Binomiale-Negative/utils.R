# ================================================================
#  utils.R — Classe UtilsTest (R5 / Reference Class)
#
#  Responsabilités :
#    - Validation des données d'entrée
#    - Regroupement bidirectionnel des classes (règle E_i >= seuil)
#    - Parsing d'un vecteur d'entiers depuis une chaîne
#    - Calculs statistiques partagés (indice de dispersion, etc.)
#
#  NOTE : les méthodes de saisie console (readline) ont été
#         supprimées — l'interface graphique Shiny les remplace.
# ================================================================

UtilsTest <- setRefClass("UtilsTest",
                         
                         fields = list(
                           seuil_regroupement = "numeric"   # seuil E_i minimum (défaut = 5)
                         ),
                         
                         methods = list(
                           
                           # ---- Constructeur ----
                           initialize = function(seuil = 5) {
                             seuil_regroupement <<- seuil
                           },
                           
                           # ----------------------------------------------------------------
                           # valider_counts(counts)
                           #   Vérifie que counts est un vecteur d'entiers >= 0.
                           #   Retourne le vecteur converti en integer, ou lève une erreur.
                           # ----------------------------------------------------------------
                           valider_counts = function(counts) {
                             if (!is.numeric(counts) || length(counts) == 0)
                               stop("Le vecteur doit être numérique et non vide.")
                             if (any(counts < 0))
                               stop("Toutes les valeurs doivent être >= 0.")
                             if (any(counts != round(counts)))
                               stop("Toutes les valeurs doivent être des entiers.")
                             as.integer(counts)
                           },
                           
                           # ----------------------------------------------------------------
                           # regrouper(observed, expected)
                           #   Fusionne les classes dont E_i < seuil_regroupement.
                           #   Fusion vers la droite d'abord, puis vers la gauche pour la
                           #   dernière classe restante.
                           #   Retourne list(observed, expected) après regroupement.
                           # ----------------------------------------------------------------
                           regrouper = function(observed, expected) {
                             seuil <- seuil_regroupement
                             
                             # Fusion vers la droite
                             i <- 1
                             while (i < length(expected)) {
                               if (expected[i] < seuil) {
                                 observed[i + 1] <- observed[i] + observed[i + 1]
                                 expected[i + 1] <- expected[i] + expected[i + 1]
                                 observed <- observed[-i]
                                 expected <- expected[-i]
                               } else {
                                 i <- i + 1
                               }
                             }
                             
                             # Fusion vers la gauche (dernière classe)
                             while (length(expected) >= 2 && tail(expected, 1) < seuil) {
                               n_last <- length(expected)
                               observed[n_last - 1] <- observed[n_last - 1] + observed[n_last]
                               expected[n_last - 1] <- expected[n_last - 1] + expected[n_last]
                               observed <- observed[-n_last]
                               expected <- expected[-n_last]
                             }
                             
                             list(observed = observed, expected = expected)
                           },
                           
                           # ----------------------------------------------------------------
                           # parser_vecteur_entiers(chaine)
                           #   Convertit "0,1,1,2, 3 0" → c(0L,1L,1L,2L,3L,0L).
                           #   Retourne NULL si conversion impossible.
                           # ----------------------------------------------------------------
                           parser_vecteur_entiers = function(chaine) {
                             tokens <- strsplit(trimws(chaine), "[,\\s]+", perl = TRUE)[[1]]
                             tokens <- tokens[nchar(tokens) > 0]
                             vals   <- suppressWarnings(as.integer(tokens))
                             if (any(is.na(vals)) || length(vals) == 0) return(NULL)
                             vals
                           },
                           
                           # ----------------------------------------------------------------
                           # indice_dispersion(counts)
                           #   Retourne sigma²/mu. = 1 → Poisson, > 1 → surdispersion.
                           # ----------------------------------------------------------------
                           indice_dispersion = function(counts) {
                             if (length(counts) < 2) return(NA_real_)
                             var(counts) / mean(counts)
                           },
                           
                           # ----------------------------------------------------------------
                           # resume_descriptif(counts)
                           #   Retourne une liste de statistiques descriptives.
                           # ----------------------------------------------------------------
                           resume_descriptif = function(counts) {
                             list(
                               n       = length(counts),
                               min     = min(counts),
                               max     = max(counts),
                               moyenne = mean(counts),
                               variance= var(counts),
                               indice  = indice_dispersion(counts),
                               tableau = as.data.frame(table(counts), stringsAsFactors = FALSE)
                             )
                           }
                         )
)
