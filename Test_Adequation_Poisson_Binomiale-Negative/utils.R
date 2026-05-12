# ================================================================
#  utils.R — Classe UtilsTest (R5 / Reference Class)
#  Responsabilités :
#    - Validation des données d'entrée
#    - Regroupement bidirectionnel des classes (règle E_i >= seuil)
#    - Parsing d'un vecteur d'entiers depuis une chaîne
#    - Saisies interactives console (entier, réel, choix, vecteur)
# ================================================================

UtilsTest <- setRefClass("UtilsTest",
                         
                         # ------ Champs (attributs) ------
                         fields = list(
                           seuil_regroupement = "numeric"   # seuil E_i minimum (défaut = 5)
                         ),
                         
                         # ------ Méthodes ------
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
                           # lire_entier_positif(invite)
                           #   Boucle jusqu'à obtenir un entier strictement positif.
                           #   Retourne NULL si l'utilisateur tape 'q'/'quitter'.
                           # ----------------------------------------------------------------
                           lire_entier_positif = function(invite) {
                             repeat {
                               saisie <- readline(prompt = invite)
                               if (trimws(tolower(saisie)) %in% c("q", "quitter")) return(NULL)
                               val <- suppressWarnings(as.integer(saisie))
                               if (!is.na(val) && val > 0) return(val)
                               cat("  [!] Entier strictement positif attendu.\n")
                             }
                           },
                           
                           # ----------------------------------------------------------------
                           # lire_reel_intervalle(invite, borne_min, borne_max)
                           #   Boucle jusqu'à obtenir un réel dans [borne_min, borne_max].
                           #   Retourne NULL si l'utilisateur tape 'q'/'quitter'.
                           # ----------------------------------------------------------------
                           lire_reel_intervalle = function(invite, borne_min, borne_max) {
                             repeat {
                               saisie <- readline(prompt = invite)
                               if (trimws(tolower(saisie)) %in% c("q", "quitter")) return(NULL)
                               val <- suppressWarnings(as.numeric(saisie))
                               if (!is.na(val) && val >= borne_min && val <= borne_max) return(val)
                               cat(sprintf("  [!] Valeur attendue entre %g et %g.\n", borne_min, borne_max))
                             }
                           },
                           
                           # ----------------------------------------------------------------
                           # lire_choix(invite, choix_valides)
                           #   Boucle jusqu'à obtenir une saisie dans choix_valides.
                           #   Insensible à la casse. Retourne la saisie normalisée.
                           # ----------------------------------------------------------------
                           lire_choix = function(invite, choix_valides) {
                             repeat {
                               saisie <- trimws(tolower(readline(prompt = invite)))
                               if (saisie %in% choix_valides) return(saisie)
                               cat("  [!] Choix invalide. Options :",
                                   paste(choix_valides, collapse = " / "), "\n")
                             }
                           }
                         )
)