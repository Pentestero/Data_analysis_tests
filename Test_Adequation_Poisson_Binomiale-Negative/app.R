# ================================================================
#  app.R — Interface Shiny pour les tests d'adéquation
#
#  Architecture :
#    app.R (UI + Server Shiny)
#      └── main.R  →  TestAdequation, SourceDonnees
#            ├── utils.R        →  UtilsTest
#            └── visualisation.R →  Visualisation
#
#  Lancement : shiny::runApp("app.R")
#           ou source("app.R")
# ================================================================

# ── Packages ──────────────────────────────────────────────────────
pkgs <- c("shiny", "shinydashboard", "ggplot2",
          "gridExtra", "MASS", "DT", "shinyjs", "bslib")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

# ── Chargement du moteur ──────────────────────────────────────────
source("main.R")          # charge aussi utils.R et visualisation.R


# ════════════════════════════════════════════════════════════════════
#  CONSTANTES DE STYLE
# ════════════════════════════════════════════════════════════════════
CSS <- "
/* ── Palette générale ── */
:root {
  --navy:   #1B2A4A;
  --bleu:   #2563EB;
  --bleu-l: #DBEAFE;
  --vert:   #059669;
  --vert-l: #D1FAE5;
  --rouge:  #DC2626;
  --rouge-l:#FEE2E2;
  --orange: #D97706;
  --orange-l:#FEF3C7;
  --gris:   #F8FAFC;
  --gris-b: #E2E8F0;
  --texte:  #1E293B;
}

body { font-family: 'Segoe UI', Arial, sans-serif;
       background: var(--gris); color: var(--texte); }

/* ── Sidebar ── */
.sidebar { background: var(--navy) !important; }
.sidebar .sidebar-menu > li > a {
  color: #CBD5E1 !important; font-size: 14px; }
.sidebar .sidebar-menu > li.active > a,
.sidebar .sidebar-menu > li > a:hover {
  background: #2E4A7A !important; color: white !important; }
.sidebar-toggle { color: white !important; }

/* ── Header ── */
.main-header .logo,
.main-header .navbar { background: var(--navy) !important; border: none; }
.main-header .logo { font-weight: 700; font-size: 16px; color: white !important; }
.main-header .navbar .sidebar-toggle { color: #CBD5E1 !important; }

/* ── Boîtes d'info ── */
.info-box { min-height: 90px; border-radius: 8px; }
.info-box .info-box-icon { border-radius: 8px 0 0 8px; font-size: 36px; }

/* ── Box ── */
.box { border-radius: 8px; box-shadow: 0 1px 6px rgba(0,0,0,.08); border: none; }
.box-header { border-radius: 8px 8px 0 0; background: var(--navy); color: white; }
.box-header .box-title { color: white; font-weight: 600; }

/* ── Boutons ── */
.btn-primary { background: var(--bleu); border: none; border-radius: 6px;
               font-weight: 600; transition: .2s; }
.btn-primary:hover { background: #1D4ED8; }
.btn-success { border-radius: 6px; font-weight: 600; }
.btn-warning { border-radius: 6px; font-weight: 600; }

/* ── Formulaire ── */
.form-control { border-radius: 6px; border: 1.5px solid var(--gris-b);
                font-size: 13px; }
.form-control:focus { border-color: var(--bleu);
                      box-shadow: 0 0 0 3px rgba(37,99,235,.15); }
.control-label { font-weight: 600; font-size: 13px; color: var(--navy); }

/* ── Onglets ── */
.nav-tabs > li.active > a { color: var(--navy); border-top: 3px solid var(--bleu);
                             font-weight: 600; }
.nav-tabs > li > a { color: #64748B; }

/* ── Badges de décision ── */
.badge-success { background: var(--vert); padding: 6px 14px;
                 border-radius: 20px; font-size: 13px; color: white; }
.badge-warning { background: var(--orange); padding: 6px 14px;
                 border-radius: 20px; font-size: 13px; color: white; }
.badge-danger  { background: var(--rouge); padding: 6px 14px;
                 border-radius: 20px; font-size: 13px; color: white; }

/* ── Panels de résultats ── */
.panel-result {
  background: white; border-radius: 8px; padding: 18px;
  box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 16px;
}
.panel-result h4 {
  color: var(--navy); font-weight: 700; margin-top: 0;
  border-bottom: 2px solid var(--bleu-l); padding-bottom: 8px;
}
.panel-result .stat-row {
  display: flex; justify-content: space-between;
  padding: 5px 0; border-bottom: 1px solid #F1F5F9; font-size: 13px;
}
.panel-result .stat-val { font-weight: 600; color: var(--navy); }

/* ── Recommandation ── */
.reco-box {
  border-radius: 10px; padding: 20px 24px;
  font-size: 14px; line-height: 1.7; white-space: pre-line;
}
.reco-success { background: var(--vert-l);  border-left: 5px solid var(--vert);  }
.reco-warning { background: var(--orange-l);border-left: 5px solid var(--orange);}
.reco-danger  { background: var(--rouge-l); border-left: 5px solid var(--rouge); }

/* ── Progress ── */
.progress-bar { background: var(--bleu); }

/* ── Alert ── */
.shiny-notification { border-radius: 8px; font-size: 13px; }

/* ── Titre de section sidebar ── */
.sidebar-form label { color: #94A3B8; font-size: 11px;
                      text-transform: uppercase; letter-spacing: .5px; }
"


# ════════════════════════════════════════════════════════════════════
#  UI
# ════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "blue",
  
  # ── Header ──────────────────────────────────────────────────────
  dashboardHeader(
    title = span(icon("chart-bar"), " Tests du Khi-Deux"),
    titleWidth = 260
  ),
  
  # ── Sidebar ─────────────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    useShinyjs(),
    tags$head(tags$style(HTML(CSS))),
    
    sidebarMenu(id = "menu",
                menuItem("Accueil",       tabName = "accueil",    icon = icon("home")),
                menuItem("Données",       tabName = "donnees",    icon = icon("database")),
                menuItem("Paramètres",    tabName = "parametres", icon = icon("sliders-h")),
                menuItem("Résultats",     tabName = "resultats",  icon = icon("table")),
                menuItem("Graphiques",    tabName = "graphiques", icon = icon("chart-line")),
                menuItem("À propos",      tabName = "apropos",    icon = icon("info-circle"))
    ),
    
    tags$hr(style = "border-color:#2E4A7A; margin: 10px 16px;"),
    
    # Bouton d'action rapide
    div(style = "padding: 0 16px 12px;",
        actionButton("btn_lancer", "Lancer l'analyse",
                     icon = icon("play"),
                     class = "btn-primary btn-block",
                     style = "width:100%; margin-top:6px;"),
        actionButton("btn_reset", "Réinitialiser",
                     icon = icon("undo"),
                     class = "btn-default btn-block",
                     style = "width:100%; margin-top:8px; color:#475569;")
    )
  ),
  
  # ── Body ─────────────────────────────────────────────────────────
  dashboardBody(
    tabItems(
      
      # ══════════════════════════════════════════════════════════════
      # Onglet ACCUEIL
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "accueil",
              
              # Titre
              fluidRow(
                column(12,
                       div(style = paste0(
                         "background: linear-gradient(135deg, #1B2A4A, #2563EB);",
                         "color: white; border-radius: 12px; padding: 36px 40px;",
                         "margin-bottom: 24px;"),
                         h1(style = "margin:0; font-size:28px; font-weight:700;",
                            icon("chart-bar"), " Tests d'adéquation au Khi-deux"),
                         p(style = "font-size:16px; opacity:.85; margin:12px 0 0;",
                           "Loi de Poisson & Loi Binomiale Négative pour les données de comptage"),
                         div(style = "margin-top:20px;",
                             span(class = "badge-success",
                                  icon("check"), " Script R5 / Reference Classes"),
                             span(style = "margin-left:10px;",
                                  class = "badge-success",
                                  icon("code"), " Architecture OO")
                         )
                       )
                )
              ),
              
              # Cartes info
              fluidRow(
                infoBox("Tests", "2 tests khi-deux",
                        icon = icon("vial"), color = "blue",   width = 3),
                infoBox("Classes", "4 classes R5",
                        icon = icon("cubes"), color = "navy",  width = 3),
                infoBox("Graphiques", "4 graphiques ggplot2",
                        icon = icon("chart-line"), color = "green", width = 3),
                infoBox("Sources", "Saisie / CSV / Simulation",
                        icon = icon("database"), color = "orange", width = 3)
              ),
              
              # Description des classes
              fluidRow(
                box(title = "Architecture du programme", width = 6,
                    status = "primary", solidHeader = TRUE,
                    p("Le programme suit une architecture orientée objet R5 :"),
                    tags$ul(
                      tags$li(tags$b("UtilsTest"), " — validation, regroupement Cochran, statistiques"),
                      tags$li(tags$b("Visualisation"), " — 4 graphiques ggplot2 indépendants"),
                      tags$li(tags$b("TestAdequation"), " — moteur des deux tests khi-deux"),
                      tags$li(tags$b("SourceDonnees"), " — gestion des sources d'entrée"),
                      tags$li(tags$b("app.R"), " — interface Shiny (vue + contrôleur)")
                    ),
                    p(style = "margin-top:12px; font-size:12px; color:#64748B;",
                      "Point d'entrée : ", tags$code("shiny::runApp('app.R')"))
                ),
                box(title = "Mode d'emploi rapide", width = 6,
                    status = "primary", solidHeader = TRUE,
                    tags$ol(
                      tags$li("Allez dans l'onglet ", tags$b("Données"), " et choisissez votre source"),
                      tags$li("Dans ", tags$b("Paramètres"), ", choisissez le seuil α"),
                      tags$li("Cliquez sur ", tags$b("Lancer l'analyse"), " (barre latérale)"),
                      tags$li("Consultez les résultats dans ", tags$b("Résultats"), " et ", tags$b("Graphiques"))
                    )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════
      # Onglet DONNÉES
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "donnees",
              
              fluidRow(
                box(title = "Source des données", width = 12,
                    status = "primary", solidHeader = TRUE,
                    
                    radioButtons("source_type",
                                 label = "Choisir la source :",
                                 choices = list(
                                   "Saisie manuelle"            = "saisie",
                                   "Simulation aléatoire"        = "simulation",
                                   "Fichier CSV"                 = "csv"
                                 ),
                                 selected = "saisie", inline = TRUE
                    )
                )
              ),
              
              # ── Saisie manuelle ─────────────────────────────────────────
              conditionalPanel("input.source_type == 'saisie'",
                               fluidRow(
                                 box(title = "Saisie manuelle des données", width = 8,
                                     status = "info", solidHeader = TRUE,
                                     textAreaInput("saisie_data",
                                                   label = "Entrez vos valeurs (entiers >= 0, séparés par des virgules ou espaces) :",
                                                   value = "0,0,1,2,1,3,0,1,2,1,0,2,3,1,0,4,2,1,0,1,2,0,3,1,2",
                                                   rows = 4,
                                                   placeholder = "Ex : 0,1,1,2,3,0,2,1,0,4,..."),
                                     p(style = "color:#64748B; font-size:12px; margin-top:6px;",
                                       icon("info-circle"),
                                       " Accepte virgules, espaces ou les deux comme séparateurs.")
                                 ),
                                 box(title = "Aperçu", width = 4,
                                     status = "info", solidHeader = TRUE,
                                     verbatimTextOutput("apercu_saisie")
                                 )
                               )
              ),
              
              # ── Simulation ──────────────────────────────────────────────
              conditionalPanel("input.source_type == 'simulation'",
                               fluidRow(
                                 box(title = "Paramètres de simulation", width = 5,
                                     status = "warning", solidHeader = TRUE,
                                     
                                     radioButtons("sim_type", "Type de loi :",
                                                  choices = list(
                                                    "Poisson"            = "poisson",
                                                    "Binomiale Négative" = "negbin",
                                                    "Mélange Poisson + BN" = "melange"
                                                  ), selected = "poisson"),
                                     
                                     numericInput("sim_n", "Nombre d'observations (n) :", 200, 10, 10000, 10),
                                     
                                     conditionalPanel("input.sim_type != 'negbin'",
                                                      numericInput("sim_lambda", "Lambda (Poisson) :", 3, 0.01, 100, 0.1)
                                     ),
                                     conditionalPanel("input.sim_type != 'poisson'",
                                                      numericInput("sim_mu",   "Mu (BN) :",   3, 0.01, 100, 0.1),
                                                      numericInput("sim_size", "Size (BN) :", 2, 0.01, 100, 0.1)
                                     ),
                                     conditionalPanel("input.sim_type == 'melange'",
                                                      sliderInput("sim_prop", "Proportion Poisson :", 0.5, 0.01, 0.99, 0.01)
                                     ),
                                     
                                     checkboxInput("sim_seed_check", "Fixer la graine (reproductibilité)", FALSE),
                                     conditionalPanel("input.sim_seed_check",
                                                      numericInput("sim_seed", "Graine :", 42, 1, 99999, 1)
                                     ),
                                     
                                     actionButton("btn_simuler", "Simuler",
                                                  icon = icon("random"), class = "btn-warning")
                                 ),
                                 box(title = "Données simulées", width = 7,
                                     status = "warning", solidHeader = TRUE,
                                     verbatimTextOutput("apercu_simulation")
                                 )
                               )
              ),
              
              # ── CSV ─────────────────────────────────────────────────────
              conditionalPanel("input.source_type == 'csv'",
                               fluidRow(
                                 box(title = "Chargement d'un fichier CSV", width = 5,
                                     status = "success", solidHeader = TRUE,
                                     fileInput("csv_file", "Sélectionner le fichier CSV :",
                                               accept = ".csv",
                                               buttonLabel = "Parcourir…",
                                               placeholder = "Aucun fichier sélectionné"),
                                     uiOutput("csv_colonnes_ui"),
                                     p(style = "color:#64748B; font-size:12px;",
                                       icon("info-circle"),
                                       " Le fichier doit contenir des entiers >= 0.")
                                 ),
                                 box(title = "Aperçu du fichier CSV", width = 7,
                                     status = "success", solidHeader = TRUE,
                                     DTOutput("apercu_csv")
                                 )
                               )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════
      # Onglet PARAMÈTRES
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "parametres",
              
              fluidRow(
                box(title = "Paramètres du test", width = 6,
                    status = "primary", solidHeader = TRUE,
                    
                    radioButtons("alpha_choix", "Seuil de signification α :",
                                 choices = list(
                                   "0.01 (très strict)" = 0.01,
                                   "0.05 (standard)"    = 0.05,
                                   "0.10 (libéral)"     = 0.10,
                                   "Personnalisé"       = "custom"
                                 ), selected = 0.05),
                    
                    conditionalPanel("input.alpha_choix == 'custom'",
                                     numericInput("alpha_custom", "Valeur personnalisée α :", 0.05, 0.001, 0.2, 0.001)
                    ),
                    
                    tags$hr(),
                    
                    numericInput("seuil_cochran", "Seuil de Cochran (effectif E_i minimum) :",
                                 5, 1, 20, 1),
                    p(style = "color:#64748B; font-size:12px;",
                      "Classes avec E_i < seuil seront fusionnées avant le calcul du khi-deux.")
                ),
                
                box(title = "Sauvegarde des graphiques", width = 6,
                    status = "primary", solidHeader = TRUE,
                    
                    checkboxInput("sauvegarder_png", "Sauvegarder les graphiques en PNG", FALSE),
                    conditionalPanel("input.sauvegarder_png",
                                     textInput("nom_fichier", "Nom du fichier (sans extension) :",
                                               value = "test_adequation_plots"),
                                     p(style = "color:#64748B; font-size:12px;",
                                       icon("info-circle"),
                                       " Le fichier sera enregistré dans le répertoire de travail.")
                    ),
                    
                    tags$hr(),
                    div(style = "background:#F8FAFC; border-radius:8px; padding:14px;",
                        h5(style = "margin-top:0; color:#1B2A4A;", "Récapitulatif"),
                        uiOutput("recap_params")
                    )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════
      # Onglet RÉSULTATS
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "resultats",
              
              # Bandeau de statut
              fluidRow(
                column(12, uiOutput("statut_analyse"))
              ),
              
              # Stats descriptives
              fluidRow(
                box(title = "Statistiques descriptives", width = 4,
                    status = "info", solidHeader = TRUE,
                    uiOutput("ui_stats_desc")
                ),
                
                # Test Poisson
                box(title = "Test 1 — Loi de Poisson", width = 4,
                    status = "danger", solidHeader = TRUE,
                    uiOutput("ui_test_poisson")
                ),
                
                # Test BN
                box(title = "Test 2 — Loi Binomiale Négative", width = 4,
                    status = "success", solidHeader = TRUE,
                    uiOutput("ui_test_negbin")
                )
              ),
              
              # Tableaux khi-deux
              fluidRow(
                box(title = "Tableau khi-deux — Poisson", width = 6,
                    status = "danger", solidHeader = TRUE,
                    DTOutput("tableau_pois")
                ),
                box(title = "Tableau khi-deux — Binomiale Négative", width = 6,
                    status = "success", solidHeader = TRUE,
                    DTOutput("tableau_nb")
                )
              ),
              
              # Recommandation
              fluidRow(
                box(title = "Recommandation finale", width = 12,
                    status = "primary", solidHeader = TRUE,
                    uiOutput("ui_recommandation")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════
      # Onglet GRAPHIQUES
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "graphiques",
              
              fluidRow(
                column(12, uiOutput("statut_graphiques"))
              ),
              
              tabBox(width = 12, title = "Graphiques diagnostiques",
                     
                     tabPanel("Distribution",    icon = icon("chart-line"),
                              plotOutput("plot_freq",  height = "480px")),
                     
                     tabPanel("Histogramme",     icon = icon("chart-bar"),
                              plotOutput("plot_hist",  height = "480px")),
                     
                     tabPanel("Résidus Pearson", icon = icon("balance-scale"),
                              plotOutput("plot_resid", height = "480px")),
                     
                     tabPanel("p-valeurs",       icon = icon("percent"),
                              plotOutput("plot_pval",  height = "480px")),
                     
                     tabPanel("Vue globale",     icon = icon("th-large"),
                              plotOutput("plot_global",height = "760px"))
              ),
              
              # Bouton de téléchargement
              fluidRow(
                column(12,
                       div(style = "text-align:center; margin-top:12px;",
                           downloadButton("dl_plots", "Télécharger la vue globale (PNG)",
                                          class = "btn-primary",
                                          icon = icon("download"))
                       )
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════════════
      # Onglet À PROPOS
      # ══════════════════════════════════════════════════════════════
      tabItem(tabName = "apropos",
              fluidRow(
                box(title = "À propos de cette application", width = 8,
                    status = "primary", solidHeader = TRUE,
                    h4("Tests d'adéquation au Khi-deux pour données de comptage"),
                    p("Cette application implémente deux tests statistiques :"),
                    tags$ul(
                      tags$li(tags$b("Test de Poisson :"),
                              " khi-deux avec λ estimé par la moyenne. ddl = k − 2."),
                      tags$li(tags$b("Test Binomiale Négative :"),
                              " khi-deux avec μ et θ estimés par MLE (MASS::fitdistr). ddl = k − 3.")
                    ),
                    h4("Architecture orientée objet (R5)"),
                    tags$table(class = "table table-striped",
                               tags$thead(tags$tr(
                                 tags$th("Classe"), tags$th("Fichier"), tags$th("Rôle")
                               )),
                               tags$tbody(
                                 tags$tr(tags$td(tags$code("UtilsTest")),
                                         tags$td("utils.R"),
                                         tags$td("Validation, regroupement Cochran")),
                                 tags$tr(tags$td(tags$code("Visualisation")),
                                         tags$td("visualisation.R"),
                                         tags$td("4 graphiques ggplot2")),
                                 tags$tr(tags$td(tags$code("TestAdequation")),
                                         tags$td("main.R"),
                                         tags$td("Moteur des tests khi-deux")),
                                 tags$tr(tags$td(tags$code("SourceDonnees")),
                                         tags$td("main.R"),
                                         tags$td("Gestion des sources d'entrée")),
                                 tags$tr(tags$td(tags$code("UI + Server")),
                                         tags$td("app.R"),
                                         tags$td("Interface Shiny"))
                               )
                    )
                ),
                box(title = "Références", width = 4,
                    status = "info", solidHeader = TRUE,
                    tags$ul(
                      tags$li("Pearson, K. (1900). On the criterion..."),
                      tags$li("Venables & Ripley (2002). MASS — fitdistr()"),
                      tags$li("Wickham (2016). ggplot2"),
                      tags$li("Chambers (2008). R5 Reference Classes"),
                      tags$li("R Core Team (2024). R Language")
                    ),
                    h5("Packages utilisés"),
                    tags$code("shiny, shinydashboard, ggplot2,"),
                    tags$br(),
                    tags$code("gridExtra, MASS, DT, shinyjs")
                )
              )
      )
    )
  )
)


# ════════════════════════════════════════════════════════════════════
#  SERVER
# ════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── Instances des classes métier ───────────────────────────────────
  src    <- SourceDonnees$new()
  moteur <- TestAdequation$new()
  
  # ── Réactifs centraux ─────────────────────────────────────────────
  rv <- reactiveValues(
    counts      = NULL,   # vecteur de données courant
    ctx         = NULL,   # contexte complet retourné par executer()
    erreur      = NULL,   # message d'erreur éventuel
    sim_counts  = NULL    # données simulées (avant lancement)
  )
  
  # ── Helpers UI ──────────────────────────────────────────────────
  stat_row <- function(label, val) {
    div(class = "stat-row",
        span(label),
        span(class = "stat-val", val))
  }
  
  badge_decision <- function(rejet) {
    if (is.na(rejet)) return(span("—", style = "color:#94A3B8;"))
    if (rejet)
      span(class = "badge-danger",  icon("times"), " REJETÉ")
    else
      span(class = "badge-success", icon("check"), " Acceptable")
  }
  
  alpha_actuel <- reactive({
    if (input$alpha_choix == "custom") input$alpha_custom
    else as.numeric(input$alpha_choix)
  })
  
  # ════════════════════════════════════════════════════════════════
  #  ONGLET DONNÉES
  # ════════════════════════════════════════════════════════════════
  
  # Aperçu saisie manuelle
  output$apercu_saisie <- renderText({
    req(input$saisie_data)
    counts <- tryCatch(
      src$depuis_saisie(input$saisie_data),
      error = function(e) NULL
    )
    if (is.null(counts)) return("⚠ Données invalides")
    paste0(
      "n = ", length(counts), "\n",
      "Min = ", min(counts), "  Max = ", max(counts), "\n",
      "Moyenne = ", round(mean(counts), 3), "\n",
      "Variance = ", round(var(counts), 3), "\n",
      "Indice disp. = ", round(var(counts)/mean(counts), 3)
    )
  })
  
  # Simulation
  observeEvent(input$btn_simuler, {
    tryCatch({
      seed_val <- if (input$sim_seed_check) input$sim_seed else NA
      rv$sim_counts <- src$depuis_simulation(
        type   = input$sim_type,
        n      = input$sim_n,
        seed   = seed_val,
        lambda = input$sim_lambda,
        mu     = input$sim_mu,
        size   = input$sim_size,
        prop   = input$sim_prop
      )
      showNotification("Simulation réussie !", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error", duration = 6)
    })
  })
  
  output$apercu_simulation <- renderText({
    req(rv$sim_counts)
    c <- rv$sim_counts
    paste0(
      "n = ", length(c), "\n",
      "Min = ", min(c), "  Max = ", max(c), "\n",
      "Moyenne = ", round(mean(c), 3), "\n",
      "Variance = ", round(var(c), 3), "\n",
      "Indice disp. = ", round(var(c)/mean(c), 3), "\n\n",
      "Distribution :\n",
      paste(capture.output(print(table(c))), collapse = "\n")
    )
  })
  
  # CSV
  csv_data <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })
  
  output$csv_colonnes_ui <- renderUI({
    df <- csv_data()
    req(df)
    if (ncol(df) > 1)
      selectInput("csv_colonne", "Colonne à utiliser :", choices = names(df))
  })
  
  output$apercu_csv <- renderDT({
    req(csv_data())
    datatable(head(csv_data(), 20), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ════════════════════════════════════════════════════════════════
  #  PARAMÈTRES — récap
  # ════════════════════════════════════════════════════════════════
  output$recap_params <- renderUI({
    tagList(
      stat_row("Seuil α", alpha_actuel()),
      stat_row("Seuil Cochran", input$seuil_cochran),
      stat_row("Sauvegarde PNG", if (input$sauvegarder_png) "Oui" else "Non")
    )
  })
  
  # ════════════════════════════════════════════════════════════════
  #  LANCEMENT DE L'ANALYSE
  # ════════════════════════════════════════════════════════════════
  observeEvent(input$btn_lancer, {
    rv$erreur <- NULL
    rv$ctx    <- NULL
    
    withProgress(message = "Analyse en cours…", value = 0, {
      
      # ── 1. Récupérer les données ──────────────────────────────
      incProgress(0.2, detail = "Chargement des données…")
      counts <- tryCatch({
        switch(input$source_type,
               "saisie" = src$depuis_saisie(input$saisie_data),
               "simulation" = {
                 req(rv$sim_counts)
                 rv$sim_counts
               },
               "csv" = {
                 req(input$csv_file)
                 col <- if (!is.null(input$csv_colonne)) input$csv_colonne else NULL
                 src$depuis_csv(input$csv_file$datapath, col)
               }
        )
      }, error = function(e) {
        rv$erreur <- e$message; NULL
      })
      
      if (is.null(counts)) return()
      rv$counts <- counts
      
      # ── 2. Moteur de test ─────────────────────────────────────
      incProgress(0.4, detail = "Calcul des tests khi-deux…")
      alpha <- alpha_actuel()
      
      # Recréer le moteur avec le bon seuil Cochran
      moteur$utils <- UtilsTest$new(seuil = input$seuil_cochran)
      
      fichier_png <- if (input$sauvegarder_png)
        paste0(input$nom_fichier, ".png") else NULL
      
      ctx <- tryCatch(
        moteur$executer(counts, alpha = alpha, fichier_png = fichier_png),
        error = function(e) { rv$erreur <- e$message; NULL }
      )
      
      if (is.null(ctx)) return()
      rv$ctx <- ctx
      
      incProgress(0.4, detail = "Génération des graphiques…")
    })
    
    if (!is.null(rv$ctx)) {
      showNotification("Analyse terminée avec succès !",
                       type = "message", duration = 4)
      # Naviguer vers Résultats
      updateTabItems(session, "menu", "resultats")
    } else if (!is.null(rv$erreur)) {
      showNotification(paste("Erreur :", rv$erreur), type = "error", duration = 8)
    }
  })
  
  # Réinitialisation
  observeEvent(input$btn_reset, {
    rv$counts     <- NULL
    rv$ctx        <- NULL
    rv$erreur     <- NULL
    rv$sim_counts <- NULL
    updateTabItems(session, "menu", "accueil")
    showNotification("Application réinitialisée.", duration = 3)
  })
  
  # ════════════════════════════════════════════════════════════════
  #  ONGLET RÉSULTATS
  # ════════════════════════════════════════════════════════════════
  
  # Bandeau de statut
  output$statut_analyse <- renderUI({
    if (is.null(rv$ctx)) {
      div(style = "background:#FEF3C7; border-left:5px solid #D97706;
                   border-radius:8px; padding:14px 20px; margin-bottom:16px;",
          icon("exclamation-triangle", style = "color:#D97706;"),
          " Aucune analyse effectuée. Configurez les données et cliquez sur ",
          tags$b("Lancer l'analyse"), ".")
    } else {
      div(style = "background:#D1FAE5; border-left:5px solid #059669;
                   border-radius:8px; padding:14px 20px; margin-bottom:16px;",
          icon("check-circle", style = "color:#059669;"),
          " Analyse réalisée sur ", tags$b(rv$ctx$n), " observations (k_max = ",
          rv$ctx$k_max, ", α = ", alpha_actuel(), ").")
    }
  })
  
  # Stats descriptives
  output$ui_stats_desc <- renderUI({
    req(rv$ctx)
    s <- rv$ctx$stats
    div(class = "panel-result",
        stat_row("n (observations)",  s$n),
        stat_row("Min",               s$min),
        stat_row("Max",               s$max),
        stat_row("Moyenne",           round(s$moyenne, 4)),
        stat_row("Variance",          round(s$variance, 4)),
        stat_row("Indice dispersion", round(s$indice, 4)),
        div(style = "margin-top:10px;",
            if (s$indice < 0.8)
              span(class="badge-danger",   icon("arrow-down"), " Sous-dispersion")
            else if (s$indice > 1.2)
              span(class="badge-warning",  icon("arrow-up"),   " Sur-dispersion")
            else
              span(class="badge-success",  icon("equals"),     " Équi-dispersion")
        )
    )
  })
  
  # Test Poisson
  output$ui_test_poisson <- renderUI({
    req(rv$ctx)
    p <- rv$ctx$poisson
    div(class = "panel-result",
        stat_row("H₀",          paste0("Poisson(λ = ", round(p$lambda, 4), ")")),
        stat_row("λ estimé",    round(p$lambda, 4)),
        stat_row("Classes (après fusion)", p$nb_classes),
        stat_row("χ² calculé",  round(p$chi2, 4)),
        stat_row("ddl",         p$df),
        stat_row("χ² critique", round(p$vc, 4)),
        stat_row("p-valeur",    format(p$p_value, digits=4, scientific=TRUE)),
        div(style="margin-top:12px;", badge_decision(p$rejet))
    )
  })
  
  # Test BN
  output$ui_test_negbin <- renderUI({
    req(rv$ctx)
    nb <- rv$ctx$negbin
    if (is.null(nb)) {
      return(div(class="panel-result",
                 p(style="color:#94A3B8;",
                   icon("times-circle"),
                   " Ajustement BN impossible (données peut-être sous-dispersées).")))
    }
    div(class = "panel-result",
        stat_row("H₀",          paste0("BN(size=", round(nb$size,4),", μ=", round(nb$mu,4),")")),
        stat_row("size estimé", round(nb$size, 4)),
        stat_row("μ estimé",    round(nb$mu, 4)),
        stat_row("Classes (après fusion)", nb$nb_classes),
        stat_row("χ² calculé",  round(nb$chi2, 4)),
        stat_row("ddl",         nb$df),
        stat_row("χ² critique", round(nb$vc, 4)),
        stat_row("p-valeur",    format(nb$p_value, digits=4, scientific=TRUE)),
        div(style="margin-top:12px;", badge_decision(nb$rejet))
    )
  })
  
  # Tableau Poisson
  output$tableau_pois <- renderDT({
    req(rv$ctx)
    p <- rv$ctx$poisson
    df <- data.frame(
      Classe   = seq_along(p$obs_r),
      Observé  = p$obs_r,
      Attendu  = round(p$exp_r, 3),
      Contrib  = round((p$obs_r - p$exp_r)^2 / p$exp_r, 4)
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t")) |>
      formatStyle("Contrib",
                  backgroundColor = styleInterval(c(1, 3.84),
                                                  c("#D1FAE5", "#FEF3C7", "#FEE2E2")))
  })
  
  # Tableau BN
  output$tableau_nb <- renderDT({
    req(rv$ctx)
    nb <- rv$ctx$negbin
    if (is.null(nb))
      return(datatable(data.frame(Message = "Test BN non disponible"), rownames = FALSE))
    df <- data.frame(
      Classe   = seq_along(nb$obs_r),
      Observé  = nb$obs_r,
      Attendu  = round(nb$exp_r, 3),
      Contrib  = round((nb$obs_r - nb$exp_r)^2 / nb$exp_r, 4)
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t")) |>
      formatStyle("Contrib",
                  backgroundColor = styleInterval(c(1, 3.84),
                                                  c("#D1FAE5", "#FEF3C7", "#FEE2E2")))
  })
  
  # Recommandation
  output$ui_recommandation <- renderUI({
    req(rv$ctx)
    r     <- rv$ctx$recommandation
    classe <- switch(r$niveau,
                     "success" = "reco-success",
                     "warning" = "reco-warning",
                     "danger"  = "reco-danger"
    )
    icone <- switch(r$niveau,
                    "success" = icon("check-circle",     style="color:#059669; font-size:20px;"),
                    "warning" = icon("exclamation-triangle", style="color:#D97706; font-size:20px;"),
                    "danger"  = icon("times-circle",     style="color:#DC2626; font-size:20px;")
    )
    div(class = paste("reco-box", classe),
        div(style="margin-bottom:10px;",
            icone,
            tags$strong(style="font-size:16px; margin-left:8px;",
                        "Modèle retenu : ", r$modele)),
        p(r$message)
    )
  })
  
  # ════════════════════════════════════════════════════════════════
  #  ONGLET GRAPHIQUES
  # ════════════════════════════════════════════════════════════════
  
  output$statut_graphiques <- renderUI({
    if (is.null(rv$ctx)) {
      div(style = "background:#FEF3C7; border-left:5px solid #D97706;
                   border-radius:8px; padding:14px 20px; margin-bottom:16px;",
          icon("exclamation-triangle", style="color:#D97706;"),
          " Lancez d'abord l'analyse pour afficher les graphiques.")
    }
  })
  
  output$plot_freq  <- renderPlot({ req(rv$ctx); rv$ctx$plots$g1 })
  output$plot_hist  <- renderPlot({ req(rv$ctx); rv$ctx$plots$g2 })
  output$plot_resid <- renderPlot({ req(rv$ctx); rv$ctx$plots$g3 })
  output$plot_pval  <- renderPlot({ req(rv$ctx); rv$ctx$plots$g4 })
  
  output$plot_global <- renderPlot({
    req(rv$ctx)
    gridExtra::grid.arrange(
      rv$ctx$plots$g1, rv$ctx$plots$g2,
      rv$ctx$plots$g3, rv$ctx$plots$g4,
      ncol = 2
    )
  })
  
  # Téléchargement PNG
  output$dl_plots <- downloadHandler(
    filename = function() {
      paste0(if (input$sauvegarder_png && nchar(input$nom_fichier) > 0)
        input$nom_fichier else "test_adequation",
        "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(rv$ctx)
      grille <- gridExtra::arrangeGrob(
        rv$ctx$plots$g1, rv$ctx$plots$g2,
        rv$ctx$plots$g3, rv$ctx$plots$g4,
        ncol = 2
      )
      ggplot2::ggsave(file, plot = grille,
                      width = 14, height = 11, dpi = 150)
    }
  )
}

# ════════════════════════════════════════════════════════════════════
#  LANCEMENT
# ════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
