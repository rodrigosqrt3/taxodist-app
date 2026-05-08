library(shiny)
library(bslib)
library(taxodist)
library(ggplot2)
library(ggrepel)
library(DT)
library(shiny.i18n)

# ── Setup i18n ────────────────────────────────────────────────────────────────
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# ── Helpers ───────────────────────────────────────────────────────────────────

parse_taxa_input <- function(text) {
  taxa <- unlist(strsplit(text, "[,\n]"))
  taxa <- trimws(taxa)
  taxa <- taxa[nchar(taxa) > 0]
  unique(taxa)
}

# ── Theme / UI ────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version = 5,
  bg            = "#F5F0E8",
  fg            = "#1C1812",
  primary       = "#2D5016",
  secondary     = "#8B6914",
  success       = "#3A7D2C",
  info          = "#5B4A2E",
  font_scale    = 0.95,
  base_font     = font_google("Lora"),
  heading_font  = font_google("Playfair Display"),
  code_font     = font_google("JetBrains Mono")
) |>
  bs_add_rules("
    :root {
      --parchment:   #F5F0E8;
      --ink:         #1C1812;
      --moss:        #2D5016;
      --amber:       #8B6914;
      --rust:        #8B3A1A;
      --cream:       #FAF7F0;
      --border:      #C8B99A;
      --muted:       #7A6B55;
    }

    body {
      background-color: var(--parchment);
      background-image:
        radial-gradient(ellipse at 20% 10%, rgba(45,80,22,0.06) 0%, transparent 60%),
        radial-gradient(ellipse at 80% 90%, rgba(139,105,20,0.07) 0%, transparent 55%);
      font-family: 'Lora', Georgia, serif;
    }

    /* ── Header ── */
    .app-header {
      background: var(--moss);
      background-image: linear-gradient(135deg, #1a3009 0%, #2D5016 50%, #3d6b1e 100%);
      color: #F5F0E8;
      padding: 2rem 2.5rem 1.6rem;
      border-bottom: 3px solid var(--amber);
      position: relative;
    }
    .app-header::before {
      content: '';
      position: absolute;
      inset: 0;
      background-image: repeating-linear-gradient(
        90deg, transparent, transparent 60px,
        rgba(255,255,255,0.015) 60px, rgba(255,255,255,0.015) 61px
      );
    }
    .app-header h1 {
      font-family: 'Playfair Display', Georgia, serif;
      font-size: 2.1rem;
      font-weight: 700;
      letter-spacing: 0.01em;
      margin: 0;
      color: #F5F0E8;
      position: relative;
    }
    .app-header .subtitle {
      font-size: 0.85rem;
      color: rgba(245,240,232,0.72);
      margin-top: 0.3rem;
      letter-spacing: 0.05em;
      font-style: italic;
      position: relative;
    }
    .app-header .badge-pkg {
      display: inline-block;
      background: var(--amber);
      color: #1C1812;
      font-size: 0.72rem;
      font-family: 'JetBrains Mono', monospace;
      font-style: normal;
      padding: 0.15rem 0.55rem;
      border-radius: 2px;
      margin-left: 0.5rem;
      vertical-align: middle;
      position: relative;
    }

    /* ── Nav tabs ── */
    .nav-tabs {
      border-bottom: 2px solid var(--border);
      background: var(--cream);
      padding: 0 1.5rem;
    }
    .nav-tabs .nav-link {
      font-family: 'Playfair Display', Georgia, serif;
      font-size: 0.92rem;
      color: var(--muted);
      border: none;
      border-bottom: 3px solid transparent;
      padding: 0.75rem 1.1rem;
      border-radius: 0;
      transition: all 0.2s;
    }
    .nav-tabs .nav-link:hover { color: var(--moss); }
    .nav-tabs .nav-link.active {
      color: var(--moss);
      border-bottom-color: var(--moss);
      background: transparent;
      font-weight: 600;
    }

    /* ── Cards ── */
    .card {
      border: 1px solid var(--border);
      border-radius: 4px;
      background: var(--cream);
      box-shadow: 0 1px 4px rgba(28,24,18,0.07);
    }
    .card-header {
      background: transparent;
      border-bottom: 1px solid var(--border);
      font-family: 'Playfair Display', serif;
      font-size: 0.95rem;
      font-weight: 600;
      color: var(--moss);
      padding: 0.7rem 1.1rem;
      letter-spacing: 0.02em;
    }

    /* ── Inputs ── */
    .form-control, .form-select {
      background: #FEFCF8;
      border: 1px solid var(--border);
      border-radius: 3px;
      font-family: 'Lora', Georgia, serif;
      font-size: 0.88rem;
      color: var(--ink);
    }
    .form-control:focus, .form-select:focus {
      border-color: var(--moss);
      box-shadow: 0 0 0 2px rgba(45,80,22,0.15);
    }
    .form-label {
      font-size: 0.82rem;
      font-weight: 600;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: var(--muted);
      margin-bottom: 0.3rem;
    }

    /* ── Buttons ── */
    .btn-primary {
      background: var(--moss);
      border-color: var(--moss);
      font-family: 'Lora', serif;
      letter-spacing: 0.04em;
      border-radius: 3px;
    }
    .btn-primary:hover { background: #1a3009; border-color: #1a3009; }
    .btn-outline-secondary {
      border-color: var(--border);
      color: var(--muted);
      font-size: 0.82rem;
    }

    /* ── Result box ── */
    .result-box {
      background: #FEFCF8;
      border: 1px solid var(--border);
      border-left: 4px solid var(--moss);
      border-radius: 3px;
      padding: 1.1rem 1.3rem;
      font-family: 'Lora', Georgia, serif;
    }
    .result-distance {
      font-family: 'Playfair Display', serif;
      font-size: 2.4rem;
      color: var(--moss);
      font-weight: 700;
      line-height: 1;
    }
    .result-label {
      font-size: 0.72rem;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      color: var(--muted);
    }
    .result-mrca {
      font-style: italic;
      color: var(--amber);
      font-size: 1.05rem;
    }
    .result-meta {
      font-size: 0.82rem;
      color: var(--muted);
    }

    /* ── Lineage display ── */
    .lineage-node {
      display: inline-block;
      background: var(--parchment);
      border: 1px solid var(--border);
      border-radius: 2px;
      padding: 0.1rem 0.5rem;
      margin: 0.15rem 0.1rem;
      font-size: 0.8rem;
      font-family: 'Lora', serif;
      font-style: italic;
      color: var(--ink);
    }
    .lineage-node.shared {
      background: #d4e8c2;
      border-color: var(--moss);
      color: var(--moss);
      font-weight: 600;
    }
    .lineage-node.mrca {
      background: var(--moss);
      color: #F5F0E8;
      border-color: var(--moss);
    }
    .lineage-arrow {
      color: var(--border);
      font-size: 0.75rem;
      margin: 0 0.05rem;
    }

    /* ── Coverage pills ── */
    .cov-found    { background:#d4e8c2; color:#1a3009; }
    .cov-notfound { background:#f5d0c8; color:#6b1a0a; }
    .cov-pill {
      display:inline-block;
      border-radius:2px;
      padding:0.2rem 0.6rem;
      font-size:0.8rem;
      font-family:'JetBrains Mono',monospace;
      margin:0.2rem;
    }
    .lineage-node.descending {
      background: #f5e6d0;
      border-color: var(--amber);
      color: var(--amber);
    }

    /* ── Misc ── */
    .section-divider {
      border: none;
      border-top: 1px solid var(--border);
      margin: 1.2rem 0;
    }
    .taxon-tag {
      display:inline-block;
      background:var(--parchment);
      border:1px solid var(--border);
      border-radius:2px;
      padding:0.1rem 0.45rem;
      font-size:0.78rem;
      font-style:italic;
      margin:0.1rem;
    }
    .spinner-border { color: var(--moss) !important; }
    .shiny-notification { font-family: 'Lora', serif; }

    /* ── Loading overlay ── */
    #loading_overlay {
      display:none;
      position:fixed;
      inset:0;
      background:rgba(245,240,232,0.65);
      z-index:9999;
      align-items:center;
      justify-content:center;
      flex-direction:column;
      gap:0.8rem;
    }
    #loading_overlay.show { display:flex; }
    .loading-text {
      font-family:'Playfair Display',serif;
      color:var(--moss);
      font-size:1rem;
    }
  ")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = app_theme,
  shiny.i18n::usei18n(i18n),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$style(HTML("
    textarea { resize: vertical; }
    .dataTables_wrapper { font-size:0.83rem; font-family:'Lora',serif; }

    .selectize-input,
    .selectize-dropdown {
      font-family: 'Segoe UI Emoji', sans-serif;
    }
  "))
  ),

  # Loading overlay
  div(id = "loading_overlay",
      div(class = "spinner-border", role = "status"),
      div(class = "loading-text", i18n$t("Querying The Taxonomicon…"))
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('show_loading', function(msg) {
      document.getElementById('loading_overlay').classList.add('show');
    });
    Shiny.addCustomMessageHandler('hide_loading', function(msg) {
      document.getElementById('loading_overlay').classList.remove('show');
    });

    /* --- NEW CODE TO TRANSLATE FILE INPUTS --- */
    Shiny.addCustomMessageHandler('translate_file_inputs', function(msg) {
      $('.shiny-input-container input[type=\"file\"]').each(function() {
        var $container = $(this).closest('.input-group');
        var $btn = $container.find('.btn-file');
        if($btn.length > 0 && $btn[0].childNodes.length > 0) {
           $btn[0].childNodes[0].nodeValue = msg.browse + ' ';
        }
        $container.find('input[type=\"text\"][readonly]').attr('placeholder', msg.no_file);
      });
    });
  ")),

  # Header
  div(class = "app-header mb-0",
      div(style = "position: absolute; right: 25px; top: 25px; z-index: 1000;",
          selectInput("selected_language", NULL,
                      choices = c(
                        "English"   = "en",
                        "Português" = "pt",
                        "Español"   = "es",
                        "Français"  = "fr",
                        "Deutsch"   = "de"
                      ),
                      width = "150px")
      ),
      h1(HTML(paste0('taxodist <span class=\"badge-pkg\">v', packageVersion("taxodist"), '</span>'))),
      div(class = "subtitle",
          textOutput("app_subtitle", inline = TRUE)
      )
  ),

  # Main tabs
  navset_tab(
    id = "main_tabs",

    # ── Tab 1: Pairwise Distance ──────────────────────────────────────────────
    nav_panel(i18n$t("Pairwise Distance"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Taxa")),
                             div(class = "p-3",
                                 textInput("pd_taxon_a", i18n$t("Taxon A"), placeholder = i18n$t("e.g. Tyrannosaurus")),
                                 textInput("pd_taxon_b", i18n$t("Taxon B"), placeholder = i18n$t("e.g. Velociraptor")),
                                 div(class = "d-flex gap-2 mt-3",
                                     actionButton("pd_run", i18n$t("Compute Distance"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("ruler")),
                                     actionButton("pd_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 div(class = "result-label mb-1", i18n$t("Try these:")),
                                 div(
                                   tags$small(class="text-muted fst-italic",
                                              "Tyrannosaurus / Velociraptor • Homo / Quercus • Nomingia / Huanansaurus"
                                   )
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("pd_result_ui")
                    )
                  ),
                  fluidRow(
                    class = "mt-3",
                    column(12,
                           uiOutput("pd_lineage_ui")
                    )
                  )
              )
    ),

    # ── Tab 2: Distance Matrix ────────────────────────────────────────────────
    nav_panel(i18n$t("Distance Matrix"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Taxa List")),
                             div(class = "p-3",
                                 textAreaInput("dm_taxa", i18n$t("Enter taxa (one per line or comma-separated)"),
                                               rows = 8,
                                               placeholder = "Tyrannosaurus\nVelociraptor\nSpinosaurus\nAllosaurus\nCarnotaurus"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("dm_run", i18n$t("Build Matrix"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("table")),
                                     actionButton("dm_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 fileInput("dm_upload", i18n$t("Or upload CSV (one taxon per row)"),
                                           accept = ".csv", width = "100%",
                                           buttonLabel = gsub("<.*?>", "", i18n$t("Browse...")),
                                           placeholder = gsub("<.*?>", "", i18n$t("No file selected")))
                             )
                           )
                    ),
                    column(8,
                           uiOutput("dm_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 3: Closest Relative ───────────────────────────────────────────────
    nav_panel(i18n$t("Closest Relative"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Query")),
                             div(class = "p-3",
                                 textInput("cr_query", i18n$t("Query Taxon"), placeholder = i18n$t("e.g. Tyrannosaurus")),
                                 textAreaInput("cr_candidates", i18n$t("Candidate Taxa (one per line or comma-separated)"),
                                               rows = 6,
                                               placeholder = "Velociraptor\nTriceratops\nBrachiosaurus\nAllosaurus"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("cr_run", i18n$t("Find Closest"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("crosshairs")),
                                     actionButton("cr_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("cr_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 4: Lineage Explorer ───────────────────────────────────────────────
    nav_panel(i18n$t("Lineage Explorer"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Taxon")),
                             div(class = "p-3",
                                 textInput("le_taxon", i18n$t("Taxon name"), placeholder = i18n$t("e.g. Homo sapiens")),
                                 actionButton("le_run", i18n$t("Get Lineage"),
                                              class = "btn btn-primary w-100 mt-2", icon = icon("sitemap")),
                                 hr(class = "section-divider"),
                                 textInput("le_clade_check", i18n$t("Check clade membership"),
                                           placeholder = i18n$t("e.g. Amniota")),
                                 actionButton("le_member_run", i18n$t("Check"),
                                              class = "btn btn-outline-secondary w-100 mt-1")
                             )
                           )
                    ),
                    column(8,
                           uiOutput("le_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 5: Search Database ─────────────────────────────────────────────
    nav_panel(i18n$t("Search Database"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Search Taxonomicon")),
                             div(class = "p-3",
                                 textInput("sd_taxon", i18n$t("Taxon name"), placeholder = i18n$t("e.g. Bacteria")),
                                 actionButton("sd_run", i18n$t("Search"),
                                              class = "btn btn-primary w-100 mt-2", icon = icon("search")),
                                 hr(class = "section-divider"),
                                 div(class = "text-muted fst-italic small",
                                     i18n$t("Use this tool to find exact numeric IDs for ambiguous taxa (homonyms or historical ranks)."),
                                     tags$br(), tags$br(),
                                     tags$b(i18n$t("Tip:")), i18n$t(" You can type or paste these numeric IDs directly into ANY other tab in this app instead of the taxon name!")
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("sd_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 6: Coverage Check ─────────────────────────────────────────────────
    nav_panel(i18n$t("Coverage Check"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Taxa to Check")),
                             div(class = "p-3",
                                 textAreaInput("cc_taxa", i18n$t("Enter taxa (one per line or comma-separated)"),
                                               rows = 8,
                                               placeholder = "Tyrannosaurus\nVelociraptor\nFakeosaurus\nHomo"
                                 ),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("cc_run", i18n$t("Check Coverage"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("check-circle")),
                                     actionButton("cc_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 fileInput("cc_upload", i18n$t("Or upload CSV (one taxon per row)"),
                                           accept = ".csv", width = "100%",
                                           buttonLabel = gsub("<.*?>", "", i18n$t("Browse...")),
                                           placeholder = gsub("<.*?>", "", i18n$t("No file selected")))
                             )
                           )
                    ),
                    column(8,
                           uiOutput("cc_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 7: Filter by Clade ────────────────────────────────────────────────
    nav_panel(i18n$t("Filter by Clade"),
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Filter Settings")),
                             div(class = "p-3",
                                 textAreaInput("fc_taxa", i18n$t("Taxa list (one per line or comma-separated)"),
                                               rows = 6,
                                               placeholder = "Tyrannosaurus\nTriceratops\nVelociraptor\nBrachiosaurus\nHomo"
                                 ),
                                 textInput("fc_clade", i18n$t("Clade to filter by"), placeholder = i18n$t("e.g. Theropoda")),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("fc_run", i18n$t("Filter"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("filter")),
                                     actionButton("fc_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 )
                             )
                           )
                    ),
                    column(8,
                           uiOutput("fc_result_ui")
                    )
                  )
              )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Language update logic ──────────────────────────────────────────────────────
  observeEvent(input$selected_language, {
    lang <- input$selected_language
    i18n$set_translation_language(lang)
    shiny.i18n::update_lang(lang)

    txt <- function(x) i18n$t(x)
    clean_txt <- function(x) gsub("<.*?>", "", i18n$t(x)) # Safely strips HTML!

    # Update standard inputs
    updateTextInput(session, "pd_taxon_a", label = txt("Taxon A"), placeholder = clean_txt("e.g. Tyrannosaurus"))
    updateTextInput(session, "pd_taxon_b", label = txt("Taxon B"), placeholder = clean_txt("e.g. Velociraptor"))
    updateActionButton(session, "pd_run", label = txt("Compute Distance"))
    updateActionButton(session, "pd_example", label = txt("Shuffle"))

    updateTextAreaInput(session, "dm_taxa", label = txt("Enter taxa (one per line or comma-separated)"))
    updateActionButton(session, "dm_run", label = txt("Build Matrix"))
    updateActionButton(session, "dm_example", label = txt("Shuffle"))

    updateTextInput(session, "cr_query", label = txt("Query Taxon"), placeholder = clean_txt("e.g. Tyrannosaurus"))
    updateTextAreaInput(session, "cr_candidates", label = txt("Candidate Taxa (one per line or comma-separated)"))
    updateActionButton(session, "cr_run", label = txt("Find Closest"))
    updateActionButton(session, "cr_example", label = txt("Shuffle"))

    updateTextInput(session, "le_taxon", label = txt("Taxon name"), placeholder = clean_txt("e.g. Homo sapiens"))
    updateActionButton(session, "le_run", label = txt("Get Lineage"))
    updateTextInput(session, "le_clade_check", label = txt("Check clade membership"), placeholder = clean_txt("e.g. Amniota"))
    updateActionButton(session, "le_member_run", label = txt("Check"))

    updateTextInput(session, "sd_taxon", label = txt("Taxon name"), placeholder = clean_txt("e.g. Bacteria"))
    updateActionButton(session, "sd_run", label = txt("Search"))

    updateTextAreaInput(session, "cc_taxa", label = txt("Enter taxa (one per line or comma-separated)"))
    updateActionButton(session, "cc_run", label = txt("Check Coverage"))
    updateActionButton(session, "cc_example", label = txt("Shuffle"))

    updateTextAreaInput(session, "fc_taxa", label = txt("Taxa list (one per line or comma-separated)"))
    updateTextInput(session, "fc_clade", label = txt("Clade to filter by"), placeholder = clean_txt("e.g. Theropoda"))
    updateActionButton(session, "fc_run", label = txt("Filter"))
    updateActionButton(session, "fc_example", label = txt("Shuffle"))

    # Safely update the File Upload buttons via Javascript!
    session$sendCustomMessage("translate_file_inputs", list(
      browse  = clean_txt("Browse..."),
      no_file = clean_txt("No file selected")
    ))
  })

  # Helper for server-side generated text (renderUI/Plots)
  tr <- reactive({
    req(input$selected_language)
    function(msg, ...) {
      translated <- i18n$t(msg)
      args <- list(...)
      if (length(args) > 0) return(do.call(sprintf, c(list(translated), args)))
      return(translated)
    }
  })

  # ── Dynamic UI Elements ─────────────────────────────────────────────────────
  output$app_subtitle <- renderText({
    tr()("Taxonomic Distance & Phylogenetic Lineage Explorer — powered by The Taxonomicon")
  })

  # ── Pairwise Distance ───────────────────────────────────────────────────────

  shuffle_pool <- c(
    "Tyrannosaurus", "Triceratops", "Carnotaurus", "Velociraptor", "Spinosaurus", "Stegosaurus", "Brachiosaurus", "Diplodocus",
    "Struthio", "Gallus", "Anas", "Columba", "Falco", "Corvus", "Ara", "Spheniscus", "Aptenodytes", "Tyto", "Bubo",
    "Homo", "Panthera", "Canis", "Felis", "Equus", "Bos", "Sus", "Ovis", "Capra", "Mus", "Rattus", "Loxodonta",
    "Crocodylus", "Alligator", "Chelonia", "Varanus", "Python", "Boa", "Iguana", "Rana", "Bufo", "Ambystoma",
    "Octopus", "Loligo", "Carcharodon", "Sphyrna", "Salmo", "Oncorhynchus", "Thunnus", "Hippocampus", "Danio",
    "Drosophila", "Apis", "Bombus", "Atta", "Camponotus", "Anopheles", "Aedes", "Culex", "Danaus", "Manduca",
    "Quercus", "Pinus", "Ginkgo", "Araucaria", "Eucalyptus", "Ficus", "Zea", "Oryza", "Triticum", "Solanum",
    "Saccharomyces", "Amanita", "Aspergillus", "Penicillium", "Candida", "Neurospora", "Pleurotus",
    "Escherichia", "Bacillus", "Staphylococcus", "Streptococcus", "Pseudomonas", "Salmonella", "Lactobacillus",
    "Plasmodium", "Trypanosoma", "Leishmania", "Giardia", "Euglena", "Tetrahymena", "Amoeba"
  )

  observeEvent(input$pd_example, {
    pair <- sample(shuffle_pool, 2)
    updateTextInput(session, "pd_taxon_a", value = pair[1])
    updateTextInput(session, "pd_taxon_b", value = pair[2])
  })

  pd_result <- eventReactive(input$pd_run, {
    req(nchar(trimws(input$pd_taxon_a)) > 0, nchar(trimws(input$pd_taxon_b)) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      taxo_distance(trimws(input$pd_taxon_a), trimws(input$pd_taxon_b), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$pd_result_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    dist_fmt <- if (is.infinite(res$distance)) "∞" else
      if (res$distance == 0) tr()("0 (ancestor)") else
        round(res$distance, 6)
    interp <- if (is.infinite(res$distance)) tr()("No common ancestor found.")
    else if (res$distance == 0) tr()("One taxon is an ancestor of the other.")
    else tr()("MRCA at depth %d; distance = 1 / %d.", res$mrca_depth, res$mrca_depth)

    tagList(
      div(class = "result-box mb-3",
          fluidRow(
            column(4,
                   div(class = "result-label", tr()("Distance")),
                   div(class = "result-distance", dist_fmt)
            ),
            column(8,
                   div(class = "result-label", tr()("Most Recent Common Ancestor")),
                   div(class = "result-mrca", res$mrca),
                   div(class = "result-meta mt-1",
                       tr()("MRCA depth: %d | Depth %s: %d | Depth %s: %d",
                            res$mrca_depth, res$taxon_a, res$depth_a, res$taxon_b, res$depth_b)
                   ),
                   div(class = "result-meta mt-1 fst-italic", interp)
            )
          )
      )
    )
  })

  output$pd_lineage_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(NULL)
    ta <- trimws(input$pd_taxon_a)
    tb <- trimws(input$pd_taxon_b)
    lin_a <- get_lineage(ta)
    lin_b <- get_lineage(tb)
    if (is.null(lin_a) || is.null(lin_b)) return(NULL)

    mrca_d <- res$mrca_depth
    shared  <- lin_a[seq_len(mrca_d)]

    render_lineage <- function(lin, label, shared_depth) {
      nodes <- lapply(seq_along(lin), function(i) {
        cls <- if (i == shared_depth) "lineage-node mrca"
        else if (i <= shared_depth) "lineage-node shared"
        else "lineage-node"
        tagList(
          if (i > 1) span(class = "lineage-arrow", "›") else NULL,
          span(class = cls, lin[i])
        )
      })
      tagList(
        div(class = "result-label mb-1", label),
        div(style = "line-height: 2.2;", nodes)
      )
    }

    card(
      card_header(tr()("Lineage Comparison")),
      div(class = "p-3",
          fluidRow(
            column(12,
                   div(class = "mb-2",
                       span(class = "lineage-node shared", "■"), tr()(" Shared trunk  "),
                       span(class = "lineage-node mrca", "■"), tr()(" MRCA  "),
                       span(class = "lineage-node", "■"), tr()(" Unique")
                   )
            )
          ),
          fluidRow(
            column(6, render_lineage(lin_a, ta, mrca_d)),
            column(6, render_lineage(lin_b, tb, mrca_d))
          )
      )
    )
  })

  # ── Distance Matrix ─────────────────────────────────────────────────────────

  observeEvent(input$dm_example, {
    n <- sample(5:7, 1)
    taxa <- sample(shuffle_pool, n)
    updateTextAreaInput(session, "dm_taxa", value = paste(taxa, collapse = "\n"))
  })

  observeEvent(input$dm_upload, {
    req(input$dm_upload)
    df <- tryCatch(read.csv(input$dm_upload$datapath, header = FALSE, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (!is.null(df)) {
      taxa_str <- paste(as.character(df[[1]]), collapse = "\n")
      updateTextAreaInput(session, "dm_taxa", value = taxa_str)
    }
  })

  dm_result <- eventReactive(input$dm_run, {
    taxa <- parse_taxa_input(input$dm_taxa)
    req(length(taxa) >= 2)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      distance_matrix(taxa, verbose = FALSE, progress = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })


  output$dm_result_ui <- renderUI({
    dm <- dm_result()
    if (is.null(dm)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))
    n  <- length(attr(dm, "Labels"))
    ht <- max(350, n * 58)

    tagList(
      card(
        card_header(tr()("Distance Matrix")),
        div(class = "p-2", DTOutput("dm_table"))
      ),
      fluidRow(
        class = "mt-3",
        column(6,
               card(
                 card_header(tr()("Dendrogram")),
                 div(class = "p-2", plotOutput("dm_dendro", height = paste0(ht, "px")))
               )
        ),
        column(6,
               card(
                 card_header(tr()("Ordination (PCoA)")),
                 div(class = "p-2", plotOutput("dm_pcoa", height = paste0(ht, "px")))
               )
        )
      )
    )
  })

  output$dm_dendro <- renderPlot({
    dm <- dm_result()
    req(dm)

    hc <- hclust(dm, method = "average")
    n  <- length(hc$labels)

    leaf_y        <- numeric(n)
    leaf_y[hc$order] <- seq_len(n)
    node_x <- hc$height
    node_y <- numeric(n - 1)

    get_node_y <- function(k) {
      if (k < 0) return(leaf_y[-k])
      node_y[k]
    }

    for (i in seq_len(n - 1)) {
      node_y[i] <- mean(c(get_node_y(hc$merge[i, 1]),
                          get_node_y(hc$merge[i, 2])))
    }

    seg_list <- do.call(rbind, lapply(seq_len(n - 1), function(i) {
      px   <- node_x[i]
      left <- hc$merge[i, 1]
      right<- hc$merge[i, 2]
      cy_l <- get_node_y(left)
      cy_r <- get_node_y(right)
      ch_l <- if (left  < 0) 0 else node_x[left]
      ch_r <- if (right < 0) 0 else node_x[right]
      data.frame(
        x    = c(cy_l, cy_l, cy_r),
        xend = c(cy_r, cy_l, cy_r),
        y    = c(px,   ch_l, ch_r),
        yend = c(px,   px,   px),
        stringsAsFactors = FALSE
      )
    }))

    tip_df <- data.frame(x = leaf_y, label = hc$labels, stringsAsFactors = FALSE)

    get_leaves <- function(k) {
      if (k < 0) return(hc$labels[-k])
      c(get_leaves(hc$merge[k, 1]), get_leaves(hc$merge[k, 2]))
    }
    node_mrca <- sapply(seq_len(n - 1), function(i) {
      lvs <- get_leaves(i)
      tryCatch({
        r <- mrca(lvs[1], lvs[length(lvs)])
        if (is.null(r)) NA_character_ else r
      }, error = function(e) NA_character_)
    })

    node_df <- data.frame(x = node_y, y = node_x, mrca = node_mrca, stringsAsFactors = FALSE)
    node_df <- node_df[!is.na(node_df$mrca), ]

    max_dist  <- max(hc$height)
    max_chars <- max(nchar(hc$labels))
    label_gap   <- max_dist * 0.000001
    label_width <- max_chars * max_dist * 0.018

    ggplot() +
      geom_segment(data = seg_list, aes(x = x, xend = xend, y = y, yend = yend), colour = "#2D5016", linewidth = 1.1) +
      geom_text(data = tip_df, aes(x = x, label = label), y = -label_gap, hjust = 0, vjust = 0.5, size = 5.5, family = "serif", fontface = "italic", colour = "#1C1812") +
      geom_text(data = node_df, aes(x = x, y = y, label = mrca), hjust = 0.5, vjust = -0.65, size = 6, family = "serif", fontface = "italic", colour = "#8B6914") +
      geom_point(data = node_df, aes(x = x, y = y), colour = "#8B6914", fill = "#FAF7F0", shape = 21, size = 3.5, stroke = 1.4) +
      coord_flip(clip = "off") +
      scale_y_reverse(limits = c(max_dist * 1.04, -(label_gap + label_width)), expand = expansion(mult = c(0.05, 0.7))) +
      scale_x_continuous(expand = expansion(mult = 0.05)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "serif") +
      theme(
        axis.text       = element_blank(),
        axis.ticks      = element_blank(),
        panel.grid      = element_blank(),
        plot.background = element_rect(fill = "#FAF7F0", colour = NA),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }, bg = "#FAF7F0")

  output$dm_table <- renderDT({
    dm <- dm_result()
    req(dm)
    mat     <- as.matrix(dm)
    mat_fmt <- round(mat, 5)
    datatable(mat_fmt,
              options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
              class = "compact hover"
    ) |>
      formatStyle(columns = colnames(mat_fmt),
                  background = styleInterval(c(0.05, 0.15, 0.3), c("#d4e8c2", "#eaf3d9", "#fdf6e3", "#f8e8d8"))
      )
  })

  output$dm_pcoa <- renderPlot({
    dm <- dm_result()
    req(dm)

    ord <- taxo_ordinate(dm, k = 2)
    df <- as.data.frame(ord$points)
    colnames(df) <- c("PC1", "PC2")
    df$Taxon <- rownames(df)

    gof_pct <- round(ord$GOF[1] * 100, 1)

    ggplot(df, aes(x = PC1, y = PC2, label = Taxon)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#C8B99A", linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#C8B99A", linewidth = 0.5) +
      geom_point(color = "#8B6914", fill = "#FAF7F0", shape = 21, size = 3.5, stroke = 1.4) +
      geom_text_repel(
        family = "serif", fontface = "italic", color = "#1C1812", size = 5,
        box.padding = 0.5, point.padding = 0.3, segment.color = "#C8B99A", segment.alpha = 0.6
      ) +
      labs(x = tr()("Coordinate 1"), y = tr()("Coordinate 2"),
           subtitle = tr()("Goodness-of-fit: %s%%", gof_pct)) +
      theme_minimal(base_family = "serif") +
      theme(
        panel.grid.major = element_line(color = "#EAE3D5"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#FAF7F0", colour = NA),
        plot.margin = margin(10, 20, 10, 10),
        plot.subtitle = element_text(color = "#7A6B55", face = "italic")
      )
  }, bg = "#FAF7F0")

  # ── Closest Relative ────────────────────────────────────────────────────────

  observeEvent(input$cr_example, {
    picked <- sample(shuffle_pool, sample(5:6, 1))
    updateTextInput(session, "cr_query", value = picked[1])
    updateTextAreaInput(session, "cr_candidates", value = paste(picked[-1], collapse = "\n"))
  })

  cr_result <- eventReactive(input$cr_run, {
    req(nchar(trimws(input$cr_query)) > 0)
    candidates <- parse_taxa_input(input$cr_candidates)
    req(length(candidates) >= 1)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))

    query <- trimws(input$cr_query)
    dist_df <- withCallingHandlers(
      closest_relative(query, candidates, verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (is.null(dist_df)) return(NULL)

    query_depth <- lineage_depth(query)
    cand_depths <- sapply(candidates, function(t) {
      d <- lineage_depth(t)
      if (is.null(d)) NA_integer_ else as.integer(d)
    })
    dist_df$depth <- cand_depths[dist_df$taxon]

    list(df = dist_df, query = query, query_depth = query_depth)
  })

  SHALLOW_THRESHOLD <- 25L

  output$cr_result_ui <- renderUI({
    res <- cr_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    df          <- res$df
    query       <- res$query
    query_depth <- res$query_depth
    closest     <- df[1, ]

    max_dist <- max(df$distance, na.rm = TRUE)
    if (is.na(max_dist) || max_dist == 0) max_dist <- 1

    any_shallow <- any(!is.na(df$depth) & df$depth < SHALLOW_THRESHOLD)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row      <- df[i, ]
      bar_pct  <- if (is.na(row$distance)) 0L else round(100 * row$distance / max_dist)
      bar_col  <- if (i == 1L) "#2D5016" else "#8B6914"
      is_shallow <- !is.na(row$depth) && row$depth < SHALLOW_THRESHOLD
      depth_lbl  <- if (is.na(row$depth)) "?" else as.character(row$depth)
      warn_icon  <- if (is_shallow) " ⚠" else ""

      tags$tr(
        style = if (is_shallow) "background:#fffbf0;" else "",
        tags$td(
          style = "font-style:italic; padding:0.4rem 0.6rem;",
          row$taxon, tags$span(style="color:#8B6914; font-style:normal;", warn_icon)
        ),
        tags$td(
          style = "padding:0.4rem 0.6rem; font-family:'JetBrains Mono',monospace; font-size:0.82rem;",
          if (is.na(row$distance)) "NA" else round(row$distance, 6)
        ),
        tags$td(
          style = "padding:0.4rem 0.6rem; font-family:'JetBrains Mono',monospace; font-size:0.78rem; color:#7A6B55;",
          depth_lbl
        ),
        tags$td(style = "padding:0.4rem 0.6rem; width:35%;",
                div(style = sprintf("height:10px; width:%d%%; background:%s; border-radius:2px;", bar_pct, bar_col), "")
        )
      )
    })

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", tr()("Closest relative to")),
          div(style = "font-family:'Playfair Display',serif; font-size:1.3rem; font-style:italic;", query),
          div(class = "result-meta", if (!is.null(query_depth)) tr()("lineage depth: %d", query_depth) else ""),
          div(class = "result-label mt-2", tr()("is")),
          div(class = "result-mrca", closest$taxon),
          div(class = "result-meta",
              tr()("distance = %s", round(closest$distance, 6)),
              if (!is.na(closest$depth)) tr()(" | lineage depth: %d", closest$depth) else ""
          )
      ),
      if (any_shallow)
        div(
          style = "background:#fffbf0; border:1px solid #e0c97a; border-left:4px solid #8B6914; border-radius:3px; padding:0.7rem 1rem; margin-bottom:0.8rem; font-size:0.83rem; color:#5B4A2E;",
          tags$b(tr()("⚠ Data quality notice: ")),
          tr()("One or more taxa have a shallow lineage depth (< 25 nodes) in The Taxonomicon, meaning they are poorly resolved in the database. Their distances may be artificially large and rankings unreliable. Check the Lineage Explorer tab for details.")
        ),
      card(
        card_header(tr()("All candidates ranked")),
        div(class = "p-0",
            tags$table(
              class = "table table-sm mb-0",
              style = "font-size:0.85rem;",
              tags$thead(tags$tr(
                tags$th(style="padding:0.4rem 0.6rem;", tr()("Taxon")),
                tags$th(style="padding:0.4rem 0.6rem;", tr()("Distance")),
                tags$th(style="padding:0.4rem 0.6rem;", tr()("Depth")),
                tags$th(style="padding:0.4rem 0.6rem;", "")
              )),
              tags$tbody(rows)
            )
        )
      )
    )
  })

  # ── Lineage Explorer ────────────────────────────────────────────────────────

  le_lineage <- eventReactive(input$le_run, {
    req(nchar(trimws(input$le_taxon)) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      get_lineage(trimws(input$le_taxon), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  le_member <- eventReactive(input$le_member_run, {
    req(le_lineage(), nchar(trimws(input$le_clade_check)) > 0)
    is_member(trimws(input$le_taxon), trimws(input$le_clade_check))
  })

  output$le_result_ui <- renderUI({
    lin <- le_lineage()
    if (is.null(lin)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    nodes <- lapply(seq_along(lin), function(i) {
      tagList(
        if (i > 1) span(class = "lineage-arrow", "›") else NULL,
        span(class = "lineage-node", lin[i])
      )
    })

    mem <- tryCatch(le_member(), error = function(e) NULL)
    mem_ui <- if (!is.null(mem) && nchar(trimws(input$le_clade_check)) > 0) {
      col <- if (isTRUE(mem)) "#d4e8c2" else "#f5d0c8"
      txt <- if (isTRUE(mem)) tr()("%s IS a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      else tr()("%s is NOT a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      div(style = sprintf("background:%s; border-radius:3px; padding:0.6rem 1rem; font-size:0.88rem; margin-top:0.8rem; font-style:italic;", col), txt)
    } else NULL

    tagList(
      card(
        card_header(tr()("Lineage of %s (%d nodes)", trimws(input$le_taxon), length(lin))),
        div(class = "p-3",
            div(style = "line-height:2.4;", nodes),
            mem_ui
        )
      )
    )
  })

  # ── Search Database ─────────────────────────────────────────────────────────

  sd_result <- eventReactive(input$sd_run, {
    req(nchar(trimws(input$sd_taxon)) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))

    withCallingHandlers(
      taxo_search(trimws(input$sd_taxon), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$sd_result_ui <- renderUI({
    res <- sd_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("No matches found.")))

    tagList(
      card(
        card_header(tr()("Search results for '%s'", trimws(input$sd_taxon))),
        div(class = "p-2",
            renderDT({
              datatable(
                res,
                options = list(pageLength = 10, dom = "t", scrollX = TRUE),
                rownames = FALSE,
                selection = "none",
                colnames = c(tr()("Numeric ID"), tr()("Accepted Name / Rank"))
              ) |>
                formatStyle('id', fontWeight = 'bold', color = '#2D5016')
            })
        )
      )
    )
  })

  # ── Coverage Check ──────────────────────────────────────────────────────────

  observeEvent(input$cc_example, {
    taxa <- c(sample(shuffle_pool, 4), "Fakeosaurus", "Imaginarius")
    updateTextAreaInput(session, "cc_taxa", value = paste(sample(taxa), collapse = "\n"))
  })

  observeEvent(input$cc_upload, {
    req(input$cc_upload)
    df <- tryCatch(read.csv(input$cc_upload$datapath, header = FALSE, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (!is.null(df)) {
      taxa_str <- paste(as.character(df[[1]]), collapse = "\n")
      updateTextAreaInput(session, "cc_taxa", value = taxa_str)
    }
  })

  cc_result <- eventReactive(input$cc_run, {
    taxa <- parse_taxa_input(input$cc_taxa)
    req(length(taxa) >= 1)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    withCallingHandlers(
      check_coverage(taxa, verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$cc_result_ui <- renderUI({
    res <- cc_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    n_found    <- sum(res, na.rm = TRUE)
    n_notfound <- sum(!res, na.rm = TRUE)

    pills <- lapply(names(res), function(nm) {
      cls <- if (isTRUE(res[nm])) "cov-pill cov-found" else "cov-pill cov-notfound"
      span(class = cls, nm)
    })

    tagList(
      div(class = "result-box mb-3",
          fluidRow(
            column(6,
                   div(class = "result-label", tr()("Found in Taxonomicon")),
                   div(class = "result-distance", style = "color:#2D5016;", n_found)
            ),
            column(6,
                   div(class = "result-label", tr()("Not found")),
                   div(class = "result-distance", style = "color:#8B3A1A;", n_notfound)
            )
          )
      ),
      card(
        card_header(tr()("Coverage by taxon")),
        div(class = "p-3", pills)
      )
    )
  })

  # ── Filter by Clade ─────────────────────────────────────────────────────────

  observeEvent(input$fc_example, {
    clades <- list(
      list(clade = "Theropoda",  taxa = c("Tyrannosaurus","Velociraptor","Spinosaurus","Carnotaurus","Triceratops","Brachiosaurus")),
      list(clade = "Dinosauria", taxa = c("Tyrannosaurus","Triceratops","Brachiosaurus","Velociraptor","Homo","Quercus")),
      list(clade = "Amniota",    taxa = c("Homo","Panthera","Canis","Tyrannosaurus","Drosophila","Quercus"))
    )
    pick <- clades[[sample(length(clades), 1)]]
    updateTextAreaInput(session, "fc_taxa",  value = paste(pick$taxa,  collapse = "\n"))
    updateTextInput(session,    "fc_clade", value = pick$clade)
  })

  fc_result <- eventReactive(input$fc_run, {
    taxa  <- parse_taxa_input(input$fc_taxa)
    clade <- trimws(input$fc_clade)
    req(length(taxa) >= 1, nchar(clade) > 0)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    list(
      all   = taxa,
      kept  = withCallingHandlers(
        filter_clade(taxa, clade, verbose = FALSE),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); character(0) }
      ),
      clade = clade
    )
  })

  output$fc_result_ui <- renderUI({
    res <- fc_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    excluded <- setdiff(res$all, res$kept)
    make_tags <- function(taxa, cls) lapply(taxa, function(t) span(class = cls, t))

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", tr()("Clade filter")),
          div(style = "font-family:'Playfair Display',serif; font-size:1.2rem; font-style:italic;",
              res$clade),
          div(class = "result-meta mt-1",
              tr()("%d of %d taxa retained", length(res$kept), length(res$all)))
      ),
      card(
        card_header(tr()("Results")),
        div(class = "p-3",
            div(class = "result-label mb-1", tr()("In %s (%d)", res$clade, length(res$kept))),
            if (length(res$kept) > 0) div(make_tags(res$kept, "taxon-tag")) else div(class="text-muted fst-italic small", tr()("none")),
            hr(class = "section-divider"),
            div(class = "result-label mb-1", tr()("Not in %s (%d)", res$clade, length(excluded))),
            if (length(excluded) > 0) div(make_tags(excluded, "taxon-tag")) else div(class="text-muted fst-italic small", tr()("none"))
        )
      )
    )
  })
}

shinyApp(ui, server)
