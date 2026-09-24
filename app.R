library(shiny)
library(bslib)
library(taxodist)
library(ggplot2)
library(ggrepel)
library(DT)
library(shiny.i18n)

if (file.exists("taxobase.rds")) {
  tryCatch(
    load_cache("taxobase.rds"),
    error = function(e) warning("Could not load the bundled lineage cache: ", conditionMessage(e))
  )
}

MAX_MATRIX_TAXA <- 50L
SHALLOW_LINEAGE_GUIDE <- 25L

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

read_taxa_csv <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  separator <- if (length(first_line) && lengths(regmatches(first_line, gregexpr(";", first_line))) >
                   lengths(regmatches(first_line, gregexpr(",", first_line)))) ";" else ","

  with_header <- tryCatch(
    read.table(
      path,
      header = TRUE,
      sep = separator,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(e) NULL
  )

  accepted_headers <- c("taxon", "taxa", "species", "name", "scientific_name")
  taxon_column <- if (!is.null(with_header)) {
    match(TRUE, tolower(trimws(names(with_header))) %in% accepted_headers)
  } else {
    NA_integer_
  }

  values <- if (!is.na(taxon_column)) {
    with_header[[taxon_column]]
  } else {
    without_header <- read.table(
      path,
      header = FALSE,
      sep = separator,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
    if (ncol(without_header) < 1L) stop("The CSV file does not contain a taxon column.")
    without_header[[1]]
  }

  values <- trimws(as.character(values))
  unique(values[!is.na(values) & nzchar(values)])
}

example_taxa <- function() {
  c(
    "Tyrannosaurus",
    "Velociraptor",
    "Triceratops",
    "Brachiosaurus",
    "Homo",
    "Quercus"
  )
}

# ── Theme / UI ────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version = 5,
  bg            = "#faf6ee",
  fg            = "#3d2510",
  primary       = "#5c3d1e",
  secondary     = "#8b5e3c",
  success       = "#6b7c3a",
  info          = "#6b4c30",
  font_scale    = 0.95,
  base_font     = font_google("Lora"),
  heading_font  = font_google("Playfair Display"),
  code_font     = font_google("DM Mono")
) |>
  bs_add_rules("
    :root, [data-bs-theme='light'] {
      --bg:      #faf6ee;
      --border:  #5c3d1e;
      --branch:  #8b5e3c;
      --tip:     #b8865a;
      --ink:     #3d2510;
      --muted:   #6b4c30;
      --rule:    #d9c9b0;
      --card-bg: #f3ede0;
      --code-bg: #ede5d0;
      --lineage-bg:          #fbf8f2;
      --lineage-text:        #3d2510;
      --lineage-shared-bg:   #e5d5bc;
      --lineage-shared-text: #40270f;
      --lineage-mrca-bg:     #5c3d1e;
      --lineage-mrca-text:   #fffaf3;
      --lineage-desc-bg:     #f0e4d0;
      --lineage-desc-text:   #5b3b20;
      --success-bg:          #e3eddc;
      --success-text:        #28451f;
      --danger-bg:           #f5d8d1;
      --danger-text:         #6b1a0a;
      --notice-bg:           #f7f0e5;
      --notice-text:         #5a462f;
      --overlay-bg:          rgba(250,246,238,0.72);
      --primary-text:        #fffaf3;
      --primary-hover-bg:    #3a2410;
      --primary-hover-text:  #fffaf3;
      --bs-body-bg:       var(--bg);
      --bs-body-color:    var(--ink);
      --bs-card-bg:       var(--card-bg);
      --bs-border-color:  var(--rule);
      --bs-primary:       var(--border);
    }

    [data-bs-theme='dark'] {
      --bg:      #1a1a1a;
      --border:  #e5c19d;
      --branch:  #c2946d;
      --tip:     #ede5d0;
      --ink:     #f4f1ea;
      --muted:   #b0a290;
      --rule:    #3d362e;
      --card-bg: #26211c;
      --code-bg: #2d2620;
      --lineage-bg:          #211d18;
      --lineage-text:        #f4f1ea;
      --lineage-shared-bg:   #493726;
      --lineage-shared-text: #f6eadc;
      --lineage-mrca-bg:     #d8b38c;
      --lineage-mrca-text:   #24160a;
      --lineage-desc-bg:     #382b20;
      --lineage-desc-text:   #eed7be;
      --success-bg:          #2d412b;
      --success-text:        #dcebd8;
      --danger-bg:           #4b2925;
      --danger-text:         #f5d4cc;
      --notice-bg:           #33291f;
      --notice-text:         #e3d3bf;
      --overlay-bg:          rgba(20,18,16,0.78);
      --primary-text:        #24160a;
      --primary-hover-bg:    #c2946d;
      --primary-hover-text:  #1a120b;

      --bs-body-bg:       var(--bg);
      --bs-body-color:    var(--ink);
      --bs-card-bg:       var(--card-bg);
      --bs-border-color:  var(--rule);
      --bs-primary:       var(--border);
    }

    body {
      background-color: var(--bg);
      background-image:
        radial-gradient(ellipse at 20% 10%, rgba(92,61,30,0.05) 0%, transparent 60%),
        radial-gradient(ellipse at 80% 90%, rgba(139,94,60,0.06) 0%, transparent 55%);
      font-family: 'Lora', Georgia, serif;
    }

    /* ── Header ── */
    .app-header {
      background: var(--border);
      background-image: linear-gradient(135deg, #3a2410 0%, #5c3d1e 50%, #6e4a25 100%);
      color: #faf6ee;
      padding: 2rem 2.5rem 1.6rem;
      border-bottom: 3px solid var(--tip);
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
      color: #faf6ee;
      position: relative;
    }
    .app-header .subtitle {
      font-size: 0.85rem;
      color: rgba(250,246,238,0.72);
      margin-top: 0.3rem;
      letter-spacing: 0.05em;
      font-style: italic;
      position: relative;
    }
    .app-header .badge-pkg {
      display: inline-block;
      background: var(--tip);
      color: #3d2510;
      font-size: 0.72rem;
      font-family: 'DM Mono', monospace;
      font-style: normal;
      padding: 0.15rem 0.55rem;
      border-radius: 2px;
      margin-left: 0.5rem;
      vertical-align: middle;
      position: relative;
    }
    .header-controls {
      position: absolute;
      right: 25px;
      top: 25px;
      z-index: 1000;
      display: flex;
      gap: 0.8rem;
      align-items: flex-start;
      height: 38px;
    }
    .theme-control,
    .language-control {
      height: 38px;
      min-height: 38px;
    }
    .theme-control {
      display: flex;
      align-items: center;
      flex: 0 0 auto;
    }
    .language-control {
      width: 150px;
      flex: 0 0 150px;
    }
    .header-controls .shiny-input-container,
    .header-controls .form-group,
    .header-controls .form-check,
    .header-controls .selectize-control {
      margin-top: 0 !important;
      margin-bottom: 0 !important;
    }
    .header-controls .bslib-input-dark-mode,
    .header-controls .form-check {
      display: flex;
      align-items: center;
      min-height: 38px;
    }
    .header-controls .selectize-input {
      display: flex;
      align-items: center;
      min-height: 38px;
      padding-top: 0.45rem;
      padding-bottom: 0.45rem;
      background: var(--bg) !important;
      color: var(--ink) !important;
      border-color: var(--rule) !important;
    }
    .header-controls .selectize-control,
    .header-controls .selectize-input {
      height: 38px;
    }
    .header-controls .selectize-input > *,
    .header-controls .selectize-input input {
      color: var(--ink) !important;
    }
    .header-controls .selectize-dropdown {
      background: var(--card-bg) !important;
      color: var(--ink) !important;
      border-color: var(--rule) !important;
    }
    .header-controls .selectize-dropdown .option {
      background: transparent;
      color: var(--ink) !important;
    }
    .header-controls .selectize-dropdown .option.active,
    .header-controls .selectize-dropdown .option.selected {
      background: var(--lineage-shared-bg) !important;
      color: var(--lineage-shared-text) !important;
    }

    /* ── Nav tabs ── */
    .nav-tabs {
      border-bottom: 2px solid var(--rule);
      background: var(--card-bg);
      padding: 0 1.5rem;
      flex-wrap: nowrap;
      overflow-x: auto;
      scrollbar-width: thin;
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
      white-space: nowrap;
    }
    .nav-tabs .nav-link:hover { color: var(--border); }
    .nav-tabs .nav-link.active {
      color: var(--border);
      border-bottom-color: var(--border);
      background: transparent;
      font-weight: 600;
    }

    /* ── Cards ── */
    .card {
      border: 1px solid var(--rule);
      border-radius: 4px;
      background: var(--card-bg);
      box-shadow: 0 1px 4px rgba(61,37,16,0.07);
    }
    .card-header {
      background: transparent;
      border-bottom: 1px solid var(--rule);
      font-family: 'Playfair Display', serif;
      font-size: 0.95rem;
      font-weight: 600;
      color: var(--border);
      padding: 0.7rem 1.1rem;
      letter-spacing: 0.02em;
    }

    /* ── Inputs ── */
    .form-control, .form-select {
      background: var(--bg);
      border: 1px solid var(--rule);
      border-radius: 3px;
      font-family: 'Lora', Georgia, serif;
      font-size: 0.88rem;
      color: var(--ink);
    }
    .form-control:focus, .form-select:focus {
      border-color: var(--border);
      box-shadow: 0 0 0 2px rgba(92,61,30,0.15);
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
      background: var(--border);
      border-color: var(--border);
      color: var(--primary-text);
      font-family: 'Lora', serif;
      letter-spacing: 0.04em;
      border-radius: 3px;
    }
    .btn-primary:hover,
    .btn-primary:focus {
      background: var(--primary-hover-bg);
      border-color: var(--primary-hover-bg);
      color: var(--primary-hover-text);
    }
    .btn-outline-secondary {
      border-color: var(--rule);
      color: var(--muted);
      font-size: 0.82rem;
    }

    /* ── Result box ── */
    .result-box {
      background: var(--bg);
      border: 1px solid var(--rule);
      border-left: 4px solid var(--border);
      border-radius: 3px;
      padding: 1.1rem 1.3rem;
      font-family: 'Lora', Georgia, serif;
    }
    .result-distance {
      font-family: 'Playfair Display', serif;
      font-size: 2.4rem;
      color: var(--border);
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
      color: var(--branch);
      font-size: 1.05rem;
    }
    .result-meta {
      font-size: 0.82rem;
      color: var(--muted);
    }

    /* ── Lineage display ── */
    .lineage-node {
      display: inline-block;
      background: var(--lineage-bg);
      border: 1px solid var(--rule);
      border-radius: 2px;
      padding: 0.1rem 0.5rem;
      margin: 0.15rem 0.1rem;
      font-size: 0.8rem;
      font-family: 'Lora', serif;
      font-style: italic;
      color: var(--lineage-text);
    }
    .lineage-node.shared {
      background: var(--lineage-shared-bg);
      border-color: var(--branch);
      color: var(--lineage-shared-text);
      font-weight: 600;
    }
    .lineage-node.mrca {
      background: var(--lineage-mrca-bg);
      color: var(--lineage-mrca-text);
      border-color: var(--lineage-mrca-bg);
      font-weight: 700;
    }
    .lineage-arrow {
      color: var(--rule);
      font-size: 0.75rem;
      margin: 0 0.05rem;
    }

    /* ── Coverage pills ── */
    .cov-found    { background: var(--success-bg); color: var(--success-text); }
    .cov-notfound { background: var(--danger-bg); color: var(--danger-text); }
    .cov-pill {
      display: inline-block;
      border-radius: 2px;
      padding: 0.2rem 0.6rem;
      font-size: 0.8rem;
      font-family: 'DM Mono', monospace;
      margin: 0.2rem;
    }
    .lineage-node.descending {
      background: var(--lineage-desc-bg);
      border-color: var(--tip);
      color: var(--lineage-desc-text);
    }

    .membership-result {
      border: 1px solid var(--rule);
      border-radius: 3px;
      padding: 0.6rem 1rem;
      margin-top: 0.8rem;
      font-size: 0.88rem;
      font-style: italic;
    }
    .membership-result.member {
      background: var(--success-bg);
      color: var(--success-text);
    }
    .membership-result.non-member {
      background: var(--danger-bg);
      color: var(--danger-text);
    }

    .warning-icon { color: var(--branch); font-style: normal; }
    .depth-value { color: var(--muted); }
    .table > tbody > tr.shallow-lineage-row > * {
      background-color: var(--notice-bg);
    }
    .data-quality-notice {
      background: var(--notice-bg);
      border: 1px solid var(--rule);
      border-left: 4px solid var(--branch);
      border-radius: 3px;
      padding: 0.7rem 1rem;
      margin-bottom: 0.8rem;
      font-size: 0.83rem;
      color: var(--notice-text);
    }
    .coverage-found-count { color: var(--success-text); }
    .coverage-missing-count { color: var(--danger-text); }

    /* ── Misc ── */
    .section-divider {
      border: none;
      border-top: 1px solid var(--rule);
      margin: 1.2rem 0;
    }
    .taxon-tag {
      display: inline-block;
      background: var(--bg);
      border: 1px solid var(--rule);
      border-radius: 2px;
      padding: 0.1rem 0.45rem;
      font-size: 0.78rem;
      font-style: italic;
      margin: 0.1rem;
    }
    .distance-track {
      position: relative;
      height: 8px;
      width: 100%;
      border-radius: 999px;
      background: var(--rule);
    }
    .distance-marker {
      position: absolute;
      top: 50%;
      width: 12px;
      height: 12px;
      border-radius: 50%;
      transform: translate(-50%, -50%);
      border: 2px solid var(--card-bg);
      background: var(--branch);
    }
    .distance-marker.closest { background: var(--border); }
    .distance-marker.candidate { background: var(--branch); }
    .method-note {
      margin: 0 1.5rem 1.5rem;
      padding: 0.75rem 1rem;
      border: 1px solid var(--rule);
      border-left: 4px solid var(--branch);
      border-radius: 3px;
      background: var(--card-bg);
      color: var(--muted);
      font-size: 0.82rem;
    }
    .spinner-border { color: var(--border) !important; }
    .shiny-notification { font-family: 'Lora', serif; }

    /* ── Loading overlay ── */
    #loading_overlay {
      display: none;
      position: fixed;
      inset: 0;
      background: var(--overlay-bg);
      z-index: 9999;
      align-items: center;
      justify-content: center;
      flex-direction: column;
      gap: 0.8rem;
    }
    #loading_overlay.show { display: flex; }
    .loading-text {
      font-family: 'Playfair Display', serif;
      color: var(--border);
      font-size: 1rem;
    }
    :where(button, input, select, textarea, a):focus-visible {
      outline: 3px solid var(--tip);
      outline-offset: 2px;
    }
    @media (max-width: 768px) {
      .app-header {
        padding: 4.8rem 1.25rem 1.25rem;
      }
      .app-header h1 {
        font-size: 1.75rem;
      }
      .nav-tabs {
        padding: 0 0.5rem;
      }
      .nav-tabs .nav-link {
        padding: 0.7rem 0.85rem;
      }
      .header-controls {
        top: 18px;
        right: 18px;
        gap: 0.55rem;
      }
    }
  ")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = app_theme,
  shiny.i18n::usei18n(i18n),

  tags$head(

    tags$meta(
      name = "google-site-verification",
      content = "_sq1opIeTo7uYCMfevdGTqez7DmT-iVVFboDbPylWx8"
    ),

    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),

    tags$style(HTML("
      textarea { resize: vertical; }
      .dataTables_wrapper {
        font-size:0.83rem;
        font-family:'Lora',serif;
      }

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
      div(class = "header-controls",
          div(class = "theme-control",
              input_dark_mode(id = "dark_mode_toggle")
          ),
          div(class = "language-control",
              selectInput("selected_language", NULL,
                          choices = c(
                            "English"   = "en",
                            "Português" = "pt",
                            "Español"   = "es",
                            "Français"  = "fr",
                            "Deutsch"   = "de"
                          ),
                          width = "150px")
          )
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
                                           placeholder = gsub("<.*?>", "", i18n$t("No file selected"))),
                                 uiOutput("dm_template_ui")
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
                                           placeholder = gsub("<.*?>", "", i18n$t("No file selected"))),
                                 uiOutput("cc_template_ui")
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
  ),
  div(
    class = "method-note",
    icon("circle-info"),
    tags$span(style = "margin-left:0.4rem;", textOutput("method_note", inline = TRUE))
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
    tr()("Taxonomic Distance & Hierarchy Explorer — powered by The Taxonomicon")
  })

  output$method_note <- renderText({
    tr()("Distances represent depth in a taxonomic classification, not evolutionary time or phylogenetic branch length.")
  })

  output$dm_template_ui <- renderUI({
    downloadButton(
      "dm_template",
      tr()("Download CSV template"),
      class = "btn btn-outline-secondary btn-sm w-100"
    )
  })

  output$cc_template_ui <- renderUI({
    downloadButton(
      "cc_template",
      tr()("Download CSV template"),
      class = "btn btn-outline-secondary btn-sm w-100"
    )
  })

  plot_palette <- reactive({
    if (identical(input$dark_mode_toggle, "dark")) {
      list(
        background = "#26211c",
        ink = "#f4f1ea",
        muted = "#b0a290",
        rule = "#3d362e",
        branch = "#c2946d",
        point = "#e5c19d"
      )
    } else {
      list(
        background = "#f3ede0",
        ink = "#3d2510",
        muted = "#6b4c30",
        rule = "#d9c9b0",
        branch = "#8b5e3c",
        point = "#5c3d1e"
      )
    }
  })

  output$dm_template <- downloadHandler(
    filename = function() "taxodist-taxa-template.csv",
    content = function(file) {
      write.csv(data.frame(taxon = example_taxa()), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$cc_template <- downloadHandler(
    filename = function() "taxodist-coverage-template.csv",
    content = function(file) {
      write.csv(data.frame(taxon = example_taxa()), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

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
    tryCatch(
      taxo_distance(trimws(input$pd_taxon_a), trimws(input$pd_taxon_b), verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$pd_result_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    dist_fmt <- if (is.infinite(res$distance)) "∞" else
      if (res$distance == 0) tr()("0 (identical)") else
        round(res$distance, 6)
    interp <- if (is.infinite(res$distance)) tr()("No common ancestor found.")
    else if (res$distance == 0) tr()("The two inputs resolve to the same hierarchy node.")
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
    lin_a <- tryCatch(get_lineage(ta), error = function(e) NULL)
    lin_b <- tryCatch(get_lineage(tb), error = function(e) NULL)
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
    taxa <- tryCatch(
      read_taxa_csv(input$dm_upload$datapath),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        character(0)
      }
    )
    if (length(taxa) > 0L) {
      updateTextAreaInput(session, "dm_taxa", value = paste(taxa, collapse = "\n"))
      showNotification(tr()("Loaded %d unique taxa from CSV.", length(taxa)), type = "message")
    }
  })

  dm_result <- eventReactive(input$dm_run, {
    taxa <- parse_taxa_input(input$dm_taxa)
    req(length(taxa) >= 2)
    if (length(taxa) > MAX_MATRIX_TAXA) {
      showNotification(
        tr()("Distance matrices are limited to %d taxa in the web app.", MAX_MATRIX_TAXA),
        type = "warning",
        duration = 8
      )
      return(NULL)
    }
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    result <- tryCatch(
      distance_matrix(taxa, verbose = FALSE, progress = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(result) && (anyNA(result) || any(!is.finite(result)))) {
      showNotification(
        tr()("Some distances are missing or infinite. The table is available, but clustering and ordination require a complete finite matrix."),
        type = "warning",
        duration = 10
      )
    }
    result
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
    validate(need(
      !anyNA(dm) && all(is.finite(dm)),
      tr()("Clustering requires a complete matrix with finite distances.")
    ))

    pal <- plot_palette()

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
    if (n > 20L) node_df <- node_df[0, , drop = FALSE]

    max_dist  <- max(hc$height)
    if (!is.finite(max_dist) || max_dist <= 0) max_dist <- 1
    max_chars <- max(nchar(hc$labels))
    label_gap   <- max_dist * 0.000001
    label_width <- max_chars * max_dist * 0.018

    ggplot() +
      geom_segment(data = seg_list, aes(x = x, xend = xend, y = y, yend = yend),
                   colour = pal$point, linewidth = 1.1) +
      geom_text(data = tip_df, aes(x = x, label = label),
                y = -label_gap, hjust = 0, vjust = 0.5,
                size = max(3, 5.2 - n * 0.05),
                family = "serif", fontface = "italic", colour = pal$ink) +
      geom_text(data = node_df, aes(x = x, y = y, label = mrca),
                hjust = 0.5, vjust = -0.65, size = 4.2,
                family = "serif", fontface = "italic", colour = pal$branch) +
      geom_point(data = node_df, aes(x = x, y = y),
                 colour = pal$branch, fill = pal$background, shape = 21, size = 3.2, stroke = 1.2) +
      coord_flip(clip = "off") +
      scale_y_reverse(limits = c(max_dist * 1.04, -(label_gap + label_width)),
                      expand = expansion(mult = c(0.05, 0.7))) +
      scale_x_continuous(expand = expansion(mult = 0.05)) +
      labs(x = NULL, y = tr()("Taxonomic hierarchy distance")) +
      theme_minimal(base_family = "serif") +
      theme(
        axis.text.x     = element_text(color = pal$muted, size = 8),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_blank(),
        axis.title.x    = element_text(color = pal$muted, size = 9),
        panel.grid      = element_blank(),
        plot.background = element_rect(fill = pal$background, colour = NA),
        panel.background = element_rect(fill = pal$background, colour = NA),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }, bg = "transparent")

  output$dm_table <- renderDT({
    dm <- dm_result()
    req(dm)
    pal <- plot_palette()
    mat     <- as.matrix(dm)
    mat_fmt <- round(mat, 5)
    table <- datatable(
      mat_fmt,
      options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
      class = "compact hover"
    )

    finite_values <- mat[is.finite(mat)]
    if (length(finite_values) > 0L && diff(range(finite_values)) > 0) {
      cuts <- seq(min(finite_values), max(finite_values), length.out = 5)[2:4]
      table_colors <- if (identical(input$dark_mode_toggle, "dark")) {
        c("#493522", "#3d3025", "#332a23", "#2a241f")
      } else {
        c("#d9c9b0", "#e8dcc8", "#f0e8d8", "#faf6ee")
      }
      table <- table |>
        formatStyle(
          columns = colnames(mat_fmt),
          background = styleInterval(
            cuts,
            table_colors
          ),
          color = pal$ink
        )
    }
    table
  })

  output$dm_pcoa <- renderPlot({
    dm <- dm_result()
    req(dm)

    validate(need(
      !anyNA(dm) && all(is.finite(dm)),
      tr()("Ordination requires a complete matrix with finite distances.")
    ))

    pal <- plot_palette()
    ord <- taxo_ordinate(dm, k = 2)
    validate(need(!is.null(ord$points), tr()("Ordination could not be computed.")))
    df <- as.data.frame(ord$points)
    colnames(df) <- c("PC1", "PC2")
    df$Taxon <- rownames(df)

    gof_pct <- round(ord$GOF[1] * 100, 1)
    pcoa_subtitle <- tr()("Goodness-of-fit: %s%%", gof_pct)
    if (is.finite(gof_pct) && gof_pct < 50) {
      pcoa_subtitle <- paste(
        pcoa_subtitle,
        tr()("Limited two-dimensional representation."),
        sep = " — "
      )
    }
    eig_pos <- ord$eig[ord$eig > 0]
    var_pct <- 100 * eig_pos / sum(eig_pos)
    pc1_label <- tr()("Coordinate 1")
    pc2_label <- tr()("Coordinate 2")
    if (length(var_pct) >= 2L) {
      pc1_label <- sprintf("%s (%.1f%%)", pc1_label, var_pct[1])
      pc2_label <- sprintf("%s (%.1f%%)", pc2_label, var_pct[2])
    }

    ggplot(df, aes(x = PC1, y = PC2, label = Taxon)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = pal$rule, linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = pal$rule, linewidth = 0.5) +
      geom_point(color = pal$branch, fill = pal$background, shape = 21, size = 3.5, stroke = 1.4) +
      geom_text_repel(
        family = "serif", fontface = "italic", color = pal$ink, size = 4.2,
        box.padding = 0.5, point.padding = 0.3,
        segment.color = pal$rule, segment.alpha = 0.6
      ) +
      labs(x = pc1_label, y = pc2_label, subtitle = pcoa_subtitle) +
      theme_minimal(base_family = "serif") +
      theme(
        text = element_text(color = pal$ink),
        axis.text = element_text(color = pal$muted),
        panel.grid.major = element_line(color = pal$rule),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = pal$background, colour = NA),
        panel.background = element_rect(fill = pal$background, colour = NA),
        plot.margin      = margin(10, 20, 10, 10),
        plot.subtitle    = element_text(color = pal$muted, face = "italic")
      )
  }, bg = "transparent")

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
    dist_df <- tryCatch(
      closest_relative(query, candidates, verbose = FALSE),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (is.null(dist_df)) return(NULL)

    query_depth <- tryCatch(lineage_depth(query), error = function(e) NULL)
    cand_depths <- sapply(candidates, function(t) {
      d <- tryCatch(lineage_depth(t), error = function(e) NULL)
      if (is.null(d)) NA_integer_ else as.integer(d)
    })
    dist_df$depth <- cand_depths[dist_df$taxon]

    list(df = dist_df, query = query, query_depth = query_depth)
  })

  output$cr_result_ui <- renderUI({
    res <- cr_result()
    if (is.null(res)) return(div(class="p-3 text-muted fst-italic", tr()("Results will appear here.")))

    df          <- res$df
    query       <- res$query
    query_depth <- res$query_depth
    closest     <- df[1, ]

    finite_distances <- df$distance[is.finite(df$distance)]
    max_dist <- if (length(finite_distances) > 0L) max(finite_distances) else 1
    if (!is.finite(max_dist) || max_dist <= 0) max_dist <- 1

    any_shallow <- any(!is.na(df$depth) & df$depth < SHALLOW_LINEAGE_GUIDE)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row      <- df[i, ]
      marker_pct <- if (is.na(row$distance)) 0L else if (is.infinite(row$distance)) 100L else
        round(100 * row$distance / max_dist)
      marker_pct <- min(100L, max(0L, marker_pct))
      is_shallow <- !is.na(row$depth) && row$depth < SHALLOW_LINEAGE_GUIDE
      depth_lbl  <- if (is.na(row$depth)) "?" else as.character(row$depth)
      warn_icon  <- if (is_shallow) " ⚠" else ""

      tags$tr(
        class = if (is_shallow) "shallow-lineage-row" else NULL,
        tags$td(
          style = "font-style:italic; padding:0.4rem 0.6rem;",
          row$taxon, tags$span(class = "warning-icon", warn_icon)
        ),
        tags$td(
          style = "padding:0.4rem 0.6rem; font-family:'DM Mono',monospace; font-size:0.82rem;",
          if (is.na(row$distance)) "NA" else if (is.infinite(row$distance)) "∞" else round(row$distance, 6)
        ),
        tags$td(
          class = "depth-value",
          style = "padding:0.4rem 0.6rem; font-family:'DM Mono',monospace; font-size:0.78rem;",
          depth_lbl
        ),
        tags$td(style = "padding:0.4rem 0.9rem; width:35%;",
                div(
                  class = "distance-track",
                  title = tr()("Left is closer; right is more distant."),
                  div(
                    class = paste("distance-marker", if (i == 1L) "closest" else "candidate"),
                    style = sprintf("left:%d%%;", marker_pct)
                  )
                )
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
          class = "data-quality-notice",
          tags$b(tr()("⚠ Data quality notice: ")),
          tr()("One or more taxa have relatively few hierarchy nodes in The Taxonomicon. Their distances may therefore be coarser and should be interpreted with attention to source resolution.")
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
                tags$th(style="padding:0.4rem 0.6rem;", tr()("Relative separation"))
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
    tryCatch(
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
      txt <- if (isTRUE(mem)) tr()("%s IS a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      else tr()("%s is NOT a member of %s", trimws(input$le_taxon), trimws(input$le_clade_check))
      div(class = paste("membership-result", if (isTRUE(mem)) "member" else "non-member"), txt)
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

    tryCatch(
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
                formatStyle('id', fontWeight = 'bold', color = '#5c3d1e')
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
    taxa <- tryCatch(
      read_taxa_csv(input$cc_upload$datapath),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        character(0)
      }
    )
    if (length(taxa) > 0L) {
      updateTextAreaInput(session, "cc_taxa", value = paste(taxa, collapse = "\n"))
      showNotification(tr()("Loaded %d unique taxa from CSV.", length(taxa)), type = "message")
    }
  })

  cc_result <- eventReactive(input$cc_run, {
    taxa <- parse_taxa_input(input$cc_taxa)
    req(length(taxa) >= 1)
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    tryCatch(
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
                   div(class = "result-distance coverage-found-count", n_found)
            ),
            column(6,
                   div(class = "result-label", tr()("Not found")),
                   div(class = "result-distance coverage-missing-count", n_notfound)
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
      kept  = tryCatch(
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
