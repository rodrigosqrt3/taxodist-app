library(shiny)
library(bslib)
library(taxodist)
library(ggplot2)
library(ggrepel)
library(DT)
library(shiny.i18n)

# Load the bundled lineage cache only when it was built (by build_cache.R)
# with the installed taxodist version, so lineages parsed by an older version
# are never served.
local({
  info_file <- "lineage-cache.dcf"
  cache_file <- "lineage-cache.rds"
  if (!file.exists(info_file) || !file.exists(cache_file)) return()
  info <- read.dcf(info_file)
  if (!identical(unname(info[1, "taxodist_version"]), as.character(packageVersion("taxodist")))) {
    message("Ignoring lineage cache built with taxodist ", info[1, "taxodist_version"],
            "; run build_cache.R to rebuild it.")
    return()
  }
  tryCatch(
    suppressMessages(load_cache(cache_file)),
    error = function(e) warning("Could not load the bundled lineage cache: ", conditionMessage(e))
  )
})

MAX_MATRIX_TAXA <- 50L
MAX_GROUP_TAXA <- 30L
SHALLOW_LINEAGE_GUIDE <- 25L
NODE_LABEL_LIMIT <- 25L

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

# Run a taxodist call and return list(value, warnings, error). Warnings (for
# example the choice made for an ambiguous name) and errors are collected so
# they can be shown to the user instead of being lost; cli messages are muted.
run_query <- function(expr) {
  warnings <- character(0)
  error <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(value = value, warnings = unique(warnings), error = error)
}

# Lineages are cached by taxodist, so repeated lookups are cheap.
safe_lineage <- function(taxon) {
  tryCatch(suppressMessages(suppressWarnings(get_lineage(taxon))), error = function(e) NULL)
}

format_distance <- function(x, digits = 6) {
  ifelse(is.na(x), "NA", ifelse(is.infinite(x), "∞", format(round(x, digits), scientific = FALSE)))
}

# Coordinates for drawing a TaxodistTree / phylo object: tips at distance 0,
# each internal node at the distance separating the taxa that diverge there.
tree_layout <- function(tree) {
  n_tips <- length(tree$tip.label)
  n_nodes <- n_tips + tree$Nnode
  height <- numeric(n_nodes)
  for (row in rev(seq_len(nrow(tree$edge)))) {
    parent <- tree$edge[row, 1]
    child <- tree$edge[row, 2]
    height[parent] <- max(height[parent], height[child] + tree$edge.length[row])
  }
  y <- numeric(n_nodes)
  y[seq_len(n_tips)] <- seq_len(n_tips)
  for (row in rev(seq_len(nrow(tree$edge)))) {
    parent <- tree$edge[row, 1]
    children <- tree$edge[tree$edge[, 1] == parent, 2]
    y[parent] <- mean(range(y[children]))
  }
  x <- 2 * height
  edges <- data.frame(
    x = x[tree$edge[, 2]], xend = x[tree$edge[, 1]],
    y = y[tree$edge[, 2]], yend = y[tree$edge[, 2]]
  )
  internal <- (n_tips + 1L):n_nodes
  verticals <- do.call(rbind, lapply(internal, function(node) {
    children <- tree$edge[tree$edge[, 1] == node, 2]
    data.frame(x = x[node], xend = x[node], y = min(y[children]), yend = max(y[children]))
  }))
  list(
    segments = rbind(edges, verticals),
    tips = data.frame(x = 0, y = y[seq_len(n_tips)], label = tree$tip.label),
    nodes = data.frame(x = x[internal], y = y[internal], label = tree$node.label),
    max_x = max(x)
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

    /* ── Interactive nodes, links, and notices ── */
    .lineage-node[data-node] { cursor: pointer; }
    .lineage-node[data-node]:hover {
      outline: 2px solid var(--tip);
      outline-offset: 1px;
    }
    .example-link {
      color: var(--branch);
      cursor: pointer;
      font-style: italic;
      text-decoration: underline dotted;
      white-space: nowrap;
    }
    .example-link:hover { color: var(--border); }
    .cov-unknown { background: var(--notice-bg); color: var(--notice-text); border: 1px dashed var(--branch); }
    .coverage-unknown-count { color: var(--notice-text); }
    .search-id { font-weight: 700; color: var(--border); }
    .result-actions {
      display: flex;
      flex-wrap: wrap;
      gap: 0.5rem;
      margin-top: 0.8rem;
    }
    .result-actions .btn { font-size: 0.8rem; }

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

taxa_upload_input <- function(id) {
  fileInput(id, i18n$t("Or upload CSV (one taxon per row)"),
            accept = ".csv", width = "100%",
            buttonLabel = gsub("<.*?>", "", i18n$t("Browse...")),
            placeholder = gsub("<.*?>", "", i18n$t("No file selected")))
}

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

    /* Clicking a lineage node explores that clade. */
    $(document).on('click', '.lineage-node[data-node]', function() {
      Shiny.setInputValue('explore_node', $(this).attr('data-node'), {priority: 'event'});
    });

    /* Clicking an example pair fills and runs the pairwise comparison. */
    $(document).on('click', '.example-link[data-a]', function(event) {
      event.preventDefault();
      Shiny.setInputValue('example_pair',
        {a: $(this).attr('data-a'), b: $(this).attr('data-b')}, {priority: 'event'});
    });

    Shiny.addCustomMessageHandler('copy_text', function(msg) {
      if (navigator.clipboard) { navigator.clipboard.writeText(msg.text); }
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
    nav_panel(i18n$t("Pairwise Distance"), value = "pairwise",
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
                                 tags$small(
                                   lapply(seq_along(example_pairs), function(i) {
                                     pair <- example_pairs[[i]]
                                     tagList(
                                       if (i > 1) " • " else NULL,
                                       tags$a(class = "example-link", href = "#",
                                              `data-a` = pair[1], `data-b` = pair[2],
                                              paste(pair, collapse = " / "))
                                     )
                                   })
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
    nav_panel(i18n$t("Distance Matrix"), value = "matrix",
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
                                 taxa_upload_input("dm_upload"),
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

    # ── Tab 3: Between Groups ─────────────────────────────────────────────────
    nav_panel(i18n$t("Between Groups"), value = "groups",
              div(class = "container-fluid py-4",
                  fluidRow(
                    column(4,
                           card(
                             card_header(i18n$t("Two groups of taxa")),
                             div(class = "p-3",
                                 textAreaInput("bg_rows", i18n$t("Group 1 (rows)"), rows = 5,
                                               placeholder = "Tyrannosaurus\nTriceratops\nHomo"),
                                 textAreaInput("bg_cols", i18n$t("Group 2 (columns)"), rows = 5,
                                               placeholder = "Velociraptor\nGallus\nQuercus"),
                                 div(class = "d-flex gap-2 mt-2",
                                     actionButton("bg_run", i18n$t("Compare Groups"),
                                                  class = "btn btn-primary flex-grow-1", icon = icon("table-cells")),
                                     actionButton("bg_example", i18n$t("Shuffle"),
                                                  class = "btn btn-outline-secondary", icon = icon("shuffle"))
                                 ),
                                 hr(class = "section-divider"),
                                 div(class = "text-muted fst-italic small",
                                     i18n$t("Computes only the distances from every taxon in group 1 to every taxon in group 2, for example from community members to reference taxa."))
                             )
                           )
                    ),
                    column(8,
                           uiOutput("bg_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 4: Closest Relative ───────────────────────────────────────────────
    nav_panel(i18n$t("Closest Relative"), value = "closest",
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

    # ── Tab 5: Lineage Explorer ───────────────────────────────────────────────
    nav_panel(i18n$t("Lineage Explorer"), value = "lineage",
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
                                              class = "btn btn-outline-secondary w-100 mt-1"),
                                 hr(class = "section-divider"),
                                 div(class = "text-muted fst-italic small",
                                     i18n$t("Tip: click any node of a lineage to explore that clade."))
                             )
                           )
                    ),
                    column(8,
                           uiOutput("le_result_ui")
                    )
                  )
              )
    ),

    # ── Tab 6: Search Database ─────────────────────────────────────────────
    nav_panel(i18n$t("Search Database"), value = "search",
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

    # ── Tab 7: Coverage Check ─────────────────────────────────────────────────
    nav_panel(i18n$t("Coverage Check"), value = "coverage",
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
                                 taxa_upload_input("cc_upload"),
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

    # ── Tab 8: Filter by Clade ────────────────────────────────────────────────
    nav_panel(i18n$t("Filter by Clade"), value = "filter",
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

    updateTextInput(session, "pd_taxon_a", label = txt("Taxon A"), placeholder = clean_txt("e.g. Tyrannosaurus"))
    updateTextInput(session, "pd_taxon_b", label = txt("Taxon B"), placeholder = clean_txt("e.g. Velociraptor"))
    updateActionButton(session, "pd_run", label = txt("Compute Distance"))
    updateActionButton(session, "pd_example", label = txt("Shuffle"))

    updateTextAreaInput(session, "dm_taxa", label = txt("Enter taxa (one per line or comma-separated)"))
    updateActionButton(session, "dm_run", label = txt("Build Matrix"))
    updateActionButton(session, "dm_example", label = txt("Shuffle"))

    updateTextAreaInput(session, "bg_rows", label = txt("Group 1 (rows)"))
    updateTextAreaInput(session, "bg_cols", label = txt("Group 2 (columns)"))
    updateActionButton(session, "bg_run", label = txt("Compare Groups"))
    updateActionButton(session, "bg_example", label = txt("Shuffle"))

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

  with_loading <- function(expr) {
    session$sendCustomMessage("show_loading", list())
    on.exit(session$sendCustomMessage("hide_loading", list()))
    expr
  }

  # Show collected warnings (e.g. which entry was used for a homonym) and errors.
  report <- function(query) {
    if (!is.null(query$error)) {
      showNotification(query$error, type = "error", duration = 10)
    }
    for (warning_text in query$warnings) {
      showNotification(
        tagList(tags$b(tr()("Note from The Taxonomicon:")), tags$br(),
                tags$span(style = "white-space: pre-line;", warning_text)),
        type = "warning", duration = 15
      )
    }
    invisible(query)
  }

  placeholder <- function(text = tr()("Results will appear here.")) {
    div(class = "p-3 text-muted fst-italic", text)
  }

  not_found_notice <- function(taxa) {
    div(
      class = "data-quality-notice",
      tags$b(tr()("Not found: ")),
      paste(taxa, collapse = ", "),
      tags$br(),
      tr()("These names were not found in The Taxonomicon, or the server could not be reached. Check the spelling, try the Search Database tab, or try again later.")
    )
  }

  # Clickable lineage rendering shared by the pairwise and lineage tabs.
  lineage_nodes <- function(lineage, classes = rep("lineage-node", length(lineage))) {
    lapply(seq_along(lineage), function(i) {
      tagList(
        if (i > 1) span(class = "lineage-arrow", "›") else NULL,
        span(class = classes[i], `data-node` = lineage[i],
             title = tr()("Explore %s", lineage[i]), lineage[i])
      )
    })
  }

  # ── Share links ─────────────────────────────────────────────────────────────

  app_url <- function(params) {
    cd <- session$clientData
    port <- if (nzchar(cd$url_port) && !cd$url_port %in% c("80", "443")) paste0(":", cd$url_port) else ""
    query <- paste(
      vapply(names(params), function(name) {
        paste0(name, "=", utils::URLencode(params[[name]], reserved = TRUE))
      }, character(1)),
      collapse = "&"
    )
    paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname, "?", query)
  }

  share_button <- function(id) {
    actionButton(id, tr()("Share link"), icon = icon("link"),
                 class = "btn btn-outline-secondary btn-sm")
  }

  show_share_modal <- function(params) {
    params$lang <- input$selected_language
    url <- app_url(params)
    showModal(modalDialog(
      title = tr()("Share this result"),
      tags$p(tr()("Anyone opening this link will see the same query:")),
      tags$input(type = "text", class = "form-control", value = url, readonly = NA,
                 onclick = "this.select();"),
      footer = tagList(
        actionButton("share_copy", tr()("Copy"), icon = icon("copy"), class = "btn btn-primary"),
        modalButton(tr()("Close"))
      ),
      easyClose = TRUE
    ))
    share_text(url)
  }
  share_text <- reactiveVal(NULL)
  observeEvent(input$share_copy, {
    session$sendCustomMessage("copy_text", list(text = share_text()))
    showNotification(tr()("Link copied."), type = "message", duration = 3)
  })

  # ── Dynamic UI Elements ─────────────────────────────────────────────────────
  output$app_subtitle <- renderText({
    tr()("Taxonomic Distance & Hierarchy Explorer — powered by The Taxonomicon")
  })

  output$method_note <- renderText({
    tr()("Distances represent depth in a taxonomic classification, not evolutionary time or phylogenetic branch length.")
  })

  output$dm_template_ui <- renderUI({
    downloadButton("dm_template", tr()("Download CSV template"),
                   class = "btn btn-outline-secondary btn-sm w-100")
  })

  output$cc_template_ui <- renderUI({
    downloadButton("cc_template", tr()("Download CSV template"),
                   class = "btn btn-outline-secondary btn-sm w-100")
  })

  plot_palette <- reactive({
    if (identical(input$dark_mode_toggle, "dark")) {
      list(background = "#26211c", ink = "#f4f1ea", muted = "#b0a290",
           rule = "#3d362e", branch = "#c2946d", point = "#e5c19d")
    } else {
      list(background = "#f3ede0", ink = "#3d2510", muted = "#6b4c30",
           rule = "#d9c9b0", branch = "#8b5e3c", point = "#5c3d1e")
    }
  })

  template_handler <- function(filename) {
    downloadHandler(
      filename = function() filename,
      content = function(file) {
        write.csv(data.frame(taxon = example_taxa()), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  }
  output$dm_template <- template_handler("taxodist-taxa-template.csv")
  output$cc_template <- template_handler("taxodist-coverage-template.csv")

  csv_handler <- function(filename, data) {
    downloadHandler(
      filename = function() filename,
      content = function(file) write.csv(data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
  }

  upload_into <- function(upload_id, textarea_id) {
    observeEvent(input[[upload_id]], {
      req(input[[upload_id]])
      taxa <- tryCatch(
        read_taxa_csv(input[[upload_id]]$datapath),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          character(0)
        }
      )
      if (length(taxa) > 0L) {
        updateTextAreaInput(session, textarea_id, value = paste(taxa, collapse = "\n"))
        showNotification(tr()("Loaded %d unique taxa from CSV.", length(taxa)), type = "message")
      }
    })
  }
  upload_into("dm_upload", "dm_taxa")
  upload_into("cc_upload", "cc_taxa")

  # ── Pairwise Distance ───────────────────────────────────────────────────────

  pd_query <- reactiveVal(NULL)

  run_pairwise <- function(a, b) {
    a <- trimws(a)
    b <- trimws(b)
    if (!nzchar(a) || !nzchar(b)) return()
    updateTextInput(session, "pd_taxon_a", value = a)
    updateTextInput(session, "pd_taxon_b", value = b)
    pd_query(list(a = a, b = b, run = Sys.time()))
  }

  observeEvent(input$pd_run, run_pairwise(input$pd_taxon_a, input$pd_taxon_b))
  observeEvent(input$example_pair, {
    nav_select("main_tabs", "pairwise")
    run_pairwise(input$example_pair$a, input$example_pair$b)
  })
  observeEvent(input$pd_example, {
    pair <- sample(shuffle_pool, 2)
    updateTextInput(session, "pd_taxon_a", value = pair[1])
    updateTextInput(session, "pd_taxon_b", value = pair[2])
  })

  pd_result <- reactive({
    query <- pd_query()
    req(query)
    with_loading({
      result <- report(run_query({
        lin_a <- get_lineage(query$a)
        lin_b <- get_lineage(query$b)
        missing <- c(if (is.null(lin_a)) query$a, if (is.null(lin_b)) query$b)
        list(
          a = query$a, b = query$b, lin_a = lin_a, lin_b = lin_b, missing = missing,
          distance = if (length(missing) == 0L) taxo_distance(query$a, query$b) else NULL
        )
      }))
      result$value
    })
  })

  output$pd_result_ui <- renderUI({
    res <- pd_result()
    if (is.null(res)) return(placeholder())
    if (length(res$missing) > 0L) return(not_found_notice(res$missing))
    d <- res$distance

    dist_fmt <- if (is.infinite(d$distance)) "∞" else
      if (d$distance == 0) tr()("0 (identical)") else round(d$distance, 6)
    interp <- if (is.infinite(d$distance)) tr()("No common ancestor found.")
    else if (d$distance == 0) tr()("The two inputs resolve to the same hierarchy node.")
    else tr()("MRCA at depth %d; distance = 1 / %d.", d$mrca_depth, d$mrca_depth)

    div(class = "result-box mb-3",
        fluidRow(
          column(4,
                 div(class = "result-label", tr()("Distance")),
                 div(class = "result-distance", dist_fmt)
          ),
          column(8,
                 div(class = "result-label", tr()("Most Recent Common Ancestor")),
                 div(class = "result-mrca", if (is.na(d$mrca)) "—" else d$mrca),
                 div(class = "result-meta mt-1",
                     tr()("MRCA depth: %d | Depth %s: %d | Depth %s: %d",
                          d$mrca_depth, res$a, d$depth_a, res$b, d$depth_b)
                 ),
                 div(class = "result-meta mt-1 fst-italic", interp),
                 div(class = "result-actions", share_button("pd_share"))
          )
        )
    )
  })

  observeEvent(input$pd_share, {
    res <- pd_result()
    req(res)
    show_share_modal(list(tab = "pairwise", a = res$a, b = res$b))
  })

  output$pd_lineage_ui <- renderUI({
    res <- pd_result()
    if (is.null(res) || length(res$missing) > 0L) return(NULL)
    mrca_d <- res$distance$mrca_depth

    render_lineage <- function(lin, label) {
      classes <- ifelse(seq_along(lin) == mrca_d, "lineage-node mrca",
                        ifelse(seq_along(lin) < mrca_d, "lineage-node shared", "lineage-node"))
      tagList(
        div(class = "result-label mb-1", label),
        div(style = "line-height: 2.2;", lineage_nodes(lin, classes))
      )
    }

    card(
      card_header(tr()("Lineage Comparison")),
      div(class = "p-3",
          div(class = "mb-2",
              span(class = "lineage-node shared", "■"), tr()(" Shared trunk  "),
              span(class = "lineage-node mrca", "■"), tr()(" MRCA  "),
              span(class = "lineage-node", "■"), tr()(" Unique")
          ),
          fluidRow(
            column(6, render_lineage(res$lin_a, res$a)),
            column(6, render_lineage(res$lin_b, res$b))
          )
      )
    )
  })

  # ── Distance Matrix ─────────────────────────────────────────────────────────

  dm_query <- reactiveVal(NULL)

  observeEvent(input$dm_example, {
    taxa <- sample(shuffle_pool, sample(5:7, 1))
    updateTextAreaInput(session, "dm_taxa", value = paste(taxa, collapse = "\n"))
  })

  observeEvent(input$dm_run, {
    taxa <- parse_taxa_input(input$dm_taxa)
    if (length(taxa) < 2L) {
      showNotification(tr()("Enter at least two taxa."), type = "warning")
      return()
    }
    if (length(taxa) > MAX_MATRIX_TAXA) {
      showNotification(tr()("Distance matrices are limited to %d taxa in the web app.", MAX_MATRIX_TAXA),
                       type = "warning", duration = 8)
      return()
    }
    dm_query(list(taxa = taxa, run = Sys.time()))
  })

  dm_result <- reactive({
    query <- dm_query()
    req(query)
    with_loading({
      result <- report(run_query({
        matrix <- distance_matrix(query$taxa, verbose = FALSE, progress = FALSE)
        found <- !vapply(query$taxa, function(t) is.null(safe_lineage(t)), logical(1))
        complete <- all(found) && all(is.finite(matrix))
        tree <- if (complete) {
          tryCatch(suppressWarnings(taxo_tree(query$taxa, progress = FALSE)), error = function(e) NULL)
        }
        list(taxa = query$taxa, matrix = matrix, missing = query$taxa[!found],
             complete = complete, tree = tree)
      }))
      res <- result$value
      if (!is.null(res) && !res$complete && length(res$missing) == 0L) {
        showNotification(
          tr()("Some distances are missing or infinite. The table is available, but clustering and ordination require a complete finite matrix."),
          type = "warning", duration = 10
        )
      }
      res
    })
  })

  output$dm_result_ui <- renderUI({
    res <- dm_result()
    if (is.null(res)) return(placeholder())
    ht <- max(350, length(res$taxa) * 30)

    tagList(
      if (length(res$missing) > 0L) not_found_notice(res$missing),
      card(
        card_header(tr()("Distance Matrix")),
        div(class = "p-2", DTOutput("dm_table")),
        div(class = "px-3 pb-3 result-actions",
            downloadButton("dm_download_csv", tr()("Matrix (CSV)"), class = "btn btn-outline-secondary btn-sm"),
            if (!is.null(res$tree)) downloadButton("dm_download_newick", tr()("Tree (Newick)"), class = "btn btn-outline-secondary btn-sm"),
            downloadButton("dm_download_bundle", tr()("Analysis bundle (JSON)"), class = "btn btn-outline-secondary btn-sm"),
            share_button("dm_share")
        )
      ),
      fluidRow(
        class = "mt-3",
        column(6,
               card(
                 card_header(tr()("Classification tree")),
                 div(class = "p-2", plotOutput("dm_tree", height = paste0(ht, "px"))),
                 if (!is.null(res$tree)) div(class = "px-3 pb-3",
                     downloadButton("dm_download_tree_png", tr()("Download PNG"), class = "btn btn-outline-secondary btn-sm"))
               )
        ),
        column(6,
               card(
                 card_header(tr()("Ordination (PCoA)")),
                 div(class = "p-2", plotOutput("dm_pcoa", height = paste0(ht, "px"))),
                 if (res$complete) div(class = "px-3 pb-3",
                     downloadButton("dm_download_pcoa_png", tr()("Download PNG"), class = "btn btn-outline-secondary btn-sm"))
               )
        )
      )
    )
  })

  observeEvent(input$dm_share, {
    res <- dm_result()
    req(res)
    show_share_modal(list(tab = "matrix", taxa = paste(res$taxa, collapse = ",")))
  })

  output$dm_table <- renderDT({
    res <- dm_result()
    req(res)
    pal <- plot_palette()
    mat <- as.matrix(res$matrix)
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
        formatStyle(columns = colnames(mat_fmt),
                    background = styleInterval(cuts, table_colors),
                    color = pal$ink)
    }
    table
  })

  tree_plot <- function(tree, pal) {
    layout <- tree_layout(tree)
    n <- nrow(layout$tips)
    max_x <- if (layout$max_x > 0) layout$max_x else 1
    label_space <- max(nchar(layout$tips$label)) * max_x * 0.022
    nodes <- layout$nodes[nzchar(layout$nodes$label), , drop = FALSE]
    if (n > NODE_LABEL_LIMIT) nodes <- nodes[0, , drop = FALSE]

    ggplot() +
      geom_segment(data = layout$segments, aes(x = x, xend = xend, y = y, yend = yend),
                   colour = pal$point, linewidth = 1) +
      geom_text(data = layout$tips, aes(x = x, y = y, label = label),
                hjust = 0, nudge_x = -max_x * 0.015, size = max(3, 5 - n * 0.05),
                family = "serif", fontface = "italic", colour = pal$ink) +
      geom_point(data = nodes, aes(x = x, y = y),
                 colour = pal$branch, fill = pal$background, shape = 21, size = 3, stroke = 1.1) +
      geom_text(data = nodes, aes(x = x, y = y, label = label),
                vjust = -0.8, size = 3.6, family = "serif", fontface = "italic", colour = pal$branch) +
      scale_x_reverse(limits = c(max_x * 1.05, -label_space)) +
      scale_y_continuous(expand = expansion(mult = 0.06)) +
      labs(x = tr()("Taxonomic hierarchy distance"), y = NULL) +
      theme_minimal(base_family = "serif") +
      theme(
        axis.text.x      = element_text(color = pal$muted, size = 8),
        axis.text.y      = element_blank(),
        axis.title.x     = element_text(color = pal$muted, size = 9),
        panel.grid       = element_blank(),
        plot.background  = element_rect(fill = pal$background, colour = NA),
        panel.background = element_rect(fill = pal$background, colour = NA),
        plot.margin      = margin(10, 10, 10, 10)
      )
  }

  pcoa_plot <- function(matrix, pal) {
    ord <- taxo_ordinate(matrix, k = 2)
    df <- as.data.frame(ord$points)
    if (ncol(df) == 1L) df$V2 <- 0
    colnames(df) <- c("PC1", "PC2")
    df$Taxon <- rownames(df)

    gof_pct <- round(ord$GOF[1] * 100, 1)
    pcoa_subtitle <- tr()("Goodness-of-fit: %s%%", gof_pct)
    if (is.finite(gof_pct) && gof_pct < 50) {
      pcoa_subtitle <- paste(pcoa_subtitle, tr()("Limited two-dimensional representation."), sep = " — ")
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
      geom_text_repel(family = "serif", fontface = "italic", color = pal$ink, size = 4.2,
                      box.padding = 0.5, point.padding = 0.3,
                      segment.color = pal$rule, segment.alpha = 0.6) +
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
  }

  output$dm_tree <- renderPlot({
    res <- dm_result()
    req(res)
    validate(need(res$complete, tr()("Clustering requires a complete matrix with finite distances.")))
    validate(need(!is.null(res$tree), tr()("The classification tree could not be built.")))
    tree_plot(res$tree, plot_palette())
  }, bg = "transparent")

  output$dm_pcoa <- renderPlot({
    res <- dm_result()
    req(res)
    validate(need(res$complete, tr()("Ordination requires a complete matrix with finite distances.")))
    pcoa_plot(res$matrix, plot_palette())
  }, bg = "transparent")

  png_handler <- function(filename, build) {
    downloadHandler(
      filename = function() filename,
      content = function(file) {
        ggsave(file, plot = build(), width = 9, height = max(5, length(dm_result()$taxa) * 0.3),
               dpi = 200, bg = plot_palette()$background)
      }
    )
  }
  output$dm_download_tree_png <- png_handler("taxodist-tree.png",
                                             function() tree_plot(dm_result()$tree, plot_palette()))
  output$dm_download_pcoa_png <- png_handler("taxodist-pcoa.png",
                                             function() pcoa_plot(dm_result()$matrix, plot_palette()))

  output$dm_download_csv <- downloadHandler(
    filename = function() "taxodist-distance-matrix.csv",
    content = function(file) {
      mat <- as.matrix(dm_result()$matrix)
      write.csv(data.frame(taxon = rownames(mat), mat, check.names = FALSE),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dm_download_newick <- downloadHandler(
    filename = function() "taxodist-tree.nwk",
    content = function(file) {
      tree <- dm_result()$tree
      writeLines(ape::write.tree(tree), file)
    }
  )

  output$dm_download_bundle <- downloadHandler(
    filename = function() "taxodist-analysis.json",
    content = function(file) {
      with_loading({
        bundle <- suppressWarnings(taxo_bundle(dm_result()$taxa, ambiguity = "first", progress = FALSE))
        write_taxodist_bundle(bundle, file)
      })
    }
  )

  # ── Between Groups ──────────────────────────────────────────────────────────

  observeEvent(input$bg_example, {
    picked <- sample(shuffle_pool, 7)
    updateTextAreaInput(session, "bg_rows", value = paste(picked[1:3], collapse = "\n"))
    updateTextAreaInput(session, "bg_cols", value = paste(picked[4:7], collapse = "\n"))
  })

  bg_result <- eventReactive(input$bg_run, {
    rows <- parse_taxa_input(input$bg_rows)
    cols <- parse_taxa_input(input$bg_cols)
    if (length(rows) == 0L || length(cols) == 0L) {
      showNotification(tr()("Enter at least one taxon in each group."), type = "warning")
      return(NULL)
    }
    if (length(rows) + length(cols) > MAX_GROUP_TAXA) {
      showNotification(tr()("Group comparisons are limited to %d taxa in total in the web app.", MAX_GROUP_TAXA),
                       type = "warning", duration = 8)
      return(NULL)
    }
    with_loading({
      result <- report(run_query({
        all_taxa <- unique(c(rows, cols))
        found <- !vapply(all_taxa, function(t) is.null(safe_lineage(t)), logical(1))
        list(matrix = cross_distances(rows, cols, progress = FALSE), missing = all_taxa[!found])
      }))
      result$value
    })
  })

  output$bg_result_ui <- renderUI({
    res <- bg_result()
    if (is.null(res)) return(placeholder())
    tagList(
      if (length(res$missing) > 0L) not_found_notice(res$missing),
      card(
        card_header(tr()("Distances between groups")),
        div(class = "p-2", DTOutput("bg_table")),
        div(class = "px-3 pb-3 result-actions",
            downloadButton("bg_download_csv", tr()("Matrix (CSV)"), class = "btn btn-outline-secondary btn-sm"))
      )
    )
  })

  output$bg_table <- renderDT({
    res <- bg_result()
    req(res)
    mat <- res$matrix
    display <- matrix(format_distance(mat, 5), nrow = nrow(mat), dimnames = dimnames(mat))
    datatable(display, options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
              class = "compact hover", selection = "none")
  })

  output$bg_download_csv <- downloadHandler(
    filename = function() "taxodist-group-distances.csv",
    content = function(file) {
      mat <- bg_result()$matrix
      write.csv(data.frame(taxon = rownames(mat), mat, check.names = FALSE),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Closest Relative ────────────────────────────────────────────────────────

  observeEvent(input$cr_example, {
    picked <- sample(shuffle_pool, sample(5:6, 1))
    updateTextInput(session, "cr_query", value = picked[1])
    updateTextAreaInput(session, "cr_candidates", value = paste(picked[-1], collapse = "\n"))
  })

  cr_result <- eventReactive(input$cr_run, {
    query <- trimws(input$cr_query)
    candidates <- parse_taxa_input(input$cr_candidates)
    if (!nzchar(query) || length(candidates) == 0L) {
      showNotification(tr()("Enter a query taxon and at least one candidate."), type = "warning")
      return(NULL)
    }
    with_loading({
      result <- report(run_query({
        query_lin <- get_lineage(query)
        if (is.null(query_lin)) {
          list(query = query, missing = query)
        } else {
          ranking <- closest_relative(query, candidates, verbose = FALSE)
          depths <- vapply(ranking$taxon, function(t) {
            lin <- safe_lineage(t)
            if (is.null(lin)) NA_integer_ else length(lin)
          }, integer(1))
          ranking$depth <- unname(depths)
          list(query = query, query_depth = length(query_lin), df = ranking,
               missing = ranking$taxon[is.na(ranking$distance)])
        }
      }))
      result$value
    })
  })

  output$cr_result_ui <- renderUI({
    res <- cr_result()
    if (is.null(res)) return(placeholder())
    if (is.null(res$df)) return(not_found_notice(res$missing))

    df <- res$df
    found <- df[!is.na(df$distance), , drop = FALSE]
    if (nrow(found) == 0L) return(not_found_notice(res$missing))
    closest <- found[1, ]

    finite_distances <- df$distance[is.finite(df$distance)]
    max_dist <- if (length(finite_distances) > 0L) max(finite_distances) else 1
    if (!is.finite(max_dist) || max_dist <= 0) max_dist <- 1

    any_shallow <- any(!is.na(df$depth) & df$depth < SHALLOW_LINEAGE_GUIDE)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      marker_pct <- if (is.na(row$distance)) 0L else if (is.infinite(row$distance)) 100L else
        round(100 * row$distance / max_dist)
      marker_pct <- min(100L, max(0L, marker_pct))
      is_shallow <- !is.na(row$depth) && row$depth < SHALLOW_LINEAGE_GUIDE
      depth_lbl <- if (is.na(row$depth)) "?" else as.character(row$depth)

      tags$tr(
        class = if (is_shallow) "shallow-lineage-row" else NULL,
        tags$td(style = "font-style:italic; padding:0.4rem 0.6rem;",
                row$taxon, tags$span(class = "warning-icon", if (is_shallow) " ⚠" else "")),
        tags$td(style = "padding:0.4rem 0.6rem; font-family:'DM Mono',monospace; font-size:0.82rem;",
                format_distance(row$distance)),
        tags$td(class = "depth-value",
                style = "padding:0.4rem 0.6rem; font-family:'DM Mono',monospace; font-size:0.78rem;",
                depth_lbl),
        tags$td(style = "padding:0.4rem 0.9rem; width:35%;",
                if (!is.na(row$distance)) div(
                  class = "distance-track",
                  title = tr()("Left is closer; right is more distant."),
                  div(class = paste("distance-marker", if (i == 1L) "closest" else "candidate"),
                      style = sprintf("left:%d%%;", marker_pct))
                ))
      )
    })

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", tr()("Closest relative to")),
          div(style = "font-family:'Playfair Display',serif; font-size:1.3rem; font-style:italic;", res$query),
          div(class = "result-meta", tr()("lineage depth: %d", res$query_depth)),
          div(class = "result-label mt-2", tr()("is")),
          div(class = "result-mrca", closest$taxon),
          div(class = "result-meta",
              tr()("distance = %s", format_distance(closest$distance)),
              if (!is.na(closest$depth)) tr()(" | lineage depth: %d", closest$depth) else "")
      ),
      if (length(res$missing) > 0L) not_found_notice(res$missing),
      if (any_shallow)
        div(class = "data-quality-notice",
            tags$b(tr()("⚠ Data quality notice: ")),
            tr()("One or more taxa have relatively few hierarchy nodes in The Taxonomicon. Their distances may therefore be coarser and should be interpreted with attention to source resolution.")),
      card(
        card_header(tr()("All candidates ranked")),
        div(class = "p-0",
            tags$table(
              class = "table table-sm mb-0",
              style = "font-size:0.85rem;",
              tags$thead(tags$tr(
                tags$th(style = "padding:0.4rem 0.6rem;", tr()("Taxon")),
                tags$th(style = "padding:0.4rem 0.6rem;", tr()("Distance")),
                tags$th(style = "padding:0.4rem 0.6rem;", tr()("Depth")),
                tags$th(style = "padding:0.4rem 0.6rem;", tr()("Relative separation"))
              )),
              tags$tbody(rows)
            )
        ),
        div(class = "px-3 pb-3 result-actions",
            downloadButton("cr_download_csv", tr()("Ranking (CSV)"), class = "btn btn-outline-secondary btn-sm"))
      )
    )
  })

  output$cr_download_csv <- csv_handler("taxodist-closest-relatives.csv", function() {
    res <- cr_result()
    data.frame(query = res$query, res$df, check.names = FALSE)
  })

  # ── Lineage Explorer ────────────────────────────────────────────────────────

  le_query <- reactiveVal(NULL)
  le_member <- reactiveVal(NULL)

  explore <- function(taxon) {
    taxon <- trimws(taxon)
    if (!nzchar(taxon)) return()
    updateTextInput(session, "le_taxon", value = taxon)
    le_member(NULL)
    le_query(list(taxon = taxon, run = Sys.time()))
  }

  observeEvent(input$le_run, explore(input$le_taxon))
  observeEvent(input$explore_node, {
    nav_select("main_tabs", "lineage")
    explore(input$explore_node)
  })

  le_lineage <- reactive({
    query <- le_query()
    req(query)
    with_loading({
      result <- report(run_query(get_lineage(query$taxon, verbose = FALSE)))
      list(taxon = query$taxon, lineage = result$value)
    })
  })

  observeEvent(input$le_member_run, {
    res <- le_lineage()
    clade <- trimws(input$le_clade_check)
    req(res, !is.null(res$lineage), nzchar(clade))
    le_member(list(
      taxon = res$taxon,
      clade = clade,
      member = tolower(clade) %in% tolower(trimws(res$lineage))
    ))
  })

  output$le_result_ui <- renderUI({
    res <- le_lineage()
    if (is.null(res)) return(placeholder())
    if (is.null(res$lineage)) return(not_found_notice(res$taxon))
    lin <- res$lineage

    mem <- le_member()
    mem_ui <- if (!is.null(mem) && identical(mem$taxon, res$taxon)) {
      txt <- if (mem$member) tr()("%s IS a member of %s", mem$taxon, mem$clade)
      else tr()("%s is NOT a member of %s", mem$taxon, mem$clade)
      div(class = paste("membership-result", if (mem$member) "member" else "non-member"), txt)
    }

    card(
      card_header(tr()("Lineage of %s (%d nodes)", res$taxon, length(lin))),
      div(class = "p-3",
          div(style = "line-height:2.4;", lineage_nodes(lin)),
          mem_ui,
          div(class = "result-actions", share_button("le_share"))
      )
    )
  })

  observeEvent(input$le_share, {
    res <- le_lineage()
    req(res)
    show_share_modal(list(tab = "lineage", taxon = res$taxon))
  })

  # ── Search Database ─────────────────────────────────────────────────────────

  sd_result <- eventReactive(input$sd_run, {
    query <- trimws(input$sd_taxon)
    req(nzchar(query))
    with_loading({
      result <- report(run_query(taxo_search(query, verbose = FALSE)))
      list(query = query, results = result$value)
    })
  })

  output$sd_result_ui <- renderUI({
    res <- sd_result()
    if (is.null(res)) return(placeholder(tr()("Search results will appear here.")))
    if (is.null(res$results) || nrow(res$results) == 0L) {
      return(div(class = "data-quality-notice", tr()("No matches found for '%s'.", res$query)))
    }
    card(
      card_header(tr()("Search results for '%s'", res$query)),
      div(class = "p-2", DTOutput("sd_table")),
      div(class = "px-3 pb-3 text-muted fst-italic small",
          tr()("Select a row to explore the lineage of that entry."))
    )
  })

  output$sd_table <- renderDT({
    res <- sd_result()
    req(res, res$results)
    datatable(
      res$results,
      options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
      rownames = FALSE,
      selection = "single",
      colnames = c(tr()("Numeric ID"), tr()("Accepted Name / Rank"))
    ) |>
      formatStyle("id", fontWeight = "bold")
  })

  observeEvent(input$sd_table_rows_selected, {
    res <- sd_result()
    row <- input$sd_table_rows_selected
    req(res, res$results, length(row) == 1L)
    nav_select("main_tabs", "lineage")
    explore(res$results$id[[row]])
  })

  # ── Coverage Check ──────────────────────────────────────────────────────────

  observeEvent(input$cc_example, {
    taxa <- c(sample(shuffle_pool, 4), "Fakeosaurus", "Imaginarius")
    updateTextAreaInput(session, "cc_taxa", value = paste(sample(taxa), collapse = "\n"))
  })

  cc_result <- eventReactive(input$cc_run, {
    taxa <- parse_taxa_input(input$cc_taxa)
    req(length(taxa) >= 1)
    with_loading({
      result <- report(run_query(check_coverage(taxa, verbose = FALSE)))
      result$value
    })
  })

  output$cc_result_ui <- renderUI({
    res <- cc_result()
    if (is.null(res)) return(placeholder())

    # check_coverage() returns NA when the server could not be reached.
    n_found    <- sum(res %in% TRUE)
    n_notfound <- sum(res %in% FALSE)
    n_unknown  <- sum(is.na(res))

    pills <- lapply(names(res), function(nm) {
      value <- res[[nm]]
      cls <- if (is.na(value)) "cov-pill cov-unknown" else if (value) "cov-pill cov-found" else "cov-pill cov-notfound"
      span(class = cls, nm)
    })

    tagList(
      div(class = "result-box mb-3",
          fluidRow(
            column(if (n_unknown > 0) 4 else 6,
                   div(class = "result-label", tr()("Found in Taxonomicon")),
                   div(class = "result-distance coverage-found-count", n_found)),
            column(if (n_unknown > 0) 4 else 6,
                   div(class = "result-label", tr()("Not found")),
                   div(class = "result-distance coverage-missing-count", n_notfound)),
            if (n_unknown > 0) column(4,
                   div(class = "result-label", tr()("Could not be checked")),
                   div(class = "result-distance coverage-unknown-count", n_unknown))
          )
      ),
      card(
        card_header(tr()("Coverage by taxon")),
        div(class = "p-3", pills),
        div(class = "px-3 pb-3 result-actions",
            downloadButton("cc_download_csv", tr()("Coverage (CSV)"), class = "btn btn-outline-secondary btn-sm"))
      )
    )
  })

  output$cc_download_csv <- csv_handler("taxodist-coverage.csv", function() {
    res <- cc_result()
    data.frame(taxon = names(res), found = unname(res))
  })

  # ── Filter by Clade ─────────────────────────────────────────────────────────

  observeEvent(input$fc_example, {
    pick <- clade_examples[[sample(length(clade_examples), 1)]]
    updateTextAreaInput(session, "fc_taxa",  value = paste(pick$taxa,  collapse = "\n"))
    updateTextInput(session,    "fc_clade", value = pick$clade)
  })

  fc_result <- eventReactive(input$fc_run, {
    taxa  <- parse_taxa_input(input$fc_taxa)
    clade <- trimws(input$fc_clade)
    if (length(taxa) == 0L || !nzchar(clade)) {
      showNotification(tr()("Enter at least one taxon and a clade."), type = "warning")
      return(NULL)
    }
    with_loading({
      result <- report(run_query({
        membership <- vapply(taxa, function(t) {
          lineage <- get_lineage(t)
          if (is.null(lineage)) NA else tolower(clade) %in% tolower(trimws(lineage))
        }, logical(1))
        list(taxa = taxa, clade = clade, membership = membership)
      }))
      result$value
    })
  })

  output$fc_result_ui <- renderUI({
    res <- fc_result()
    if (is.null(res)) return(placeholder())

    kept     <- res$taxa[res$membership %in% TRUE]
    excluded <- res$taxa[res$membership %in% FALSE]
    missing  <- res$taxa[is.na(res$membership)]
    make_tags <- function(taxa) lapply(taxa, function(t) span(class = "taxon-tag", t))
    none <- div(class = "text-muted fst-italic small", tr()("none"))

    tagList(
      div(class = "result-box mb-3",
          div(class = "result-label", tr()("Clade filter")),
          div(style = "font-family:'Playfair Display',serif; font-size:1.2rem; font-style:italic;", res$clade),
          div(class = "result-meta mt-1",
              tr()("%d of %d taxa retained", length(kept), length(res$taxa)))
      ),
      if (length(missing) > 0L) not_found_notice(missing),
      card(
        card_header(tr()("Results")),
        div(class = "p-3",
            div(class = "result-label mb-1", tr()("In %s (%d)", res$clade, length(kept))),
            if (length(kept) > 0) div(make_tags(kept)) else none,
            hr(class = "section-divider"),
            div(class = "result-label mb-1", tr()("Not in %s (%d)", res$clade, length(excluded))),
            if (length(excluded) > 0) div(make_tags(excluded)) else none
        ),
        div(class = "px-3 pb-3 result-actions",
            downloadButton("fc_download_csv", tr()("Result (CSV)"), class = "btn btn-outline-secondary btn-sm"))
      )
    )
  })

  output$fc_download_csv <- csv_handler("taxodist-clade-filter.csv", function() {
    res <- fc_result()
    data.frame(taxon = res$taxa, clade = res$clade, member = unname(res$membership))
  })

  # ── Links that open a prepared query (?tab=...&a=...&b=...) ──────────────────

  observeEvent(session$clientData$url_search, once = TRUE, {
    params <- parseQueryString(session$clientData$url_search)
    if (length(params) == 0L) return()
    if (!is.null(params$lang) && params$lang %in% c("en", "pt", "es", "fr", "de")) {
      updateSelectInput(session, "selected_language", selected = params$lang)
    }
    tab <- params$tab
    if (identical(tab, "pairwise") && !is.null(params$a) && !is.null(params$b)) {
      nav_select("main_tabs", "pairwise")
      run_pairwise(params$a, params$b)
    } else if (identical(tab, "lineage") && !is.null(params$taxon)) {
      nav_select("main_tabs", "lineage")
      explore(params$taxon)
    } else if (identical(tab, "matrix") && !is.null(params$taxa)) {
      taxa <- parse_taxa_input(params$taxa)
      if (length(taxa) >= 2L && length(taxa) <= MAX_MATRIX_TAXA) {
        nav_select("main_tabs", "matrix")
        updateTextAreaInput(session, "dm_taxa", value = paste(taxa, collapse = "\n"))
        dm_query(list(taxa = taxa, run = Sys.time()))
      }
    }
  })
}

shinyApp(ui, server)
