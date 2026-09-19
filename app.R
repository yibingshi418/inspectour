# =============================================================================
#  Contour Inspector
#  Interactive inspection of normalised f0 trajectories for prosodic
#  categorisation in undescribed systems.
#
#  Run with:  shiny::runApp("app.R")
#  Requires:  shiny, ggplot2, plotly, dplyr, tidyr, readr, DT, scales
# =============================================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# ---- defaults matching your existing data --------------------------------
DEFAULTS <- list(
  time    = "time",
  f0      = "norm_f0",
  token   = "ind_no",                     # alone identifies one contour
  speaker = "speaker",
  colour  = "citation_no",
  hover   = c("speaker", "citation_tone", "citation_no", "token")
)

# ---- bundled sample dataset ---------------------------------------------
# Disyllabic tone-sandhi contours: 85 tokens, 8 speakers, 20 normalised time
# points per token (10 per syllable), annotated for citation tone, sandhi
# tone, historical tone category and syntactic structure.
SAMPLE_PATH  <- file.path("data", "sample_contours.csv")
# ASCII only: this string goes through renderText(), which mangles non-ASCII
# characters under a non-UTF-8 locale (they come out as <U+2014> and the like)
SAMPLE_LABEL <- "sample data - disyllabic tone sandhi, 85 tokens, 8 speakers"

# fixed qualitative palette for native-plotly colour-by (ColorBrewer Dark2)
QUAL_PALETTE <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                  "#66A61E", "#E6AB02", "#A6761D", "#666666")

# ---- guide steps: one source of truth for the compact guide -------------
COMPACT_GUIDE <- list(
  list(id = "step_browse", label = "Browse data",
       target = "browseDataSection", tab = NULL,
       text = paste("Upload a .csv or .rds file here to get started, or click",
                    "“Try the sample data” to load a ready-made tone-sandhi",
                    "dataset and explore straight away.")),
  list(id = "step_data_specifics", label = "Data specifics",
       target = c("colsDetails", "dataPreviewSection", "filterSection"), tab = NULL,
       text = paste("Set Time, f0, Speaker, and Token id on the left to match your",
                    "columns, check the preview on the right to confirm it loaded",
                    "correctly, and filter which tokens you're working with if you",
                    "don't need all of them.")),
  list(id = "step_inspect_plot", label = "Inspect & Plot specifics",
       target = c("plotDetails", "inspectSection"), tab = NULL,
       text = paste("Adjust how the plot looks (breaks, hover, range) on the left, and set",
                    "View, Colour by, and Facet to organise contours by whatever matters",
                    "for your question.")),
  list(id = "step_curate", label = "Curate",
       target = "curateDetails", tab = NULL,
       text = paste("Click a line in the plot, or search by token id, to select it, then",
                    "label and record your decision. Tick \"Hide other settings while",
                    "curating\" to declutter the page while you work."))
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

pick_col <- function(choices, preferred, fallback = choices[1]) {
  for (p in preferred) {
    if (p %in% choices) return(p)
  }
  for (p in preferred) {
    idx <- match(tolower(p), tolower(choices))
    if (!is.na(idx)) return(choices[idx])
  }
  for (p in preferred) {
    hit <- choices[grepl(p, choices, ignore.case = TRUE, fixed = TRUE) |
                     sapply(choices, function(ch) grepl(ch, p, ignore.case = TRUE, fixed = TRUE))]
    if (length(hit)) return(hit[1])
  }
  fallback
}

# Range for the f0 slider, derived from whichever f0 column is in play.
# Kept as a plain function so it can be reasoned about and tested on its own.
f0_slider_range <- function(y) {
  y <- suppressWarnings(as.numeric(y))
  y <- y[is.finite(y)]
  if (!length(y)) return(NULL)
  pad <- diff(range(y)) * 0.05
  if (!is.finite(pad) || pad == 0) pad <- 1
  lo <- floor(min(y) - pad)
  hi <- ceiling(max(y) + pad)
  list(min = lo, max = hi, value = c(lo, hi),
       step = signif(max((hi - lo) / 100, 1e-3), 1))
}

field_block <- function(label, help = NULL, widget) {
  tags$div(class = "field-block",
           tags$label(class = "control-label", label),
           if (!is.null(help)) tags$div(class = "field-help", help),
           widget
  )
}

brand_name <- function() {
  HTML("<span class='brand-name'><span class='word1'>Inspec</span><em>tour</em></span>")
}

numbered_card <- function(intro, items) {
  tags$div(class = "numbered-card",
           tags$p(class = "numbered-card-intro", intro),
           lapply(seq_along(items), function(i) {
             tags$div(class = "numbered-item",
                      tags$div(class = "numbered-badge", i),
                      tags$div(class = "numbered-content", items[[i]])
             )
           })
  )
}

# =============================================================================
#  UI
# =============================================================================
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Fraunces:wght@500;600&",
                            "family=Inter:wght@400;500&display=swap"))
  ),
  tags$head(tags$style(HTML("
    .field-block { margin-bottom: 18px; }
    .field-block .control-label { display: block; margin-bottom: 2px; font-weight: 600; }
    .field-block .field-help { color: #737373; font-size: 12px; margin: 2px 0 6px; line-height: 1.35; }

    details > summary { cursor: pointer; list-style: none; }
    details > summary::-webkit-details-marker { display: none; }
    details > summary::before {
      content: '\\25B6';
      display: inline-block;
      margin-right: 6px;
      font-size: 11px;
      transition: transform 0.15s ease;
    }
    details[open] > summary::before { transform: rotate(90deg); }

    body, .nav-item, label, input, select, .selectize-input, .help-block {
      font-family: 'Inter', sans-serif;
    }
    .app-title, h2, h3, h4, details > summary b {
      font-family: 'Fraunces', serif;
      font-weight: 600;
    }

    :root {
      --brand-primary: #111827;
      --brand-accent:  #0EA5E9;
      --brand-warm:    #F59E0B;
    }

    .btn, button.btn, .btn-default {
      background: var(--brand-warm) !important;
      border-color: var(--brand-warm) !important;
      color: #111827 !important;
      font-family: 'Inter', sans-serif;
    }
    .btn:hover, button.btn:hover { background: #D97706 !important; }

    .content-card {
      background: #FFFFFF;
      border: 1px solid #E5E7EB;
      border-radius: 12px;
      padding: 24px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      margin-bottom: 20px;
    }
    .content-card p {
      font-size: 16px;
      line-height: 1.7;
      color: #1F2430;
    }
    .content-card ul {
      font-size: 16px;
      line-height: 1.7;
      color: #1F2430;
      padding-left: 22px;
      margin-bottom: 16px;
    }
    .content-card li { margin-bottom: 6px; }
    .content-card h2 {
      font-size: 30px;
      margin-bottom: 6px;
    }
    .content-card .subheadline {
      font-family: 'Fraunces', serif;
      font-style: italic;
      font-size: 18px;
      color: #6B7280;
      margin-bottom: 20px;
    }
    .content-card h3 {
      font-size: 19px;
      margin-top: 28px;
      margin-bottom: 10px;
    }
    .section-summary {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 24px;
      margin-top: 28px;
      margin-bottom: 14px;
      padding-bottom: 8px;
      border-bottom: 3px solid var(--brand-accent);
      display: inline-block;
    }
    .numbered-card {
      background: #E0F2FE;
      border-radius: 12px;
      padding: 20px 24px;
      margin: 16px 0 24px;
    }
    .numbered-card-intro {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 16px;
      color: #111827;
      margin-bottom: 14px;
    }
    .numbered-item {
      display: flex;
      align-items: flex-start;
      gap: 14px;
      margin-bottom: 14px;
    }
    .numbered-item:last-child { margin-bottom: 0; }
    .numbered-badge {
      flex: 0 0 auto;
      width: 26px;
      height: 26px;
      border-radius: 50%;
      background: var(--brand-accent);
      color: #FFFFFF;
      font-weight: 700;
      font-size: 13px;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .numbered-content {
      font-size: 16px;
      line-height: 1.6;
      color: #1F2430;
      padding-top: 2px;
    }
    .brand-name {
      font-family: 'Fraunces', serif;
    }
    .brand-name em {
      font-style: italic;
      color: var(--brand-accent);
    }
    .app-header {
      display: flex;
      align-items: center;
      gap: 28px;
      padding: 18px 24px;
      background: #F8FAFC;
      border-bottom: 1px solid #E5E7EB;
      margin-bottom: 18px;
    }
    .app-title {
      font-size: 26px;
      font-weight: 600;
      letter-spacing: -0.2px;
      white-space: nowrap;
    }
    .word1 { color: var(--brand-primary); font-weight: 700; }
    .app-title em {
      font-style: italic;
      color: var(--brand-accent);
    }
    .app-nav { display: flex; gap: 8px; align-items: center; }
    .nav-item {
      color: #4B5563 !important;
      text-decoration: none !important;
      font-size: 15px;
      padding: 6px 14px;
      border-radius: 999px;
    }
    .nav-item.active {
      color: var(--brand-primary) !important;
      font-weight: 500;
      background: #F3F4F6;
    }
    .nav-tabs { display: none !important; }

    body.hide-sidebar .col-sm-3 { display: none !important; }
    body.hide-sidebar .col-sm-9 { width: 100% !important; }

    body.focus-curate #dataPreviewSection,
    body.focus-curate #dataPreviewSection + hr,
    body.focus-curate #filterSection,
    body.focus-curate #filterSection + hr {
      display: none !important;
    }

    .flash-highlight {
      outline: 3px solid #F59E0B !important;
      outline-offset: 3px;
      border-radius: 8px;
      transition: outline-color 0.3s ease;
    }
    .guide-link { cursor: pointer; }

    .or-divider {
      display: flex;
      align-items: center;
      gap: 10px;
      color: #9CA3AF;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      margin: 14px 0 12px;
    }
    .or-divider::before, .or-divider::after {
      content: '';
      flex: 1;
      height: 1px;
      background: #E5E7EB;
    }
    .btn.btn-sample, button.btn.btn-sample {
      background: #FFFFFF !important;
      border: 1px solid var(--brand-accent) !important;
      color: #0369A1 !important;
      font-weight: 600;
    }
    .btn.btn-sample:hover, button.btn.btn-sample:hover {
      background: #E0F2FE !important;
    }
    .home-cta {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 12px;
      margin: 4px 0 26px;
    }
    .btn.btn-cta, button.btn.btn-cta {
      background: var(--brand-accent) !important;
      border-color: var(--brand-accent) !important;
      color: #FFFFFF !important;
      font-weight: 600;
      font-size: 15px;
      padding: 10px 22px;
      border-radius: 999px;
    }
    .btn.btn-cta:hover, button.btn.btn-cta:hover { background: #0284C7 !important; }
    .btn.btn-cta-ghost, button.btn.btn-cta-ghost {
      background: #FFFFFF !important;
      border: 1px solid #D1D5DB !important;
      color: #374151 !important;
      font-size: 15px;
      padding: 10px 22px;
      border-radius: 999px;
    }
    .btn.btn-cta-ghost:hover, button.btn.btn-cta-ghost:hover { background: #F3F4F6 !important; }
    .home-cta-note {
      flex: 1 1 260px;
      font-size: 13px;
      color: #6B7280;
      line-height: 1.5;
    }

    .active-dataset {
      margin-top: 12px;
      font-size: 12px;
      color: #4B5563;
      line-height: 1.4;
      min-height: 1em;
    }
  "))),
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('setDetailsOpen', function(msg) {
      var d = document.getElementById(msg.id);
      if (d) d.open = msg.open;
    });
    Shiny.addCustomMessageHandler('setActiveNav', function(tab) {
      document.querySelectorAll('.nav-item').forEach(function(el) {
        el.classList.toggle('active', el.getAttribute('data-tab') === tab);
      });
    });
    Shiny.addCustomMessageHandler('toggleSidebar', function(hide) {
      document.body.classList.toggle('hide-sidebar', hide);
    });
    Shiny.addCustomMessageHandler('toggleFocusCurate', function(on) {
      document.body.classList.toggle('focus-curate', on);
      if (on) {
        ['colsDetails', 'plotDetails'].forEach(function(id) {
          var el = document.getElementById(id);
          if (el) el.open = false;
        });
      }
    });
    Shiny.addCustomMessageHandler('highlightSection', function(ids) {
      if (!Array.isArray(ids)) ids = [ids];
      setTimeout(function() {
        ids.forEach(function(id, idx) {
          var el = document.getElementById(id);
          if (!el) return;
          if (el.tagName === 'DETAILS') el.open = true;
          if (idx === 0) el.scrollIntoView({behavior: 'smooth', block: 'center'});
          el.classList.add('flash-highlight');
          setTimeout(function() { el.classList.remove('flash-highlight'); }, 1500);
        });
      }, 350);
    });
  "))),
  
  tags$div(class = "app-header",
           HTML("
      <svg width='52' height='44' viewBox='0 0 88 74'>
        <path d='M4,36 Q24,4 44,36 T84,36' fill='none' stroke='#111827' stroke-width='2.5'/>
        <path d='M4,46 Q24,22 44,46 T84,46' fill='none' stroke='#0EA5E9' stroke-width='2.5'/>
        <circle cx='24' cy='8' r='4' fill='#111827'/>
        <circle cx='64' cy='66' r='4' fill='#0EA5E9'/>
      </svg>
      <span class='app-title'><span class='word1'>Inspec</span><em>tour</em></span>
    "),
           tags$div(class = "app-nav",
                    actionLink("nav_home", label = "Home",
                               class = "nav-item", `data-tab` = "Home"),
                    actionLink("nav_inspect", label = "Inspect",
                               class = "nav-item", `data-tab` = "Inspect")
           )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # ---- data: step 1 of the guide -------------------------------------
      tags$div(id = "browseDataSection",
               fileInput("file", "Data (.csv or .rds)", accept = c(".csv", ".rds")),
               helpText("Long format: one row per time point per token."),

               tags$div(class = "or-divider", "or"),

               actionButton("use_sample", "Try the sample data",
                            class = "btn-sample", width = "100%"),
               helpText("85 disyllabic tone-sandhi contours from 8 speakers,",
                        "annotated for citation tone, sandhi tone and syntactic",
                        "structure. Nothing to download — it loads instantly."),

               tags$div(class = "active-dataset", textOutput("active_dataset"))
      ),
      tags$hr(),
      
      # ---- column mapping ------------------------------------------------
      conditionalPanel(
        "output.dataLoaded == true",
        tags$details(
          id = "colsDetails",
          open = NA,
          tags$summary(tags$b("Data specifics")),
          tags$br(),
          
          field_block("Time", widget = selectInput("col_time", label = NULL, choices = NULL)),
          
          field_block("f0",
                      "Defaults to a normalised f0 column (e.g. norm_f0) if found.",
                      selectInput("col_f0", label = NULL, choices = NULL)),
          
          field_block("Speaker", widget = selectInput("col_speaker", label = NULL, choices = NULL)),
          
          field_block("Token id",
                      "Which column identifies one recorded token.",
                      selectInput("col_token", label = NULL, choices = NULL))
        ),
        tags$br(),
        
        tags$details(
          id = "plotDetails",
          open = NA,
          tags$summary(tags$b("Plot specifics")),
          tags$br(),
          
          field_block("Mark a break (optional)",
                      paste("Draws a line wherever this column's value changes",
                            "within a token, e.g. syllable, word, or phrase boundaries."),
                      selectInput("col_break", label = NULL, choices = c("(none)"))),
          
          field_block("Break style",
                      "Insert a marker line, or actually disconnect the contour at that point.",
                      radioButtons("break_style", label = NULL,
                                   choices = c("Insert boundary line" = "boundary",
                                               "Disconnect the contour" = "disconnect"),
                                   selected = "boundary")),
          
          field_block("Show on hover", widget =
                        selectizeInput("col_hover", label = NULL,
                                       choices = NULL, multiple = TRUE)),
          
          field_block("f0 range", widget =
                        sliderInput("ylim", label = NULL, min = -6, max = 6,
                                    value = c(-4, 4), step = 0.5)),
          
          field_block("Plot height (px)", widget =
                        numericInput("plot_height", label = NULL, 620,
                                     min = 300, max = 2000, step = 20)),
          
          tags$br(),
          
          # ---- appearance, collapsed by default: only needed when exporting --
          tags$details(
            tags$summary(tags$b("Appearance")),
            sliderInput("lw_token", "Token line width",
                        min = 0.1, max = 2, value = 0.4, step = 0.05),
            sliderInput("alpha_token", "Token opacity",
                        min = 0.05, max = 1, value = 0.75, step = 0.05),
            sliderInput("lw_mean", "Mean line width",
                        min = 0.4, max = 3, value = 1.1, step = 0.1),
            sliderInput("x_breaks", "x-axis labels",
                        min = 2, max = 10, value = 4, step = 1),
            sliderInput("font_size", "Text size",
                        min = 7, max = 20, value = 13, step = 1),
            sliderInput("axis_size", "Axis number size",
                        min = 5, max = 16, value = 9, step = 1),
            checkboxInput("show_legend", "Show legend", value = TRUE)
          )
        ),
        
        tags$hr(),
        
        # ---- Inspect-tab-only controls ------------------------------
        conditionalPanel(
          "input.main_tabs == 'Inspect'",
          
          # ---- select & curate ------------------------------------------------
          tags$details(
            id = "curateDetails",
            open = NA,
            tags$summary(tags$b("Select & Curate")),
            tags$br(),
            
            checkboxInput("focus_curate", "Hide other settings while curating", value = FALSE),
            tags$hr(),
            
            selectizeInput("highlight", "Highlight token(s)",
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = "click a line, or type to search...")),
            helpText("Click any line in the plot to select it, or search by",
                     "token id here, and both stay in sync."),
            tags$hr(),
            
            tags$b("Record a decision"),
            selectizeInput("label_value", "Label for current selection",
                           choices = NULL, options = list(
                             create = TRUE,
                             placeholder = "choose an existing label, or type a new one"
                           )),
            actionButton("label_add_selected", "Apply to selected tokens"),
            helpText("Applies to whatever is currently selected or highlighted above,",
                     "which is usually what you want."),
            br(),
            actionButton("label_add_filtered", "Apply to all filtered tokens"),
            helpText("Applies to every token matching the current Filter by settings,",
                     "not just the selected ones. Use this only if that's what you mean."),
            actionButton("label_undo", "Undo last"),
            br(), br(),
            downloadButton("label_dl", "Download curated dataset (.csv)")
          )
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Home",
                 br(),
                 tags$div(class = "content-card",
                          tags$h2(brand_name(), ": inspecting contours through grouping and comparison"),
                          tags$div(class = "subheadline", "Discovering contour patterns in complex data structure"),

                          # The sidebar is hidden on this tab, so the way in has
                          # to live here; otherwise a first-time visitor has no
                          # visible route to any data at all.
                          tags$div(class = "home-cta",
                                   actionButton("use_sample_home",
                                                "Try the sample data",
                                                class = "btn-cta"),
                                   actionButton("nav_inspect_cta",
                                                "Load my own data",
                                                class = "btn-cta-ghost"),
                                   tags$div(class = "home-cta-note",
                                            "The sample is 85 disyllabic tone-sandhi contours from 8",
                                            "speakers — enough to try every part of the tool without",
                                            "downloading anything.")
                          ),

                          tags$details(
                            open = NA,
                            tags$summary(class = "section-summary", "What it does"),
                            tags$br(),
                            tags$p(brand_name(), " shows every contour in a dataset together at",
                                   "first, with no categories assumed. You can look at everything as a",
                                   "whole, select specific tokens to compare directly against each other,",
                                   "and then group and colour contours by variables that matter for your",
                                   "research question, such as speaker, syntactic structure, or tone",
                                   "combination, to see whether a pattern holds consistently or varies",
                                   "across them.")
                          ),
                          
                          tags$details(
                            open = NA,
                            tags$summary(class = "section-summary", "Why it matters"),
                            tags$br(),
                            tags$p(tags$b("Compared to traditional transcription,"), " ", brand_name(),
                                   " keeps the raw acoustic detail intact and lets groupings emerge from",
                                   "the data itself. Looking directly at contours makes it easy to see which",
                                   "ones genuinely resemble each other and which stand apart. Relying on",
                                   "transcribed values alone can obscure this: is a 51 tone really different",
                                   "from a 52, or is that just transcription noise? This is especially useful",
                                   "for fieldworkers working with a system they don't yet know well."),
                            tags$p(tags$b("Compared to machine clustering,"), " ", brand_name(),
                                   " is complementary rather than competing. Clustering has become",
                                   "increasingly central to prosodic research in recent years, but it has a",
                                   "real entry cost for linguists without a strong computational background,",
                                   "and it is never a one-click process: every run depends on human",
                                   "decisions about parameters and settings. It can also struggle when a",
                                   "dataset is small or unevenly distributed, for example when some",
                                   "categories are represented by only one or two tokens, which is often",
                                   "exactly the situation in the early stages of fieldwork on an",
                                   "under-studied prosodic system. ", brand_name(),
                                   " helps fill that gap: giving fieldworkers a clearer sense of how many",
                                   "contour shapes are actually present, and what they look like, before or",
                                   "alongside running any clustering."),
                            
                            numbered_card(
                              intro = "This flexibility is especially useful in fieldwork:",
                              items = list(
                                tagList(tags$b("Nothing gets lost."), " Whatever you choose to show on",
                                        "hover, such as speaker or tone category, stays attached to every",
                                        "contour throughout."),
                                tagList(tags$b("Zoom in without losing the big picture."), " Switch between",
                                        "individual tokens and group means at any point, so you can hold",
                                        "the overall pattern in mind while still checking specific tokens",
                                        "closely."),
                                tagList(tags$b("Facet by any variable, instantly."), " Changing which",
                                        "variable you facet by takes one click, making it easy to check",
                                        "whether a pattern holds across speakers, syntactic structures, or",
                                        "tone combinations."),
                                tagList(tags$b("Single out what matters."), " Borderline or unusual",
                                        "contours can be isolated and inspected closely, rather than",
                                        "getting lost in a crowd of overlapping lines.")
                              )
                            )
                          ),
                          
                          HTML("
                     <svg width='100%' viewBox='0 0 680 240' role='img'>
                       <title>Three-stage figure: all tokens, group by variable, curate</title>
                       <defs>
                         <marker id='arrow' viewBox='0 0 10 10' refX='8' refY='5' markerWidth='6' markerHeight='6' orient='auto-start-reverse'>
                           <path d='M2 1L8 5L2 9' fill='none' stroke='#9CA3AF' stroke-width='1.5' stroke-linecap='round' stroke-linejoin='round'/>
                         </marker>
                       </defs>
                       <text x='90' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>All tokens</text>
                       <path d='M30,110 Q60,82 90,110 Q120,138 150,110' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,120 Q60,145 90,120 Q120,95 150,120' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,115 Q60,88 90,115 Q120,142 150,115' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,105 Q60,130 90,105 Q120,80 150,105' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,118 Q60,92 90,118 Q120,144 150,118' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <line x1='165' y1='115' x2='195' y2='115' stroke='#9CA3AF' stroke-width='1.5' marker-end='url(#arrow)'/>
                       <text x='335' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>Group by variable</text>
                       <rect x='205' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='205' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='250' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S1</text>
                       <path d='M215,105 Q235,88 255,105 Q275,122 285,110' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <path d='M215,115 Q235,98 255,115 Q275,132 285,120' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M215,110 Q235,93 255,110 Q275,127 285,115' fill='none' stroke='#111827' stroke-width='1.2' opacity='0.6'/>
                       <rect x='300' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='300' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='345' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S2</text>
                       <path d='M310,100 Q330,118 350,100 Q370,82 380,95' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M310,110 Q330,128 350,110 Q370,92 380,105' fill='none' stroke='#0EA5E9' stroke-width='1.2' opacity='0.6'/>
                       <path d='M310,120 Q330,103 350,120 Q370,137 380,125' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <rect x='395' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='395' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='440' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S3</text>
                       <path d='M405,108 Q425,90 445,108 Q465,126 475,113' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <path d='M405,118 Q425,136 445,118 Q465,100 475,112' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M405,113 Q425,95 445,113 Q465,131 475,118' fill='none' stroke='#111827' stroke-width='1.2' opacity='0.6'/>
                       <line x1='215' y1='182' x2='235' y2='182' stroke='#111827' stroke-width='2'/>
                       <text x='240' y='186' font-family='Inter, sans-serif' font-size='10' fill='#4B5563'>NP</text>
                       <line x1='285' y1='182' x2='305' y2='182' stroke='#0EA5E9' stroke-width='2'/>
                       <text x='310' y='186' font-family='Inter, sans-serif' font-size='10' fill='#4B5563'>VP</text>
                       <text x='440' y='186' font-family='Inter, sans-serif' font-size='9' font-style='italic' fill='#9CA3AF'>(colour = syntactic structure)</text>
                       <line x1='500' y1='115' x2='530' y2='115' stroke='#9CA3AF' stroke-width='1.5' marker-end='url(#arrow)'/>
                       <text x='605' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>Curate</text>
                       <rect x='545' y='35' width='80' height='18' rx='9' fill='#F3F4F6'/>
                       <text x='585' y='47' text-anchor='middle' font-family='Inter, sans-serif' font-size='10' fill='#111827'>pattern A</text>
                       <path d='M545,95 Q565,75 585,95 Q605,115 625,95' fill='none' stroke='#111827' stroke-width='2'/>
                       <path d='M545,107 Q565,125 585,107 Q605,89 625,107' fill='none' stroke='#0EA5E9' stroke-width='2'/>
                       <rect x='545' y='150' width='80' height='18' rx='9' fill='#F3F4F6'/>
                       <text x='585' y='162' text-anchor='middle' font-family='Inter, sans-serif' font-size='10' fill='#0EA5E9'>pattern B</text>
                     </svg>
                   ")
                 )),
        tabPanel("Inspect",
                 br(),
                 tags$div(class = "content-card",
                          uiOutput("guide_ui"),
                          conditionalPanel("output.dataLoaded != true",
                                           h4("Load a dataset to begin."),
                                           verbatimTextOutput("format_help")),
                          conditionalPanel("output.dataLoaded == true",
                                           
                                           # ---- step 2 target: confirm data loaded correctly ------
                                           tags$div(id = "dataPreviewSection",
                                                    tags$h4("Data preview"),
                                                    DT::dataTableOutput("filtered_peek")
                                           ),
                                           tags$hr(),
                                           
                                           # ---- filter, grouped with Data specifics (step 2) ------
                                           tags$div(id = "filterSection",
                                                    selectizeInput("filter_vars", "Filter by", choices = NULL,
                                                                   multiple = TRUE,
                                                                   options = list(placeholder = "add a variable...")),
                                                    uiOutput("filter_controls"),
                                                    uiOutput("filter_status")
                                           ),
                                           tags$hr(),
                                           
                                           tags$div(id = "inspectSection",
                                                    fluidRow(
                                                      column(3, radioButtons("view", "View",
                                                                             choices = c("Individual tokens" = "tokens",
                                                                                         "Group means"       = "means",
                                                                                         "Means over tokens" = "both"),
                                                                             selected = "tokens")),
                                                      column(3, selectInput("col_colour", "Colour by", choices = NULL)),
                                                      column(3, selectInput("facet_x", "Facet (columns)", choices = NULL)),
                                                      column(3, selectInput("facet_y", "Facet (rows)",    choices = NULL))
                                                    ),
                                                    fluidRow(
                                                      column(3), column(3),
                                                      column(3, numericInput("facet_ncol", "Facet columns (single facet only)",
                                                                             2, min = 1, max = 10, step = 1)),
                                                      column(3)
                                                    ),
                                                    uiOutput("plot_ui"),
                                                    helpText("Use the camera icon in the plot's toolbar to",
                                                             "download it as an image.")
                                           ),
                                           br(),
                                           verbatimTextOutput("status"),
                                           tags$hr(),
                                           
                                           # ---- curation log, next to the plot it documents ------------
                                           tags$h4("Curation log"),
                                           DT::dataTableOutput("curation_log_table"))
                 ))
      )
    )
  )
)

# =============================================================================
#  Server
# =============================================================================
server <- function(input, output, session) {
  
  labels_log <- reactiveVal(
    data.frame(token_key = character(), label = character(),
               stringsAsFactors = FALSE)
  )
  # snapshots of labels_log before each labelling action, so "Undo last"
  # undoes one action rather than removing every token carrying a label
  undo_stack <- reactiveVal(list())

  output$curation_log_table <- DT::renderDataTable({
    DT::datatable(labels_log(),
                  options = list(pageLength = 5, order = list(list(0, "desc"))),
                  rownames = FALSE)
  })
  
  # ---- nav highlighting & Home hides the sidebar ---------------------------
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("setActiveNav", input$main_tabs)
    session$sendCustomMessage("toggleSidebar", input$main_tabs == "Home")
  })
  
  # ---- header nav links drive the underlying tabset ------------------------
  observeEvent(input$nav_home,    updateTabsetPanel(session, "main_tabs", selected = "Home"))
  observeEvent(input$nav_inspect, updateTabsetPanel(session, "main_tabs", selected = "Inspect"))
  
  # ---- focus-curate toggle: hide setup sections while curating -------------
  observeEvent(input$focus_curate, {
    session$sendCustomMessage("toggleFocusCurate", isTRUE(input$focus_curate))
  })
  
  # ---- guide-step links: switch tab if needed, then scroll to and flash
  # ---- whichever section(s) that step points to
  lapply(COMPACT_GUIDE, function(step) {
    local({
      s <- step
      observeEvent(input[[s$id]], {
        if (!is.null(s$tab)) {
          updateTabsetPanel(session, "main_tabs", selected = s$tab)
        }
        session$sendCustomMessage("highlightSection", as.list(s$target))
      })
    })
  })
  
  # ---- read data ----------------------------------------------------------
  # One store, two ways in: the file upload, or the bundled sample dataset.
  # Everything downstream reads raw(), so neither path is privileged.
  data_store <- reactiveVal(NULL)     # list(df = <data.frame>, source = <chr>)

  read_table <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    df <- switch(ext,
                 csv = readr::read_csv(path, show_col_types = FALSE),
                 rds = readRDS(path),
                 stop("Please choose a .csv or .rds file."))
    as.data.frame(df)
  }

  load_into_app <- function(df, source_label) {
    data_store(list(df = df, source = source_label))
    # a new dataset means the old selections and labels no longer refer to
    # anything, so clear them rather than leaving stale state behind
    labels_log(data.frame(token_key = character(), label = character(),
                          stringsAsFactors = FALSE))
    undo_stack(list())
    updateSelectizeInput(session, "highlight", selected = character(0))
  }

  observeEvent(input$file, {
    tryCatch(
      load_into_app(read_table(input$file$datapath, input$file$name),
                    input$file$name),
      error = function(e)
        showNotification(paste("Could not read that file:", conditionMessage(e)),
                         type = "error", duration = 8)
    )
  })

  load_sample <- function() {
    if (!file.exists(SAMPLE_PATH)) {
      showNotification(
        paste0("Sample dataset not found at ", SAMPLE_PATH,
               ". Make sure the data/ folder sits next to app.R."),
        type = "error", duration = 10)
      return()
    }
    tryCatch({
      load_into_app(read_table(SAMPLE_PATH, SAMPLE_PATH), SAMPLE_LABEL)
      updateTabsetPanel(session, "main_tabs", selected = "Inspect")
      showNotification("Sample dataset loaded — have a look at the Inspect tab.",
                       type = "message", duration = 5)
    }, error = function(e)
      showNotification(paste("Could not read the sample dataset:",
                             conditionMessage(e)), type = "error", duration = 8)
    )
  }

  # the sidebar button and the Home-tab call to action do the same thing
  observeEvent(input$use_sample,      load_sample())
  observeEvent(input$use_sample_home, load_sample())
  observeEvent(input$nav_inspect_cta,
               updateTabsetPanel(session, "main_tabs", selected = "Inspect"))

  raw <- reactive({
    ds <- data_store()
    req(ds)
    ds$df
  })

  data_loaded <- reactive(!is.null(data_store()))

  output$active_dataset <- renderText({
    ds <- data_store()
    if (is.null(ds)) "" else paste("Loaded:", ds$source)
  })
  outputOptions(output, "active_dataset", suspendWhenHidden = FALSE)

  output$dataLoaded <- reactive(data_loaded())
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # ---- guide starts with just step 1; the rest appear once data is loaded --
  output$guide_ui <- renderUI({
    steps <- if (data_loaded()) COMPACT_GUIDE else COMPACT_GUIDE[1]
    numbered_card(
      intro = "Quick guide, click any step to jump to it:",
      items = lapply(steps, function(s) {
        tagList(actionLink(s$id, tags$b(s$label), class = "guide-link"),
                " ", s$text)
      })
    )
  })
  
  output$format_help <- renderText({
    paste(
      "Click \"Try the sample data\" in the sidebar to explore straight away,",
      "or load your own file.",
      "",
      "Expected: long format, one row per time point per token.",
      "",
      "  time      numeric position within the syllable (1..n)",
      "  norm_f0   normalised f0 at that point",
      "  <id cols> one or more columns that together identify a token",
      "  <meta>    any number of grouping variables (speaker, tone, item...)",
      "",
      "Column roles are set in the sidebar after loading.",
      sep = "\n"
    )
  })
  
  # ---- populate selectors on load ---------------------------------------
  observeEvent(raw(), {
    df   <- raw()
    cols <- sort(names(df))
    is_cat <- vapply(df[cols], function(x) !is.numeric(x) || dplyr::n_distinct(x) <= 30,
                     logical(1))
    cat_cols <- cols[is_cat]
    
    updateSelectInput(session, "col_time", choices = cols,
                      selected = pick_col(cols, DEFAULTS$time))
    updateSelectInput(session, "col_f0", choices = cols,
                      selected = pick_col(cols, DEFAULTS$f0))
    updateSelectInput(session, "col_speaker", choices = cols,
                      selected = pick_col(cols, DEFAULTS$speaker))
    updateSelectInput(session, "col_token", choices = cols,
                      selected = pick_col(cols, DEFAULTS$token))
    
    updateSelectInput(session, "col_break",
                      choices  = c("(none)", cat_cols),
                      selected = "(none)")
    
    updateSelectizeInput(session, "col_hover", choices = cols,
                         selected = DEFAULTS$hover[DEFAULTS$hover %in% cols])
    updateSelectInput(session, "col_colour", choices = c("(none)", cat_cols),
                      selected = pick_col(cat_cols, DEFAULTS$colour, "(none)"))
    updateSelectInput(session, "facet_x", choices = c("(none)", cat_cols),
                      selected = "(none)")
    updateSelectInput(session, "facet_y", choices = c("(none)", cat_cols),
                      selected = "(none)")
    updateSelectizeInput(session, "filter_vars", choices = cat_cols,
                         selected = character(0))
  })

  # ---- keep the f0 range slider on the scale of the chosen f0 column -------
  # Without this the slider stays on the -6..6 range that suits normalised f0,
  # so choosing a raw Hz column silently plots an empty panel.
  observeEvent(list(raw(), input$col_f0), {
    df <- raw()
    req(input$col_f0, input$col_f0 %in% names(df))
    r <- f0_slider_range(df[[input$col_f0]])
    req(r)
    updateSliderInput(session, "ylim", min = r$min, max = r$max,
                      value = r$value, step = r$step)
  }, ignoreInit = FALSE)
  
  # ---- a stable key per token -------------------------------------------
  keyed <- reactive({
    df <- raw()
    req(input$col_token)
    df$tok_key <- as.character(df[[input$col_token]])
    df
  })
  
  observeEvent(keyed(), {
    updateSelectizeInput(session, "highlight",
                         choices = sort(unique(keyed()$tok_key)),
                         server = TRUE)
  })
  
  # ---- dynamic filter widgets -------------------------------------------
  output$filter_controls <- renderUI({
    vars <- input$filter_vars
    if (is.null(vars) || !length(vars)) return(NULL)
    df <- raw()
    lapply(vars, function(v) {
      selectizeInput(paste0("flt_", v), v,
                     choices  = sort(unique(as.character(df[[v]]))),
                     selected = NULL, multiple = TRUE,
                     options  = list(placeholder = "all"))
    })
  })
  
  # ---- plain-language summary of the active filter, if any -----------------
  output$filter_status <- renderUI({
    vars <- input$filter_vars %||% character(0)
    if (!length(vars)) {
      return(tags$p(tags$b("Current dataset: "), "full dataset"))
    }
    parts <- vapply(vars, function(v) {
      sel <- input[[paste0("flt_", v)]]
      if (is.null(sel) || !length(sel)) paste0(v, " = all")
      else paste0(v, " = ", paste(sel, collapse = "/"))
    }, character(1))
    tags$p(tags$b("Current dataset: "), "filtered by ", paste(parts, collapse = ", "))
  })
  
  # ---- quick peek at the currently filtered rows ----------------------------
  output$filtered_peek <- DT::renderDataTable({
    DT::datatable(filtered(),
                  options = list(pageLength = 5, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- filtered data ------------------------------------------------------
  filtered <- reactive({
    df <- keyed()
    for (v in input$filter_vars %||% character(0)) {
      sel <- input[[paste0("flt_", v)]]
      if (!is.null(sel) && length(sel)) {
        df <- df[as.character(df[[v]]) %in% sel, , drop = FALSE]
      }
    }
    df
  })
  
  # ---- grouping variables for the "means" view ---------------------------
  group_vars <- reactive({
    v <- c(input$col_colour, input$facet_x, input$facet_y)
    v[v != "(none)" & !is.na(v) & nzchar(v)]
  })
  
  # ---- plot ---------------------------------------------------------------
  output$plot_ui <- renderUI({
    plotlyOutput("plot", height = paste0(input$plot_height %||% 620, "px"))
  })
  
  output$plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No tokens match the current filters."))
    
    tcol <- input$col_time
    ycol <- input$col_f0
    ccol <- input$col_colour
    
    df$t_num <- suppressWarnings(as.numeric(df[[tcol]]))
    df$f0_num <- suppressWarnings(as.numeric(df[[ycol]]))
    df <- df[!is.na(df$t_num) & !is.na(df$f0_num), , drop = FALSE]
    validate(need(nrow(df) > 0, "Time / f0 columns are not numeric."))
    
    hover_cols <- intersect(input$col_hover %||% character(0), names(df))
    df$hover_txt <- if (length(hover_cols)) {
      apply(df[, hover_cols, drop = FALSE], 1, function(r)
        paste(paste0(hover_cols, ": ", r), collapse = "\n"))
    } else df$tok_key
    
    has_col <- !is.null(ccol) && ccol != "(none)"
    if (has_col) df$col_f <- as.factor(df[[ccol]])
    
    hi <- input$highlight %||% character(0)
    df$is_hi <- df$tok_key %in% hi
    any_hi <- length(hi) > 0
    
    # ---- break marking: boundary line, or actually disconnect the contour ---
    has_break <- !is.null(input$col_break) && input$col_break != "(none)" &&
      input$col_break %in% names(df)
    disconnect_mode <- has_break && identical(input$break_style, "disconnect")
    boundary_x <- numeric(0)
    df$plot_group <- df$tok_key
    
    if (has_break) {
      df$syll_val <- df[[input$col_break]]
      df <- df %>%
        arrange(tok_key, t_num) %>%
        group_by(tok_key) %>%
        mutate(changed = syll_val != dplyr::lag(syll_val),
               seg_id = cumsum(ifelse(is.na(changed), 0, changed))) %>%
        ungroup()
      
      if (disconnect_mode) {
        df$plot_group <- interaction(df$tok_key, df$seg_id, drop = TRUE)
      } else {
        boundary_x <- unique(df$t_num[which(df$changed)])
      }
    }
    
    # =========================================================================
    #  NATIVE PLOTLY PATH - "Individual tokens" only, so click-to-select works
    # =========================================================================
    if (identical(input$view, "tokens")) {
      
      lw   <- input$lw_token %||% 0.4
      a0   <- input$alpha_token %||% 0.75
      fs_  <- input$font_size %||% 13
      yrng <- input$ylim %||% NULL      # honour the f0 range here too, not
      legend_shown <- character(0)      # only on the ggplot paths
      
      build_panel <- function(sub) {
        p <- plot_ly(source = "group_src")
        toks <- unique(sub$tok_key)
        
        for (tk in toks) {
          tsub <- sub[sub$tok_key == tk, , drop = FALSE]
          tsub <- tsub[order(tsub$t_num), ]
          
          if (disconnect_mode && "changed" %in% names(tsub)) {
            brk_rows <- which(tsub$changed)
            if (length(brk_rows)) {
              gap_row <- tsub[1, , drop = FALSE]
              gap_row[1, c("t_num", "f0_num")] <- NA
              pieces <- list(); start <- 1
              for (r in brk_rows) {
                pieces[[length(pieces) + 1]] <- tsub[start:(r - 1), , drop = FALSE]
                pieces[[length(pieces) + 1]] <- gap_row
                start <- r
              }
              pieces[[length(pieces) + 1]] <- tsub[start:nrow(tsub), , drop = FALSE]
              tsub <- do.call(rbind, pieces)
            }
          }
          
          is_sel <- tk %in% hi
          grp_name <- if (has_col) as.character(tsub$col_f[1]) else NA
          base_col <- if (has_col) {
            lvl <- as.integer(tsub$col_f[1])
            QUAL_PALETTE[((lvl - 1) %% length(QUAL_PALETTE)) + 1]
          } else "#4682B4"
          
          show_leg <- FALSE
          if (has_col && !is_sel && !(grp_name %in% legend_shown)) {
            show_leg <- TRUE
            legend_shown <<- c(legend_shown, grp_name)
          }
          
          p <- add_trace(p, data = tsub, x = ~t_num, y = ~f0_num,
                         type = "scatter", mode = "lines",
                         line = list(color = if (is_sel) "black" else base_col,
                                     width = if (is_sel) max(lw * 2.5, 1.5) else lw),
                         opacity = if (is_sel) 1 else a0,
                         text = ~hover_txt, hoverinfo = "text",
                         customdata = tk,
                         connectgaps = FALSE,
                         name = if (has_col) grp_name else NULL,
                         legendgroup = if (has_col) grp_name else NULL,
                         showlegend = show_leg)
        }
        
        if (!disconnect_mode && has_break) {
          bx <- unique(sub$t_num[which(sub$changed)])
          if (length(bx) && nrow(sub)) {
            yr <- range(sub$f0_num, na.rm = TRUE)
            for (b in bx) {
              # hex, not an R colour name: plotly.js does not understand
              # "grey50" and silently falls back to its default palette,
              # which paints every boundary line a different colour
              p <- add_trace(p, x = c(b, b), y = yr, type = "scatter", mode = "lines",
                             line = list(color = "#9CA3AF", dash = "dash", width = 1),
                             hoverinfo = "none", showlegend = FALSE)
            }
          }
        }
        
        p <- layout(p, xaxis = list(title = "", zeroline = FALSE),
                    yaxis = list(title = "", zeroline = FALSE, range = yrng))
        p
      }
      
      fx <- input$facet_x; fy <- input$facet_y
      has_fx <- !is.null(fx) && fx != "(none)"
      has_fy <- !is.null(fy) && fy != "(none)"
      
      if (!has_fx && !has_fy) {
        final_plot <- build_panel(df) %>%
          layout(xaxis = list(title = tcol, zeroline = FALSE),
                 yaxis = list(title = ycol, zeroline = FALSE, range = yrng))
      } else {
        if (has_fx) df$fx_val <- as.character(df[[fx]])
        if (has_fy) df$fy_val <- as.character(df[[fy]])
        right_margin <- 40
        
        if (has_fx && has_fy) {
          xlevels <- sort(unique(df$fx_val)); ylevels <- sort(unique(df$fy_val))
          ncol_ <- length(xlevels); nrow_ <- length(ylevels)
          panels <- list()
          for (yl in ylevels) for (xl in xlevels) {
            sub <- df[df$fx_val == xl & df$fy_val == yl, , drop = FALSE]
            panels[[length(panels) + 1]] <- if (nrow(sub) > 0) build_panel(sub) else
              plot_ly(source = "group_src") %>%
              layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
          }
          
          final_plot <- do.call(plotly::subplot,
                                c(panels, list(nrows = nrow_, shareX = TRUE, shareY = TRUE,
                                               titleX = TRUE, titleY = TRUE, margin = 0.04)))
          
          lbl_size <- (input$axis_size %||% 9) + 2
          col_anns <- lapply(seq_along(xlevels), function(j) {
            list(x = (j - 0.5) / ncol_, y = 1.02, xref = "paper", yref = "paper",
                 text = xlevels[j], showarrow = FALSE, font = list(size = lbl_size),
                 xanchor = "center", yanchor = "bottom")
          })
          row_anns <- lapply(seq_along(ylevels), function(i) {
            list(x = 1.01, y = 1 - (i - 0.5) / nrow_, xref = "paper", yref = "paper",
                 text = ylevels[i], showarrow = FALSE, font = list(size = lbl_size),
                 xanchor = "left", yanchor = "middle", textangle = 90)
          })
          final_plot <- layout(final_plot, annotations = c(col_anns, row_anns))
          right_margin <- 60
          
        } else {
          var <- if (has_fx) fx else fy
          df$fv <- as.character(df[[var]])
          lvls <- sort(unique(df$fv))
          ncol_ <- min(input$facet_ncol %||% 2, length(lvls))
          nrow_ <- ceiling(length(lvls) / ncol_)
          panels <- lapply(lvls, function(lv) build_panel(df[df$fv == lv, , drop = FALSE]))
          
          final_plot <- do.call(plotly::subplot,
                                c(panels, list(nrows = nrow_, shareX = TRUE, shareY = TRUE,
                                               titleX = TRUE, titleY = TRUE, margin = 0.04)))
          
          lbl_size <- (input$axis_size %||% 9) + 2
          anns <- lapply(seq_along(panels), function(i) {
            col_i <- (i - 1) %% ncol_
            row_i <- (i - 1) %/% ncol_
            list(x = (col_i + 0.5) / ncol_, y = 1 - (row_i / nrow_) - 0.01,
                 xref = "paper", yref = "paper",
                 text = lvls[i], showarrow = FALSE, font = list(size = lbl_size),
                 xanchor = "center", yanchor = "bottom")
          })
          final_plot <- layout(final_plot, annotations = anns)
        }
        
        final_plot <- layout(final_plot, margin = list(r = right_margin))
      }
      
      # height is set on plotlyOutput(); passing it here as well is deprecated
      final_plot <- layout(final_plot,
                           font = list(size = fs_),
                           showlegend = isTRUE(input$show_legend))
      # click-to-select depends on this event actually being registered;
      # without it plotly warns and event_data() can come back empty
      final_plot <- plotly::event_register(final_plot, "plotly_click")
      final_plot <- config(final_plot, displaylogo = FALSE,
                           toImageButtonOptions = list(format = "png",
                                                       filename = "inspectour_group_inspection",
                                                       scale = 2))
      return(final_plot)
    }
    
    # =========================================================================
    #  GGPLOT PATH - "Group means" and "Means over tokens" (unchanged)
    # =========================================================================
    p <- ggplot()
    
    if (length(boundary_x)) {
      p <- p + geom_vline(xintercept = boundary_x, linetype = "dashed",
                          colour = "grey50", linewidth = 0.4)
    }
    
    if (input$view %in% c("tokens", "both")) {
      a0 <- input$alpha_token
      base_alpha <- if (input$view == "both") a0 * 0.35 else if (any_hi) a0 * 0.2 else a0
      bg <- df[!df$is_hi, , drop = FALSE]
      if (nrow(bg)) {
        p <- p + geom_line(
          data = bg,
          aes(x = t_num, y = f0_num, group = plot_group,
              colour = if (has_col) col_f else NULL,
              text = hover_txt),
          alpha = base_alpha, linewidth = input$lw_token
        )
      }
      if (any_hi) {
        fg <- df[df$is_hi, , drop = FALSE]
        if (nrow(fg)) {
          p <- p + geom_line(
            data = fg,
            aes(x = t_num, y = f0_num, group = plot_group, text = hover_txt),
            colour = "black", linewidth = max(input$lw_token * 2.5, 1)
          )
        }
      }
    }
    
    if (input$view %in% c("means", "both")) {
      gv <- group_vars()
      summ <- df %>%
        group_by(across(all_of(c(gv, "t_num")))) %>%
        summarise(m = mean(f0_num, na.rm = TRUE),
                  s = sd(f0_num, na.rm = TRUE),
                  n = dplyr::n_distinct(tok_key),
                  .groups = "drop")
      summ$s[is.na(summ$s)] <- 0
      summ$grp_key <- if (length(gv)) {
        do.call(paste, c(lapply(gv, function(v) summ[[v]]), sep = " | "))
      } else "all"
      if (has_col) summ$col_f <- as.factor(summ[[ccol]])
      summ$hover_txt <- paste0(summ$grp_key, "\nn tokens: ", summ$n)
      
      p <- p +
        geom_ribbon(data = summ,
                    aes(x = t_num, ymin = m - s, ymax = m + s,
                        group = grp_key,
                        fill = if (has_col) col_f else NULL),
                    alpha = 0.15, colour = NA) +
        geom_line(data = summ,
                  aes(x = t_num, y = m, group = grp_key,
                      colour = if (has_col) col_f else NULL,
                      text = hover_txt),
                  linewidth = input$lw_mean)
    }
    
    fx <- input$facet_x; fy <- input$facet_y
    if (fx != "(none)" && fy != "(none)") {
      p <- p + facet_grid(as.formula(paste0("`", fy, "` ~ `", fx, "`")),
                          labeller = label_value)
    } else if (fx != "(none)") {
      p <- p + facet_wrap(as.formula(paste0("~ `", fx, "`")),
                          ncol = max(1, input$facet_ncol %||% 2),
                          labeller = label_both)
    } else if (fy != "(none)") {
      p <- p + facet_wrap(as.formula(paste0("~ `", fy, "`")),
                          ncol = 1, labeller = label_both)
    }
    
    fs <- input$font_size %||% 13
    as_ <- input$axis_size %||% 9
    
    p <- p +
      scale_x_continuous(breaks = scales::pretty_breaks(input$x_breaks %||% 4)) +
      coord_cartesian(ylim = input$ylim) +
      labs(x = tcol, y = ycol,
           colour = if (has_col) ccol else NULL,
           fill   = if (has_col) ccol else NULL) +
      theme_bw() +
      theme(panel.spacing.y = unit(0.02, "cm"),
            panel.spacing.x = unit(0.15, "cm"),
            text        = element_text(size = fs),
            axis.text   = element_text(size = as_),
            strip.text  = element_text(size = as_ + 1,
                                       margin = margin(2, 2, 2, 2)),
            legend.position = if (isTRUE(input$show_legend)) "right" else "none")
    
    ggplotly(p, tooltip = "text",
             height = input$plot_height) %>%
      layout(hovermode = "closest",
             showlegend = isTRUE(input$show_legend)) %>%
      plotly::event_register("plotly_click") %>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(format = "png",
                                         filename = "inspectour_group_inspection",
                                         scale = 2))
  })
  
  # ---- click a line in Inspect toggles it in the highlight box ----
  observeEvent(event_data("plotly_click", source = "group_src"), {
    click <- event_data("plotly_click", source = "group_src")
    req(click)
    tk <- click$customdata[1]
    req(!is.null(tk), !is.na(tk))
    cur <- input$highlight %||% character(0)
    new_sel <- if (tk %in% cur) setdiff(cur, tk) else union(cur, tk)
    updateSelectizeInput(session, "highlight", selected = new_sel)
  })
  
  # ---- status line --------------------------------------------------------
  output$status <- renderText({
    df <- filtered()
    if (!nrow(df)) return("0 tokens")
    n_tok <- dplyr::n_distinct(df$tok_key)
    paste0(n_tok, " tokens shown",
           "\nlabels recorded: ", nrow(labels_log()))
  })
  
  # ---- working labels -----------------------------------------------------
  observeEvent(labels_log(), {
    existing <- sort(unique(labels_log()$label))
    updateSelectizeInput(session, "label_value", choices = existing,
                         selected = isolate(input$label_value), server = TRUE)
  }, ignoreNULL = FALSE)
  
  apply_label_to <- function(keys) {
    if (is.null(input$label_value) || !nzchar(input$label_value)) {
      showNotification("Choose or type a label first.", type = "warning",
                       duration = 4)
      return()
    }
    if (!length(keys)) {
      showNotification("Nothing selected to label.", type = "warning",
                       duration = 4)
      return()
    }
    undo_stack(c(undo_stack(), list(labels_log())))
    old <- labels_log()
    old <- old[!(old$token_key %in% keys), , drop = FALSE]
    labels_log(rbind(old,
                     data.frame(token_key = keys,
                                label = input$label_value,
                                stringsAsFactors = FALSE)))
  }
  
  observeEvent(input$label_add_selected, {
    apply_label_to(unique(input$highlight %||% character(0)))
  })
  
  observeEvent(input$label_add_filtered, {
    apply_label_to(unique(filtered()$tok_key))
  })
  
  observeEvent(input$label_undo, {
    st <- undo_stack()
    if (!length(st)) {
      showNotification("Nothing left to undo.", type = "warning", duration = 3)
      return()
    }
    labels_log(st[[length(st)]])
    undo_stack(st[-length(st)])
  })
  
  output$label_dl <- downloadHandler(
    filename = function() paste0("inspectour_curated_dataset_", Sys.Date(), ".csv"),
    content  = function(f) {
      df <- keyed()
      lg <- labels_log()
      df$label <- lg$label[match(df$tok_key, lg$token_key)]
      readr::write_csv(df, f)
    }
  )
}

shinyApp(ui, server)