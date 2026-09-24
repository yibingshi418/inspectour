
# =============================================================================
#  Server
#  shinyApp(ui, server) itself lives in app.R, one level up -- this file
#  only defines the server function.
# =============================================================================
server <- function(input, output, session) {

  # each row is one correction: this token's value for this existing column
  # is now this new value -- so several different columns can be corrected
  # independently, rather than bolting on one generic "label" column.
  edit_log <- reactiveVal(
    data.frame(token_key = character(), variable = character(),
               new_value = character(), stringsAsFactors = FALSE)
  )

  # token whose clip should start playing on the next render of audio_panel
  last_played <- reactiveVal(NULL)

  # one row per token, one column per variable that's been corrected so far
  # (named after that variable, e.g. "trial") -- so the log reads just like
  # the dataset itself, showing the actual corrected value in each cell,
  # rather than a generic "which column changed" row
  output$curation_log_table <- DT::renderDataTable({
    lg <- edit_log()
    if (!nrow(lg)) {
      wide <- data.frame(Token = character(0))
    } else {
      wide <- tidyr::pivot_wider(lg, id_cols = token_key,
                                 names_from = variable, values_from = new_value)
      names(wide)[1] <- "Token"
    }
    DT::datatable(wide,
                  options = list(pageLength = 5, order = list(list(0, "desc"))),
                  rownames = FALSE)
  })

  output$curation_log_count <- renderText({
    paste0("(", nrow(edit_log()), ")")
  })

  # ---- nav highlighting & Home/Data hide the sidebar -----------------------
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("setActiveNav", input$main_tabs)
    session$sendCustomMessage("toggleSidebar", input$main_tabs %in% c("Home", "Data"))
  })

  # ---- header nav links drive the underlying tabset ------------------------
  observeEvent(input$nav_home,    updateTabsetPanel(session, "main_tabs", selected = "Home"))
  observeEvent(input$nav_data,    updateTabsetPanel(session, "main_tabs", selected = "Data"))
  observeEvent(input$nav_inspect, {
    req(data_loaded())
    updateTabsetPanel(session, "main_tabs", selected = "Inspect")
  })
  observeEvent(input$goto_data_from_inspect,
               updateTabsetPanel(session, "main_tabs", selected = "Data"))
  observeEvent(input$go_inspect,
               updateTabsetPanel(session, "main_tabs", selected = "Inspect"))
  # same destination as go_inspect, just reachable without scrolling past
  # the (optional) audio setup below it
  observeEvent(input$go_inspect_early,
               updateTabsetPanel(session, "main_tabs", selected = "Inspect"))

  # ---- read data --------------------------------------------------------
  # read once and cached -- this reactive has no reactive dependencies of
  # its own, so it only actually runs the first time "Try it with sample
  # data" is clicked, however many times that ends up being.
  sample_df <- reactive({
    as.data.frame(readr::read_csv(sample_data_path(), show_col_types = FALSE))
  })

  # which dataset is active, from either source -- NULL until either the
  # user uploads a file or clicks the sample-data button
  data_source <- reactiveVal(NULL)

  observeEvent(input$file, {
    data_source(list(kind = "upload", path = input$file$datapath, name = input$file$name))
  })

  observeEvent(input$load_sample, {
    data_source(list(kind = "sample"))
  })

  raw <- reactive({
    src <- data_source()
    req(src)
    if (identical(src$kind, "sample")) return(sample_df())
    ext <- tolower(tools::file_ext(src$name))
    validate(need(ext == "csv", "Please upload a .csv file"))
    df <- readr::read_csv(src$path, show_col_types = FALSE)
    as.data.frame(df)
  })

  data_loaded <- reactive(!is.null(data_source()))

  output$dataLoaded <- reactive(data_loaded())
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

  # ---- Inspect nav link greys out until a dataset is loaded ---------------
  observe({
    session$sendCustomMessage("setNavEnabled", list(tab = "Inspect", enabled = data_loaded()))
  })

  output$format_help <- renderText({
    paste(
      "Expected: long format, one row per time point per token.",
      "",
      "  time      numeric position within the syllable (1..n)",
      "  norm_f0   normalised f0 at that point",
      "  <id cols> one or more columns that together identify a token",
      "  <meta>    any number of grouping variables (speaker, tone, item...)",
      "",
      "Data specifics are set below once your file loads.",
      sep = "\n"
    )
  })

  # ---- the token id built from several columns is a variable, not a key ---
  # Combining columns produces something the dataset did not have, so it is
  # added to it under a name of its own rather than kept as a private key.
  # It then behaves like any other column: pickable as the token id, shown
  # in the preview table, offered for hover, and grouped by if it happens to
  # have few enough levels.
  derived_var <- reactive({
    if (length(token_id_active_parts()) < 2) return("")
    free_name("token_id", names(raw()))
  })

  # ---- column info, shared by selector population and the overview -------
  col_info <- reactive({
    df <- raw()
    cols <- sort(names(df))
    is_cat <- vapply(df[cols], function(x) !is.numeric(x) || dplyr::n_distinct(x) <= 30,
                     logical(1))
    cat_cols <- cols[is_cat]

    dn <- derived_var()
    if (nzchar(dn)) {
      cols <- sort(c(cols, dn))
      # counted without pasting the key together: one level per distinct
      # combination of the parts
      parts <- token_id_active_parts()
      if (all(parts %in% names(df)) &&
          nrow(unique(df[, parts, drop = FALSE])) <= 30)
        cat_cols <- sort(c(cat_cols, dn))
    }
    list(cols = cols, cat_cols = cat_cols)
  })

  # ---- populate selectors on load ---------------------------------------
  observeEvent(raw(), {
    df <- raw()
    ci <- col_info()
    cols <- ci$cols
    cat_cols <- ci$cat_cols

    updateSelectInput(session, "col_time", choices = cols,
                      selected = pick(cols, DEFAULTS$time))
    updateSelectInput(session, "col_f0", choices = cols,
                      selected = pick(cols, DEFAULTS$f0))
    updateSelectInput(session, "col_speaker", choices = cols,
                      selected = pick(cols, DEFAULTS$speaker))
    # "None" is a real choice (like "(none)" elsewhere), not an empty-string
    # value -- selectize drops options whose value is "" by default, which
    # made this one impossible to select back once left.
    updateSelectInput(session, "col_token",
                      choices = c("None", cols),
                      selected = { s <- find_token_id_col(cols); if (nzchar(s)) s else "None" })
    updateSelectizeInput(session, "token_id_parts", choices = cols,
                         selected = character(0))

    updateSelectInput(session, "col_break",
                      choices  = c("(none)", cat_cols),
                      selected = "(none)")

    updateSelectizeInput(session, "col_hover", choices = cols,
                         selected = DEFAULTS$hover[DEFAULTS$hover %in% cols])
    updateSelectInput(session, "col_colour", choices = c("(none)", cat_cols),
                      selected = pick(cat_cols, DEFAULTS$colour, "(none)"))
    updateSelectInput(session, "facet_x", choices = c("(none)", cat_cols),
                      selected = "(none)")
    updateSelectInput(session, "facet_y", choices = c("(none)", cat_cols),
                      selected = "(none)")
    updateSelectizeInput(session, "filter_vars", choices = cat_cols,
                         selected = character(0))
  })

  # ---- the generated variable has to reach the controls that list columns --
  # Appearing and disappearing as the parts are chosen and cleared, so every
  # list is rebuilt and existing selections are kept. The one thing that is
  # chosen rather than kept is the token id itself: generating the variable
  # is what fills an empty dropdown, and clearing the parts has to release
  # it again, or the control would point at a column that no longer exists.
  observeEvent(derived_var(), {
    req(data_loaded())
    ci  <- col_info()
    dn  <- derived_var()
    cur <- input$col_token %||% "None"
    if (identical(cur, "None")) cur <- ""

    sel <- if (nzchar(dn)) {
      if (nzchar(cur) && cur %in% ci$cols) cur else dn
    } else {
      if (cur %in% ci$cols) cur else find_token_id_col(ci$cols)
    }
    if (!nzchar(sel)) sel <- "None"
    updateSelectInput(session, "col_token",
                      choices = c("None", ci$cols), selected = sel)

    keep <- function(x, pool) x[x %in% pool]
    updateSelectizeInput(session, "col_hover", choices = ci$cols,
                         selected = keep(input$col_hover %||% character(0), ci$cols))
    for (id in c("col_colour", "col_break", "facet_x", "facet_y")) {
      v <- input[[id]] %||% "(none)"
      updateSelectInput(session, id, choices = c("(none)", ci$cat_cols),
                        selected = if (v %in% ci$cat_cols) v else "(none)")
    }
    updateSelectizeInput(session, "filter_vars", choices = ci$cat_cols,
                         selected = keep(input$filter_vars %||% character(0), ci$cat_cols))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # ---- token id, derived from >=2 columns instead of picked from one -------
  # (e.g. no single column identifies a recording, but speaker + item_id
  # + condition together do) -- selecting 2+ columns above takes
  # priority over the "Token id" dropdown, same pattern as elsewhere in the
  # curation controls: a more specific, explicit choice wins over a default.
  token_id_active_parts <- reactive({
    parts <- input$token_id_parts %||% character(0)
    if (length(parts) >= 2) parts else character(0)
  })

  # Six attempts at wording this could not beat simply checking it. A correct
  # token id has exactly one row per time point; one that names the item
  # instead bundles every speaker's recording of that item into one group,
  # which shows up as the same time point repeating. Report that against the
  # user's own data and columns, rather than defining "production" in prose.
  output$token_id_preview <- renderUI({
    req(data_loaded())

    # A dataset with no column named like a token id starts with the
    # dropdown empty, so say what to do rather than showing nothing.
    if (!token_id_ready())
      return(tags$div(class = "near-miss-box",
                      "No column here is named like a token id. Pick the one that",
                      "identifies a single production, or combine columns below",
                      "until together they do."))

    df <- tryCatch(keyed(), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(NULL)

    parts <- token_id_active_parts()
    label <- if (length(parts)) paste(parts, collapse = " + ") else token_col()
    if (!nzchar(label)) return(NULL)

    n_tok <- dplyr::n_distinct(df$tok_key)
    tcol  <- input$col_time
    worst <- if (!is.null(tcol) && nzchar(tcol) && tcol %in% names(df)) {
      max(table(paste(df$tok_key, df[[tcol]], sep = "\r")))
    } else 1L

    if (worst <= 1) {
      tags$div(class = "record-preview record-preview-partial",
               tags$b(label), sprintf(" gives %d tokens, one row per time point.", n_tok),
               " First: ", tags$b(df$tok_key[1]),
               if (nzchar(derived_var()))
                 tagList(" Kept as the variable ", tags$b(derived_var()), "."))
    } else {
      tags$div(class = "near-miss-box",
               tags$b(label),
               sprintf(" gives only %d groups, and each bundles up to %d separate recordings,",
                       n_tok, worst),
               " which repeat the same time points. ",
               if (length(parts)) "Add another column until every group is one recording."
               else "Combine columns below until every group is one recording.")
    }
  })
  # it lives inside a conditionalPanel that starts hidden (0 columns picked
  # yet) -- without this, Shiny suspends it while hidden and never re-runs
  # it once 2+ columns are picked and the panel becomes visible
  outputOptions(output, "token_id_preview", suspendWhenHidden = FALSE)

  # ---- which columns can be recategorised: any categorical column except
  # ---- the token id itself, which the whole app keys off of -- plus any
  # ---- brand-new variable already created via the curation log, so it
  # ---- becomes reselectable as "existing" after its first use ------------
  observe({
    ci <- col_info()
    req(ci$cat_cols)
    token_parts <- token_id_active_parts()
    token_exclude <- c(if (length(token_parts)) token_parts else token_col(),
                       derived_var())
    editable <- setdiff(ci$cat_cols, token_exclude)
    new_vars <- setdiff(unique(edit_log()$variable), editable)
    updateSelectInput(session, "edit_var_existing", choices = c(editable, new_vars),
                      selected = active_var())
  })

  # "choose an existing variable" and "create a new variable" are two
  # separate, unambiguous controls, same pattern as the value picker below --
  # a typed new variable always wins if both happen to be filled in.
  current_var <- reactive({
    new_var <- trimws(input$edit_var_new %||% "")
    if (nzchar(new_var)) new_var else (input$edit_var_existing %||% "")
  })

  # ---- once a variable to correct is chosen, offer its real, existing
  # ---- values -- so the dropdown is never empty on a fresh dataset --------
  # also refreshes when a correction is applied, so a value just typed as
  # new (for this same variable) is immediately offered as "existing" too.
  # a brand new variable (not yet a real column) simply starts with no
  # values from the data -- only whatever has been typed in for it so far.
  observeEvent(list(current_var(), edit_log()), {
    var <- current_var()
    req(nzchar(var))
    from_data <- if (var %in% names(raw())) as.character(raw()[[var]]) else character(0)
    from_edits <- edit_log()$new_value[edit_log()$variable == var]
    vals <- sort(unique(c(from_data, from_edits)))
    updateSelectInput(session, "edit_value_existing", choices = vals,
                      selected = active_value())
  }, ignoreNULL = FALSE)

  # ---- a stable key per token -------------------------------------------
  # combines two or more columns into one id string, e.g. speaker +
  # item_id + condition -> "S1_1_ct". Converts each column to
  # character on its own (never via apply()/as.matrix(), which silently pads
  # numeric columns to a common width -- e.g. item_id 1 and 11 would
  # come out as " 1" and "11", corrupting the id) before pasting them.
  build_derived_key <- function(df, parts, sep) {
    cols <- lapply(parts, function(p) as.character(df[[p]]))
    do.call(paste, c(cols, sep = sep))
  }

  # Either route gives a token id: an existing column, or several combined.
  # Combining is the more specific, explicitly taken action, so it wins.
  token_col <- reactive({
    tc <- input$col_token %||% "None"
    if (nzchar(tc) && !identical(tc, "None") && tc %in% names(raw())) tc else ""
  })

  token_id_ready <- reactive({
    length(token_id_active_parts()) >= 2 || nzchar(token_col())
  })

  keyed <- reactive({
    df <- raw()
    parts <- token_id_active_parts()
    if (length(parts)) {
      req(all(parts %in% names(df)))
      sep <- input$token_id_sep %||% "_"
      if (!nzchar(sep)) sep <- "_"
      df$tok_key <- build_derived_key(df, parts, sep)
      dn <- derived_var()
      if (nzchar(dn)) df[[dn]] <- df$tok_key
    } else {
      tc <- token_col()
      req(nzchar(tc))
      df$tok_key <- as.character(df[[tc]])
    }
    df
  })

  observeEvent(keyed(), {
    updateSelectizeInput(session, "highlight",
                         choices = sort(unique(keyed()$tok_key)),
                         server = TRUE)
  })

  # ---- Data page: plain-language overview of what was loaded --------------
  output$data_overview <- renderText({
    req(data_loaded())
    # The row count and the variables are facts about the file and are
    # reported the moment it loads. Only the token count waits on a token id
    # being chosen, so loading a file always shows something.
    df <- if (token_id_ready()) keyed() else raw()
    n_row <- nrow(df)

    # kept deliberately minimal -- speaker/f0 counts depend on column roles
    # that are only confirmed on the Inspect page, not guessed here.
    header <- if (token_id_ready()) {
      n_tok <- dplyr::n_distinct(df$tok_key)
      paste0(format(n_row, big.mark = ","), " rows across ", n_tok,
             " token", if (n_tok != 1) "s", ".")
    } else {
      paste0(format(n_row, big.mark = ","), " rows. Set a token id below",
             " to see how many tokens that is.")
    }

    ci <- col_info()
    if (!length(ci$cat_cols)) return(header)

    name_w <- max(nchar(ci$cat_cols))
    cat_lines <- vapply(ci$cat_cols, function(v) {
      lv <- sort(unique(as.character(df[[v]])))
      shown <- paste(utils::head(lv, 5), collapse = ", ")
      more <- if (length(lv) > 5) ", ..." else ""
      sprintf("  %-*s  %2d level%s  (%s%s)", name_w, v, length(lv),
              if (length(lv) != 1) "s" else " ", shown, more)
    }, character(1))

    paste(c(header, "", "Variables you can colour or facet by:", cat_lines),
          collapse = "\n")
  })

  output$full_preview <- DT::renderDataTable({
    req(data_loaded())
    DT::datatable(if (token_id_ready()) keyed() else raw(),
                  options = list(pageLength = 5, scrollX = TRUE),
                  rownames = FALSE)
  })

  # ---- dynamic filter widgets -------------------------------------------
  output$filter_controls <- renderUI({
    vars <- input$filter_vars
    if (is.null(vars) || !length(vars)) return(NULL)
    df <- raw()
    tags$div(class = "filter-controls",
      lapply(vars, function(v) {
        selectizeInput(paste0("flt_", v), v,
                       choices  = sort(unique(as.character(df[[v]]))),
                       selected = NULL, multiple = TRUE,
                       options  = list(placeholder = "all"))
      }))
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
    # audio coverage is usually partial, so being able to see only the part
    # you can actually listen to is a filter like any other
    if (isTRUE(input$only_audio)) {
      df <- df[df$tok_key %in% tokens_with_audio(), , drop = FALSE]
    }
    df
  })

  # ---- one-line status: token count, active filter, audio match ----------
  output$inspect_status <- renderUI({
    df <- filtered()
    n_tok <- if (nrow(df)) dplyr::n_distinct(df$tok_key) else 0

    vars <- input$filter_vars %||% character(0)
    filt_txt <- if (!length(vars)) {
      "full dataset"
    } else {
      parts <- vapply(vars, function(v) {
        sel <- input[[paste0("flt_", v)]]
        if (is.null(sel) || !length(sel)) paste0(v, " = all")
        else paste0(v, " = ", paste(sel, collapse = "/"))
      }, character(1))
      paste(parts, collapse = ", ")
    }
    # the audio-only tick is a filter too, so the strip must not still call
    # this the full dataset while showing a ninth of it
    if (isTRUE(input$only_audio)) {
      filt_txt <- if (!length(vars)) "with audio" else paste0(filt_txt, ", with audio")
    }

    idx <- audio_index()
    audio_txt <- if (nrow(idx)) {
      n_all_tok <- dplyr::n_distinct(keyed()$tok_key)
      n_aud <- dplyr::n_distinct(idx$tok_key)
      extra <- if (nrow(idx) > n_aud) sprintf(", %d clips", nrow(idx)) else ""
      paste0(", audio: ", n_aud, "/", n_all_tok, " matched", extra)
    } else ""

    tags$div(class = "status-strip",
             tags$b(n_tok), paste0(" token", if (n_tok != 1) "s", " shown (", filt_txt, ")", audio_txt))
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

    # export-modal settings may not exist yet until first opened
    sl  <- if (is.null(input$show_legend)) TRUE else isTRUE(input$show_legend)
    lwm <- input$lw_mean %||% 1.1

    df$t_num <- suppressWarnings(as.numeric(df[[tcol]]))
    df$f0_num <- suppressWarnings(as.numeric(df[[ycol]]))
    df <- df[!is.na(df$t_num) & !is.na(df$f0_num), , drop = FALSE]
    validate(need(nrow(df) > 0, "Time / f0 columns are not numeric."))

    # If tokens do not share time values, put each on 0..1 of its own span.
    # Without this, absolute or per-token-offset times spread contours across
    # the whole session and make every mean an average of one token.
    shared_grid <- time_grid_is_shared(df$t_num, df$tok_key)
    if (!shared_grid) df$t_num <- time_to_relative(df$t_num, df$tok_key)
    x_title <- if (shared_grid) "Normalised time" else "Time within token (0-1)"

    hover_cols <- intersect(input$col_hover %||% character(0), names(df))
    df$hover_txt <- if (length(hover_cols)) {
      apply(df[, hover_cols, drop = FALSE], 1, function(r)
        paste(paste0(hover_cols, ": ", r), collapse = "\n"))
    } else df$tok_key

    has_col <- !is.null(ccol) && ccol != "(none)"
    # levels come from the whole dataset, so a group keeps its colour when a
    # filter removes other groups
    if (has_col) df$col_f <- factor(as.character(df[[ccol]]),
                                    levels = sort(unique(as.character(keyed()[[ccol]]))))

    hi <- input$highlight %||% character(0)
    df$is_hi <- df$tok_key %in% hi
    any_hi <- length(hi) > 0

    # which tokens can actually be heard -- used to mark them below, so a
    # user is not left clicking hopefully at contours that have no clip
    aud_keys <- if (isTRUE(input$mark_audio %||% TRUE)) tokens_with_audio() else character(0)

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
      legend_shown <- character(0)

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
                         legendrank = if (has_col) 1000 + as.integer(tsub$col_f[1]) else 1000,
                         showlegend = show_leg)
        }

        # ---- a small dot at the end of every contour that has audio -------
        # One trace for all of them rather than one per token: with 85 tokens
        # the per-token alternative would add 85 more traces to a plot that
        # already has one per contour, and Plotly slows down noticeably.
        if (length(aud_keys)) {
          ends <- sub[sub$tok_key %in% aud_keys & !is.na(sub$f0_num), , drop = FALSE]
          if (nrow(ends)) {
            ends <- ends[order(ends$tok_key, ends$t_num), ]
            ends <- ends[!duplicated(ends$tok_key, fromLast = TRUE), , drop = FALSE]
            p <- add_trace(p, data = ends, x = ~t_num, y = ~f0_num,
                           type = "scatter", mode = "markers",
                           marker = list(size = 7, color = "#111827",
                                         symbol = "circle",
                                         line = list(color = "#FFFFFF", width = 1.5)),
                           text = ~paste0(hover_txt, "\n♪ click to hear"),
                           hoverinfo = "text", customdata = ~tok_key,
                           showlegend = FALSE)
          }
        }

        if (!disconnect_mode && has_break) {
          bx <- unique(sub$t_num[which(sub$changed)])
          if (length(bx) && nrow(sub)) {
            yr <- range(sub$f0_num, na.rm = TRUE)
            for (b in bx) {
              p <- add_trace(p, x = c(b, b), y = yr, type = "scatter", mode = "lines",
                             line = list(color = "#7F7F7F", dash = "dash", width = 1),
                             hoverinfo = "none", showlegend = FALSE)
            }
          }
        }

        p <- layout(p, xaxis = list(title = "", zeroline = FALSE),
                    yaxis = list(title = "", zeroline = FALSE))
        p
      }

      fx <- input$facet_x; fy <- input$facet_y
      has_fx <- !is.null(fx) && fx != "(none)"
      has_fy <- !is.null(fy) && fy != "(none)"

      if (!has_fx && !has_fy) {
        final_plot <- build_panel(df) %>%
          layout(xaxis = list(title = x_title, zeroline = FALSE),
                 yaxis = list(title = "Normalised f0", zeroline = FALSE))
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

      final_plot <- layout(final_plot,
                           height = input$plot_height %||% 620,
                           font = list(size = fs_),
                           showlegend = sl)
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
      # A mean is only meaningful where several tokens contribute. On a shared
      # grid they already do; otherwise resample each token onto one first.
      mdf <- df
      if (!shared_grid) {
        k <- max(10L, min(100L, round(stats::median(as.integer(table(df$tok_key))))))
        mdf <- resample_tokens(df, k)
        validate(need(nrow(mdf) > 0, "Not enough points per token to average."))
      }
      summ <- mdf %>%
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
                  linewidth = lwm)
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
      # same colours as the individual-token view, so a group keeps its colour
      # when the view is switched
      scale_colour_manual(values = setNames(rep(QUAL_PALETTE, length.out = max(1, nlevels(df$col_f %||% factor()))), levels(df$col_f %||% factor())), limits = force) +
      scale_fill_manual(values = setNames(rep(QUAL_PALETTE, length.out = max(1, nlevels(df$col_f %||% factor()))), levels(df$col_f %||% factor())), limits = force) +
      coord_cartesian(ylim = input$ylim) +
      labs(x = x_title, y = "Normalised f0",
           colour = if (has_col) ccol else NULL,
           fill   = if (has_col) ccol else NULL) +
      theme_bw() +
      theme(panel.spacing.y = unit(0.02, "cm"),
            panel.spacing.x = unit(0.15, "cm"),
            text        = element_text(size = fs),
            axis.text   = element_text(size = as_),
            strip.text  = element_text(size = as_ + 1,
                                       margin = margin(2, 2, 2, 2)),
            legend.position = if (sl) "right" else "none")

    gp <- ggplotly(p, tooltip = "text", height = input$plot_height)
    # ggplotly names a trace "(HMML,1)" when colour and fill share a variable;
    # keep only the group label, and show each group once in the legend
    best <- list()
    for (i in seq_along(gp$x$data)) {
      tr <- gp$x$data[[i]]
      gp$x$data[[i]]$showlegend <- FALSE
      if (is.null(tr$name) || !nzchar(tr$name)) next
      nm <- sub("^\\((.*),[0-9]+\\)$", "\\1", tr$name)
      gp$x$data[[i]]$name <- nm
      gp$x$data[[i]]$legendgroup <- nm
      is_band <- !is.null(tr$fill) && !identical(tr$fill, "none")
      w <- tr$line$width %||% 0
      if (!is_band && (is.null(best[[nm]]) || w > best[[nm]][2])) best[[nm]] <- c(i, w)
    }
    for (nm in names(best)) gp$x$data[[best[[nm]][1]]]$showlegend <- TRUE
    gp %>%
      layout(hovermode = "closest",
             showlegend = sl) %>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(format = "png",
                                         filename = "inspectour_group_inspection",
                                         scale = 2))
  })

  # ---- click a line in Inspect toggles it in the highlight box ----
  # ---- selecting (not deselecting) also cues its audio, if any ----
  observeEvent(event_data("plotly_click", source = "group_src"), {
    click <- event_data("plotly_click", source = "group_src")
    req(click)
    tk <- click$customdata[1]
    req(!is.null(tk), !is.na(tk))
    cur <- input$highlight %||% character(0)
    adding <- !(tk %in% cur)
    new_sel <- if (adding) union(cur, tk) else setdiff(cur, tk)
    updateSelectizeInput(session, "highlight", selected = new_sel)
    last_played(if (adding) tk else NULL)
  })

  # =========================================================================
  #  AUDIO
  #
  #  Two modes, sharing everything downstream:
  #
  #    no TextGrids  -- each audio file is one token, matched by file name.
  #    with TextGrids -- tokens are located INSIDE longer recordings, and the
  #                      clip is cut out server side before it is served.
  #
  #  The cutting is not an optimisation, it is a requirement. Shiny's
  #  addResourcePath answers a range request with the whole file and no
  #  Accept-Ranges header, so serving a 90 MB session recording directly
  #  would make the browser download all of it to play half a second.
  #  Measured: a clip from a 97 MB / 548 s recording is ~110 KB and is cut
  #  in well under a tenth of a second, wherever in the file it sits.
  # =========================================================================

  # one served prefix per session, cleaned up when the session ends
  audio_prefix <- paste0("inspectour_audio_", session$token)
  clip_prefix  <- paste0("inspectour_clips_", session$token)
  clip_dir     <- file.path(tempdir(), paste0("clips_", session$token))

  session$onSessionEnded(function() {
    try(removeResourcePath(audio_prefix), silent = TRUE)
    try(removeResourcePath(clip_prefix), silent = TRUE)
    try(unlink(clip_dir, recursive = TRUE), silent = TRUE)
  })

  # =========================================================================
  #  FOLDER PICKER
  #
  #  A browser's file input hands the server the CONTENTS of the files you
  #  choose and deliberately withholds their location -- there is no web API
  #  that returns a folder path. So a normal "Browse..." button physically
  #  cannot fill in a directory. Since Inspectour runs locally, R can list
  #  the disk itself, which is what this does: same button, different
  #  mechanism. The text box still accepts a pasted path.
  # =========================================================================
  picker_target <- reactiveVal(NULL)
  picker_path   <- reactiveVal(NULL)

  # Derived, not stored: the listing is a pure function of the current path,
  # and having the renderer write it into a reactiveVal made the render a
  # side-effecting consumer -- which Shiny is right to object to, and which
  # also meant the click handler depended on something having been drawn.
  picker_subdirs <- reactive({
    p <- picker_path()
    if (is.null(p) || !dir.exists(p)) return(character(0))
    subs <- tryCatch(sort(basename(list.dirs(p, recursive = FALSE))),
                     error = function(e) character(0))
    # dot-folders are never where a corpus lives and would bury the real
    # ones under Library/.cache noise in a home directory
    subs[!startsWith(subs, ".")]
  })

  # resolved once so the quick-link buttons keep stable ids across renders
  quick_paths <- local({
    h <- path.expand("~")
    q <- c(Home = h,
           Desktop   = file.path(h, "Desktop"),
           Documents = file.path(h, "Documents"),
           Downloads = file.path(h, "Downloads"),
           Volumes   = "/Volumes")          # external drives on macOS
    q[dir.exists(q)]
  })

  open_picker <- function(target, current) {
    cur <- trimws(current %||% "")
    picker_target(target)
    picker_path(if (nzchar(cur) && dir.exists(cur)) normalizePath(cur) else path.expand("~"))
    showModal(modalDialog(
      title = "Choose a folder",
      uiOutput("dir_picker_ui"),
      footer = tagList(tagAppendAttributes(modalButton("Cancel"), class = "btn-sample"),
                       actionButton("dir_use", "Use this folder")),
      size = "l", easyClose = TRUE))
  }

  observeEvent(input$browse_audio_dir, open_picker("audio_dir", input$audio_dir))
  observeEvent(input$browse_tg_dir,    open_picker("tg_dir",    input$tg_dir))

  for (.i in seq_along(quick_paths)) local({
    j <- .i
    observeEvent(input[[paste0("dir_quick_", j)]], picker_path(quick_paths[[j]]))
  })

  observeEvent(input$dir_up, {
    p <- picker_path(); req(p)
    up <- dirname(p)
    if (nzchar(up) && !identical(up, p)) picker_path(up)
  })

  # folders are clicked by INDEX, not by name: a name would have to be
  # escaped into the onclick handler, and folder names contain quotes and
  # apostrophes often enough to matter
  observeEvent(input$dir_click, {
    i <- suppressWarnings(as.integer(input$dir_click))
    d <- picker_subdirs(); p <- picker_path()
    req(!is.na(i), i >= 1, i <= length(d), p)
    picker_path(normalizePath(file.path(p, d[i]), mustWork = FALSE))
  })

  observeEvent(input$dir_use, {
    tgt <- picker_target(); p <- picker_path()
    req(tgt, p)
    updateTextInput(session, tgt, value = p)
    removeModal()
  })

  output$dir_picker_ui <- renderUI({
    p <- picker_path()
    if (is.null(p)) return(NULL)
    subs <- picker_subdirs()

    aud <- list.files(p, pattern = AUDIO_EXT, ignore.case = TRUE)
    tgs <- list.files(p, pattern = TG_EXT, ignore.case = TRUE)
    n_aud <- length(aud); n_tg <- length(tgs)

    tagList(
      tags$div(class = "dir-quick",
               lapply(seq_along(quick_paths), function(i)
                 actionButton(paste0("dir_quick_", i), names(quick_paths)[i],
                              class = "btn-sample"))),
      tags$div(class = "dir-crumb", p),
      actionButton("dir_up", "↑ Up one level", class = "btn-sample"),
      tags$div(class = "dir-list",
               if (!length(subs))
                 tags$em(style = "color:#6B7280; padding:6px; display:block;",
                         "No sub-folders here.")
               else
                 lapply(seq_along(subs), function(i)
                   tags$a(href = "#",
                          onclick = sprintf(
                            "Shiny.setInputValue('dir_click', %d, {priority:'event'}); return false;", i),
                          paste0("\U0001F4C1  ", subs[i])))),
      # Name the files rather than only counting them. A count alone is
      # unfalsifiable -- "2 audio files" in a folder that visibly holds one
      # leaves nothing to check, and Finder hides things R can see.
      tags$div(class = paste("record-preview",
                             if (n_aud || n_tg) "record-preview-ready" else "record-preview-empty"),
               sprintf("%d audio file%s and %d TextGrid%s here.",
                       n_aud, if (n_aud != 1) "s" else "",
                       n_tg,  if (n_tg  != 1) "s" else ""),
               if (n_aud) tags$div(class = "clip-meta", style = "margin-top:6px;",
                                   paste(utils::head(aud, 8), collapse = "  ·  "),
                                   if (n_aud > 8) sprintf("  · +%d more", n_aud - 8)),
               if (n_tg) tags$div(class = "clip-meta", style = "margin-top:3px;",
                                  paste(utils::head(tgs, 8), collapse = "  ·  "),
                                  if (n_tg > 8) sprintf("  · +%d more", n_tg - 8)))
    )
  })

  # ---- bundled sample audio -------------------------------------------------
  # One click stands in for both the recordings and the TextGrids boxes. It
  # also loads the sample dataset, since the recordings only match that.
  # Anything you then supply yourself (a folder or an upload, in either
  # step) switches the sample off again, so it never competes with your own.
  use_sample_audio <- reactiveVal(FALSE)

  observeEvent(input$load_sample_audio, {
    if (!nzchar(sample_audio_dir())) {
      showNotification("No sample audio is bundled with this copy of the app.",
                       type = "warning")
      return()
    }
    if (!identical(data_source()$kind, "sample")) data_source(list(kind = "sample"))
    updateTextInput(session, "audio_dir", value = "")
    updateTextInput(session, "tg_dir", value = "")
    use_sample_audio(TRUE)
    # take them straight to what it produced
    session$sendCustomMessage("scrollToResult", TRUE)
  })

  sample_off <- function(v) if (use_sample_audio() && length(v) && any(nzchar(trimws(v))))
    use_sample_audio(FALSE)
  observeEvent(input$audio_dir,   sample_off(input$audio_dir),   ignoreInit = TRUE)
  observeEvent(input$tg_dir,      sample_off(input$tg_dir),      ignoreInit = TRUE)
  observeEvent(input$audio_files, sample_off(input$audio_files$name), ignoreInit = TRUE)
  observeEvent(input$tg_files,    sample_off(input$tg_files$name),    ignoreInit = TRUE)
  # a different dataset won't match the sample recordings
  observeEvent(data_source(), {
    if (!identical(data_source()$kind, "sample")) use_sample_audio(FALSE)
  }, ignoreInit = TRUE)

  # ---- where the recordings are: a folder, or uploaded files --------------
  audio_root <- reactive({
    if (use_sample_audio()) return(sample_audio_dir())
    up  <- input$audio_files
    dir <- trimws(input$audio_dir %||% "")

    if (nzchar(dir) && dir.exists(dir)) return(normalizePath(dir))
    if (!is.null(up) && nrow(up)) {
      d <- file.path(tempdir(), paste0("audio_", session$token))
      if (!dir.exists(d)) dir.create(d, recursive = TRUE)
      # Shiny renames uploads (0.wav, 1.wav...); restore the original names
      # so that they can be matched against token ids
      file.copy(up$datapath, file.path(d, up$name), overwrite = TRUE)
      return(d)
    }
    NULL
  })

  audio_files_tbl <- reactive({
    root <- audio_root()
    if (is.null(root)) return(NULL)
    f <- list.files(root, pattern = AUDIO_EXT, ignore.case = TRUE)
    if (!length(f)) return(NULL)
    addResourcePath(audio_prefix, root)
    data.frame(file = tools::file_path_sans_ext(f), name = f,
               path = file.path(root, f),
               size = file.size(file.path(root, f)),
               stringsAsFactors = FALSE)
  })

  # Same idea as the TextGrid "read from the same folder" note below -- a
  # small confirmation that files were actually found, right where you'd
  # otherwise wonder whether the folder path or the upload took. The folder
  # box and the upload box both feed audio_root(), so this just reports
  # which of the two actually supplied them.
  output$audio_files_note <- renderUI({
    tbl <- audio_files_tbl()
    if (is.null(tbl)) return(NULL)
    dir <- trimws(input$audio_dir %||% "")
    from_dir <- nzchar(dir) && dir.exists(dir)
    n <- nrow(tbl)
    tags$div(class = "record-preview record-preview-partial",
             sprintf("%d %srecording%s %s.", n,
                     if (use_sample_audio()) "sample " else "",
                     if (n != 1) "s" else "",
                     if (use_sample_audio()) "loaded"
                     else if (from_dir) "read from this folder" else "uploaded"))
  })

  # ---- where the TextGrids are -------------------------------------------
  tg_root <- reactive({
    if (use_sample_audio()) {
      d <- sample_audio_dir()
      return(if (length(list.files(d, pattern = TG_EXT, ignore.case = TRUE))) d else NULL)
    }
    up  <- input$tg_files
    dir <- trimws(input$tg_dir %||% "")
    if (nzchar(dir) && dir.exists(dir)) return(normalizePath(dir))
    if (!is.null(up) && nrow(up)) {
      d <- file.path(tempdir(), paste0("tg_", session$token))
      if (!dir.exists(d)) dir.create(d, recursive = TRUE)
      file.copy(up$datapath, file.path(d, up$name), overwrite = TRUE)
      return(d)
    }
    NULL
  })

  # Grids usually sit beside their recordings, so the box fills itself in when
  # a recordings folder is chosen that contains some. Filling the box, rather
  # than quietly searching that folder behind an empty box, means what the app
  # is reading is always the path you can see -- and it stays editable, so
  # grids kept somewhere else are a matter of changing it.
  observeEvent(input$audio_dir, {
    d <- trimws(input$audio_dir %||% "")
    if (!nzchar(d) || !dir.exists(d)) return()
    if (nzchar(trimws(input$tg_dir %||% ""))) return()   # never overwrite a choice
    # mirror the path exactly as it was entered rather than normalizePath()'s
    # resolved form: through a symlinked folder the two differ, and quietly
    # rewriting a path someone just chose reads as the app having gone
    # somewhere else
    if (length(list.files(d, pattern = TG_EXT, ignore.case = TRUE)))
      updateTextInput(session, "tg_dir", value = d)
  })

  tg_intervals <- reactive({
    root <- tg_root()
    if (is.null(root)) return(NULL)
    paths <- list.files(root, pattern = TG_EXT, ignore.case = TRUE, full.names = TRUE)
    if (!length(paths)) return(NULL)
    textgrid_read_dir(paths)
  })

  # A label like "47 1 HHRF" is a space-separated record, but not every
  # corpus's convention is space -- some use underscores, some dashes.
  # Guessed from the tier's own labels (am_guess_label_sep()) rather than
  # asked: nobody actually knows the answer as a regex, and the labels
  # already say it themselves.
  tg_label_sep_regex <- reactive({
    iv <- tg_intervals()
    if (is.null(iv)) return("[[:space:]]+")
    tier <- input$tg_tier %||% ""
    am_guess_label_sep(iv$label[iv$tier == tier])
  })

  # ---- tier + label settings, offered only once grids are actually found --
  output$tg_tier_ui <- renderUI({
    iv <- tg_intervals()
    if (is.null(iv)) return(NULL)
    errs <- attr(iv, "errors") %||% character(0)
    tiers <- unique(iv$tier)
    if (!length(tiers)) {
      return(tags$div(class = "near-miss-box",
                      "TextGrids were found, but none contain an interval tier."))
    }
    lbl <- sprintf("%s (%d intervals)", tiers, as.integer(table(iv$tier)[tiers]))

    tagList(
      if (length(errs))
        tags$div(class = "near-miss-box",
                 tags$b(length(errs)), " TextGrid(s) could not be read: ",
                 paste(utils::head(names(errs), 3), collapse = ", ")),
      # tiers hold labels, not tokens themselves -- a token is what those
      # labels, read together, identify.
      #
      # This whole block re-renders on every keystroke in the folder path
      # boxes above (tg_intervals() reads input$tg_dir directly), so the
      # tier choice reads its own previous value back through isolate() --
      # otherwise typing in a path field would silently reset it.
      field_block("Tier with the token labels", NULL,
                  selectInput("tg_tier", label = NULL,
                              choices = setNames(tiers, lbl),
                              selected = local({
                                prev <- isolate(input$tg_tier)
                                if (!is.null(prev) && prev %in% tiers) prev else tiers[1]
                              }))),
      # what was read and where from, and the first label on the chosen
      # tier, as one box -- see tg_label_preview below
      uiOutput("tg_label_preview")
    )
  })

  # One box saying what was actually read: how many grids and from where,
  # then the first label on the chosen tier as a sanity check. Kept whole --
  # splitting a label into parts is a step 3 decision, shown there beside
  # the control that uses it. Its own output (not drawn inside tg_tier_ui)
  # so changing the tier only redraws this box, never the tier picker.
  output$tg_label_preview <- renderUI({
    iv <- tg_intervals()
    if (is.null(iv) || !nrow(iv)) return(NULL)
    n_tg <- length(unique(iv$file))
    same_dir <- local({
      a <- trimws(input$audio_dir %||% ""); g <- trimws(input$tg_dir %||% "")
      nzchar(a) && nzchar(g) && dir.exists(a) && dir.exists(g) &&
        identical(normalizePath(a), normalizePath(g))
    })
    read_line <- if (same_dir)
      sprintf("%d TextGrid%s read from the same folder as your recordings, filled in above. Change that path if your grids are elsewhere.",
              n_tg, if (n_tg != 1) "s" else "")
    else if (use_sample_audio())
      sprintf("%d sample TextGrid%s loaded.", n_tg, if (n_tg != 1) "s" else "")
    else sprintf("%d TextGrid%s read.", n_tg, if (n_tg != 1) "s" else "")
    tier <- input$tg_tier %||% unique(iv$tier)[1]
    labs <- trimws(iv$label[iv$tier == tier])
    labs <- labs[nzchar(labs)]
    tags$div(class = "record-preview record-preview-partial tg-read-note",
             tags$div(read_line),
             if (length(labs))
               tags$div(style = "margin-top: 4px;", "First label: ", tags$b(labs[1])))
  })

  # ---- which dataset columns jointly identify a token in the audio --------
  # The token id was already settled on this page, so the audio mapping
  # inherits it instead of asking a second time: its component columns when
  # it was built from several, otherwise the single column itself (labels
  # that read S1_3_ct outright are a normal arrangement). If neither can be
  # produced from the audio, the preview says so and the fold below lets a
  # different set of columns be chosen.
  # A dataset can carry a token id column AND have one built from several
  # columns. Both name the same production, but they are different strings,
  # and only the TextGrid labels can settle which of the two they spell out.
  # So that question is asked here, where the labels are in view, rather than
  # guessed on the Data page where they are not.
  two_token_ids <- reactive({
    length(token_id_active_parts()) >= 2 && nzchar(token_col())
  })

  default_match_cols <- reactive({
    parts <- token_id_active_parts()
    tc    <- token_col()
    # The bundled recording names tokens by speaker (file name) and item
    # number (label), not by the sample's token_id, so the sample starts
    # out matching on those two columns -- the setup it would otherwise take
    # a first-time user several steps to find.
    if (use_sample_audio() && all(SAMPLE_AUDIO_MATCH %in% col_info()$cols))
      return(SAMPLE_AUDIO_MATCH)
    if (two_token_ids())
      return(if (identical(input$audio_id_basis %||% "parts", "column")) tc else parts)
    if (length(parts) >= 2) return(parts)
    if (nzchar(tc)) return(tc)
    sp <- input$col_speaker %||% ""
    c(sp[sp %in% col_info()$cols])
  })

  # The token id as Data specifics itself treats it -- the same columns
  # keyed() uses for tok_key -- with none of default_match_cols()'s own
  # two-token-ids branching folded in. "Your token id, set above" reads
  # this, so it always states what was actually set on that page and never
  # flips when a later, audio-only choice (which of two ids the labels
  # spell out, or a custom override below) changes what is matched on.
  data_token_id_cols <- reactive({
    parts <- token_id_active_parts()
    if (length(parts) >= 2) return(parts)
    tc <- token_col()
    if (nzchar(tc)) return(tc)
    sp <- input$col_speaker %||% ""
    c(sp[sp %in% col_info()$cols])
  })

  # Switching the basis has to reach the override below, or the remembered
  # selection there would quietly outrank the answer just given.
  observeEvent(default_match_cols(), {
    updateSelectizeInput(session, "audio_match_cols", selected = default_match_cols())
  }, ignoreInit = TRUE)

  match_cols <- reactive({
    d <- default_match_cols()
    m <- input$audio_match_cols %||% character(0)
    if (!length(m)) return(d)
    # Kept in exactly the order it was picked in. Matching is a join and does
    # not care, but the example token id and the rows below both follow this
    # order, and quietly re-sorting what someone typed reads as the app
    # knowing better. (The picker lists chosen columns first, in this same
    # order, so a re-render never shuffles it either.)
    m
  })

  token_tbl <- reactive({
    df <- keyed()
    cols <- match_cols()
    req(length(cols) > 0, all(cols %in% names(df)))
    u <- unique(df[, c("tok_key", cols), drop = FALSE])
    u[!duplicated(u$tok_key), , drop = FALSE]
  })

  # ---- grouped tokens from the chosen tier -------------------------------
  # Whether a token spans several intervals. The checkbox draws ticked by
  # default, so an unset value (before the browser has echoed it back) means
  # ticked everywhere this is read -- otherwise the first render groups one
  # way and the next another, and a guess made in between goes stale.
  multi_part_on <- reactive({
    v <- input$tg_multi_part
    is.null(v) || isTRUE(v)
  })

  all_grouped_tokens <- reactive({
    iv <- tg_intervals()
    if (is.null(iv)) return(NULL)
    tier <- input$tg_tier %||% ""
    sub <- iv[iv$tier == tier, , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    # intervals sharing the chosen label part are one token (part 1 unless
    # told otherwise -- the item number, in the usual layout); unticked,
    # every interval is a token of its own
    kf <- suppressWarnings(as.integer(input$tg_key_field %||% 1))
    if (is.na(kf) || kf < 1) kf <- 1L
    audio_group_tokens(sub, key_field = if (multi_part_on()) kf else NA,
                       label_sep = tg_label_sep_regex())
  })

  # The same grouping, told how many intervals a token spans once a length
  # is picked, so that a repeat said without a pause (and without a counter
  # in its labels) is still cut into productions of that length.
  chunked_grouped_tokens <- reactive({
    np <- if (multi_part_on()) input$tg_nparts %||% nparts_default() else "all"
    if (!nzchar(np) || np == "all") return(all_grouped_tokens())
    iv <- tg_intervals()
    sub <- iv[iv$tier == (input$tg_tier %||% ""), , drop = FALSE]
    kf <- suppressWarnings(as.integer(input$tg_key_field %||% 1))
    if (is.na(kf) || kf < 1) kf <- 1L
    audio_group_tokens(sub, key_field = kf, label_sep = tg_label_sep_regex(),
                       chunk = as.integer(np))
  })

  # How many intervals a token is made of, listed from what the grouping
  # actually found, with counts, so the answer is a pick rather than a guess.
  # A recording that mixes lengths restarts item numbers in each, and picking
  # the one the dataset describes keeps a disyllable from also collecting the
  # monosyllable that happens to share its number.
  #
  # "Any number" is "all", not "": selectize treats an empty value as its
  # placeholder rather than an option, so once something else was picked
  # there was no way back to it.
  # the sample recording holds monosyllables and disyllables whose item
  # numbers overlap; the sample dataset is all disyllables
  nparts_default <- reactive(if (use_sample_audio()) SAMPLE_AUDIO_NPARTS else "all")
  observeEvent(use_sample_audio(), {
    if (use_sample_audio() && !is.null(isolate(input$tg_nparts)))
      updateSelectInput(session, "tg_nparts", selected = SAMPLE_AUDIO_NPARTS)
  }, ignoreInit = TRUE)

  nparts_choices <- reactive({
    tk <- all_grouped_tokens()
    if (is.null(tk) || !nrow(tk)) return(c("Any number" = "all"))
    tb <- table(tk$n_parts)
    c("Any number" = "all",
      setNames(names(tb), sprintf("%s interval%s (%d tokens)", names(tb),
                                  ifelse(names(tb) == "1", "", "s"), as.integer(tb))))
  })

  # The dropdown is drawn once (in audio_map_parts) and only its choices are
  # updated afterwards. Redrawing it -- as it used to be, inside a block that
  # re-rendered whenever the grouping changed -- threw away the pick the
  # moment it was made, since picking changes the grouping.
  observeEvent(nparts_choices(), {
    ch <- nparts_choices()
    prev <- isolate(input$tg_nparts) %||% nparts_default()
    updateSelectInput(session, "tg_nparts", choices = ch,
                      selected = if (prev %in% ch) prev else "all")
  }, ignoreInit = TRUE)

  grouped_tokens <- reactive({
    tk <- chunked_grouped_tokens()
    if (is.null(tk)) return(NULL)
    # only meaningful while tokens are grouped; once unticked every interval
    # is a token of one, and a leftover "2" would otherwise empty the list
    np <- if (multi_part_on()) input$tg_nparts %||% nparts_default() else "all"
    if (nzchar(np) && np != "all") tk <- tk[tk$n_parts == as.integer(np), , drop = FALSE]
    if (!nrow(tk)) return(NULL)
    tk
  })

  # ---- source options, and a first guess at the right one ----------------
  source_choices <- function(n_label_fields, n_file_parts, const_val = NULL) {
    ch <- c("TextGrid label (whole)" = "label")
    if (n_label_fields > 1)
      ch <- c(ch, setNames(paste0("label:", seq_len(n_label_fields)),
                           sprintf("TextGrid label, part %d", seq_len(n_label_fields))))
    ch <- c(ch, "File name (whole)" = "filestem")
    if (n_file_parts > 1)
      ch <- c(ch, setNames(paste0("file:", seq_len(n_file_parts)),
                           sprintf("File name, part %d", seq_len(n_file_parts))))
    if (!is.null(const_val))
      ch <- c(ch, setNames(paste0("const:", const_val),
                           sprintf("Always \"%s\"", const_val)))
    ch
  }

  # Score each candidate by how much of the column's real range of values it
  # can actually produce. Picking the best one automatically means the usual
  # arrangement (speaker in the file name, item number in the label) needs no
  # configuration at all, while anything unusual is still adjustable.
  guess_source <- function(col, tk, choices) {
    want <- unique(token_tbl()[[col]])
    sc <- vapply(choices, function(src) {
      cv <- am_source_coverage(tk, src, want, label_sep = tg_label_sep_regex())
      if (cv$total == 0) 0 else cv$n / cv$total
    }, numeric(1))
    if (max(sc) <= 0) return(choices[1])
    choices[which.max(sc)]
  }

  # Step 3 is four sibling outputs (see ui.R), not one. As a single block it
  # redrew every control inside it whenever any of them changed the
  # grouping -- so typing a new position number rebuilt the box mid-keystroke
  # and emptied it, and picking a token length reset the dropdown to its old
  # value. Each piece now depends only on what it actually shows, and the
  # controls a person types into or picks from are never redrawn by their
  # own effect.

  # TextGrids read on the chosen tier -- deliberately NOT grouped_tokens(),
  # which changes with the position number and length picked below
  grids_ready <- reactive({
    iv <- tg_intervals()
    !is.null(iv) && nrow(iv) > 0 && (input$tg_tier %||% "") %in% iv$tier
  })

  # what gates the whole step, in the order it has to be resolved
  map_gate <- reactive({
    if (is.null(audio_files_tbl())) return("no_audio")
    if (!token_id_ready()) return("no_token_id")
    if (!grids_ready()) return("no_grids")
    "ok"
  })

  # the token id, the two-ids question, and the override
  output$audio_map_head <- renderUI({
    gate <- map_gate()
    if (gate == "no_audio") return(helpText("Add recordings above to continue."))
    # Without a token id there is nothing for a label or file name to be
    # mapped onto, and the section below would otherwise just render its
    # pieces empty (e.g. "Your token id, set above: " with nothing after
    # the colon). Say so directly and point back to where it is set, rather
    # than leaving the user to guess why the section looks broken.
    if (gate == "no_token_id")
      return(tags$div(class = "near-miss-box",
        "Set a ", tags$a(class = "jump-link", href = "#token_id_row", "Unique token ID"),
        " above first — there is no token id yet for a label or file name to be mapped to."))
    if (gate == "no_grids")
      return(helpText("Without TextGrids, each audio file is matched whole to the",
                      "token named by its file name, e.g. S1_3_ct.wav."))
    ci <- col_info()
    # read, not subscribed to: the override updates itself in place (see the
    # observer on default_match_cols()), so redrawing it here on every pick
    # would only close its dropdown under the person typing into it
    cols <- isolate(match_cols())

    tagList(
      # Always the id as Data specifics set it -- never swapped out for
      # whatever is actually being matched on, so a custom override below or
      # an answer to the two-ids question never makes this line lie about
      # what was configured up there.
      tags$div(class = "map-inherit",
               "Your token id, set ",
               tags$a(class = "jump-link", href = "#token_id_row", "above"),
               ": ", tags$b(paste(data_token_id_cols(), collapse = " + "))),
      if (two_token_ids())
        tags$div(class = "map-inherit",
                 tags$div(class = "map-inherit-q",
                          "You set up two token ids. Which one do your TextGrid labels spell out?"),
                 radioButtons(
                   "audio_id_basis", label = NULL,
                   choices = setNames(
                     c("parts", "column"),
                     c(paste("the combined id:",
                             paste(token_id_active_parts(), collapse = " + ")),
                       paste("the", token_col(), "column"))),
                   selected = isolate(input$audio_id_basis) %||% "parts")),
      # The override lives right under the id it overrides, and its
      # open/closed state is read back from a JS listener (see ui.R), so a
      # redraw never snaps an opened fold shut.
      tags$details(class = "map-cols-more",
        open = if (isTRUE(isolate(input$map_cols_open))) NA else NULL,
        tags$summary("The TextGrids name tokens differently"),
        tags$div(class = "field-help", style = "margin: 4px 0 6px;",
                 paste("Match on another set of columns instead. They have to",
                       "tell your tokens apart between them.")),
        # the chosen columns go first, in the order they were picked:
        # selectize lays selected items out in the order of the choices, so
        # an alphabetical choice list would silently re-sort what was typed
        selectizeInput("audio_match_cols", label = NULL,
                       choices = c(cols, setdiff(ci$cols, cols)), selected = cols,
                       multiple = TRUE,
                       options = list(placeholder = "pick columns...")))
    )
  })

  # only when what is matched on differs from the token id stated above
  output$audio_map_matching <- renderUI({
    if (map_gate() != "ok") return(NULL)
    cols <- match_cols()
    if (identical(cols, data_token_id_cols())) return(NULL)
    tags$div(class = "map-inherit",
             "Matching on: ", tags$b(paste(cols, collapse = " + ")))
  })

  # Part specification: whether a token spans more than one interval (on by
  # default -- fieldwork audio is usually segmented per syllable), then which
  # part of the label the pieces of one token share, then how many intervals
  # that turns out to give. The last is counted, not asked: grouping by the
  # shared part shows what lengths the recording actually holds. Depends on
  # the tier and its labels only; every control reads its own previous value
  # back through isolate(), so nothing picked is ever redrawn by its own effect.
  output$audio_map_parts <- renderUI({
    if (map_gate() != "ok") return(NULL)
    iv <- tg_intervals()
    labs <- trimws(iv$label[iv$tier == (input$tg_tier %||% "")])
    labs <- labs[nzchar(labs)]
    n_parts <- max(1L, lengths(strsplit(labs, tg_label_sep_regex())))
    key_ch <- setNames(as.character(seq_len(n_parts)), sprintf("part %d", seq_len(n_parts)))
    prev_key <- isolate(input$tg_key_field) %||% "1"
    ch <- isolate(nparts_choices())
    prev_np <- isolate(input$tg_nparts) %||% nparts_default()

    tagList(
      checkboxInput("tg_multi_part",
                    "A token spans more than one interval",
                    value = local({
                      prev <- isolate(input$tg_multi_part)
                      if (is.null(prev)) TRUE else isTRUE(prev)
                    })),
      conditionalPanel(
        "input.tg_multi_part",
        field_block("Which part of the label tells which intervals belong to the same token?", NULL,
                    tagList(
                      # the example IS the explanation -- see tg_key_example
                      uiOutput("tg_key_example"),
                      selectInput("tg_key_field", label = NULL, choices = key_ch,
                                  selected = if (prev_key %in% key_ch) prev_key else "1"))),
        # A session recording often holds several token pools --
        # monosyllables, disyllables, trisyllables -- and an item number
        # restarts in each one. Without this, citation "03" matches in all
        # three at once and a disyllable silently acquires the
        # monosyllable's audio too.
        field_block("How many intervals does a token span?", NULL,
                    selectInput("tg_nparts", label = NULL, choices = ch,
                                selected = if (prev_np %in% ch) prev_np else "all")),
        uiOutput("tg_split_note"))
    )
  })

  # How the same item said more than once was told apart, since nothing
  # above asks it: a part of the label that counts the intervals, a pause,
  # or the token length just picked. Only shown when repeats were found.
  output$tg_split_note <- renderUI({
    if (map_gate() != "ok" || !multi_part_on()) return(NULL)
    tk <- chunked_grouped_tokens()
    sb <- attr(tk, "split_by")
    if (is.null(tk) || is.null(sb)) return(NULL)
    kf <- suppressWarnings(as.integer(input$tg_key_field %||% 1))
    # a repeat is the same item again right after itself -- item numbers
    # that recur across the mono-, di- and trisyllable lists are not
    tk <- tk[order(tk$file, tk$xmin), , drop = FALSE]
    key <- paste(tk$file, am_field(tk$label, kf, tg_label_sep_regex()))
    n_rep <- if (length(key) > 1) sum(key[-1] == key[-length(key)]) else 0L
    how <- c(
      if (!is.na(sb$counter))
        sprintf("part %d of the label, which counts the intervals (1, 2, ...) and starts again with each production", sb$counter),
      if (!is.na(sb$pause))
        sprintf("a pause of %.2f s or more between intervals", sb$pause),
      if (!is.na(sb$chunk))
        sprintf("cutting a run of intervals that share part %d into tokens of %d", kf, sb$chunk))
    if (!length(how) || n_rep == 0) return(NULL)
    tags$div(class = "field-help", style = "margin: -4px 0 10px;",
             sprintf("Some items were said more than once (%d). Repeats are told apart by %s.",
                     n_rep, paste(how, collapse = ", then by ")))
  })

  # A real token that spans several intervals under the current choice,
  # shown whole and then with its first label numbered part by part, so the
  # shared part can simply be read off it. Before anything groups (a part
  # that differs between pieces was picked), the first label alone. Its own
  # output, so following the choice never redraws the dropdown beside it.
  output$tg_key_example <- renderUI({
    if (map_gate() != "ok") return(NULL)
    iv <- tg_intervals()
    labs <- trimws(iv$label[iv$tier == (input$tg_tier %||% "")])
    labs <- labs[nzchar(labs)]
    tk <- all_grouped_tokens()
    multi <- if (!is.null(tk)) tk[tk$n_parts > 1, , drop = FALSE] else NULL
    lab <- if (!is.null(multi) && nrow(multi)) trimws(multi$label[1])
           else if (length(labs)) labs[1] else ""
    if (!nzchar(lab)) return(NULL)
    parts0 <- strsplit(lab, tg_label_sep_regex())[[1]]
    numbered <- paste(sprintf("part %d = %s", seq_along(parts0), parts0), collapse = ", ")
    if (!is.null(multi) && nrow(multi)) {
      pieces <- strsplit(multi$all_labels[1], " + ", fixed = TRUE)[[1]]
      tags$div(class = "record-preview record-preview-partial",
               tags$div(sprintf("Example token (%d intervals): ", length(pieces)),
                        HTML(paste(sprintf("<b>%s</b>", htmltools::htmlEscape(trimws(pieces))),
                                   collapse = " + "))),
               tags$div(style = "margin-top: 4px;", "Its first label: ", numbered))
    } else {
      tags$div(class = "record-preview record-preview-partial",
               "Example label: ", tags$b(lab), " → ", numbered)
    }
  })

  # where each part of the token id comes from -- the one piece that does
  # need to follow the grouping, since label parts and guesses depend on it
  output$audio_map_rows <- renderUI({
    if (map_gate() != "ok") return(NULL)
    tk <- grouped_tokens()
    if (is.null(tk)) return(NULL)

    # Parts of the LABEL only exist as a mapping option once a token is
    # actually said to span more than one interval -- otherwise "part 1" and
    # "the whole label" are the same thing, and offering both is just noise.
    #
    # Read with the same NULL-means-TRUE fallback as the checkbox's own
    # default, rather than a bare isTRUE(). Without this, the very first
    # render (before the browser has echoed the checkbox's default back)
    # sees multi_part as FALSE while the checkbox itself draws checked --
    # and a source guessed under that mismatch (whole label, since parts
    # weren't offered yet) then reads as "already chosen" once parts do
    # appear, silently outranking the better guess.
    multi_part <- multi_part_on()
    n_lab <- if (multi_part)
      max(1L, max(lengths(strsplit(trimws(tk$label), tg_label_sep_regex())))) else 1L
    n_fil <- max(1L, max(lengths(strsplit(tk$file, "_", fixed = TRUE))))
    cols <- match_cols()

    rows <- lapply(cols, function(cl) {
      vals <- unique(as.character(token_tbl()[[cl]]))
      const_val <- if (length(vals) == 1) vals else NULL
      ch <- source_choices(n_lab, n_fil, const_val)
      # A remembered choice is kept only if it is still on offer. Never let a
      # stale selection from an earlier configuration silently outrank a
      # guess that would actually work.
      sel <- isolate(input[[paste0("aud_src_", cl)]])
      if (is.null(sel) || !(sel %in% ch)) sel <- guess_source(cl, tk, ch)
      tags$div(class = "map-row",
               tags$span(class = "map-col", cl),
               tags$span(class = "map-arrow", "←"),
               selectInput(paste0("aud_src_", cl), label = NULL,
                           choices = ch, selected = sel, width = "260px"))
    })

    tagList(
      tags$div(style = "margin: 10px 0 8px; font-weight: 600; color: #1F2430; font-size: 13.5px;",
               "Where each part is found in the textgrids:"),
      rows)
  })

  # Kept as its own output rather than drawn inside the rows above: that one
  # reads the dropdowns through isolate() (so that changing a choice does not
  # rebuild every control and lose focus), which would leave an inline
  # preview frozen at whatever it said when the row was first drawn.
  output$audio_map_preview <- renderUI({
    if (map_gate() != "ok") return(NULL)
    tk <- grouped_tokens()
    if (is.null(tk)) return(NULL)
    cols <- match_cols()

    # Too few columns is the quietest way to get this wrong: matching on
    # speaker alone attaches every recording in the file to one token, and
    # the result still reports a match. Say how far the chosen columns
    # actually get, and name the column that would help most.
    tt <- token_tbl()
    sig <- do.call(paste, c(lapply(cols, function(cl) am_norm(tt[[cl]])), list(sep = "\r")))
    n_uniq <- length(unique(sig))
    not_unique <- NULL
    if (n_uniq < nrow(tt)) {
      spare <- setdiff(col_info()$cat_cols, c(cols, "tok_key"))
      best <- NULL
      if (length(spare)) {
        gain <- vapply(spare, function(cl) {
          length(unique(paste(sig, am_norm(keyed()[[cl]][match(tt$tok_key, keyed()$tok_key)]))))
        }, numeric(1))
        if (max(gain) > n_uniq) best <- spare[which.max(gain)]
      }
      not_unique <- tags$div(
        class = "near-miss-box",
        sprintf("These columns tell apart only %d of your %d tokens, so one recording would be attached to several of them.",
                n_uniq, nrow(tt)),
        if (!is.null(best)) tagList(" Adding ", tags$b(best), " would help."))
    }
    lines <- lapply(cols, function(cl) {
      src <- input[[paste0("aud_src_", cl)]]
      if (is.null(src)) return(NULL)
      vals <- unique(as.character(token_tbl()[[cl]]))
      cv <- am_source_coverage(tk, src, vals, label_sep = tg_label_sep_regex())
      ok <- cv$n > 0
      # no arrow here: the map-row above already uses one to mean "comes
      # from", pointing the other way, and a second arrow reading the
      # opposite direction was confusing rather than clarifying
      txt <- if (ok)
        sprintf("%s gives %s, matching %d of %d %s value%s",
                cl, paste(utils::head(cv$got, 3), collapse = ", "),
                cv$n, cv$total, cl, if (cv$total != 1) "s" else "")
      else
        sprintf("%s gives %s, matching none of the %d %s value%s",
                cl, paste(utils::head(cv$got, 3), collapse = ", "),
                cv$total, cl, if (cv$total != 1) "s" else "")
      tags$div(class = paste("map-preview", if (!ok) "bad"), txt)
    })

    # Put the columns back together the same way the dataset's own token id
    # is built, so what shows here is directly comparable to a real
    # tok_key -- not just the per-column pieces from the lines below.
    example <- {
      vals <- vapply(cols, function(cl) {
        src <- input[[paste0("aud_src_", cl)]]
        if (is.null(src)) return(NA_character_)
        v <- tryCatch(am_resolve_source(tk[1, , drop = FALSE], src,
                                        label_sep = tg_label_sep_regex()),
                     error = function(e) NA_character_)
        if (!length(v)) NA_character_ else v[1]
      }, character(1))
      if (any(is.na(vals) | !nzchar(vals))) NULL
      else paste(vals, collapse = input$token_id_sep %||% "_")
    }

    tagList(
      if (!is.null(example))
        tags$div(class = "record-preview record-preview-partial",
                 "Example match: ", tags$b(example)),
      tags$div(class = "field-help", style = "margin:10px 0 4px;",
               "Produces:"), lines, not_unique)
  })

  audio_sources <- reactive({
    cols <- match_cols()
    s <- lapply(cols, function(cl) input[[paste0("aud_src_", cl)]])
    names(s) <- cols
    s[!vapply(s, is.null, logical(1))]
  })

  # ---- the match itself ---------------------------------------------------
  audio_match <- reactive({
    tk <- grouped_tokens()
    if (is.null(tk) || is.null(audio_files_tbl())) return(NULL)
    cols <- match_cols()
    src <- audio_sources()
    if (!length(cols) || !all(cols %in% names(src))) return(NULL)
    tt <- token_tbl()
    tryCatch(
      audio_build_index(tk, tt, cols, src, label_sep = tg_label_sep_regex(),
                        # the sample's labels pad item numbers ("03"), its data does not
                        loose = isTRUE(input$audio_loose) || use_sample_audio()),
      error = function(e) structure(list(error = conditionMessage(e)), class = "audio_err"))
  })

  # ---- one row per playable clip: tok_key, file path, start, end ----------
  # Falls back to whole-file-per-token when there are no TextGrids, so the
  # original behaviour still works untouched.
  audio_index <- reactive({
    af <- audio_files_tbl()
    empty <- data.frame(tok_key = character(0), path = character(0),
                        name = character(0), xmin = numeric(0), xmax = numeric(0),
                        occurrence = integer(0), n_parts = integer(0),
                        stringsAsFactors = FALSE)
    if (is.null(af)) return(empty)

    m <- audio_match()
    if (is.null(m) || inherits(m, "audio_err")) {
      # ---- no TextGrids: match file stems to token ids, as before --------
      keys <- sort(unique(keyed()$tok_key))
      i <- match(am_norm(keys), am_norm(af$file))
      j <- which(is.na(i))
      if (length(j)) {
        k <- match(tolower(am_norm(keys[j])), tolower(am_norm(af$file)))
        i[j] <- k
      }
      ok <- !is.na(i)
      if (!any(ok)) return(empty)
      return(data.frame(tok_key = keys[ok], path = af$path[i[ok]],
                        name = af$name[i[ok]], xmin = NA_real_, xmax = NA_real_,
                        occurrence = 1L, n_parts = 1L, stringsAsFactors = FALSE))
    }

    idx <- m$index
    if (!nrow(idx)) return(empty)
    # NOT match(): the grid's stem and its recording's stem are often not
    # identical (ProsodyPro writes <name>.TextGrid beside <name>_original.WAV)
    i <- audio_pair_files(idx$file, af$file)
    ok <- !is.na(i)
    data.frame(tok_key = idx$tok_key[ok], path = af$path[i[ok]],
               name = af$name[i[ok]], xmin = idx$xmin[ok], xmax = idx$xmax[ok],
               occurrence = idx$occurrence[ok], n_parts = idx$n_parts[ok],
               stringsAsFactors = FALSE)
  })

  tokens_with_audio <- reactive(unique(audio_index()$tok_key))

  output$hasAudio <- reactive(length(tokens_with_audio()) > 0)
  outputOptions(output, "hasAudio", suspendWhenHidden = FALSE)

  # ---- cut a clip on demand, and cache it ---------------------------------
  # Only ever called for tokens the user has actually selected, so a corpus
  # of thousands costs nothing until something is clicked.
  clip_url <- function(row) {
    if (!dir.exists(clip_dir)) dir.create(clip_dir, recursive = TRUE)
    addResourcePath(clip_prefix, clip_dir)

    # whole-file mode, or a format that cannot be cut without re-encoding
    if (is.na(row$xmin) || !grepl("\\.wav$", row$name, ignore.case = TRUE)) {
      addResourcePath(audio_prefix, dirname(row$path))
      return(paste0(audio_prefix, "/", utils::URLencode(row$name, reserved = TRUE)))
    }

    safe <- gsub("[^A-Za-z0-9._-]", "-", paste0(row$tok_key, "__", row$occurrence))
    out  <- file.path(clip_dir, paste0(safe, ".wav"))
    if (!file.exists(out)) {
      ok <- tryCatch({ wav_clip(row$path, out, row$xmin, row$xmax, pad = CLIP_PAD); TRUE },
                     error = function(e) FALSE)
      if (!ok) {
        addResourcePath(audio_prefix, dirname(row$path))
        return(paste0(audio_prefix, "/", utils::URLencode(row$name, reserved = TRUE)))
      }
    }
    paste0(clip_prefix, "/", utils::URLencode(basename(out), reserved = TRUE))
  }

  # ---- what happened, in plain language -----------------------------------
  output$audio_status <- renderUI({
    if (is.null(audio_files_tbl())) return(NULL)
    n_tok <- dplyr::n_distinct(keyed()$tok_key)
    idx <- audio_index()
    n_aud <- dplyr::n_distinct(idx$tok_key)
    m <- audio_match()

    if (inherits(m, "audio_err"))
      return(tags$div(class = "near-miss-box", tags$b("Could not match: "), m$error))

    matched_lbl <- if (n_aud == 0) sprintf("In this dataset, 0 of %d tokens have a matched recording", n_tok)
      else if (n_aud < n_tok) sprintf("In this dataset, %d of %d tokens have a matched recording",
                                      n_aud, n_tok)
      else sprintf("all %d tokens in this dataset have a matched recording", n_tok)
    pills <- list(
      tags$span(class = paste("diag-pill", if (n_aud) "good" else "warn"), matched_lbl))
    if (!is.null(m) && nrow(m$incomplete))
      pills <- c(pills, list(tags$span(class = "diag-pill warn",
                sprintf("%d could not be identified", nrow(m$incomplete)))))
    # Naming which tokens matched, not just how many, is what lets someone
    # actually check the result against what they expected.
    matched_list <- if (n_aud > 0) {
      keys <- sort(unique(idx$tok_key))
      tags$details(class = "matched-tokens",
        tags$summary(sprintf("Show the %d matched token id%s", n_aud, if (n_aud != 1) "s" else "")),
        tags$div(class = "token-chips",
                 lapply(keys, function(k) tags$span(class = "token-chip", k))))
    } else NULL

    # Name every file the app is actually working with, and say which grid
    # each recording was paired with -- plainly, not behind a fold, since
    # this is the one place that states what was actually found on disk
    # rather than what the app inferred from it. Counts alone are
    # unfalsifiable: a folder that looks like it holds one recording may
    # hold two as far as R is concerned, and Finder will not show the
    # difference.
    af <- audio_files_tbl()
    tg <- tg_intervals()
    files_box <- local({
      pairing <- NULL
      if (!is.null(tg) && nrow(tg)) {
        stems <- unique(tg$file)
        j <- audio_pair_files(stems, af$file)
        pairing <- data.frame(
          TextGrid = paste0(stems, ".TextGrid"),
          Recording = ifelse(is.na(j), "(none found)", af$name[j]),
          stringsAsFactors = FALSE)
      }
      unreadable <- af$name[is.na(af$size) | af$size == 0]

      lines <- if (!is.null(pairing) && nrow(pairing)) {
        lapply(seq_len(nrow(pairing)), function(i)
          sprintf("%s, %s", pairing$Recording[i], pairing$TextGrid[i]))
      } else {
        list(paste(sprintf("%s (%.1f MB)", af$name, af$size / 1e6), collapse = "   ·   "))
      }

      header <- sprintf("%d recording%s, %d TextGrid%s found",
                        nrow(af), if (nrow(af) != 1) "s" else "",
                        if (is.null(tg)) 0L else length(unique(tg$file)),
                        if (!is.null(tg) && length(unique(tg$file)) != 1) "s" else "")

      tagList(
        # a single pairing reads better on one line than split across a
        # header and a lone line below it; several pairings still get one
        # line each, so the list stays scannable
        if (length(lines) == 1)
          tags$div(class = "field-help", style = "margin: 6px 0 4px;",
                   paste0(header, ": "), tags$span(class = "clip-meta", lines[[1]]))
        else tagList(
          tags$div(class = "field-help", style = "margin: 6px 0 4px;", paste0(header, ":")),
          lapply(lines, function(l) tags$div(class = "clip-meta", l))),
        if (length(unreadable))
          tags$div(class = "near-miss-box",
                   sprintf("%s recording%s could not be read: %s. On macOS this usually",
                           length(unreadable), if (length(unreadable) != 1) "s" else "",
                           paste(unreadable, collapse = ", ")),
                   " means the file lives in iCloud and has not been downloaded.",
                   " Open it once in Finder to pull it down."))
    })

    # A token whose clips differ in syllable count is almost never a genuine
    # repetition -- it means the same item number was matched in two
    # different pools (a monosyllable AND a disyllable, say). Worth saying
    # loudly, because the audio would otherwise just sound wrong.
    pool_box <- NULL
    if (nrow(idx) && "n_parts" %in% names(idx)) {
      mixed <- names(which(tapply(idx$n_parts, idx$tok_key,
                                  function(x) length(unique(x)) > 1)))
      if (length(mixed)) {
        pool_box <- tags$div(
          class = "near-miss-box",
          sprintf("%d token%s matched intervals of different lengths, e.g. ",
                  length(mixed), if (length(mixed) != 1) "s" else ""),
          tags$code(mixed[1]),
          ". That usually means one item number appears in tokens of more",
          " than one length. Set ", tags$b("How many intervals does a token span?"),
          " above to the one your dataset describes.")
      }
    }

    near_box <- NULL
    if (!is.null(m) && nrow(m$near_misses)) {
      nm <- m$near_misses
      ex <- utils::head(nm, 3)
      near_box <- tags$div(
        class = "near-miss-box",
        tags$p(style = "margin:0 0 8px;",
               sprintf('%d more token%s would match if a leading "0" and capitalisation',
                       nrow(nm), if (nrow(nm) != 1) "s" else ""),
               " were ignored, e.g. ",
               HTML(paste(sprintf("<code>%s</code> ↔ <code>%s</code>",
                                  ex$from_audio, ex$looks_like), collapse = ", ")), "."),
        checkboxInput("audio_loose",
                      "Ignore leading zeros and capitalisation when matching",
                      value = FALSE))
    }

    # What was found on disk, then what it added up to, then which tokens
    # specifically got it -- in that order, so each line answers the
    # question the one before it raises.
    tagList(tags$hr(class = "subtle-hr"),
            sub_label("Result"),
            files_box,
            tags$div(class = "diag-grid", pills),
            matched_list,
            pool_box, near_box)
  })

  # ---- one player per clip, under the plot --------------------------------
  output$audio_panel <- renderUI({
    idx <- audio_index()
    if (!nrow(idx)) return(NULL)

    keys <- sort(unique(idx$tok_key))
    token_list <- tags$details(
      tags$summary(tags$b(sprintf("Tokens with audio (%d)", length(keys)))),
      tags$div(class = "token-chips",
               lapply(keys, function(k) tags$span(class = "token-chip", k))))

    sel <- intersect(input$highlight %||% character(0), idx$tok_key)
    if (!length(sel)) {
      return(tagList(helpText("Click a contour to hear it. Contours with a dot have audio."),
                     token_list))
    }

    rows_df <- idx[idx$tok_key %in% sel, , drop = FALSE]
    rows_df <- rows_df[order(match(rows_df$tok_key, sel), rows_df$occurrence), , drop = FALSE]
    n_takes <- table(rows_df$tok_key)
    lp <- last_played()

    rows <- lapply(seq_len(nrow(rows_df)), function(i) {
      r <- rows_df[i, ]
      url <- clip_url(r)
      dur <- if (is.na(r$xmin)) NA else (r$xmax - r$xmin)
      # autoplay only the first take of the token just clicked, never a
      # whole set at once
      auto <- identical(r$tok_key, lp) && r$occurrence == min(rows_df$occurrence[rows_df$tok_key == r$tok_key])
      tags$div(
        class = "clip-row",
        tags$span(class = "clip-name", r$tok_key),
        if (n_takes[[r$tok_key]] > 1)
          tags$span(class = "clip-take", paste("take", r$occurrence)),
        tags$audio(src = url, controls = NA, preload = "none",
                   autoplay = if (auto) NA else NULL,
                   style = "height:32px;"),
        if (!is.na(dur))
          tags$span(class = "clip-meta", sprintf("%.2f–%.2fs (%.2fs)",
                                                 r$xmin, r$xmax, dur))
      )
    })

    tagList(
      if (nrow(rows_df) > 1)
        tags$div(style = "margin:8px 0;",
                 actionButton("audio_play_seq", "Play selected in sequence")),
      tags$div(style = "margin-top:8px;", rows),
      tags$div(style = "margin-top:12px;", token_list)
    )
  })

  observeEvent(input$audio_play_seq, {
    idx <- audio_index()
    sel <- intersect(input$highlight %||% character(0), idx$tok_key)
    if (!length(sel)) return()
    rows_df <- idx[idx$tok_key %in% sel, , drop = FALSE]
    rows_df <- rows_df[order(match(rows_df$tok_key, sel), rows_df$occurrence), , drop = FALSE]
    urls <- vapply(seq_len(nrow(rows_df)), function(i) clip_url(rows_df[i, ]), character(1))
    session$sendCustomMessage("playSequence", unname(urls))
  })

  # ---- recording a correction ---------------------------------------------
  # "existing value" and "type a new value" are two separate, unambiguous
  # controls -- a typed new value always wins if both happen to be filled in.
  # active_value tracks what *should* be selected in the dropdown across a
  # choices refresh, since relying on isolate(input$edit_value_existing)
  # there would race against the explicit selection we set right after applying.
  active_value <- reactiveVal("")
  # same race-safe pattern for which variable should be selected, so a
  # brand-new variable just applied becomes the active "existing variable"
  active_var <- reactiveVal("")

  current_value <- reactive({
    new_val <- trimws(input$edit_value_new %||% "")
    if (nzchar(new_val)) new_val else (input$edit_value_existing %||% "")
  })

  # a variable-only helper, shared by both preview outputs below
  var_status <- reactive({
    var <- current_var()
    if (!nzchar(var)) return(list(var = "", is_new = FALSE, bit = NULL))
    ci <- col_info()
    known_vars <- c(ci$cat_cols %||% character(0), unique(edit_log()$variable))
    is_new_var <- !(var %in% known_vars)
    bit <- if (is_new_var) tags$span("new column “", tags$b(var), "”")
    else              tags$span("“", tags$b(var), "”")
    list(var = var, is_new = is_new_var, bit = bit)
  })

  # confirm the variable the moment it's recognised, right where it was
  # typed -- typing it is enough, no need to also fill in a value (or click
  # Apply) just to see that it landed. Sits right after the variable inputs.
  output$var_preview <- renderUI({
    vs <- var_status()
    if (!nzchar(vs$var)) return(NULL)
    if (!nzchar(current_value())) {
      tags$div(class = "record-preview record-preview-partial",
               tags$span("Recognised ", vs$bit, " -- now pick or type a value below."))
    } else {
      tags$div(class = "record-preview record-preview-partial",
               tags$span("Recognised ", vs$bit, "."))
    }
  })

  # live confirmation of what the Apply buttons below will actually do --
  # only once both a variable and a value are in place
  output$record_preview <- renderUI({
    vs <- var_status()
    val <- current_value()
    if (!nzchar(vs$var) || !nzchar(val)) return(NULL)
    tags$div(class = "record-preview record-preview-ready",
             tags$span("Will set ", vs$bit, " to “", tags$b(val), "”."))
  })

  apply_edit_to <- function(keys) {
    var <- current_var()
    val <- current_value()
    req(nzchar(var), nzchar(val))
    if (!length(keys)) return()
    old <- edit_log()
    # a fresh correction to the same (token, variable) replaces the old one
    old <- old[!(old$token_key %in% keys & old$variable == var), , drop = FALSE]
    active_value(val)
    edit_log(rbind(old,
                   data.frame(token_key = keys, variable = var, new_value = val,
                              stringsAsFactors = FALSE)))
    # the value just used is now an existing one for this variable -- select
    # it and clear the "new value" box so the next click doesn't reuse the text
    updateTextInput(session, "edit_value_new", value = "")
    # the variable just used (new or existing) is now a real, selectable
    # variable -- clear the "new variable" box and let the refreshed
    # "existing variable" dropdown (choices observer above) pick it up
    updateTextInput(session, "edit_var_new", value = "")
    active_var(var)
  }

  # keep the "existing value" dropdown's selection sensible whenever its
  # choices refresh (e.g. right after applying a correction above)
  observeEvent(input$edit_value_existing, {
    active_value(input$edit_value_existing)
  }, ignoreInit = TRUE)

  # same for the "existing variable" dropdown
  observeEvent(input$edit_var_existing, {
    active_var(input$edit_var_existing)
  }, ignoreInit = TRUE)

  # ---- dynamic button labels double as the help text they replace --------
  output$label_add_selected_ui <- renderUI({
    n <- length(unique(input$highlight %||% character(0)))
    actionButton("label_add_selected", paste0("Apply to selected (", n, ")"))
  })

  output$label_add_filtered_ui <- renderUI({
    n <- dplyr::n_distinct(filtered()$tok_key)
    actionButton("label_add_filtered", paste0("Apply to all in filter (", n, ")"))
  })

  observeEvent(input$label_add_selected, {
    apply_edit_to(unique(input$highlight %||% character(0)))
  })

  observeEvent(input$label_add_filtered, {
    apply_edit_to(unique(filtered()$tok_key))
  })

  observeEvent(input$label_undo, {
    lg <- edit_log()
    if (!nrow(lg)) return()
    last_var <- lg$variable[nrow(lg)]
    last_val <- lg$new_value[nrow(lg)]
    active_value("")
    edit_log(lg[!(lg$variable == last_var & lg$new_value == last_val), , drop = FALSE])
  })

  # applies every recorded correction back onto a dataset, one (token,
  # variable) edit at a time -- corrects existing categories or adds new
  # ones, exactly like editing the value would have in the file. Shared by
  # both downloads below so "full" and "curated-only" always agree.
  apply_corrections <- function(df) {
    lg <- edit_log()
    if (nrow(lg)) {
      # a correction may name a brand-new variable that doesn't exist in
      # the data yet -- create it (starting blank for every other row)
      # before writing in the corrected values, so nothing is dropped
      for (v in unique(lg$variable)) {
        if (!(v %in% names(df))) df[[v]] <- NA_character_
      }
      for (i in seq_len(nrow(lg))) {
        v <- lg$variable[i]
        df[[v]][df$tok_key == lg$token_key[i]] <- lg$new_value[i]
      }
    }
    df
  }

  # ---- everything: every row of the original dataset, corrections merged in
  output$label_dl <- downloadHandler(
    filename = function() paste0("inspectour_full_dataset_", Sys.Date(), ".csv"),
    content  = function(f) readr::write_csv(apply_corrections(keyed()), f)
  )

  # ---- just the tokens that were actually corrected, same columns/corrections
  output$label_dl_curated <- downloadHandler(
    filename = function() paste0("inspectour_curated_tokens_only_", Sys.Date(), ".csv"),
    content  = function(f) {
      df <- apply_corrections(keyed())
      touched <- unique(edit_log()$token_key)
      readr::write_csv(df[df$tok_key %in% touched, , drop = FALSE], f)
    }
  )
}
