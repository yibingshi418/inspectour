# =============================================================================
#  Contour Inspector
#  Interactive inspection of normalised f0 trajectories for prosodic
#  categorisation in undescribed systems.
#
#  Run with:  shiny::runApp("path/to/this/folder")
#  (or open this file in RStudio and click "Run App" -- RStudio detects
#  the app from app.R the same way)
#
#  Requires:  shiny, ggplot2, plotly, dplyr, tidyr, readr, DT
#
#  Pages: Home (about) / Data (load + map + audio setup, one-time) /
#         Inspect (filter, plot, curate -- everything you touch repeatedly)
#
#  AUDIO: clips are located through Praat TextGrids and cut server side.
#  See R/textgrid.R and R/audio_match.R for why it works that way.
#
#  LAYOUT: this file is intentionally thin. Shiny automatically sources
#  every .R file in R/ (alphabetically) before running the app, so that
#  is where the actual code lives:
#
#    R/helpers.R      constants + small helpers shared by ui.R and server.R
#    R/textgrid.R      Praat TextGrid reader
#    R/wavclip.R        extracts one interval from a WAV file
#    R/audio_match.R     matches TextGrid intervals to dataset tokens
#    R/ui.R               the page itself (ui <- fluidPage(...))
#    R/server.R            all reactive logic (server <- function(...))
#
#  data/sample_data.csv.gz is the bundled "Try it with sample data" set,
#  read directly by server.R -- nothing here needs to know about it.
# =============================================================================

shinyApp(ui, server)
