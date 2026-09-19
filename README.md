# Inspectour

**Inspecting f0 contours through grouping and comparison.**

Inspectour is a Shiny app for looking at normalised f0 trajectories when you do
not yet know what the categories are. It shows every contour in a dataset at
once, with nothing assumed, and lets you group, colour, facet, isolate and
label them interactively until the structure in the data becomes visible.

It is aimed at fieldwork on under-described prosodic systems, where the usual
two options are both awkward: transcription flattens the acoustic detail you
are trying to reason about, and clustering needs parameter decisions — and a
reasonable number of tokens per category — that you often cannot make yet.
Inspectour sits in between, as a way of seeing how many contour shapes are
actually present before or alongside any formal analysis.

<p align="center">
  <img src="docs/screenshot-tokens.png" width="85%" alt="All tokens coloured by citation tone, with a syllable boundary marked">
</p>

## Try it in one click

Clone the repository, open it in R, and run:

```r
# install once
install.packages(c("shiny", "ggplot2", "plotly", "dplyr",
                   "tidyr", "readr", "DT", "scales"))

shiny::runApp(".")
```

Then press **Try the sample data** on the landing page. That loads
`data/sample_contours.csv` — 85 disyllabic tone-sandhi contours from 8
speakers — so you can exercise every part of the tool without preparing
anything. See [`data/README.md`](data/README.md) for the columns.

## What you can do

| | |
|---|---|
| **See everything at once** | Every token is drawn, with no category imposed. Overlap is the point: it is what tells you whether two shapes are really distinct. |
| **Group and compare** | Colour by any variable, facet by one or two more. Checking whether a pattern survives across speakers is one click. |
| **Mark structure** | Draw a boundary line wherever a column changes within a token (syllable, word, phrase), or disconnect the contour there entirely. |
| **Switch scale** | Move between individual tokens, group means with ±1 SD ribbons, or means drawn over the tokens they summarise. |
| **Single out tokens** | Click a line to select it, or search by token id. Selection and search stay in sync. |
| **Record decisions** | Label the current selection, build up a curation log, undo, and export the whole dataset with your labels attached as a new column. |

<p align="center">
  <img src="docs/screenshot-facets.png" width="85%" alt="The same contours faceted by speaker">
</p>

## Your own data

Inspectour expects **long format** — one row per time point per token:

| column | role |
|---|---|
| time | numeric position within the token (1…n) |
| f0 | the value to plot; a normalised column such as `norm_f0` is picked up automatically if present |
| token id | one column that uniquely identifies a single recorded token |
| speaker | speaker identifier |
| anything else | any number of grouping variables — tone category, syntactic structure, elicitation condition, and so on |

Column roles are guessed on load and can be reassigned in the sidebar, so your
column names do not have to match anything. `.csv` and `.rds` are both accepted.

## Tests

```bash
Rscript tests/test_app.R
```

Drives the server with `shiny::testServer` and covers data loading, column
auto-detection, axis rescaling, every plotting path (tokens, means,
means-over-tokens, single and grid faceting, boundary and disconnect modes),
filtering, and the full labelling and undo cycle.

## Citing

See [`CITATION.cff`](CITATION.cff).

## Licence

MIT — see [`LICENSE`](LICENSE).
