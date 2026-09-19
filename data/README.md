# Sample dataset

`sample_contours.csv` — 1,700 rows: 85 disyllabic tokens × 20 normalised time
points (10 per syllable), from 8 speakers, elicited in citation condition.

It is included so that anyone can open Inspectour and use every feature
immediately, without preparing data first. Press **Try the sample data** in the
app.

| column | description |
|---|---|
| `speaker` | speaker identifier, S1–S8 |
| `token` | the word elicited |
| `diortri` | disyllabic (`di`) or trisyllabic (`tri`) item |
| `focus_condition` | elicitation condition (`ct` = citation) |
| `focus_no` | focus position, where applicable |
| `citation_tone` | citation tones of the constituent syllables, e.g. `HH_HL` |
| `sandhi_tone` | transcribed surface sandhi contour over the whole word, e.g. `HMML` |
| `sandhi_tone_var` | an alternative transcription of the same contour |
| `syllable_no` | position of the syllable within the word (1, 2) |
| `citation_no` | index of the citation-tone combination |
| `ind_no` | **token id** — uniquely identifies one recorded token |
| `sync_tone1`–`sync_tone3` | synchronic tone category per syllable |
| `hist_tone1`–`hist_tone3` | historical tone category per syllable |
| `syntax` | syntactic structure of the item |
| `time` | normalised time point, 1–20 |
| `f0` | raw f0 in Hz |
| `norm_f0` | speaker-normalised f0 — the app's default y-axis |

Good things to try with it: colour by `citation_tone` and facet by `speaker` to
see how consistent the sandhi pattern is across speakers; set **Mark a break**
to `syllable_no` to see where the syllable boundary falls; switch to **Group
means** to compare averaged shapes; then colour by `syntax` to check whether
structure matters.
