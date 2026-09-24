# Sample data

## `sample_data.csv.gz`

1,700 rows: 85 disyllabic tokens × 20 normalised time points (10 per
syllable), from 8 speakers, elicited in citation form.

It is included so that anyone can open Inspectour and use every feature
straight away, without preparing data first. Press **Try it with sample data**
on the Data page.

| column | description |
|---|---|
| `speaker` | speaker identifier, S1–S8 |
| `token` | the word elicited |
| `citation_tone` | citation tones of the two syllables, e.g. `HH_HL` |
| `sandhi_tone` | transcribed surface sandhi contour over the whole word, e.g. `HMML` |
| `syllable_no` | position of the syllable within the word (1, 2) |
| `item_id` | item number in the elicitation list |
| `token_id` | **token id**: uniquely identifies one recorded token (`<speaker>_<item>_ct`) |
| `sync_tone1`, `sync_tone2` | synchronic tone category per syllable |
| `hist_tone1`, `hist_tone2` | historical tone category per syllable |
| `syntax` | syntactic structure of the item |
| `time` | normalised time point, 1–20 |
| `f0` | raw f0 in Hz |
| `norm_f0` | speaker-normalised f0, the app's default y-axis |

Good things to try: colour by `citation_tone` and facet by `speaker` to see how
consistent the sandhi pattern is across speakers; set **Mark a break** to
`syllable_no` to see where the syllable boundary falls; switch to **Group
means** to compare averaged shapes; then colour by `syntax` to check whether
structure matters.

## `sample_audio/`

A 2.5-minute mono excerpt of speaker S2's citation-form recording, with its
Praat TextGrid. The TextGrid has one interval per syllable, labelled
`<item> <syllable> <citation tones> <sandhi tones>`, e.g. `03 1 HHHH MMMM`.
The file name supplies the speaker, and the first label field supplies
`item_id`. Press **Try it with sample audio** to load it: the 9 S2 tokens in
the sample dataset become playable.
