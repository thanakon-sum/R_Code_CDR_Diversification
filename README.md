# R code for the figures of "Is Diversification Always Necessary? Evaluating CDR Portfolios Across Net-Negative Pathways"

This folder draws all figures of the paper, the five main figures and Supplementary Figures 2-14 (Supplementary Fig. 1 is a diagram of the scenario framework, without data), from the figure data in
`data/`.
Every number that appears in a figure is in one table, `data/figure_data.csv`; the scripts read only this table (and, for
the maps, `data/map_polygons.csv`). They do not need the model, GAMS or any GDX file.

## How to run

1. Download and unzip this folder (any location; spaces in the path are fine).
2. Draw the figures in one of these ways:
   - Terminal: `Rscript run_all.R` from this folder, or `Rscript /path/to/NCOMMS_Diversification_figure_code/run_all.R` from anywhere.
   - RStudio: open `NCOMMS_Diversification_figure_code.Rproj`, then run `source("run_all.R")`.
   - One figure only: run its `Figure N ... .R` or `Supplementary Figure N ... .R` script in the same way.
3. The figures are written to `output/` as PNG (600 dpi) and PDF: `Figure_1` to `Figure_5` and `Supplementary_Figure_2` to
   `Supplementary_Figure_14`.

No path needs to be edited. Missing R packages are installed from CRAN on the first run. Each figure runs in its own R session.

## Requirements

- R 4.3 or later.
- R packages: ggplot2, dplyr, tidyr, patchwork, ragg, scales, ggtext.
- Tested on macOS with R 4.4.3, ggplot2 3.5.2, dplyr 1.1.4, tidyr 1.3.1, patchwork 1.3.0, ragg 1.4.0, scales 1.4.0 and
  ggtext 0.1.2: the figures are identical, pixel for pixel, to the ones in the paper.
- The figures use the Helvetica font. Where it is not installed, a similar sans-serif font is used, so text positions may
  differ slightly.

## Scripts

| Script | Figure | Panels from |
|---|---|---|
| `Figure 1 Pathways, feasibility and CDR deployment of the portfolios.R` | Fig. 1 | `R/fig1_panels.R` |
| `Figure 2 Macroeconomic impacts of the portfolios.R` | Fig. 2 | `R/fig2_panels.R` |
| `Figure 3 Land use and food-crop prices.R` | Fig. 3 | `R/fig3_panels.R` |
| `Figure 4 Energy system and DACCS energy use.R` | Fig. 4 | `R/fig4_panels.R` |
| `Figure 5 Trade-offs across the portfolios.R` | Fig. 5 | `R/fig5_panels.R` |
| `Supplementary Figure 2 Cumulative CO2 emissions.R` | Supplementary Fig. 2 | `R/sfig2_panels.R` |
| `Supplementary Figure 3 Feasibility of the portfolios across the SSPs.R` | Supplementary Fig. 3 | `R/sfig3_panels.R` |
| `Supplementary Figure 4 Cumulative CCS.R` | Supplementary Fig. 4 | `R/sfig45_panels.R` |
| `Supplementary Figure 5 Discounted carbon price.R` | Supplementary Fig. 5 | `R/sfig45_panels.R` |
| `Supplementary Figure 6 Sectoral emissions and regional CDR.R` | Supplementary Fig. 6 | `R/sfig6_panels.R` |
| `Supplementary Figure 7 Biodiversity intactness.R` | Supplementary Fig. 7 | `R/sfig7_panels.R` |
| `Supplementary Figure 8 Economic damages from climate change.R` | Supplementary Fig. 8 | `R/sfig8_panels.R` |
| `Supplementary Figure 9 Normalised performance of the portfolios.R` | Supplementary Fig. 9 | `R/sfig9_panels.R` |
| `Supplementary Figure 10 Trade-offs in 2100.R` | Supplementary Fig. 10 | `R/fig5_panels.R` |
| `Supplementary Figure 11 CDR by technology in the sensitivity runs.R` | Supplementary Fig. 11 | `R/sfig11_panels.R` |
| `Supplementary Figure 12 Sensitivity to DACCS and EW assumptions.R` | Supplementary Fig. 12 | `R/sfig12_panels.R` |
| `Supplementary Figure 13 Feasibility indicators of the pathways.R` | Supplementary Fig. 13 | `R/sfig13_panels.R` |
| `Supplementary Figure 14 Climate outcomes from MAGICC.R` | Supplementary Fig. 14 | `R/sfig14_panels.R` |

`R/style.R` holds the shared colours (chosen to be distinguishable with colour-vision deficiency), text sizes, units and the
saving function; `R/setup.R` installs missing packages and creates `output/`.

## Data (`data/`)

`figure_data.csv` (long format, one row per number drawn):

| Column | Content |
|---|---|
| `figure`, `panel` | Figure (`Fig1` to `Fig5`, `SFig2` to `SFig14`) and panel letter |
| `series` | What the number is (for example a technology, an indicator, `range_min`/`range_max` of a ribbon, `full_combo`) |
| `portfolio` | CDR portfolio (see below), `BaU` for the baseline without climate policy |
| `pathway` | Net-negative pathway: vHigh, High, Med, Low, vLow (`BaU` for the baseline) |
| `region` | AIM-Hub region (17 regions; the SSP in `SFig3`) |
| `year`, `value`, `unit` | Year (empty for indicators over a period), value and unit |
| `source` | Model variable or calculation behind the number |

All values come from the AIM-Hub results of the 38 feasible runs (16 portfolios x 5 pathways were attempted; vHigh has no
feasible portfolio) and the baseline, from the feasibility of the same 80 portfolio-pathway combinations under SSP1 to SSP5
(Supplementary Fig. 3), from the sensitivity runs of the Full-combo portfolio in the High, Med, Low and vLow pathways with
optimistic and pessimistic DACCS and enhanced-weathering assumptions (16 runs; Supplementary Figs. 11 and 12), from the
MAGICC v7.5.3 climate assessment of the feasible runs (Fig. 1a, Supplementary Fig. 14), the Biodiversity Intactness Index
(Supplementary Fig. 7) and the climate-damage emulators (Supplementary Fig. 8), as described in the Methods of the paper. Ranges ("ribbons") span the feasible portfolios of a pathway unless the `series` says otherwise.

`map_polygons.csv`: country outlines (Natural Earth, 1:50m, public domain) in the Robinson projection (km), with the
AIM-Hub region of each country; the regional maps colour each country by the value of its region.

## Portfolio and pathway names

| Letter | CDR option |
|---|---|
| A | Afforestation |
| B | Bioenergy with carbon capture and storage (BECCS) |
| D | Direct air carbon capture and storage (DACCS) |
| O | Other land-based options: enhanced weathering, biochar and soil carbon sequestration |

Portfolios combine the letters (for example `A+B+D+O`, the Full-combo); `No CDR` uses none. In the model files every run is
named `SSP2_LB<pathway>_<options>_NoCC_No` (for example `SSP2_LBvlow_DACCS_BECCS_Afforest_Other_NoCC_No` = vLow, A+B+D+O).
The sensitivity runs add the case before `_NoCC_No`: `DACCSOptimistic`, `DACCSPessimistic`, `EWOptimistic` or `EWPessimistic`.

## License

CC BY 4.0
