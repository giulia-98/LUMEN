# LUMEN
LUMEN project (partecipatory research at LabAut, University of Pavia)

# LUMEN — Analysis Pipeline

LUMEN is an Italian survey study on access to mental health diagnoses among LGBTQ+ people and autistic people. This repository contains the R analysis pipeline that processes the REDCap export, computes subgroup counts, runs descriptive statistics and non-parametric tests, and generates PDF reports.

---

## Repository structure

```
LUMEN/
└── generale/
    ├── LUMEN_DATA.csv                # Raw REDCap export (not versioned — add to .gitignore)
    ├── load_lumen_data.R             # Data loading, labelling, consent filter
    ├── compute_subgroups.R           # Subgroup definitions and summary function
    ├── LUMEN_1_groups_counts.R       # Step 1 — counts and percentages per subgroup
    ├── LUMEN_2_report.R              # Step 2 — PDF report (tables + bar charts)
    ├── LUMEN_3_stats_prior.R         # Step 3 — descriptive statistics for priority variables
    ├── LUMEN_4_tests_prior.R         # Step 4 — non-parametric tests for priority variables
    └── LUMEN_5_stats_diagnosi.R      # Step 5 — statistics for diagnosis variables
```

### Generated files (not versioned)

| File | Produced by |
|---|---|
| `LUMEN_DATA_processed.csv` | LUMEN_1 |
| `groups_counts.csv` | LUMEN_1 |
| `report.pdf` | LUMEN_2 |
| `statistiche_prior.csv` | LUMEN_3 |
| `prior_boxplot.pdf` | LUMEN_3 |
| `test_diagnosi_prior.csv` | LUMEN_4 |
| `test_lgbt_prior.csv` | LUMEN_4 |
| `test_pairwise_prior.csv` | LUMEN_4 |
| `test_risultati_prior.csv` | LUMEN_4 |
| `test_risultati_prior.pdf` | LUMEN_4 |
| `test_significativi_prior.pdf` | LUMEN_4 |
| `stats_diagnosi_risultati.csv` | LUMEN_5 |
| `stats_n_diagnosi.csv` | LUMEN_5 |
| `stats_diagnosi_dataset_lungo.csv` | LUMEN_5 |
| `stats_diagnosi_sommario.pdf` | LUMEN_5 |

---

## Pipeline overview

The five numbered scripts form a sequential pipeline. Each step reads the output of the previous one.

```
LUMEN_DATA.csv
      │
      ▼
load_lumen_data.R ──► LUMEN_1_groups_counts.R
                              │
                    ┌─────────┴──────────┐
                    ▼                    ▼
          groups_counts.csv   LUMEN_DATA_processed.csv
                    │                    │
                    ▼                    ├──► LUMEN_3_stats_prior.R
          LUMEN_2_report.R               ├──► LUMEN_4_tests_prior.R
                    │                    └──► LUMEN_5_stats_diagnosi.R
                    ▼
              report.pdf
```

### Step 1 — `LUMEN_1_groups_counts.R`

Loads the raw REDCap CSV via `load_lumen_data.R`, computes `n_diagnosi` (number of diagnoses per respondent, 0–12), and builds a three-level subgroup structure:

**diagnosis group × LGBTQ+ group × survey variable**

Diagnosis groups (3): `aut`, `altra_diagnosi`, `no_diagnosi`  
LGBTQ+ groups (2): `lgbtq`, `no_lgbtq`  
Crossed combinations: 6, plus 5 marginal totals

Outputs `groups_counts.csv` (one row per variable × value × group) and `LUMEN_DATA_processed.csv` (respondent-level data frame with `n_diagnosi`).

### Step 2 — `LUMEN_2_report.R`

Reads `groups_counts.csv` and produces `report.pdf`: one page per survey variable with a summary table (counts and percentages for all 11 groups) and a stacked bar chart. Variables with more than 10 distinct values are split across two pages.

### Step 3 — `LUMEN_3_stats_prior.R`

Computes descriptive statistics (N, mean, SD, median, IQR, skewness, kurtosis) for all `prior_ric_*`, `prior_aut_*`, and `prior_salute_*` variables (0–10 Likert-style scales) across all subgroups. Outputs `statistiche_prior.csv` and `prior_boxplot.pdf`.

### Step 4 — `LUMEN_4_tests_prior.R`

Runs non-parametric tests on the priority variables:

- **Step 4.1 — Diagnosis effect** (Kruskal-Wallis omnibus + Mann-Whitney post-hoc, BH-FDR correction)
- **Step 4.2 — LGBTQ+ effect** (Mann-Whitney, BH-FDR correction)
- **Step 4.3 — Pairwise across all 6 crossed groups** (15 Mann-Whitney pairs per variable, BH-FDR correction)

Effect sizes: η² for Kruskal-Wallis, *r* = |Z| / √n for Mann-Whitney.

### Step 5 — `LUMEN_5_stats_diagnosi.R`

Tests whether diagnosis group (`aut` vs `altra_diagnosi`) and LGBTQ+ status (`lgbtq` vs `no_lgbtq`) influence variables related to the diagnostic pathway: referral setting, who initiated the referral, waiting times, number of incorrect diagnoses, cost, location, and number of professionals consulted.

The dataset is reshaped to long format (one row per respondent × diagnosis received). Mixed-effects models with a by-subject random intercept (`(1|record_id)`) are used to account for the nested structure. Model type varies by outcome: `clmm` for ordinal outcomes, `mblogit` for nominal, `glmer` (binomial) for the binary outcome (`sbagliate`). p-values are computed via Likelihood Ratio Test; BH-FDR correction is applied separately for each predictor. An additional Kruskal-Wallis and Mann-Whitney test is run on `n_diagnosi` (number of diagnoses per respondent) using the wide-format dataset.

---

## Subgroup definitions (`compute_subgroups.R`)

| Key | Definition |
|---|---|
| `aut` | `aut ∈ {1, 2, 3}` (autism diagnosis, public / private / both) |
| `altra_diagnosi` | `aut == 4` OR `aut == NA` AND at least one other diagnosis column `∈ {1, 2, 3}` |
| `no_diagnosi` | `diagnosi_si_no == 2` |
| `lgbtq` | `trans_cis == 1` OR `identita_genere ∉ {1, 2}` OR `orientamento_sex ≠ 1` |
| `no_lgbtq` | `trans_cis == 2` AND `identita_genere ∈ {1, 2}` AND `orientamento_sex == 1` |

**Notes:**
- `altra_diagnosi` includes 22 respondents whose `aut` question was hidden by REDCap skip logic (`aut == NA`) but who reported at least one other diagnosis.
- 7 respondents with `diagnosi_si_no == 1` but all individual diagnosis columns at `4` or `NA` cannot be classified and are excluded from all diagnosis-group analyses (`escluso_diagnosi = TRUE`).
- Respondents with all three LGBTQ+ variables (`trans_cis`, `identita_genere`, `orientamento_sex`) equal to `NA` are excluded from LGBTQ+-group analyses (`lgbtq = NA`).
- `adhd` is not included in the `altra_diagnosi` filter columns (`pers`, `ansia`, `umore`, `comp_al`, `appr`, `psicosi`, `ocd`, `ptsd`, `dipendenza`, `altro`).

---

## Requirements

R ≥ 4.1 with the following packages:

```r
install.packages(c("Hmisc", "dplyr", "tidyr", "ggplot2",
                   "scales", "gridExtra", "grid", "moments",
                   "ordinal", "mclogit", "lme4", "dunn.test"))
```

---

## How to run

Set the working directory to `LUMEN/generale/`, place `LUMEN_DATA.csv` there, then source the pipeline in order:

```r
source("LUMEN_1_groups_counts.R")
source("LUMEN_2_report.R")
source("LUMEN_3_stats_prior.R")
source("LUMEN_4_tests_prior.R")
source("LUMEN_5_stats_diagnosi.R")
```

All output files are written to `LUMEN/generale/`.

---

## Data and privacy

`LUMEN_DATA.csv` contains survey responses and must not be committed to version control. Add it to `.gitignore`:

```
LUMEN/generale/LUMEN_DATA.csv
LUMEN/generale/LUMEN_DATA_processed.csv
```
