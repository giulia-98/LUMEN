# LUMEN
LUMEN project (partecipatory research at LabAut, University of Pavia)

R repository for analysing data collected via the **LUMEN** questionnaire, a study on
mental health, diagnostic pathways, and research priorities in LGBTQ+ populations
and/or people with neurodivergent diagnoses (autism, ADHD, personality disorders, etc.).

Data (not available in the repository are exported from REDCap as `LUMEN_DATA.csv` and 
processed through a modular four-step pipeline.

---

## Repository structure
```
LUMEN/
└── generale/
    ├── LUMEN_DATA.csv           # ← not included in the repo (sensitive data)
    ├── load_lumen_data.R        # Module – Data loading and preparation
    ├── compute_subgroups.R      # Module – Subgroup definition and computation
    ├── LUMEN_1_groups_counts.R  # Step 1  – Counts by variable and subgroup
    ├── LUMEN_2_report.R         # Step 2  – Descriptive PDF report (charts + tables)
    ├── LUMEN_3_stats_prior.R    # Step 3  – Descriptive statistics for "prior" variables
    └── LUMEN_4_tests_prior.R    # Step 4  – Inferential statistical tests
```

All files — scripts, input data, and generated outputs — live in **`LUMEN/generale/`**.
Each script automatically detects its own directory at runtime and reads/writes
all files there, whether run interactively in RStudio or via `Rscript` from the terminal.

---

## Files

### `load_lumen_data.R`

Defines the `load_lumen_data(file)` function, which loads `LUMEN_DATA.csv` and:

- assigns human-readable **labels** to all questionnaire variables (via `Hmisc::label()`);
- converts variables to **factors** with the correct levels and labels;
- filters to keep only participants who gave **informed consent**.

**Dependencies:** `Hmisc`

**Usage:**
```r
source("load_lumen_data.R")
data <- load_lumen_data()                     # looks for LUMEN_DATA.csv in LUMEN/generale/
data <- load_lumen_data("/path/to/file.csv")  # explicit path
```

---

### `compute_subgroups.R`

Defines filters and functions for building a **three-level subgroup structure**:
*categorical variable × diagnosis group × LGBTQ+ group*.

**Diagnosis groups (3):**
| Key | Description |
|-----|-------------|
| `aut` | Autism diagnosis (public, private, or both) |
| `altra_diagnosi` | Other psychiatric diagnosis, no autism |
| `no_diagnosi` | No diagnosis |

**LGBTQ+ groups (2):**
| Key | Description |
|-----|-------------|
| `lgbtq` | Trans identity OR non-binary gender identity OR non-heterosexual orientation |
| `no_lgbtq` | Cisgender AND binary gender identity AND heterosexual |

**Marginal totals** are also computed:
`aut_tot`, `altra_diagnosi_tot`, `no_diagnosi_tot` (regardless of LGBTQ+ status) and
`lgbtq_tot`, `no_lgbtq_tot` (regardless of diagnosis).

**Exported functions:**
- `compute_subgroups(data, cat_vars)` — returns a nested list
  `result[[diagnosis]][[lgbtq]][[variable]][[value]]`
- `summary_subgroups(subgroups, data)` — flattens the nested list into a tidy data frame
  with columns: `variabile`, `valore`, `label`, `diagnosi`, `lgbt`, `count`,
  `totale_gruppo`, `totale_var`, `totale_soggetti`, `pct`

**Dependencies:** none (base R only)

---

### `LUMEN_1_groups_counts.R`

**Step 1 — Counts by subgroup**

Sources `load_lumen_data.R` and `compute_subgroups.R`, then:

1. loads and filters the data;
2. calls `compute_subgroups()` and `summary_subgroups()` to produce counts and
   percentages for every categorical variable across all subgroups;
3. sorts rows (marginal diagnosis totals → marginal LGBTQ+ totals → 6 crossed groups);
4. saves the result to **`groups_counts.csv`** in `LUMEN/generale/`.

**Input:** `LUMEN_DATA.csv`  
**Output:** `groups_counts.csv`  
**Dependencies:** `Hmisc`

---

### `LUMEN_2_report.R`

**Step 2 — Descriptive PDF report**

Reads `groups_counts.csv` and produces a **multi-page A4-landscape PDF** (`report.pdf`)
with one page (or two, for variables with many categories) per questionnaire variable.
Each page contains:

- a **summary table** showing counts and percentages for all 11 subgroups
  (3 diagnosis totals + 2 LGBTQ+ totals + 6 crossed groups);
- a **stacked bar chart** showing the distribution of the 6 crossed groups within
  each response category.

**Input:** `groups_counts.csv`  
**Output:** `report.pdf`  
**Dependencies:** `ggplot2`, `dplyr`, `tidyr`, `scales`, `gridExtra`, `grid`

---

### `LUMEN_3_stats_prior.R`

**Step 3 — Descriptive statistics for priority variables**

Sources `load_lumen_data.R` and `compute_subgroups.R`, then computes descriptive
statistics for all `prior_ric`, `prior_aut`, and `prior_salute` variables
(continuous scales 0–10) across the total sample and all 11 subgroups.

Statistics computed per variable per subgroup:
`N_valid`, `N_missing`, `Mean`, `SD`, `Min`, `Q1`, `Median`, `Q3`, `Max`, `IQR`,
`Skewness`, `Kurtosis`

**Input:** `LUMEN_DATA.csv`  
**Outputs:**
- `statistiche_prior.csv` — full statistics table (rounded to 3 decimal places)
- `prior_boxplot.pdf` — one page per variable with a custom boxplot (left) and
  summary table (right), grouped by section (`prior_ric`, `prior_aut`, `prior_salute`)

**Dependencies:** `dplyr`, `moments`, `Hmisc`, `ggplot2`, `gridExtra`, `grid`

---

### `LUMEN_4_tests_prior.R`

**Step 4 — Inferential statistical tests**

Non-parametric tests comparing subgroups on all `prior_ric`, `prior_aut`, and
`prior_salute` variables. Tests are chosen because distributions are strongly
negatively skewed and some subgroups have n < 30.

Three analysis steps are run:

| Step | Test | Variables | Groups |
|------|------|-----------|--------|
| 1 – Diagnosis effect | Kruskal-Wallis + Mann-Whitney post-hoc | `prior_ric`, `prior_salute` | aut / altra_diagnosi / no_diagnosi |
| 2 – LGBTQ+ effect | Mann-Whitney U | `prior_ric`, `prior_salute` (full sample); `prior_aut` (autism group only) | lgbtq vs no_lgbtq |
| 3 – Pairwise (6 groups) | Mann-Whitney U (15 pairs) | `prior_ric`, `prior_salute` | all 6 crossed groups |

Multiple comparisons are corrected with **Benjamini-Hochberg FDR**.
Effect sizes: **η²** (Kruskal-Wallis), **r = |Z| / √n** (Mann-Whitney).

**Input:** `LUMEN_DATA.csv`  
**Outputs:**
- `test_diagnosi.csv` — Kruskal-Wallis omnibus + pairwise post-hoc (diagnosis)
- `test_lgbt.csv` — Mann-Whitney LGBTQ+ effect
- `test_pairwise.csv` — pairwise Mann-Whitney across the 6 crossed groups
- `test_risultati.csv` — all results in a single long-format file
- `test_significativi.pdf` — one page per significant variable (FDR < 0.05) with
  table of significant comparisons and auto-generated interpretation text

**Dependencies:** `dplyr`, `Hmisc`, `ggplot2`, `gridExtra`, `grid`

---

## Pipeline overview
```
LUMEN/generale/
│
│  LUMEN_DATA.csv
│       │
│       ├─────────────────────────────────────────────────────┐
│       ▼                                                     │
│  load_lumen_data.R  }                                       │
│  compute_subgroups.R}  shared modules                       │
│       │                                                     │
│       ▼                                                     ▼
│  LUMEN_1_groups_counts.R              LUMEN_3_stats_prior.R
│       │                                       │
│       │  groups_counts.csv                    ├── statistiche_prior.csv
│       ▼                                       └── prior_boxplot.pdf
│  LUMEN_2_report.R
│       │                               LUMEN_4_tests_prior.R
│       └── report.pdf                          │
│                                               ├── test_diagnosi.csv
│                                               ├── test_lgbt.csv
│                                               ├── test_pairwise.csv
│                                               ├── test_risultati.csv
│                                               └── test_significativi.pdf
```

---

## How to run

Set `LUMEN/generale/` as your working directory, then run the scripts in order:
```r
setwd("path/to/LUMEN/generale")

source("LUMEN_1_groups_counts.R")  # produces groups_counts.csv
source("LUMEN_2_report.R")         # produces report.pdf
source("LUMEN_3_stats_prior.R")    # produces statistiche_prior.csv + prior_boxplot.pdf
source("LUMEN_4_tests_prior.R")    # produces test_*.csv + test_significativi.pdf
```

Steps 3 and 4 are independent of steps 1 and 2 and can be run in any order.

---

## Requirements
```r
install.packages(c("Hmisc", "dplyr", "tidyr", "ggplot2",
                   "scales", "gridExtra", "grid", "moments"))
```

R version 4.0 or higher is recommended.

---

## Data

`LUMEN_DATA.csv` is **not included** in this repository as it contains sensitive
personal data. The file must be placed in `LUMEN/generale/` before running any script.
It is exported directly from REDCap.

---

## Notes

- All scripts detect their own directory at runtime and save outputs to the same
  folder, whether run interactively in RStudio or via `Rscript` from the terminal.
- `prior_aut_sociale` values greater than 10 are automatically divided by 10 to
  correct a known data-entry error.
- PDF reports are generated with `grid` / `gridExtra` primitives; no external
  LaTeX installation is required.
