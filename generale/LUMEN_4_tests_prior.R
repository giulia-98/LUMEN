# ==============================================================================
# LUMEN_4_tests_prior.R
#
# PURPOSE
# -------
# Runs non-parametric statistical tests to compare subgroups on all "priority"
# variables (prior_ric, prior_aut, prior_salute), measured on a 0–10 scale.
#
# RATIONALE FOR NON-PARAMETRIC TESTS
# -----------------------------------
# The priority variables show strong negative skew (high medians, long lower
# tails) and some crossed subgroups have n < 30. Non-parametric tests do not
# assume normality or homoscedasticity and are therefore preferred.
#
# ANALYSIS IN THREE STEPS
# -----------------------
#
# STEP 1 — DIAGNOSIS EFFECT  (prior_ric and prior_salute only)
#   prior_aut variables are excluded here because only the autism group
#   responds to them: a 3-group comparison would be meaningless.
#
#   Test: Kruskal-Wallis (omnibus)
#     Checks whether at least one diagnosis group (aut / altra_diagnosi /
#     no_diagnosi) differs in rank distribution from the others.
#     Non-parametric equivalent of one-way ANOVA.
#     H0: all three group distributions are identical.
#     Statistic: H (chi-square approximation), df = k - 1 = 2.
#     Effect size: eta² = (H - k + 1) / (n - k)
#       eta² ≈ 0.01 small, 0.06 medium, 0.14 large  (Cohen 1988)
#
#   Post-hoc: Mann-Whitney U  (run if KW is significant)
#     Three pairwise comparisons:
#       aut vs altra_diagnosi
#       aut vs no_diagnosi
#       altra_diagnosi vs no_diagnosi
#     H0: the two group distributions are identical.
#     Statistic: W (Wilcoxon rank-sum statistic).
#     Effect size: r = |Z| / sqrt(n)
#       r ≈ 0.10 small, 0.30 medium, 0.50 large  (Cohen 1988)
#
#   Multiple-comparison correction: Benjamini-Hochberg FDR applied separately
#     to the KW omnibus tests (across all variables) and to each post-hoc pair.
#
# STEP 2 — LGBTQ+ EFFECT
#   Test: Mann-Whitney U  (lgbtq vs no_lgbtq)
#   For prior_ric and prior_salute: uses the full sample with gruppo_lgbt.
#   For prior_aut: uses only respondents with autism (aut_x_lgbtq vs aut_x_no_lgbtq).
#   FDR correction across all variables (42 tests).
#   Effect size: r = |Z| / sqrt(n)
#
# STEP 3 — PAIRWISE COMPARISONS ACROSS ALL 6 CROSSED GROUPS (prior_ric, prior_salute)
#   prior_aut variables are excluded (same reason as Step 1).
#   Test: Mann-Whitney U on all 15 possible pairs of the 6 crossed groups.
#   FDR correction applied per variable (15 comparisons per variable).
#
# INTERPRETATION GUIDE
# --------------------
#   p and p_fdr:
#     p = raw p-value; p_fdr = Benjamini-Hochberg FDR-adjusted p.
#     FDR controls the expected rate of false positives among significant
#     results (less conservative than Bonferroni; appropriate for exploratory).
#     Thresholds: p_fdr < 0.05 (*), < 0.01 (**), < 0.001 (***)
#     In the PDF: light-red background = significant, orange = trend (< 0.10)
#
#   Effect size r (Mann-Whitney):
#     r = 0.10 small | r = 0.30 medium | r = 0.50 large
#     A large n can make a small r statistically significant: always consider
#     r alongside p_fdr.
#
#   Effect size eta² (Kruskal-Wallis):
#     eta² = 0.01 small | 0.06 medium | 0.14 large
#
#   Small subgroups:
#     Some crossed cells (e.g. altra_diagnosi_x_no_lgbtq, n ≈ 25) have low
#     power: a non-significant result does not rule out a real effect.
#
# OUTPUT FILES
# ------------
#   test_diagnosi.csv     — KW omnibus + post-hoc pairwise for diagnosis effect
#   test_lgbt.csv         — Mann-Whitney LGBTQ+ effect
#   test_pairwise.csv     — Mann-Whitney pairwise across 6 crossed groups
#   test_risultati.csv    — all three sections combined in one long file
#   test_risultati.pdf    — one page per variable: all test tables + legend
#   test_significativi.pdf— one page per variable with FDR < 0.05 results
#                           + auto-generated interpretation text
# ==============================================================================

rm(list = ls())   # clear workspace
graphics.off()    # close any open graphics devices

library(dplyr)    # data manipulation and case_when
library(Hmisc)    # required by load_lumen_data.R

# Load helper scripts
source("load_lumen_data.R")
source("compute_subgroups.R")

data <- load_lumen_data()  # read and label the survey data
data <- data[data$consenso == 1 & !is.na(data$consenso), ]  # keep consenting respondents only

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Variables for diagnosis + LGBTQ+ comparisons (prior_ric and prior_salute only)
cols_diag <- grep("^prior_ric|^prior_salute", names(data), value = TRUE)

# Variables for LGBTQ+ comparison within the autism group only (prior_aut)
cols_aut  <- grep("^prior_aut", names(data), value = TRUE)

# All priority variables combined (used for pairwise 6-group tests and LGBTQ+ step 2)
cols <- c(cols_diag, cols_aut)

# Data quality correction: prior_aut_sociale was sometimes entered on 0-100 scale
data$prior_aut_sociale <- ifelse(
  is_valid(data$prior_aut_sociale) & data$prior_aut_sociale > 10,
  data$prior_aut_sociale / 10,  # rescale incorrectly entered values to 0-10
  data$prior_aut_sociale        # keep correctly entered values as-is
)

# ==============================================================================
# GROUP VECTORS
# Create a character factor column for each of the three grouping dimensions.
# NA = respondent could not be assigned to any group.
# ==============================================================================

# Diagnosis group vector: assign each row to one of three categories (or NA)
diag_vec <- dplyr::case_when(
  DIAGNOSI_FILTERS$aut(data)            ~ "aut",
  DIAGNOSI_FILTERS$altra_diagnosi(data) ~ "altra_diagnosi",
  DIAGNOSI_FILTERS$no_diagnosi(data)    ~ "no_diagnosi",
  TRUE                                  ~ NA_character_  # unclassifiable respondents
)

# LGBTQ+ group vector: assign each row to lgbtq, no_lgbtq, or NA
lgbt_vec <- dplyr::case_when(
  LGBT_FILTERS$lgbtq(data)    ~ "lgbtq",
  LGBT_FILTERS$no_lgbtq(data) ~ "no_lgbtq",
  TRUE                         ~ NA_character_  # unclassifiable respondents
)

# Attach group vectors to the data frame as factors for easy subsetting
data$gruppo_diag <- factor(diag_vec,
  levels = c("aut", "altra_diagnosi", "no_diagnosi"))

data$gruppo_lgbt <- factor(lgbt_vec,
  levels = c("lgbtq", "no_lgbtq"))

# Create the 6-way crossed group factor (diagnosis_x_lgbtq)
data$gruppo6 <- factor(
  ifelse(!is.na(diag_vec) & !is.na(lgbt_vec),
         paste(diag_vec, lgbt_vec, sep = "_x_"), NA_character_),  # concatenate only when both are non-NA
  levels = c("aut_x_lgbtq", "aut_x_no_lgbtq",
             "altra_diagnosi_x_lgbtq", "altra_diagnosi_x_no_lgbtq",
             "no_diagnosi_x_lgbtq",    "no_diagnosi_x_no_lgbtq"))

# Print group sizes to console for verification
cat("N per gruppo diagnosi:\n");       print(table(data$gruppo_diag, useNA = "ifany"))
cat("\nN per gruppo LGBT:\n");         print(table(data$gruppo_lgbt, useNA = "ifany"))
cat("\nN per 6 gruppi incrociati:\n"); print(table(data$gruppo6,     useNA = "ifany"))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Compute Kruskal-Wallis eta-squared effect size
# Formula: (H - k + 1) / (n - k), floored at 0
kw_eta2 <- function(H, k, n) round(max((H - k + 1) / (n - k), 0), 4)

# Compute Mann-Whitney effect size r = |Z| / sqrt(n)
# Z is derived from the two-tailed p-value of the Wilcoxon test
mw_effect_r <- function(mw_obj, n) {
  z <- qnorm(mw_obj$p.value / 2)   # convert two-tailed p to Z-score
  round(abs(z) / sqrt(n), 4)       # standardised effect size
}

# Safe Mann-Whitney wrapper: returns NULL (instead of crashing) if either
# group has fewer than 3 observations or if the test raises an error
safe_mw <- function(x, y) {
  if (length(x) < 3 || length(y) < 3) return(NULL)  # minimum sample size guard
  tryCatch(
    suppressWarnings(wilcox.test(x, y, exact = FALSE, conf.int = FALSE)),  # approximate p-value
    error = function(e) NULL  # return NULL on any test error
  )
}

# Format a p-value for display:
#   p < 0.001 → scientific notation (3 significant figures, e.g. 2.34e-05)
#   otherwise → fixed notation rounded to 4 decimal places
fmt_p <- Vectorize(function(p, digits = 4) {
  if (is.na(p)) return(NA_real_)
  if (p < 0.001) return(signif(p, 3))  # scientific notation for very small p-values
  round(p, digits)                       # fixed notation for larger p-values
})

# ==============================================================================
# STEP 1: DIAGNOSIS EFFECT — Kruskal-Wallis + Mann-Whitney post-hoc
# ==============================================================================

cat("\n\n--- 1. Kruskal-Wallis + post-hoc Mann-Whitney (diagnosi) ---\n")

# The three pairs for post-hoc pairwise comparisons
coppie_diag <- list(
  c("aut",            "altra_diagnosi"),  # autism vs other diagnosis
  c("aut",            "no_diagnosi"),     # autism vs no diagnosis
  c("altra_diagnosi", "no_diagnosi")      # other diagnosis vs no diagnosis
)

# Only prior_ric and prior_salute: prior_aut is excluded because only the autism
# group has responses, making a 3-group comparison meaningless
res_diagnosi <- do.call(rbind, lapply(cols_diag, function(col) {

  df  <- data[!is.na(data$gruppo_diag), ]  # exclude unclassified respondents
  x   <- df[[col]];  grp <- df$gruppo_diag
  ok  <- !is.na(x);  xv  <- x[ok]; gv <- droplevels(grp[ok])  # remove NA values
  n   <- length(xv); k   <- nlevels(gv)  # total n and number of groups

  # Check that at least 2 groups have 3 or more valid observations
  grp_validi <- names(table(gv))[table(gv) >= 3]
  if (length(grp_validi) < 2) {
    # Too few groups: return NA row with group sizes for transparency
    row <- data.frame(
      Variabile = col, N = n,
      KW_H = NA, KW_df = NA, KW_p = NA, KW_eta2 = NA,
      stringsAsFactors = FALSE
    )
    for (cp in coppie_diag) {
      nm <- paste(cp, collapse = "_vs_")
      row[[paste0(nm, "_n1")]] <- sum(gv == cp[1], na.rm = TRUE)
      row[[paste0(nm, "_n2")]] <- sum(gv == cp[2], na.rm = TRUE)
      row[[paste0(nm, "_W")]]  <- NA
      row[[paste0(nm, "_p")]]  <- NA
      row[[paste0(nm, "_r")]]  <- NA
    }
    return(row)
  }

  kw   <- kruskal.test(xv ~ gv)                    # run Kruskal-Wallis test
  eta2 <- kw_eta2(kw$statistic, k, n)               # compute effect size

  row <- data.frame(
    Variabile = col, N = n,
    KW_H = round(kw$statistic, 3), KW_df = kw$parameter,  # KW test statistic and df
    KW_p = fmt_p(kw$p.value),   KW_eta2 = eta2,            # formatted p-value and effect size
    stringsAsFactors = FALSE
  )

  # Post-hoc pairwise Mann-Whitney for all three diagnosis pairs
  for (cp in coppie_diag) {
    nm <- paste(cp, collapse = "_vs_")          # column name stem for this pair
    g1 <- xv[gv == cp[1]]; g2 <- xv[gv == cp[2]]  # split by group
    mw <- safe_mw(g1, g2)                       # run the test (or return NULL)
    row[[paste0(nm, "_n1")]] <- length(g1)
    row[[paste0(nm, "_n2")]] <- length(g2)
    row[[paste0(nm, "_W")]]  <- if (!is.null(mw)) round(mw$statistic, 1)                       else NA  # test statistic
    row[[paste0(nm, "_p")]]  <- if (!is.null(mw)) fmt_p(mw$p.value)                           else NA  # raw p-value
    row[[paste0(nm, "_r")]]  <- if (!is.null(mw)) mw_effect_r(mw, length(g1) + length(g2))   else NA  # effect size r
  }
  row
}))

# Apply FDR correction to KW omnibus p-values across all variables
res_diagnosi$KW_p_fdr <- fmt_p(p.adjust(res_diagnosi$KW_p, method = "BH"))

# Apply FDR correction to each post-hoc pair separately across all variables
for (cp in coppie_diag) {
  nm <- paste(cp, collapse = "_vs_")
  pcol <- paste0(nm, "_p")
  res_diagnosi[[paste0(nm, "_p_fdr")]] <-
    fmt_p(p.adjust(res_diagnosi[[pcol]], method = "BH"))  # BH-corrected p per pair
}

write.csv(res_diagnosi, "test_diagnosi.csv", row.names = FALSE)  # save Step 1 results
cat("Salvato: test_diagnosi.csv\n")

# ==============================================================================
# STEP 2: LGBTQ+ EFFECT — Mann-Whitney
# ==============================================================================

cat("\n--- 2. Mann-Whitney (LGBT) ---\n")
# For prior_ric and prior_salute: compare lgbtq vs no_lgbtq using all respondents
# For prior_aut: compare lgbtq vs no_lgbtq WITHIN the autism group only

# Helper: runs Mann-Whitney for lgbtq vs no_lgbtq within a given data frame
run_mw_lgbt <- function(col, df_base) {
  df  <- df_base[!is.na(df_base$gruppo_lgbt), ]  # exclude unclassified respondents
  x   <- df[[col]]; grp <- droplevels(df$gruppo_lgbt)
  ok  <- !is.na(x); xv  <- x[ok]; gv <- grp[ok]  # remove NA values
  g1  <- xv[gv == "lgbtq"]; g2 <- xv[gv == "no_lgbtq"]  # split by LGBTQ+ group
  mw  <- safe_mw(g1, g2)   # run test or return NULL
  if (is.null(mw)) {
    return(data.frame(Variabile = col,
      N_lgbtq = length(g1), N_no_lgbtq = length(g2),
      MW_W = NA, MW_p = NA, MW_r = NA, stringsAsFactors = FALSE))
  }
  data.frame(
    Variabile   = col,
    N_lgbtq     = length(g1), N_no_lgbtq = length(g2),
    MW_W        = round(mw$statistic, 1),
    MW_p        = fmt_p(mw$p.value),
    MW_r        = mw_effect_r(mw, length(g1) + length(g2)),  # effect size
    stringsAsFactors = FALSE
  )
}

data_aut <- data[!is.na(data$gruppo_diag) & data$gruppo_diag == "aut", ]  # autism-only subset

res_lgbt <- rbind(
  do.call(rbind, lapply(cols_diag, run_mw_lgbt, df_base = data)),     # prior_ric/salute: full sample
  do.call(rbind, lapply(cols_aut,  run_mw_lgbt, df_base = data_aut))  # prior_aut: autism only
)

res_lgbt$MW_p_fdr <- fmt_p(p.adjust(res_lgbt$MW_p, method = "BH"))  # FDR correction across all variables
write.csv(res_lgbt, "test_lgbt.csv", row.names = FALSE)  # save Step 2 results
cat("Salvato: test_lgbt.csv\n")

# ==============================================================================
# STEP 3: PAIRWISE COMPARISONS ACROSS ALL 6 CROSSED GROUPS
# ==============================================================================

cat("\n--- 3. Mann-Whitney pairwise (6 gruppi incrociati) ---\n")

coppie6 <- combn(levels(data$gruppo6), 2, simplify = FALSE)  # all 15 pairs of the 6 crossed groups

# prior_aut excluded: only autism group responds, so pairwise comparison is meaningless
res_pw <- do.call(rbind, lapply(cols_diag, function(col) {
  do.call(rbind, lapply(coppie6, function(cp) {

    # Observations for each group (non-NA values only)
    g1 <- data[[col]][!is.na(data$gruppo6) & data$gruppo6 == cp[1] & !is.na(data[[col]])]
    g2 <- data[[col]][!is.na(data$gruppo6) & data$gruppo6 == cp[2] & !is.na(data[[col]])]
    mw <- safe_mw(g1, g2)  # run test or return NULL

    if (is.null(mw)) {
      return(data.frame(Variabile = col, Gruppo_1 = cp[1], Gruppo_2 = cp[2],
        N1 = length(g1), N2 = length(g2),
        MW_W = NA, MW_p = NA, MW_r = NA, stringsAsFactors = FALSE))
    }

    data.frame(
      Variabile = col, Gruppo_1 = cp[1], Gruppo_2 = cp[2],
      N1 = length(g1), N2 = length(g2),
      MW_W = round(mw$statistic, 1),
      MW_p = fmt_p(mw$p.value),
      MW_r = mw_effect_r(mw, length(g1) + length(g2)),  # effect size r
      stringsAsFactors = FALSE
    )
  }))
}))

# FDR correction applied per variable across its 15 pairwise comparisons
res_pw <- res_pw %>%
  group_by(Variabile) %>%
  mutate(MW_p_fdr = fmt_p(p.adjust(MW_p, method = "BH"))) %>%  # BH-corrected per variable
  ungroup() %>%
  as.data.frame()

write.csv(res_pw, "test_pairwise.csv", row.names = FALSE)  # save Step 3 results
cat("Salvato: test_pairwise.csv\n")

# ==============================================================================
# SUMMARY: print variables with at least one significant result (FDR < 0.05)
# ==============================================================================

cat("\n\n======================================================\n")
cat("RIEPILOGO — variabili con effetto significativo (FDR < 0.05)\n")
cat("======================================================\n")

# Step 1: KW omnibus
cat("\n1. Kruskal-Wallis diagnosi (omnibus):\n")
sig <- res_diagnosi[!is.na(res_diagnosi$KW_p_fdr) & res_diagnosi$KW_p_fdr < 0.05,
                    c("Variabile","KW_H","KW_df","KW_p","KW_p_fdr","KW_eta2")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

# Step 2: LGBTQ+ effect
cat("\n2. Mann-Whitney LGBT:\n")
sig <- res_lgbt[!is.na(res_lgbt$MW_p_fdr) & res_lgbt$MW_p_fdr < 0.05,
                c("Variabile","N_lgbtq","N_no_lgbtq","MW_W","MW_p","MW_p_fdr","MW_r")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

# Step 3: pairwise 6 groups
cat("\n3. Pairwise 6 gruppi (coppie con FDR < 0.05):\n")
sig <- res_pw[!is.na(res_pw$MW_p_fdr) & res_pw$MW_p_fdr < 0.05,
              c("Variabile","Gruppo_1","Gruppo_2","N1","N2","MW_W","MW_p","MW_p_fdr","MW_r")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

# ==============================================================================
# COMBINED CSV — all three sections in a single long-format file
# Each row = one test result, tagged with section and comparison labels
# ==============================================================================

# Section 1: KW omnibus (one row per variable, only prior_ric and prior_salute)
csv_diag <- data.frame(
  Sezione   = "1_diagnosi_KW",
  Variabile = res_diagnosi$Variabile,
  Confronto = "omnibus",           # label: omnibus test (not pairwise)
  Gruppo_1  = NA_character_,
  Gruppo_2  = NA_character_,
  N1        = res_diagnosi$N,
  N2        = NA_integer_,
  Statistica = res_diagnosi$KW_H,
  df        = res_diagnosi$KW_df,
  p         = res_diagnosi$KW_p,
  p_fdr     = res_diagnosi$KW_p_fdr,
  effect    = res_diagnosi$KW_eta2,
  effect_label = "eta2",           # effect size metric for this section
  stringsAsFactors = FALSE
)

# Section 1 post-hoc: one row per pairwise comparison per variable
csv_diag_ph <- do.call(rbind, lapply(coppie_diag, function(cp) {
  nm <- paste(cp, collapse = "_vs_")
  data.frame(
    Sezione   = "1_diagnosi_posthoc",
    Variabile = res_diagnosi$Variabile,
    Confronto = nm,               # e.g. "aut_vs_altra_diagnosi"
    Gruppo_1  = cp[1], Gruppo_2 = cp[2],
    N1        = res_diagnosi[[paste0(nm, "_n1")]],
    N2        = res_diagnosi[[paste0(nm, "_n2")]],
    Statistica = res_diagnosi[[paste0(nm, "_W")]],
    df        = NA_real_,
    p         = res_diagnosi[[paste0(nm, "_p")]],
    p_fdr     = res_diagnosi[[paste0(nm, "_p_fdr")]],
    effect    = res_diagnosi[[paste0(nm, "_r")]],
    effect_label = "r",           # effect size metric for Mann-Whitney
    stringsAsFactors = FALSE
  )
}))

# Section 2: LGBTQ+ effect (one row per variable)
csv_lgbt <- data.frame(
  Sezione   = "2_lgbt",
  Variabile = res_lgbt$Variabile,
  Confronto = "lgbtq_vs_no_lgbtq",
  Gruppo_1  = "lgbtq", Gruppo_2 = "no_lgbtq",
  N1        = res_lgbt$N_lgbtq,
  N2        = res_lgbt$N_no_lgbtq,
  Statistica = res_lgbt$MW_W,
  df        = NA_real_,
  p         = res_lgbt$MW_p,
  p_fdr     = res_lgbt$MW_p_fdr,
  effect    = res_lgbt$MW_r,
  effect_label = "r",
  stringsAsFactors = FALSE
)

# Section 3: pairwise 6 groups (one row per pair per variable)
csv_pw <- data.frame(
  Sezione   = "3_pairwise6",
  Variabile = res_pw$Variabile,
  Confronto = paste(res_pw$Gruppo_1, res_pw$Gruppo_2, sep = "_vs_"),
  Gruppo_1  = res_pw$Gruppo_1,
  Gruppo_2  = res_pw$Gruppo_2,
  N1        = res_pw$N1,
  N2        = res_pw$N2,
  Statistica = res_pw$MW_W,
  df        = NA_real_,
  p         = res_pw$MW_p,
  p_fdr     = res_pw$MW_p_fdr,
  effect    = res_pw$MW_r,
  effect_label = "r",
  stringsAsFactors = FALSE
)

csv_all <- rbind(csv_diag, csv_diag_ph, csv_lgbt, csv_pw)  # combine all sections
write.csv(csv_all, "test_risultati.csv", row.names = FALSE)  # save the combined file
cat("Salvato: test_risultati.csv\n")

# ==============================================================================
# PDF — one page per variable with all four test tables
# ==============================================================================

library(grid)       # viewports and text/rectangle grobs
library(gridExtra)  # tableGrob and arrangeGrob

# Significance colour scheme for table backgrounds
col_sig   <- "#c0392b"  # dark red  — FDR < 0.05
col_trend <- "#e67e22"  # orange    — FDR < 0.10 (trend)
col_ns    <- "grey30"   # dark grey — not significant

# Returns text and background colour based on p-value and FDR-adjusted p-value
cell_color_p <- function(p, p_fdr) {
  if (is.na(p)) return(list(txt = "—", col = col_ns))  # NA: dash and grey
  txt <- formatC(p, format = "f", digits = 4)  # format to 4 decimal places
  # Append significance asterisks based on FDR threshold
  if (!is.na(p_fdr)) {
    if (p_fdr < 0.05)  txt <- paste0(txt, " *")
    if (p_fdr < 0.01)  txt <- paste0(txt, "*")   # ** for < 0.01
    if (p_fdr < 0.001) txt <- paste0(txt, "*")   # *** for < 0.001
  }
  # Background colour: red if significant, orange if trend, grey otherwise
  col <- if (!is.na(p_fdr) && p_fdr < 0.05) col_sig else
         if (!is.na(p_fdr) && p_fdr < 0.10) col_trend else col_ns
  list(txt = txt, col = col)
}

# Builds a styled tableGrob with significance-coloured p and p_fdr columns
make_test_grob <- function(df_tab, title) {
  if (is.null(df_tab) || nrow(df_tab) == 0) return(NULL)  # nothing to draw

  n_col <- ncol(df_tab)
  n_row <- nrow(df_tab)

  # Default header fill: light grey for all columns
  head_fill <- matrix("grey88", nrow = 1, ncol = n_col)

  # Alternating white/light-grey row backgrounds for the data cells
  core_fill <- matrix(
    rep(c("white", "grey97"), length.out = n_row * n_col),
    nrow = n_row, ncol = n_col
  )

  # Colour the p and p_fdr columns based on significance
  p_col_idx    <- which(names(df_tab) == "p")
  pfdr_col_idx <- which(names(df_tab) == "p_fdr")
  if (length(p_col_idx) && length(pfdr_col_idx)) {
    for (i in seq_len(n_row)) {
      pv   <- suppressWarnings(as.numeric(df_tab[i, p_col_idx]))
      pfdr <- suppressWarnings(as.numeric(df_tab[i, pfdr_col_idx]))
      # Choose background: pink-red for significant, pale-orange for trend, white otherwise
      clr  <- if (!is.na(pfdr) && pfdr < 0.05) "#fde8e8" else
              if (!is.na(pfdr) && pfdr < 0.10) "#fef3e2" else "white"
      core_fill[i, p_col_idx]    <- clr  # colour the raw p column
      core_fill[i, pfdr_col_idx] <- clr  # colour the FDR p column the same
    }
  }

  tt <- ttheme_default(
    base_size = 6.5,
    core    = list(
      bg_params = list(fill = core_fill, col = "grey80"),  # coloured backgrounds, light border
      fg_params = list(hjust = 1, x = 0.95, fontsize = 6.5)  # right-aligned text
    ),
    colhead = list(
      bg_params = list(fill = head_fill, col = "grey70"),
      fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5, fontsize = 6.5)  # centred bold header
    )
  )

  grob <- tableGrob(df_tab, rows = NULL, theme = tt)  # build the table grob

  # Add a section title above the table
  title_grob <- textGrob(title, gp = gpar(fontsize = 8, fontface = "bold",
                                           col = "#2c3e50"), hjust = 0, x = 0.01)
  arrangeGrob(title_grob, grob, ncol = 1, heights = unit(c(0.3, 1), c("cm", "null")))  # stack title + table
}

# Generates one PDF page for a given variable showing all four test tables
make_var_page <- function(col) {

  is_aut_var <- startsWith(col, "prior_aut")  # prior_aut vars skip Step 1 and Step 3

  # ---- Table 1: KW omnibus (only for prior_ric and prior_salute) ----
  if (!is_aut_var) {
    row_kw <- res_diagnosi[res_diagnosi$Variabile == col, ]
    tab1 <- data.frame(
      Test  = "Kruskal-Wallis",
      N     = row_kw$N,
      H     = round(row_kw$KW_H, 3),
      df    = row_kw$KW_df,
      p     = fmt_p(row_kw$KW_p),
      p_fdr = fmt_p(row_kw$KW_p_fdr),
      eta2  = round(row_kw$KW_eta2, 4),
      stringsAsFactors = FALSE
    )
    # Table 2: Post-hoc pairwise diagnosis comparisons (3 rows)
    tab2_rows <- lapply(coppie_diag, function(cp) {
      nm <- paste(cp, collapse = "_vs_")
      data.frame(
        Confronto = paste(cp[1], "vs", cp[2]),  # readable pair label
        N1  = row_kw[[paste0(nm, "_n1")]],
        N2  = row_kw[[paste0(nm, "_n2")]],
        W   = round(row_kw[[paste0(nm, "_W")]], 1),
        p   = fmt_p(row_kw[[paste0(nm, "_p")]]),
        p_fdr = fmt_p(row_kw[[paste0(nm, "_p_fdr")]]),
        r   = round(row_kw[[paste0(nm, "_r")]], 4),
        stringsAsFactors = FALSE
      )
    })
    tab2 <- do.call(rbind, tab2_rows)  # combine 3 pair rows
  } else {
    tab1 <- NULL  # no KW table for prior_aut variables
    tab2 <- NULL  # no post-hoc table for prior_aut variables
  }

  # ---- Table 3: LGBTQ+ effect (always shown) ----
  row_lgbt <- res_lgbt[res_lgbt$Variabile == col, ]
  tab3 <- data.frame(
    Confronto  = "lgbtq vs no_lgbtq",
    N_lgbtq    = row_lgbt$N_lgbtq,
    N_no_lgbtq = row_lgbt$N_no_lgbtq,
    W          = round(row_lgbt$MW_W, 1),
    p          = fmt_p(row_lgbt$MW_p),
    p_fdr      = fmt_p(row_lgbt$MW_p_fdr),
    r          = round(row_lgbt$MW_r, 4),
    stringsAsFactors = FALSE
  )

  # ---- Table 4: Pairwise 6 groups (only for prior_ric and prior_salute) ----
  if (!is_aut_var) {
    pw_col <- res_pw[res_pw$Variabile == col,
                     c("Gruppo_1","Gruppo_2","N1","N2","MW_W","MW_p","MW_p_fdr","MW_r")]
    names(pw_col) <- c("Gruppo_1","Gruppo_2","N1","N2","W","p","p_fdr","r")  # rename for consistency
    pw_col$W     <- round(as.numeric(pw_col$W), 1)
    pw_col$p     <- sapply(pw_col$p,     function(x) fmt_p(as.numeric(x)))
    pw_col$p_fdr <- sapply(pw_col$p_fdr, function(x) fmt_p(as.numeric(x)))
    pw_col$r     <- round(as.numeric(pw_col$r), 4)
  } else {
    pw_col <- NULL  # no pairwise table for prior_aut variables
  }

  # Build grobs (NULL if the section does not apply to this variable)
  g1 <- if (!is.null(tab1))   make_test_grob(tab1,   "1. Kruskal-Wallis omnibus (diagnosi)")  else NULL
  g2 <- if (!is.null(tab2))   make_test_grob(tab2,   "2. Post-hoc Mann-Whitney (diagnosi)")   else NULL
  g3 <-                        make_test_grob(tab3,   "3. Mann-Whitney effetto LGBT")
  g4 <- if (!is.null(pw_col)) make_test_grob(pw_col, "4. Mann-Whitney pairwise (6 gruppi)")   else NULL

  grobs  <- Filter(Negate(is.null), list(g1, g2, g3, g4))  # drop NULL grobs
  if (length(grobs) == 0) return(invisible(NULL))  # nothing to draw for this variable

  grid.newpage()

  # Fixed header at the top: variable name + legend key
  pushViewport(viewport(x = 0, y = 0.88, width = 1, height = 0.12,
                         just = c("left", "bottom")))
  grid.text(col,
            x = 0.02, y = 0.85, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 12, fontface = "bold", col = "#2c3e50"))  # variable name
  grid.text("* FDR<0.05  ** FDR<0.01  *** FDR<0.001  |  sfondo rosso = significativo, arancio = tendenza",
            x = 0.02, y = 0.30, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 6, col = "grey50"))  # significance legend
  popViewport()

  # Left column: KW omnibus (Table 1) + post-hoc diagnosis (Table 2)
  vp_left  <- viewport(x = 0.01, y = 0.02, width = 0.48, height = 0.85,
                        just = c("left", "bottom"))
  # Right column: LGBTQ+ (Table 3) above, pairwise 6 groups (Table 4) below
  vp_right <- viewport(x = 0.51, y = 0.02, width = 0.48, height = 0.85,
                        just = c("left", "bottom"))

  pushViewport(vp_left)
  grid.draw(arrangeGrob(
    grobs[[1]],                                           # KW omnibus table
    if (length(grobs) >= 2) grobs[[2]] else nullGrob(),  # post-hoc table (or empty)
    ncol = 1, heights = unit(c(1, 3), "null")            # 1/4 KW, 3/4 post-hoc
  ))
  popViewport()

  pushViewport(vp_right)
  g3_grob <- if (length(grobs) >= 3) grobs[[3]] else nullGrob()  # LGBTQ+ table
  g4_grob <- if (length(grobs) >= 4) grobs[[4]] else nullGrob()  # pairwise 6-group table
  grid.draw(arrangeGrob(
    g3_grob, g4_grob,
    ncol = 1, heights = unit(c(1, 5), "null")  # 1/6 LGBTQ+, 5/6 pairwise
  ))
  popViewport()
}

# Generate the full results PDF (one page per variable, section separators)
pdf("test_risultati.pdf", width = 11, height = 8.5, onefile = TRUE)

sezione_corrente <- ""  # track current section to insert separator pages
for (col in cols) {
  tryCatch({
    # Determine which section this variable belongs to
    sezione <- dplyr::case_when(
      startsWith(col, "prior_ric")    ~ "Priorita di ricerca (prior_ric)",
      startsWith(col, "prior_aut")    ~ "Priorita autismo (prior_aut)",
      startsWith(col, "prior_salute") ~ "Priorita salute (prior_salute)"
    )
    # Insert a section separator page at the start of each new section
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#2c3e50", col = NA))  # dark-blue full-page background
      grid.text(sezione, x = 0.5, y = 0.5,
                gp = gpar(col = "white", fontsize = 20, fontface = "bold"))
    }
    make_var_page(col)  # draw the test-table page for this variable
  }, error = function(e) {
    message("Errore su ", col, ": ", conditionMessage(e))
    grid.newpage()
    grid.text(paste0("Errore su: ", col, "\n", conditionMessage(e)),
              x = 0.5, y = 0.5, gp = gpar(fontsize = 9, col = "red"))  # error page
  })
}

dev.off()
cat("Salvato: test_risultati.pdf\n")

# ==============================================================================
# SIGNIFICANT-RESULTS PDF — one page per variable with FDR < 0.05 results
#
# For each variable that has at least one significant result, produces a page
# containing:
#   - Table of significant comparisons only
#   - Automatically generated text describing the direction of effects
#   - Empty space for manual notes
# ==============================================================================

# ------------------------------------------------------------------------------
# direzione_effetto(): computes the direction of the effect for a given pair
# Returns a list with:
#   sym   — ">" if group 1 median > group 2 median, "<" otherwise, "=" if equal
#   med1  — rounded median for group 1
#   med2  — rounded median for group 2
# ------------------------------------------------------------------------------
direzione_effetto <- function(col, grp1_nome, grp2_nome) {
  m1_mask <- !is.na(data$gruppo6) & data$gruppo6 == grp1_nome & !is.na(data[[col]])
  m2_mask <- !is.na(data$gruppo6) & data$gruppo6 == grp2_nome & !is.na(data[[col]])

  # For diagnosis marginal comparisons, use gruppo_diag instead of gruppo6
  if (grp1_nome %in% levels(data$gruppo_diag)) {
    m1_mask <- !is.na(data$gruppo_diag) & data$gruppo_diag == grp1_nome & !is.na(data[[col]])
    m2_mask <- !is.na(data$gruppo_diag) & data$gruppo_diag == grp2_nome & !is.na(data[[col]])
  }
  if (grp1_nome %in% c("lgbtq", "no_lgbtq")) {
    # For LGBTQ+ comparisons, use the appropriate base data frame
    base <- if (startsWith(col, "prior_aut")) {
      data[!is.na(data$gruppo_diag) & data$gruppo_diag == "aut", ]  # autism only for prior_aut
    } else data
    m1_mask <- !is.na(base$gruppo_lgbt) & base$gruppo_lgbt == grp1_nome & !is.na(base[[col]])
    m2_mask <- !is.na(base$gruppo_lgbt) & base$gruppo_lgbt == grp2_nome & !is.na(base[[col]])
    med1 <- median(base[[col]][m1_mask], na.rm = TRUE)
    med2 <- median(base[[col]][m2_mask], na.rm = TRUE)
    sym  <- if (med1 > med2) ">" else if (med1 < med2) "<" else "="
    return(list(sym = sym, med1 = round(med1, 2), med2 = round(med2, 2)))
  }

  med1 <- median(data[[col]][m1_mask], na.rm = TRUE)  # median for group 1
  med2 <- median(data[[col]][m2_mask], na.rm = TRUE)  # median for group 2
  sym  <- if (med1 > med2) ">" else if (med1 < med2) "<" else "="  # direction symbol
  list(sym = sym, med1 = round(med1, 2), med2 = round(med2, 2))
}

# Human-readable labels for groups used in auto-generated text
LABEL_GRUPPI_TEST <- c(
  aut                      = "Aut",
  altra_diagnosi           = "Altra diag.",
  no_diagnosi              = "No diag.",
  lgbtq                    = "LGBT+",
  no_lgbtq                 = "No LGBT+",
  aut_x_lgbtq              = "Aut × LGBT+",
  aut_x_no_lgbtq           = "Aut × No LGBT+",
  altra_diagnosi_x_lgbtq   = "Altra diag. × LGBT+",
  altra_diagnosi_x_no_lgbtq = "Altra diag. × No LGBT+",
  no_diagnosi_x_lgbtq      = "No diag. × LGBT+",
  no_diagnosi_x_no_lgbtq   = "No diag. × No LGBT+"
)

# ------------------------------------------------------------------------------
# genera_testo(): returns a character vector of interpretation sentences for a
# given variable, covering all significant effects (FDR < 0.05).
# Returns NULL if no significant results exist.
# ------------------------------------------------------------------------------
genera_testo <- function(col) {
  frasi <- c()
  is_aut_var <- startsWith(col, "prior_aut")

  # --- Diagnosis effect (KW omnibus + post-hoc) ---
  if (!is_aut_var) {
    row_kw <- res_diagnosi[res_diagnosi$Variabile == col, ]
    kw_fdr <- as.numeric(row_kw$KW_p_fdr)
    if (!is.na(kw_fdr) && kw_fdr < 0.05) {
      frasi <- c(frasi, sprintf(
        "Effetto diagnosi significativo [KW: H=%.2f, p_fdr=%s, eta2=%.3f].",
        row_kw$KW_H, row_kw$KW_p_fdr, row_kw$KW_eta2))  # omnibus result sentence

      # Post-hoc: describe each significant pair
      for (cp in coppie_diag) {
        nm    <- paste(cp, collapse = "_vs_")
        p_fdr <- as.numeric(row_kw[[paste0(nm, "_p_fdr")]])
        if (!is.na(p_fdr) && p_fdr < 0.05) {
          dir   <- direzione_effetto(col, cp[1], cp[2])  # direction of effect
          lab1  <- LABEL_GRUPPI_TEST[cp[1]]
          lab2  <- LABEL_GRUPPI_TEST[cp[2]]
          frasi <- c(frasi, sprintf(
            "  • %s (med=%.2f) %s %s (med=%.2f)  [W=%s, r=%s, p_fdr=%s]",
            lab1, dir$med1, dir$sym, lab2, dir$med2,
            row_kw[[paste0(nm, "_W")]],
            row_kw[[paste0(nm, "_r")]],
            p_fdr))
        }
      }
    }
  }

  # --- LGBTQ+ effect ---
  row_lgbt <- res_lgbt[res_lgbt$Variabile == col, ]
  lgbt_fdr <- as.numeric(row_lgbt$MW_p_fdr)
  if (!is.na(lgbt_fdr) && lgbt_fdr < 0.05) {
    dir   <- direzione_effetto(col, "lgbtq", "no_lgbtq")  # direction of LGBTQ+ effect
    frasi <- c(frasi, sprintf(
      "Effetto LGBT+ significativo: LGBT+ (med=%.2f) %s No LGBT+ (med=%.2f)  [W=%s, r=%s, p_fdr=%s].",
      dir$med1, dir$sym, dir$med2,
      row_lgbt$MW_W, row_lgbt$MW_r, row_lgbt$MW_p_fdr))
  }

  # --- Pairwise 6-group comparisons ---
  if (!is_aut_var) {
    pw_sig <- res_pw[res_pw$Variabile == col &
                       !is.na(res_pw$MW_p_fdr) &
                       as.numeric(res_pw$MW_p_fdr) < 0.05, ]
    if (nrow(pw_sig) > 0) {
      frasi <- c(frasi, "Confronti pairwise significativi (6 gruppi):")  # section header
      for (i in seq_len(nrow(pw_sig))) {
        r <- pw_sig[i, ]
        dir  <- direzione_effetto(col, r$Gruppo_1, r$Gruppo_2)
        lab1 <- LABEL_GRUPPI_TEST[r$Gruppo_1]
        lab2 <- LABEL_GRUPPI_TEST[r$Gruppo_2]
        frasi <- c(frasi, sprintf(
          "  • %s (med=%.2f) %s %s (med=%.2f)  [W=%s, r=%s, p_fdr=%s]",
          lab1, dir$med1, dir$sym, lab2, dir$med2,
          r$MW_W, r$MW_r, r$MW_p_fdr))
      }
    }
  }

  if (length(frasi) == 0) return(NULL)  # no significant results: return nothing
  frasi
}

# ------------------------------------------------------------------------------
# make_sig_page(): draws one PDF page for a variable with significant results.
# Layout:
#   - Dark-blue header bar with variable name
#   - Table of significant comparisons (coloured by significance level)
#   - Auto-generated interpretation text
#   - Empty space at the bottom for manual notes
# ------------------------------------------------------------------------------
make_sig_page <- function(col) {
  frasi <- genera_testo(col)
  if (is.null(frasi)) return(invisible(NULL))  # nothing significant: skip

  is_aut_var <- startsWith(col, "prior_aut")

  grid.newpage()

  # Header bar: dark-blue background + variable name
  grid.rect(x = 0, y = 0.92, width = 1, height = 0.08,
            just = c("left", "bottom"),
            gp = gpar(fill = "#2c3e50", col = NA))
  grid.text(col, x = 0.02, y = 0.97, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 12, fontface = "bold", col = "white"))
  grid.text("Risultati statisticamente significativi (FDR < 0.05)",
            x = 0.99, y = 0.97, hjust = 1, vjust = 1,
            gp = gpar(fontsize = 7, col = "grey80"))  # subtitle in the header

  # ---------- Table of significant comparisons ----------
  tab_sig_rows <- list()  # collect rows for the significant-results table

  # Add diagnosis post-hoc rows if significant (skipped for prior_aut)
  if (!is_aut_var) {
    row_kw <- res_diagnosi[res_diagnosi$Variabile == col, ]
    for (cp in coppie_diag) {
      nm    <- paste(cp, collapse = "_vs_")
      p_fdr <- as.numeric(row_kw[[paste0(nm, "_p_fdr")]])
      if (!is.na(p_fdr) && p_fdr < 0.05) {
        tab_sig_rows[[length(tab_sig_rows) + 1]] <- data.frame(
          Sezione   = "Post-hoc diagnosi",
          Confronto = paste(LABEL_GRUPPI_TEST[cp[1]], "vs", LABEL_GRUPPI_TEST[cp[2]]),
          N1 = row_kw[[paste0(nm, "_n1")]],
          N2 = row_kw[[paste0(nm, "_n2")]],
          W  = row_kw[[paste0(nm, "_W")]],
          r  = row_kw[[paste0(nm, "_r")]],
          p  = row_kw[[paste0(nm, "_p")]],
          p_fdr = p_fdr,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Add LGBTQ+ row if significant
  row_lgbt <- res_lgbt[res_lgbt$Variabile == col, ]
  if (!is.na(as.numeric(row_lgbt$MW_p_fdr)) &&
      as.numeric(row_lgbt$MW_p_fdr) < 0.05) {
    tab_sig_rows[[length(tab_sig_rows) + 1]] <- data.frame(
      Sezione   = "LGBT+",
      Confronto = "LGBT+ vs No LGBT+",
      N1 = row_lgbt$N_lgbtq,
      N2 = row_lgbt$N_no_lgbtq,
      W  = row_lgbt$MW_W,
      r  = row_lgbt$MW_r,
      p  = row_lgbt$MW_p,
      p_fdr = as.numeric(row_lgbt$MW_p_fdr),
      stringsAsFactors = FALSE
    )
  }

  # Add pairwise 6-group rows if significant (skipped for prior_aut)
  if (!is_aut_var) {
    pw_sig <- res_pw[res_pw$Variabile == col &
                       !is.na(res_pw$MW_p_fdr) &
                       as.numeric(res_pw$MW_p_fdr) < 0.05, ]
    for (i in seq_len(nrow(pw_sig))) {
      r <- pw_sig[i, ]
      tab_sig_rows[[length(tab_sig_rows) + 1]] <- data.frame(
        Sezione   = "Pairwise 6 gruppi",
        Confronto = paste(LABEL_GRUPPI_TEST[r$Gruppo_1], "vs",
                          LABEL_GRUPPI_TEST[r$Gruppo_2]),
        N1 = r$N1, N2 = r$N2,
        W  = r$MW_W, r  = r$MW_r,
        p  = r$MW_p,
        p_fdr = as.numeric(r$MW_p_fdr),
        stringsAsFactors = FALSE
      )
    }
  }

  # Draw the significant-results table if there is anything to show
  if (length(tab_sig_rows) > 0) {
    tab_sig <- do.call(rbind, tab_sig_rows)  # combine rows

    # Row background colours: darker red for smaller FDR p-values
    n_row <- nrow(tab_sig); n_col <- ncol(tab_sig)
    fill_core <- matrix("white", nrow = n_row, ncol = n_col)
    for (i in seq_len(n_row)) {
      clr <- if (tab_sig$p_fdr[i] < 0.001) "#fad4d4" else  # deep pink for < 0.001
             if (tab_sig$p_fdr[i] < 0.01)  "#fde0e0" else  # medium pink for < 0.01
                                            "#fff0f0"        # pale pink for < 0.05
      fill_core[i, ] <- clr
    }
    tab_sig$p_fdr <- fmt_p(tab_sig$p_fdr)  # format p_fdr for display

    tt <- ttheme_default(
      base_size = 7.5,
      core    = list(bg_params = list(fill = fill_core, col = "grey85"),
                     fg_params = list(hjust = 1, x = 0.95)),
      colhead = list(bg_params = list(fill = "grey88", col = "grey70"),
                     fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5))
    )
    tab_grob <- tableGrob(tab_sig, rows = NULL, theme = tt)  # build table grob

    # Place the table in the upper-middle area of the page
    vp_tab <- viewport(x = 0.01, y = 0.52, width = 0.98, height = 0.38,
                       just = c("left", "bottom"))
    pushViewport(vp_tab)
    grid.draw(tab_grob)
    popViewport()
  }

  # ---------- Auto-generated interpretation text ----------
  grid.text("Interpretazione:",
            x = 0.02, y = 0.50, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 10, fontface = "bold", col = "#2c3e50"))

  y_txt <- 0.45  # starting y-position for the first interpretation sentence
  for (frase in frasi) {
    grid.text(frase, x = 0.03, y = y_txt, hjust = 0, vjust = 1,
              gp = gpar(fontsize = 9, col = "grey15"))
    y_txt <- y_txt - 0.048  # step down for each successive sentence
  }
}

# Collect all variables with at least one significant result (FDR < 0.05)
vars_sig <- unique(c(
  res_diagnosi$Variabile[!is.na(res_diagnosi$KW_p_fdr) &
                           as.numeric(res_diagnosi$KW_p_fdr) < 0.05],  # KW omnibus
  unlist(lapply(coppie_diag, function(cp) {
    nm <- paste(cp, collapse = "_vs_")
    res_diagnosi$Variabile[!is.na(res_diagnosi[[paste0(nm, "_p_fdr")]]) &
                              as.numeric(res_diagnosi[[paste0(nm, "_p_fdr")]]) < 0.05]
  })),  # post-hoc diagnosis pairs
  res_lgbt$Variabile[!is.na(res_lgbt$MW_p_fdr) &
                       as.numeric(res_lgbt$MW_p_fdr) < 0.05],  # LGBTQ+ effect
  res_pw$Variabile[!is.na(res_pw$MW_p_fdr) &
                     as.numeric(res_pw$MW_p_fdr) < 0.05]       # pairwise 6 groups
))
vars_sig <- cols[cols %in% vars_sig]  # restore the original variable order

cat(sprintf("\nVariabili con almeno un effetto significativo (FDR < 0.05): %d\n",
            length(vars_sig)))
cat(paste(vars_sig, collapse = ", "), "\n")

# Generate the significant-results PDF
pdf("test_significativi.pdf", width = 11, height = 8.5, onefile = TRUE)

# Cover page: list all significant variables
grid.newpage()
grid.rect(gp = gpar(fill = "#2c3e50", col = NA))  # dark-blue full-page background
grid.text("Risultati Significativi", x = 0.5, y = 0.60,
          gp = gpar(col = "white", fontsize = 24, fontface = "bold"))
grid.text(sprintf("%d variabili con FDR < 0.05", length(vars_sig)),
          x = 0.5, y = 0.50,
          gp = gpar(col = "grey80", fontsize = 14))
grid.text(paste(strwrap(paste(vars_sig, collapse = " | "), width = 90),
                collapse = "\n"),
          x = 0.5, y = 0.35,
          gp = gpar(col = "grey70", fontsize = 7))  # variable list on the cover
grid.text("Ogni pagina: tabella confronti sig. + interpretazione automatica + spazio note",
          x = 0.5, y = 0.20,
          gp = gpar(col = "grey60", fontsize = 8, fontface = "italic"))

sezione_corrente <- ""
for (col in vars_sig) {
  tryCatch({
    # Determine section for this variable
    sezione <- dplyr::case_when(
      startsWith(col, "prior_ric")    ~ "Priorita di ricerca (prior_ric)",
      startsWith(col, "prior_aut")    ~ "Priorita autismo (prior_aut)",
      startsWith(col, "prior_salute") ~ "Priorita salute (prior_salute)"
    )
    # Insert section separator page if the section has changed
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#34495e", col = NA))  # slightly lighter dark-blue
      grid.text(sezione, x = 0.5, y = 0.5,
                gp = gpar(col = "white", fontsize = 18, fontface = "bold"))
    }
    make_sig_page(col)  # draw the significant-results page for this variable
  }, error = function(e) {
    message("Errore su ", col, ": ", conditionMessage(e))
    grid.newpage()
    grid.text(paste0("Errore su: ", col, "\n", conditionMessage(e)),
              x = 0.5, y = 0.5, gp = gpar(fontsize = 9, col = "red"))  # error page
  })
}

dev.off()
cat(sprintf("Salvato: test_significativi.pdf (%d pagine variabile + copertina + sezioni)\n",
            length(vars_sig)))
