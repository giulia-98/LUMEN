# ==============================================================================
# LUMEN_1_groups_counts.R
#
# PURPOSE
# -------
# Entry point for Step 1 of the LUMEN analysis pipeline.
# Loads the survey data, computes the three-level subgroup structure
# (categorical variable × diagnosis × LGBTQ+), and exports a sorted CSV
# containing counts and percentages for every variable × group combination.
#
# INPUT
#   LUMEN_DATA.csv          — raw REDCap export (loaded via load_lumen_data.R)
#
# OUTPUT
#   groups_counts.csv       — flat table with columns:
#                             variabile, valore, label, diagnosi, lgbt,
#                             count, totale_gruppo, totale_var,
#                             totale_soggetti, pct
#
# ROW ORDER IN OUTPUT
#   1. Diagnosis marginals (aut_tot, altra_diagnosi_tot, no_diagnosi_tot)
#   2. LGBTQ+ marginals    (lgbtq_tot, no_lgbtq_tot)
#   3. All 6 crossed combinations (diagnosi × lgbt)
#
# DEPENDENCIES
#   load_lumen_data.R       — data loading and labelling
#   compute_subgroups.R     — subgroup filter logic and summary function
#   Hmisc                   — required by load_lumen_data.R
# ==============================================================================

rm(list = ls())      # clear all objects from the workspace
graphics.off()       # close all open graphics devices

library(Hmisc)  # required for label() used in load_lumen_data()

# Load the data-loading and subgroup-computation helpers
source("load_lumen_data.R")
source("compute_subgroups.R")

data <- load_lumen_data()  # read LUMEN_DATA.csv, apply labels/factors, filter to consenting respondents

# Keep only consenting respondents (redundant safety check; load_lumen_data() already filters)
data <- data[data$consenso == 1 & !is.na(data$consenso), ]

# ==============================================================================
# N_DIAGNOSI
#
# Counts the number of diagnoses received by each respondent.
# Diagnosis variables (aut, adhd, pers, ansia, umore, comp_al, appr, psicosi,
# ocd, ptsd, dipendenza, altro) are coded: 1=pubblico, 2=privato, 3=entrambi,
# 4=no. A diagnosis is present when the value is 1, 2 or 3.
# NA values (question not shown because diagnosi_si_no == 2) are treated as 0.
# Result is an integer from 0 to 12, inserted after diagnosi_si_no.
# ==============================================================================

diag_vars <- c("aut", "adhd", "pers", "ansia", "umore",
               "comp_al", "appr", "psicosi", "ocd", "ptsd",
               "dipendenza", "altro")

data$n_diagnosi <- rowSums(
  sapply(diag_vars, function(v) data[[v]] %in% c(1, 2, 3)),
  na.rm = TRUE
)
label(data$n_diagnosi) <- "Numero di diagnosi ricevute"

# Reorder columns: insert n_diagnosi right after diagnosi_si_no
col_order <- colnames(data)
insert_after <- which(col_order == "diagnosi_si_no")
col_order <- append(
  col_order[col_order != "n_diagnosi"],
  "n_diagnosi",
  after = insert_after
)
data <- data[, col_order]

# ==============================================================================
# COMPUTE SUBGROUPS
#
# Produces a nested list:
#   subgroups[[diagnosi]][[lgbt]][[variabile]][[valore]]
# with 6 crossed cells + 5 marginal groups (see compute_subgroups.R header).
# ==============================================================================

subgroups <- compute_subgroups(data)  # build the full nested subgroup structure

df <- summary_subgroups(subgroups, data)  # flatten to a data frame (one row per variable × value × group)

# ==============================================================================
# SORT OUTPUT ROWS
# Order: diagnosis marginals first, then LGBTQ+ marginals, then crossed cells.
# This makes the CSV easier to read and matches the expected report layout.
# ==============================================================================

# Define the three sets of group identifiers used for sorting
gruppi_totali_diagnosi <- c("aut_tot", "altra_diagnosi_tot", "no_diagnosi_tot")  # diagnosis marginals
gruppi_totali_lgbt     <- c("lgbtq_tot", "no_lgbtq_tot")                         # LGBTQ+ marginals

# Subset rows belonging to each category
df_tot_diagnosi <- df[df$diagnosi %in% gruppi_totali_diagnosi, ]  # rows for diagnosis marginals

df_tot_lgbt     <- df[df$lgbt     %in% gruppi_totali_lgbt, ]      # rows for LGBTQ+ marginals

df_combinazioni <- df[!(df$diagnosi %in% gruppi_totali_diagnosi) &
                      !(df$lgbt     %in% gruppi_totali_lgbt), ]   # rows for all 6 crossed cells

# Concatenate in the desired order: diagnosi totals → LGBT totals → crossed cells
df_ordinato <- rbind(df_tot_diagnosi, df_tot_lgbt, df_combinazioni)

# ==============================================================================
# SAVE OUTPUT CSV
# Resolves the script's own directory robustly, supporting both:
#   - RStudio source() calls  (reads ofile from call frames)
#   - Rscript from terminal   (reads --file= argument)
# Falls back to getwd() if neither is available.
# ==============================================================================

script_dir <- tryCatch({
  frames <- sys.frames()                                               # all active call frames
  ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))  # extract ofile where present
  if (length(ofiles) > 0) {
    dirname(normalizePath(ofiles[[length(ofiles)]]))  # use the innermost sourced file path
  } else {
    args     <- commandArgs(trailingOnly = FALSE)                   # get command-line arguments
    file_arg <- args[grepl("^--file=", args)]                       # find the --file= argument
    if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()  # derive directory or fall back to wd
  }
}, error = function(e) getwd())  # if anything fails, use current working directory

output_path <- file.path(script_dir, "groups_counts.csv")  # build the full output file path
write.csv(df_ordinato, output_path, row.names = FALSE)      # write the sorted data frame; no row indices
message("File salvato in: ", output_path)                   # confirm the save location

# Save the enriched respondent-level data frame (with n_diagnosi column)
data_path <- file.path(script_dir, "LUMEN_DATA_processed.csv")  # output path next to the script
write.csv(data, data_path, row.names = FALSE)                    # write all respondents x variables
message("Dataframe salvato in: ", data_path)                     # confirm the save location
