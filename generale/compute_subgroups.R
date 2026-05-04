# ==============================================================================
# compute_subgroups.R
#
# PURPOSE
# -------
# Defines the logic for splitting the LUMEN survey respondents into a
# THREE-LEVEL subgroup structure:
#   categorical variable  ×  diagnosis group  ×  LGBTQ+ group
#
# DIAGNOSIS GROUPS (3):
#   aut            — autism diagnosis (public / private / both)
#   altra_diagnosi — other psychiatric diagnosis, no autism.
#                    Includes respondents with aut=NA (question not shown by
#                    REDCap skip logic) but at least one other diagnosis present.
#                    These 22 subjects were previously unclassified.
#   no_diagnosi    — no diagnosis at all
#
# EXCLUDED from diagnosis groups:
#   7 respondents with diagnosi_si_no==1 but all individual diagnosis columns
#   == 4 or NA — genuinely ambiguous → ESCLUSI_FILTER()
#
# LGBTQ+ GROUPS (2):
#   lgbtq    — trans OR non-binary gender identity OR non-heterosexual
#   no_lgbtq — cisgender AND binary gender identity AND heterosexual
#
# EXCLUDED from LGBTQ+ groups:
#   respondents with NA on all three gender/orientation variables
#   → LGBTQ_ESCLUSI_FILTER()
#
# EXPORTED FUNCTIONS / OBJECTS:
#   DIAGNOSI_FILTERS             — list of 3 diagnosis filter functions
#   LGBT_FILTERS                 — list of 2 LGBTQ+ filter functions
#   ESCLUSI_FILTER(data)         — TRUE for 7 ambiguous diagnosis respondents
#   LGBTQ_ESCLUSI_FILTER(data)   — TRUE for respondents unclassifiable on LGBTQ+
#   compute_subgroups(data, cat_vars)  — builds the nested subgroup list
#   summary_subgroups(subgroups, data) — flattens the list into a data frame
# ==============================================================================


# ------------------------------------------------------------------------------
# Helper: NA check
# ------------------------------------------------------------------------------

# Returns TRUE only if x is not NA (used as a building block for all filters)
is_valid <- function(x) !is.na(x)


# ------------------------------------------------------------------------------
# Diagnosis filters  (data → logical vector, one element per row)
# Each filter:
#   1. verifies that the relevant column is not NA
#   2. applies the expected value condition
# ------------------------------------------------------------------------------

DIAGNOSI_FILTERS <- list(

  # aut: respondent has an autism diagnosis (public=1, private=2, or both=3)
  # i.e. aut %in% {1, 2, 3}  →  aut != 4 and not NA
  aut = function(data) {
    is_valid(data$aut) & data$aut != 4  # value 4 means "no autism diagnosis"
  },

  # altra_diagnosi: no autism diagnosis, but at least one other psychiatric
  # diagnosis is present.
  # "No autism" = aut == 4 (explicit No) OR aut == NA (question not shown).
  # Includes 22 respondents whose aut question was skipped by REDCap logic
  # but who reported at least one other diagnosis.
  altra_diagnosi = function(data) {
    diag_cols <- c("pers", "ansia", "umore", "comp_al", "appr",
                   "psicosi", "ocd", "ptsd", "dipendenza", "altro")

    ha_altra <- Reduce(`|`, lapply(diag_cols, function(col) {
      is_valid(data[[col]]) & data[[col]] != 4
    }))

    # "No autism": explicitly No (aut == 4) OR question not shown (aut NA)
    no_aut <- !is_valid(data$aut) | data$aut == 4

    ha_altra & no_aut
  },

  # no_diagnosi: respondent explicitly answered "no" to any diagnosis
  # diagnosi_si_no == 2 means "No" (and must not be NA)
  no_diagnosi = function(data) {
    is_valid(data$diagnosi_si_no) & data$diagnosi_si_no == 2
  }
)


# ------------------------------------------------------------------------------
# LGBTQ+ filters  (data → logical vector, one element per row)
# Each filter:
#   1. checks that all relevant columns are not NA where needed
#   2. applies inclusion/exclusion logic
# ------------------------------------------------------------------------------

LGBT_FILTERS <- list(

  # lgbtq: respondent meets AT LEAST ONE of the following (OR logic):
  #   - trans_cis == 1       (trans person)
  #   - identita_genere not in {1, 2}  (non-binary / non-man / non-woman)
  #   - orientamento_sex != 1          (non-heterosexual)
  # Each condition is evaluated only where the variable is not NA
  lgbtq = function(data) {
    is_trans      <- is_valid(data$trans_cis)       & data$trans_cis == 1            # trans
    is_id_diversa <- is_valid(data$identita_genere)  & !(data$identita_genere %in% c(1, 2))  # non-binary
    is_or_diverso <- is_valid(data$orientamento_sex) & data$orientamento_sex != 1    # non-hetero

    is_trans | is_id_diversa | is_or_diverso  # any one condition is sufficient
  },

  # no_lgbtq: respondent meets ALL THREE of the following (AND logic):
  #   - cis: trans_cis == 2 and not NA
  #   - binary gender identity: identita_genere in {1, 2} and not NA
  #   - heterosexual: orientamento_sex == 1 and not NA
  # If any of the three variables is NA → FALSE (cannot classify)
  no_lgbtq = function(data) {
    is_cis        <- is_valid(data$trans_cis)       & data$trans_cis == 2            # cisgender
    is_id_binaria <- is_valid(data$identita_genere)  & data$identita_genere %in% c(1, 2)  # man or woman
    is_etero      <- is_valid(data$orientamento_sex) & data$orientamento_sex == 1    # heterosexual

    is_cis & is_id_binaria & is_etero  # all three must hold
  }
)


# ------------------------------------------------------------------------------
# ESCLUSI_FILTER
#
# Identifies respondents who cannot be assigned to any diagnosis group:
#   - answered diagnosi_si_no == 1 ("Yes, I have a diagnosis"), BUT
#   - all individual diagnosis columns are either 4 ("No") or NA
# These 7 subjects are excluded from all diagnosis-based analyses.
# Usage: data <- data[!ESCLUSI_FILTER(data), ]
# ------------------------------------------------------------------------------

ESCLUSI_FILTER <- function(data) {
  diag_cols_all <- c("aut", "pers", "ansia", "umore", "comp_al", "appr",
                     "psicosi", "ocd", "ptsd", "dipendenza", "altro")

  # TRUE if every diagnosis column is NA or == 4
  tutte_no <- Reduce(`&`, lapply(diag_cols_all, function(col) {
    !is_valid(data[[col]]) | data[[col]] == 4
  }))

  is_valid(data$diagnosi_si_no) & data$diagnosi_si_no == 1 & tutte_no
}


# ------------------------------------------------------------------------------
# LGBTQ_ESCLUSI_FILTER
#
# Identifies respondents who cannot be assigned to either LGBTQ+ group because
# all three classification variables (trans_cis, identita_genere,
# orientamento_sex) are NA. These subjects are excluded from LGBTQ+ analyses.
# Usage: data <- data[!LGBTQ_ESCLUSI_FILTER(data), ]
# ------------------------------------------------------------------------------

LGBTQ_ESCLUSI_FILTER <- function(data) {
  !is_valid(data$trans_cis) &
  !is_valid(data$identita_genere) &
  !is_valid(data$orientamento_sex)
}


# ------------------------------------------------------------------------------
# Master list of all categorical variables present in the survey dataset
# ------------------------------------------------------------------------------

ALL_CAT_VARS <- c(
  # consent and demographics
  "consenso", "eta", "sesso_nascita", "trans_cis", "identita_genere",
  "orientamento_sex", "regione", "centro_abitato",
  # education and socioeconomic
  "titolo_studio", "titolo_studio_genitore1", "titolo_studio_genitore2",
  "figli", "quanti_figli", "reddito",
  # mental health service usage and diagnosis
  "servizi_salute_mentale", "diagnosi_si_no",
  # number of diagnoses received (0–12; computed in LUMEN_1_groups_counts.R)
  "n_diagnosi",
  # individual diagnosis variables (value 1-3 = diagnosed, 4 = not diagnosed)
  "aut", "adhd", "pers", "ansia", "umore", "comp_al", "appr",
  "psicosi", "ocd", "ptsd", "dipendenza", "altro",
  # indicator columns added by LUMEN_1_groups_counts.R (Sì/No categoricals)
  "lgbtq", "aut_ind", "altra_diagnosi_ind", "no_diagnosi_ind",
  "livello_aut",  # autism support level
  # referral pathway for each diagnosis
  "invio_aut", "invio_adhd", "invio_person", "invio_ansia", "invio_umore",
  "invio_comp_al", "invio_appr", "invio_psicosi", "invio_ocd", "invio_ptsd",
  "invio_dipendenza", "invio_altro",
  # waiting time to first appointment per diagnosis
  "prima_visita_aut", "prima_visita_adhd", "prima_visita_pers",
  "prima_visita_ansia", "prima_visita_umore", "prima_visita_comp_al",
  "prima_visita_appr", "prima_visita_psicosi", "prima_visita_ocd",
  "prima_visita_ptsd", "prima_visita_dipendenza", "prima_visita_altro",
  # waiting time for official diagnosis per disorder
  "att_diagnosi_aut", "att_diagnosi_adhd", "att_diagnosi_pers",
  "att_diagnosi_ansia", "att_diagnosi_umore", "att_diagnosi_comp_al",
  "att_diagnosi_appr", "att_diagnosi_psicosi", "att_diagnosi_ocd",
  "att_diagnosi_ptsd", "att_diagnosi_dipendenza", "att_diagnosi_altro",
  # whether incorrect diagnoses were received per disorder
  "sbagliate_aut", "sbagliate_adhd", "sbagliate_pers", "sbagliate_ansia",
  "sbagliate_umore", "sbagliate_comp_al", "sbagliate_appr", "sbagliate_psicosi",
  "sbagliate_ocd", "sbagliate_ptsd", "sbagliate_dipendenza", "sbagliate_altro",
  # cost of diagnosis per disorder
  "costo_aut", "costo_adhd", "costo_pers", "costo_ansia", "costo_umore",
  "costo_comp_al", "costo_appr", "costo_psicosi", "costo_ocd", "costo_ptsd",
  "costo_dipendenza", "costo_altro",
  # geographical location where diagnosis was obtained
  "dove_aut", "dove_adhd", "dove_pers", "dove_ansia", "dove_umore",
  "dove_comp_al", "dove_appr", "dove_psicosi", "dove_ocd", "dove_ptsd",
  "dove_dipendenza", "dove_altro",
  # number of professionals seen per disorder
  "n_prof_aut", "n_prof_adhd", "n_prof_pers", "n_prof_ansia", "n_prof_umore",
  "n_prof_comp_al", "n_prof_appr", "n_prof_psicosi", "n_prof_ocd", "n_prof_ptsd",
  "n_prof_dipendenza", "n_prof_altro",
  # autism care pathway variables
  "presa_carico_aut",
  "percorsi_aut___1", "percorsi_aut___2", "percorsi_aut___3", "percorsi_aut___4",
  "percorsi_aut___5", "percorsi_aut___6", "percorsi_aut___7", "percorsi_aut___8",
  "durata_percorsi", "sei_in_carico", "interruzione_percorso",
  "questionario_lumen_complete"  # REDCap completion status
)


# ------------------------------------------------------------------------------
# build_group_entries()
#
# INTERNAL HELPER: given a base logical filter (which rows belong to a group)
# and a set of categorical variables, builds a nested list:
#   result[[var_name]][[value_as_string]] = list(var, value, index, count)
#
# Only subjects with a non-NA answer (is_valid) are counted.
# Subjects outside base_filter are excluded entirely.
# ------------------------------------------------------------------------------

build_group_entries <- function(data, base_filter, cat_vars) {
  result <- list()  # initialise output container

  for (var_name in cat_vars) {
    col <- data[[var_name]]  # extract the column vector for this variable

    # Subjects in the group AND with a valid (non-NA) answer for this variable
    valid_mask <- base_filter & is_valid(col)
    valori     <- sort(unique(col[valid_mask]))  # sorted unique observed values

    result[[var_name]] <- list()  # initialise nested list for this variable

    for (val in valori) {
      index <- valid_mask & col == val  # logical mask for this specific value
      key   <- as.character(val)        # string key for the nested list
      result[[var_name]][[key]] <- list(
        var   = var_name,   # variable name
        value = val,        # original (numeric) value
        index = index,      # logical mask of matching rows
        count = sum(index)  # number of matching subjects
      )
    }
  }

  result  # return the complete nested list for one group
}


# ------------------------------------------------------------------------------
# compute_subgroups()
#
# MAIN FUNCTION: returns the full nested subgroup structure:
#   result[[diagnosi]][[lgbt]][[variabile]][[valore]]
# Each leaf is: list(var, value, index, count)
#
# Structure produced:
#   ① 6 crossed cells  (3 diagnosis × 2 LGBTQ+)
#   ② 3 diagnosis marginals (lgbt slot = "tot")
#   ③ 2 LGBTQ+ marginals   (diagnosi slot = "tot")
# ------------------------------------------------------------------------------

compute_subgroups <- function(data, cat_vars = NULL) {

  # Default: use all ALL_CAT_VARS that are actually present in data
  if (is.null(cat_vars)) cat_vars <- intersect(ALL_CAT_VARS, names(data))

  result <- list()  # top-level output list

  # ------------------------------------------------------------------
  # ① Crossed cells: 3 diagnosis × 2 LGBTQ+ = 6 combinations
  # ------------------------------------------------------------------
  for (d_nome in names(DIAGNOSI_FILTERS)) {
    d_filter <- DIAGNOSI_FILTERS[[d_nome]](data)  # logical vector for this diagnosis group
    result[[d_nome]] <- list()

    for (l_nome in names(LGBT_FILTERS)) {
      l_filter    <- LGBT_FILTERS[[l_nome]](data)  # logical vector for this LGBTQ+ group
      base_filter <- d_filter & l_filter            # intersection: must belong to BOTH groups

      # Build the variable-level entries for this crossed cell
      result[[d_nome]][[l_nome]] <- build_group_entries(data, base_filter, cat_vars)
    }
  }

  # ------------------------------------------------------------------
  # ② Diagnosis marginals: each diagnosis group, ignoring LGBTQ+ status
  #    Stored as result[["aut_tot"]][["tot"]], etc.
  # ------------------------------------------------------------------
  for (d_nome in names(DIAGNOSI_FILTERS)) {
    d_filter <- DIAGNOSI_FILTERS[[d_nome]](data)  # diagnosis filter only (no LGBTQ+ condition)
    d_key    <- paste0(d_nome, "_tot")            # e.g. "aut_tot"

    result[[d_key]] <- list()
    result[[d_key]][["tot"]] <- build_group_entries(data, d_filter, cat_vars)  # "tot" = no LGBTQ+ split
  }

  # ------------------------------------------------------------------
  # ③ LGBTQ+ marginals: each LGBTQ+ group, ignoring diagnosis
  #    Stored as result[["tot"]][["lgbtq_tot"]], etc.
  # ------------------------------------------------------------------
  if (is.null(result[["tot"]])) result[["tot"]] <- list()  # ensure the "tot" slot exists

  for (l_nome in names(LGBT_FILTERS)) {
    l_filter <- LGBT_FILTERS[[l_nome]](data)  # LGBTQ+ filter only (no diagnosis condition)
    l_key    <- paste0(l_nome, "_tot")        # e.g. "lgbtq_tot"

    result[["tot"]][[l_key]] <- build_group_entries(data, l_filter, cat_vars)
  }

  return(result)  # return the complete 3-level nested list
}


# ------------------------------------------------------------------------------
# summary_subgroups()
#
# Flattens the nested list produced by compute_subgroups() into a data frame.
#
# Output columns:
#   variabile       — variable name
#   valore          — raw numeric value
#   label           — human-readable label (from the paired .factor column)
#   diagnosi        — diagnosis group name (or "aut_tot", etc.)
#   lgbt            — LGBTQ+ group name (or "tot", "lgbtq_tot", etc.)
#   count           — number of subjects in this cell
#   totale_gruppo   — total subjects in the group (regardless of variable)
#   totale_var      — subjects in the group with a valid answer for this variable
#                     (denominator of pct; sum of pct over values = 100%)
#   totale_soggetti — total subjects in the full dataset (nrow(data))
#   pct             — count / totale_var * 100, rounded to 1 decimal
# ------------------------------------------------------------------------------

summary_subgroups <- function(subgroups, data) {

  totale_soggetti <- nrow(data)  # total number of survey respondents
  rows <- list()                 # collector for output rows

  for (d_nome in names(subgroups)) {
    for (l_nome in names(subgroups[[d_nome]])) {

      # Reconstruct the base logical mask for this group
      if (d_nome == "tot") {
        # LGBTQ+ marginal: remove the "_tot" suffix to retrieve the filter name
        l_base      <- sub("_tot$", "", l_nome)
        gruppo_mask <- LGBT_FILTERS[[l_base]](data)

      } else if (grepl("_tot$", d_nome)) {
        # Diagnosis marginal: remove the "_tot" suffix to retrieve the filter name
        d_base      <- sub("_tot$", "", d_nome)
        gruppo_mask <- DIAGNOSI_FILTERS[[d_base]](data)

      } else {
        # Standard crossed cell: intersect diagnosis and LGBTQ+ filters
        gruppo_mask <- DIAGNOSI_FILTERS[[d_nome]](data) &
                       LGBT_FILTERS[[l_nome]](data)
      }

      totale_gruppo_base <- sum(gruppo_mask)  # N subjects in this group

      for (var_name in names(subgroups[[d_nome]][[l_nome]])) {

        col        <- data[[var_name]]                   # numeric column
        factor_col <- data[[paste0(var_name, ".factor")]] # labelled factor column (may be NULL)

        # Denominator: subjects in the group who gave a valid (non-NA) answer
        totale_var <- sum(gruppo_mask & is_valid(col))

        for (key in names(subgroups[[d_nome]][[l_nome]][[var_name]])) {
          entry <- subgroups[[d_nome]][[l_nome]][[var_name]][[key]]  # leaf list

          # Look up the human-readable label from the .factor column
          label <- NA_character_
          if (!is.null(factor_col)) {
            # Find the first row that matches this value to extract the factor label
            idx <- which(is_valid(data[[var_name]]) &
                           as.character(data[[var_name]]) == key)[1]
            if (length(idx) > 0 && !is.na(idx))
              label <- as.character(factor_col[idx])
          }

          # Append one row to the output list
          rows[[length(rows) + 1]] <- data.frame(
            variabile       = var_name,
            valore          = key,
            label           = label,
            diagnosi        = d_nome,
            lgbt            = l_nome,
            count           = entry$count,
            totale_gruppo   = totale_gruppo_base,
            totale_var      = totale_var,
            totale_soggetti = totale_soggetti,
            pct             = if (totale_var > 0)             # avoid division by zero
                                round(entry$count / totale_var * 100, 1)
                              else NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  do.call(rbind, rows)  # bind all rows into a single data frame and return
}
