# ==============================================================================
# LUMEN_3_stats_prior.R
#
# PURPOSE
# -------
# Computes descriptive statistics for all "priority" variables (prior_ric,
# prior_aut, prior_salute), measured on a 0–10 continuous scale.
# Statistics are calculated for the total sample and for every diagnosis ×
# LGBTQ+ subgroup defined in compute_subgroups.R.
#
# Outputs a CSV table and a PDF with side-by-side boxplots and summary tables
# for each variable, one page per variable.
#
# INPUT
#   LUMEN_DATA.csv          — raw survey data (loaded via load_lumen_data.R)
#
# OUTPUT
#   statistiche_prior.csv   — one row per variable × subgroup, columns:
#                             Sottogruppo, Variabile, Gruppo_var,
#                             N_validi, N_mancanti, Media, DS,
#                             Min, Q1, Mediana, Q3, Max, IQR,
#                             Asimmetria, Curtosi
#   prior_boxplot.pdf       — landscape PDF, one page per priority variable
#
# DEPENDENCIES
#   load_lumen_data.R       — data loading and labelling
#   compute_subgroups.R     — subgroup filters (DIAGNOSI_FILTERS, LGBT_FILTERS)
#   dplyr, moments          — data manipulation and skewness/kurtosis
#   ggplot2, gridExtra, grid — PDF layout and custom boxplots
# ==============================================================================

rm(list = ls())   # clear workspace
graphics.off()    # close any open graphics devices

library(dplyr)    # data manipulation
library(moments)  # skewness() and kurtosis()
library(Hmisc)    # required by load_lumen_data.R

# Load helper scripts that define load_lumen_data() and subgroup filter lists
source("load_lumen_data.R")
source("compute_subgroups.R")

data <- load_lumen_data()  # read LUMEN_DATA.csv, apply labels/factors, filter to consenting respondents


# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Select all prior_ric, prior_aut, and prior_salute columns from the data frame
cols <- grep("^prior_ric|^prior_aut|^prior_salute", names(data), value = TRUE)
prior_data <- data[, cols]  # keep only the priority variables for statistics

# Data quality correction:
# prior_aut_sociale was sometimes entered on a 0–100 scale instead of 0–10.
# Divide values > 10 by 10 to bring them into the correct range.
prior_data$prior_aut_sociale <- ifelse(
  is_valid(prior_data$prior_aut_sociale) & prior_data$prior_aut_sociale > 10,
  prior_data$prior_aut_sociale / 10,  # rescale incorrectly entered values
  prior_data$prior_aut_sociale        # keep correctly entered values as-is
)

# ==============================================================================
# SUBGROUP DEFINITION
# Reuses the filter functions from compute_subgroups.R to construct logical
# vectors identifying each subgroup.
#
# Diagnosis groups (3 marginals):
#   aut            = respondents with autism diagnosis
#   altra_diagnosi = other psychiatric diagnosis, no autism
#   no_diagnosi    = no diagnosis
#
# LGBTQ+ groups (2 marginals):
#   lgbtq          = trans OR non-binary OR non-heterosexual
#   no_lgbtq       = cisgender AND binary gender AND heterosexual
#
# Crossed cells (6 combinations): diagnosis × LGBTQ+
# ==============================================================================

SOTTOGRUPPI <- c(
  # Marginal diagnosis groups (regardless of LGBTQ+ status)
  list(
    aut            = DIAGNOSI_FILTERS$aut(data),            # autism group
    altra_diagnosi = DIAGNOSI_FILTERS$altra_diagnosi(data), # other diagnosis group
    no_diagnosi    = DIAGNOSI_FILTERS$no_diagnosi(data),    # no diagnosis group
    lgbtq          = LGBT_FILTERS$lgbtq(data),              # LGBTQ+ group (all diagnoses)
    no_lgbtq       = LGBT_FILTERS$no_lgbtq(data)            # non-LGBTQ+ group (all diagnoses)
  ),
  # All 6 crossed cells (diagnosis × LGBTQ+)
  list(
    aut_x_lgbtq           = DIAGNOSI_FILTERS$aut(data)            & LGBT_FILTERS$lgbtq(data),       # autism × LGBTQ+
    aut_x_no_lgbtq        = DIAGNOSI_FILTERS$aut(data)            & LGBT_FILTERS$no_lgbtq(data),    # autism × non-LGBTQ+
    altra_diag_x_lgbtq    = DIAGNOSI_FILTERS$altra_diagnosi(data) & LGBT_FILTERS$lgbtq(data),       # other diag × LGBTQ+
    altra_diag_x_no_lgbtq = DIAGNOSI_FILTERS$altra_diagnosi(data) & LGBT_FILTERS$no_lgbtq(data),   # other diag × non-LGBTQ+
    no_diag_x_lgbtq       = DIAGNOSI_FILTERS$no_diagnosi(data)    & LGBT_FILTERS$lgbtq(data),       # no diag × LGBTQ+
    no_diag_x_no_lgbtq    = DIAGNOSI_FILTERS$no_diagnosi(data)    & LGBT_FILTERS$no_lgbtq(data)     # no diag × non-LGBTQ+
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Returns the variable family name based on column name prefix
get_gruppo_var <- function(col) {
  dplyr::case_when(
    startsWith(col, "prior_ric")    ~ "prior_ric",    # general research priorities
    startsWith(col, "prior_aut")    ~ "prior_aut",    # autism-specific priorities
    startsWith(col, "prior_salute") ~ "prior_salute"  # health-area priorities
  )
}

# Computes descriptive statistics for a numeric vector x.
# Values are NOT rounded here; rounding happens only at display time (CSV or table).
# This ensures that the boxplot and the summary table always show the same numbers.
calc_stats <- function(x) {
  x_val <- x[is_valid(x)]           # remove NA values before computing
  q     <- quantile(x_val, probs = c(0.25, 0.75))  # Q1 and Q3
  data.frame(
    N_validi   = length(x_val),                    # count of non-NA responses
    N_mancanti = sum(!is_valid(x)),                 # count of missing responses
    Media      = mean(x_val),                       # arithmetic mean
    DS         = sd(x_val),                         # standard deviation
    Min        = if (length(x_val)) min(x_val) else NA_real_,  # minimum (NA if no data)
    Q1         = q[[1]],                            # first quartile (25th percentile)
    Mediana    = median(x_val),                     # median (50th percentile)
    Q3         = q[[2]],                            # third quartile (75th percentile)
    Max        = if (length(x_val)) max(x_val) else NA_real_,  # maximum (NA if no data)
    IQR        = q[[2]] - q[[1]],                  # interquartile range
    Asimmetria = moments::skewness(x_val),          # Fisher's skewness coefficient
    Curtosi    = moments::kurtosis(x_val) - 3       # excess kurtosis (0 = normal)
  )
}

# ==============================================================================
# COMPUTE STATISTICS — total sample + all subgroups
# ==============================================================================

# Prepend the total sample (all respondents) as the first group
tutti_gruppi <- c(list(totale = rep(TRUE, nrow(data))), SOTTOGRUPPI)

# Iterate over all groups and all priority variables to build the statistics table
all_stats <- do.call(rbind, lapply(names(tutti_gruppi), function(grp_name) {
  mask <- tutti_gruppi[[grp_name]]         # logical vector identifying this group's rows
  sub  <- prior_data[mask, , drop = FALSE]  # subset prior_data to this group

  # For each priority variable, compute statistics and tag with group/variable info
  do.call(rbind, lapply(cols, function(col) {
    s <- calc_stats(sub[[col]])  # compute descriptive statistics for this variable
    cbind(
      data.frame(
        Sottogruppo = grp_name,             # subgroup name
        Variabile   = col,                  # variable name
        Gruppo_var  = get_gruppo_var(col),  # variable family (prior_ric / prior_aut / prior_salute)
        stringsAsFactors = FALSE
      ),
      s  # append the statistics columns
    )
  }))
}))

rownames(all_stats) <- NULL  # drop numeric row names from cbind

# ==============================================================================
# SAVE CSV
# Round numeric columns to 3 decimal places for the CSV only.
# all_stats retains exact values for use in the boxplot and table functions.
# ==============================================================================

all_stats_csv <- all_stats  # copy to avoid modifying the in-memory object
round_cols <- c("Media", "DS", "Min", "Q1", "Mediana", "Q3", "Max",
                "IQR", "Asimmetria", "Curtosi")
all_stats_csv[round_cols] <- lapply(all_stats_csv[round_cols],
                                    function(x) round(x, 3))  # round to 3 dp

write.csv(all_stats_csv, "statistiche_prior.csv", row.names = FALSE)  # write CSV without row indices

# Console summary: confirm how many rows were written
cat("Statistiche calcolate su", length(cols), "variabili x",
    length(tutti_gruppi), "sottogruppi =", nrow(all_stats), "righe totali.\n")
cat("File salvato: statistiche_prior.csv\n\n")

# Print the group sizes so the user can verify subgroup membership counts
cat("N per sottogruppo:\n")
print(sapply(tutti_gruppi, sum))

# ==============================================================================
# PDF: boxplots + summary tables per variable
# ==============================================================================

library(ggplot2)    # bar/geom layers for boxplot construction
library(gridExtra)  # tableGrob and arrangeGrob for PDF layout
library(grid)       # viewport, grid.newpage, grid.text

# Fixed display order of subgroups (top to bottom in boxplot, rows in table)
ORDINE_GRUPPI <- c(
  "totale",
  "aut",            "aut_x_lgbtq",       "aut_x_no_lgbtq",
  "altra_diagnosi", "altra_diag_x_lgbtq","altra_diag_x_no_lgbtq",
  "no_diagnosi",    "no_diag_x_lgbtq",   "no_diag_x_no_lgbtq",
  "lgbtq",          "no_lgbtq"
)

# Human-readable labels for the subgroups (used in table and boxplot y-axis)
LABEL_GRUPPI <- c(
  totale                  = "Totale",
  aut                     = "Aut (tot)",
  aut_x_lgbtq             = "Aut × LGBT+",
  aut_x_no_lgbtq          = "Aut × No LGBT+",
  altra_diagnosi          = "Altra diag. (tot)",
  altra_diag_x_lgbtq      = "Altra diag. × LGBT+",
  altra_diag_x_no_lgbtq   = "Altra diag. × No LGBT+",
  no_diagnosi             = "No diag. (tot)",
  no_diag_x_lgbtq         = "No diag. × LGBT+",
  no_diag_x_no_lgbtq      = "No diag. × No LGBT+",
  lgbtq                   = "LGBT+ (tot)",
  no_lgbtq                = "No LGBT+ (tot)"
)

# Colours for boxplot boxes and lines: 3 diagnosis families + marginal LGBTQ+
# Within each family: dark = marginal total, medium = LGBTQ+ crossed, light = non-LGBTQ+ crossed
COL_GRUPPI <- c(
  totale                  = "#555555",  # grey for total sample
  aut                     = "#1a6faf",  # medium blue for autism total
  aut_x_lgbtq             = "#5aaee0",  # lighter blue for autism × LGBTQ+
  aut_x_no_lgbtq          = "#a8d4f5",  # very light blue for autism × non-LGBTQ+
  altra_diagnosi          = "#b85c00",  # medium orange for other diagnosis total
  altra_diag_x_lgbtq      = "#e0883a",  # lighter orange for other diag × LGBTQ+
  altra_diag_x_no_lgbtq   = "#f5c98a",  # very light orange for other diag × non-LGBTQ+
  no_diagnosi             = "#2e8b57",  # medium green for no diagnosis total
  no_diag_x_lgbtq         = "#5dbf85",  # lighter green for no diag × LGBTQ+
  no_diag_x_no_lgbtq      = "#a8dfc0",  # very light green for no diag × non-LGBTQ+
  lgbtq                   = "#7b2d8b",  # dark purple for LGBTQ+ marginal
  no_lgbtq                = "#c07fd4"   # light purple for non-LGBTQ+ marginal
)

# ------------------------------------------------------------------------------
# make_table_grob(): builds a summary statistics table for one variable
# Rows = subgroups (in ORDINE_GRUPPI order); columns = N, mean, SD, median, Q1, Q3, IQR
# Row backgrounds are tinted with the corresponding group colour (very transparent).
# ------------------------------------------------------------------------------
make_table_grob <- function(var_name, stats_df, ordine, label_map, col_map) {

  # Filter to the rows for this variable and the requested subgroups
  sub <- stats_df[stats_df$Variabile == var_name &
                    stats_df$Sottogruppo %in% ordine, , drop = FALSE]

  # Re-order rows to match ORDINE_GRUPPI
  sub <- sub[match(ordine[ordine %in% sub$Sottogruppo], sub$Sottogruppo), ]

  # Round statistics at display time only (all_stats remains unrounded)
  tab <- data.frame(
    Gruppo   = label_map[sub$Sottogruppo],  # human-readable group name
    N_validi = sub$N_validi,
    Media    = round(sub$Media,   2),
    DS       = round(sub$DS,      2),
    Mediana  = round(sub$Mediana, 2),
    Q1       = round(sub$Q1,      2),
    Q3       = round(sub$Q3,      2),
    IQR      = round(sub$IQR,     2),
    stringsAsFactors = FALSE
  )

  # Row background colours: each row gets a very transparent version of its group colour
  row_colors <- col_map[sub$Sottogruppo]
  fill_core  <- matrix(
    sapply(row_colors, function(col) adjustcolor(col, alpha.f = 0.18)),  # 18% opacity
    nrow = nrow(tab), ncol = ncol(tab)
  )

  tt <- ttheme_default(
    base_size = 7,
    core = list(
      bg_params = list(fill = fill_core, col = NA),  # coloured row backgrounds, no cell border
      fg_params = list(hjust = 1, x = 0.95)          # right-aligned cell text
    ),
    colhead = list(
      bg_params = list(fill = "grey92", col = NA),   # light-grey header background
      fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5)  # centred bold header text
    )
  )
  tableGrob(tab, rows = NULL, theme = tt)  # build and return the table grob
}

# ------------------------------------------------------------------------------
# make_boxplot(): builds a horizontal boxplot for one variable.
# Uses pre-computed statistics from all_stats (exact values, not re-rounded).
# The box is drawn with explicit geometric primitives instead of geom_boxplot()
# to avoid instabilities with coord_flip.
#
# Elements drawn:
#   geom_segment  — lower whisker (from Min/fence to Q1)
#   geom_segment  — upper whisker (from Q3 to Max/fence)
#   geom_segment  — tick marks at whisker ends
#   geom_rect     — box from Q1 to Q3
#   geom_segment  — median line within the box
#
# Whisker fences: lower = max(Min, Q1 - 1.5 × IQR), upper = min(Max, Q3 + 1.5 × IQR)
# ------------------------------------------------------------------------------
make_boxplot <- function(var_name, stats_df, ordine, label_map, col_map) {

  # Filter to the rows for this variable and the requested subgroups
  sub <- stats_df[stats_df$Variabile == var_name &
                    stats_df$Sottogruppo %in% ordine, ]

  if (nrow(sub) == 0) return(NULL)  # no data for this variable: return nothing

  # Determine which subgroups are actually present in the data
  grp_presenti <- ordine[ordine %in% sub$Sottogruppo]
  lev          <- label_map[grp_presenti]
  lev          <- lev[!is.na(lev)]  # drop any unmapped names

  n_grp   <- length(grp_presenti)

  # Build one row per subgroup with all geometric values needed
  plot_df <- do.call(rbind, lapply(seq_along(grp_presenti), function(i) {
    grp <- grp_presenti[i]
    row <- sub[sub$Sottogruppo == grp, ]
    q1  <- row$Q1
    q3  <- row$Q3
    iqr <- row$IQR
    data.frame(
      Gruppo  = factor(label_map[grp], levels = lev),
      colore  = col_map[grp],
      y_pos   = n_grp - i + 1,                  # y position: first group at top (highest y)
      q1      = q1,
      mediana = row$Mediana,
      q3      = q3,
      w_inf   = max(row$Min, q1 - 1.5 * iqr),   # lower whisker end (Tukey fence or Min)
      w_sup   = min(row$Max, q3 + 1.5 * iqr),   # upper whisker end (Tukey fence or Max)
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)  # nothing to plot

  half_w <- 0.3   # half-height of the box rectangle

  ggplot(plot_df) +
    # Lower whisker: horizontal segment from w_inf to Q1
    geom_segment(aes(x = w_inf, xend = q1,
                     y = y_pos, yend = y_pos,
                     colour = colore), linewidth = 0.6) +
    # Upper whisker: horizontal segment from Q3 to w_sup
    geom_segment(aes(x = q3, xend = w_sup,
                     y = y_pos, yend = y_pos,
                     colour = colore), linewidth = 0.6) +
    # Tick at lower whisker end (short vertical notch)
    geom_segment(aes(x = w_inf, xend = w_inf,
                     y = y_pos - half_w * 0.5, yend = y_pos + half_w * 0.5,
                     colour = colore), linewidth = 0.5) +
    # Tick at upper whisker end (short vertical notch)
    geom_segment(aes(x = w_sup, xend = w_sup,
                     y = y_pos - half_w * 0.5, yend = y_pos + half_w * 0.5,
                     colour = colore), linewidth = 0.5) +
    # Box from Q1 to Q3 (filled rectangle with slight transparency)
    geom_rect(aes(xmin = q1, xmax = q3,
                  ymin = y_pos - half_w, ymax = y_pos + half_w,
                  fill = colore),
              colour = "grey40", linewidth = 0.3, alpha = 0.85) +
    # Median line (dark, thicker segment within the box)
    geom_segment(aes(x = mediana, xend = mediana,
                     y = y_pos - half_w, yend = y_pos + half_w,
                     colour = colore),
                 colour = "grey15", linewidth = 1.1) +
    scale_colour_identity() +  # use raw colour hex strings directly
    scale_fill_identity() +    # same for fill
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +  # x-axis: 0–10 scale
    scale_y_continuous(
      breaks = rev(seq_along(grp_presenti)),  # y ticks at each group's position
      labels = lev,                           # use human-readable group labels
      limits = c(0.5, length(grp_presenti) + 0.5)  # clip to visible range
    ) +
    labs(x = NULL, y = NULL) +  # no axis titles
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major.y = element_blank(),  # no horizontal gridlines (clutters the plot)
      panel.grid.minor   = element_blank(),
      axis.text.y  = element_text(size = 7),
      axis.text.x  = element_text(size = 7),
      plot.margin  = margin(4, 6, 4, 4)
    )
}

# ------------------------------------------------------------------------------
# Section header pages: one coloured separator page per variable family
# ------------------------------------------------------------------------------
SEZIONI <- list(
  prior_ric    = "Priorità di ricerca (prior_ric)",    # general research priorities section
  prior_aut    = "Priorità autismo (prior_aut)",        # autism-specific priorities section
  prior_salute = "Priorità salute (prior_salute)"       # health-area priorities section
)

# ------------------------------------------------------------------------------
# Generate PDF: one page per priority variable, section separators inserted
# when the variable family changes.
# ------------------------------------------------------------------------------
pdf("prior_boxplot.pdf", width = 11, height = 8.5, onefile = TRUE)  # landscape US-letter-ish PDF

sezione_corrente <- ""  # tracks the currently active section to detect changes

for (var_name in cols) {

  tryCatch({

    sezione <- get_gruppo_var(var_name)  # determine which section this variable belongs to

    # Insert a section separator page when moving to a new variable family
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#2c3e50", col = NA))  # dark-blue full-page background
      grid.text(
        SEZIONI[[sezione]],
        x = 0.5, y = 0.5,
        gp = gpar(col = "white", fontsize = 22, fontface = "bold")  # centred white title
      )
    }

    # Build the boxplot from the pre-computed statistics in all_stats
    bp <- make_boxplot(var_name, all_stats,
                       ORDINE_GRUPPI, LABEL_GRUPPI, COL_GRUPPI)

    # Build the summary table
    tb <- make_table_grob(var_name, all_stats,
                          ORDINE_GRUPPI, LABEL_GRUPPI, COL_GRUPPI)

    # Layout: boxplot on the left (56% width), table on the right (40% width)
    grid.newpage()

    # Variable name as page title (top-left, dark blue)
    grid.text(
      var_name,
      x = 0.02, y = 0.97, hjust = 0, vjust = 1,
      gp = gpar(fontsize = 11, fontface = "bold", col = "#2c3e50")
    )

    # Boxplot viewport: left side of page
    if (!is.null(bp)) {
      vp_left <- viewport(x = 0.01, y = 0.03, width = 0.56, height = 0.91,
                          just = c("left", "bottom"))
      print(bp, vp = vp_left)  # render the ggplot object into this viewport
    }

    # Table viewport: right side of page
    vp_right <- viewport(x = 0.59, y = 0.03, width = 0.40, height = 0.91,
                         just = c("left", "bottom"))
    pushViewport(vp_right)
    grid.draw(tb)    # render the table grob
    popViewport()

  }, error = function(e) {
    # On error: do not crash; instead insert an error page and print a warning
    message("Errore su variabile '", var_name, "': ", conditionMessage(e))
    grid.newpage()
    grid.text(
      paste0("Errore su: ", var_name, "\n", conditionMessage(e)),
      x = 0.5, y = 0.5,
      gp = gpar(fontsize = 10, col = "red")  # red error message on blank page
    )
  })
}

dev.off()                                    # finalise and close the PDF
cat("PDF salvato: prior_boxplot.pdf\n")
