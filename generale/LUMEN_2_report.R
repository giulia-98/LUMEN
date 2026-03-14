# ==============================================================================
# LUMEN_2_report.R
#
# PURPOSE
# -------
# Reads the groups_counts.csv produced by LUMEN_1_groups_counts.R and
# generates a multi-page PDF report (one page per survey variable).
# Each page contains:
#   - A header bar with the variable name
#   - A summary table (counts + percentages for every group × value cell)
#   - A stacked bar chart (absolute counts, coloured by group combination)
#
# When a variable has more than 10 distinct values the table and chart are
# placed on separate pages to avoid overcrowding.
#
# INPUT
#   groups_counts.csv   — produced by LUMEN_1_groups_counts.R
#
# OUTPUT
#   report.pdf          — A4 landscape PDF, one or two pages per variable
#
# DEPENDENCIES
#   ggplot2, dplyr, tidyr, scales, gridExtra, grid
# ==============================================================================

rm(list=ls())      # clear all objects from the workspace
graphics.off()     # close any open graphics devices

library(ggplot2)    # data visualisation (bar charts)
library(dplyr)      # data manipulation (filter, mutate, group_by, …)
library(tidyr)      # reshaping (pivot_wider)
library(scales)     # axis label formatting (comma)
library(gridExtra)  # arranging multiple grobs on a page (tableGrob, arrangeGrob)
library(grid)       # low-level grid graphics (viewport, textGrob, rectGrob, …)

# ── CONFIG ────────────────────────────────────────────────────────────────────

INPUT_CSV  <- "groups_counts.csv"  # input file produced by LUMEN_1
OUTPUT_PDF <- "report.pdf"         # output PDF filename

# Fixed display order for diagnosis and LGBTQ+ groups
DIAGNOSI_ORDER <- c("aut", "altra_diagnosi", "no_diagnosi")  # 3 diagnosis groups
LGBT_ORDER     <- c("lgbtq", "no_lgbtq")                     # 2 LGBTQ+ groups

# Human-readable labels for the 3 diagnosis groups
DIAGNOSI_LABELS <- c(
  aut            = "Autismo",
  altra_diagnosi = "Altra diagnosi",
  no_diagnosi    = "Nessuna diagnosi"
)

# Human-readable labels for the 2 LGBTQ+ groups
LGBT_LABELS <- c(
  lgbtq    = "LGBTQ+",
  no_lgbtq = "Non LGBTQ+"
)

# Fixed order of the 6 crossed combinations (diagnosis × LGBTQ+) for legend and stacking
COMBO_ORDER <- c(
  "aut_lgbtq",            "aut_no_lgbtq",
  "altra_diagnosi_lgbtq", "altra_diagnosi_no_lgbtq",
  "no_diagnosi_lgbtq",    "no_diagnosi_no_lgbtq"
)

# Human-readable labels for the 6 combinations (shown in legend)
COMBO_LABELS <- c(
  aut_lgbtq                = "Autismo · LGBTQ+",
  aut_no_lgbtq             = "Autismo · Non LGBTQ+",
  altra_diagnosi_lgbtq     = "Altra diagnosi · LGBTQ+",
  altra_diagnosi_no_lgbtq  = "Altra diagnosi · Non LGBTQ+",
  no_diagnosi_lgbtq        = "Nessuna diagnosi · LGBTQ+",
  no_diagnosi_no_lgbtq     = "Nessuna diagnosi · Non LGBTQ+"
)

# Fill colours: saturated tone = LGBTQ+, pastel tone = non-LGBTQ+
COMBO_COLORS <- c(
  aut_lgbtq                = "#2255AA",  # dark blue
  aut_no_lgbtq             = "#7BAAE0",  # light blue
  altra_diagnosi_lgbtq     = "#C05010",  # dark orange
  altra_diagnosi_no_lgbtq  = "#F0A87A",  # light orange
  no_diagnosi_lgbtq        = "#2E7D32",  # dark green
  no_diagnosi_no_lgbtq     = "#88CC8A"   # light green
)

# Names of the 5 marginal totals (used to separate them from crossed cells)
TOT_DIAGNOSI_ORDER <- c("aut_tot", "altra_diagnosi_tot", "no_diagnosi_tot")  # diagnosis marginals
TOT_LGBT_ORDER     <- c("lgbtq_tot", "no_lgbtq_tot")                         # LGBTQ+ marginals

# Labels for diagnosis marginals (shown in table headers)
TOT_DIAGNOSI_LABELS <- c(
  aut_tot            = "Autismo (tot)",
  altra_diagnosi_tot = "Altra diag. (tot)",
  no_diagnosi_tot    = "Nessuna diag. (tot)"
)

# Labels for LGBTQ+ marginals (shown in table headers)
TOT_LGBT_LABELS <- c(
  lgbtq_tot    = "LGBTQ+ (tot)",
  no_lgbtq_tot = "Non LGBTQ+ (tot)"
)

# Header fill colours for diagnosis marginal columns in tables
TOT_DIAGNOSI_COLORS <- c(
  aut_tot            = "#162E6E",  # very dark blue
  altra_diagnosi_tot = "#7A2E00",  # very dark orange
  no_diagnosi_tot    = "#1A4D1C"   # very dark green
)

# Header fill colours for LGBTQ+ marginal columns in tables
TOT_LGBT_COLORS <- c(
  lgbtq_tot    = "#7B2D8B",  # dark purple
  no_lgbtq_tot = "#B07CC6"   # light purple
)

# Variables to exclude from the report entirely
EXCLUDE_VARS <- c("consenso", "questionario_lumen_complete")

# ── LOAD DATA ─────────────────────────────────────────────────────────────────

df_raw <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)  # read the counts CSV

# Fill missing or empty labels with the raw numeric value (fallback display)
df_raw$label <- ifelse(is.na(df_raw$label) | df_raw$label == "",
                       as.character(df_raw$valore), df_raw$label)

df_raw <- df_raw[!df_raw$variabile %in% EXCLUDE_VARS, ]  # drop excluded variables

# Separate rows for crossed cells from rows for marginal totals
# Crossed cells: diagnosi in DIAGNOSI_ORDER AND lgbt in LGBT_ORDER
df_combo <- df_raw[df_raw$diagnosi %in% DIAGNOSI_ORDER &
                   df_raw$lgbt     %in% LGBT_ORDER, ]

df_tot_diag <- df_raw[df_raw$diagnosi %in% TOT_DIAGNOSI_ORDER, ]  # diagnosis marginal rows
df_tot_lgbt <- df_raw[df_raw$lgbt     %in% TOT_LGBT_ORDER, ]      # LGBTQ+ marginal rows

# Create a single "combo" key for crossed cells (e.g. "aut_lgbtq")
df_combo$combo <- paste(df_combo$diagnosi, df_combo$lgbt, sep = "_")
df_combo$combo <- factor(df_combo$combo, levels = COMBO_ORDER)  # enforce fixed display order

# Create "combo" key for marginal rows (the group name itself serves as the key)
df_tot_diag$combo <- df_tot_diag$diagnosi   # e.g. "aut_tot"
df_tot_lgbt$combo <- df_tot_lgbt$lgbt       # e.g. "lgbtq_tot"

# Canonical order for survey variables (matches the REDCap questionnaire order)
DB_VAR_ORDER <- c(
  "eta", "sesso_nascita", "trans_cis", "identita_genere", "orientamento_sex",
  "regione", "centro_abitato", "titolo_studio", "titolo_studio_genitore1",
  "titolo_studio_genitore2", "figli", "quanti_figli", "reddito",
  "servizi_salute_mentale", "diagnosi_si_no",
  "aut", "adhd", "pers", "ansia", "umore", "comp_al", "appr",
  "psicosi", "ocd", "ptsd", "dipendenza", "altro", "livello_aut",
  "invio_aut", "invio_adhd", "invio_person", "invio_ansia", "invio_umore",
  "invio_comp_al", "invio_appr", "invio_psicosi", "invio_ocd", "invio_ptsd",
  "invio_dipendenza", "invio_altro",
  "prima_visita_aut", "prima_visita_adhd", "prima_visita_pers",
  "prima_visita_ansia", "prima_visita_umore", "prima_visita_comp_al",
  "prima_visita_appr", "prima_visita_psicosi", "prima_visita_ocd",
  "prima_visita_ptsd", "prima_visita_dipendenza", "prima_visita_altro",
  "att_diagnosi_aut", "att_diagnosi_adhd", "att_diagnosi_pers",
  "att_diagnosi_ansia", "att_diagnosi_umore", "att_diagnosi_comp_al",
  "att_diagnosi_appr", "att_diagnosi_psicosi", "att_diagnosi_ocd",
  "att_diagnosi_ptsd", "att_diagnosi_dipendenza", "att_diagnosi_altro",
  "sbagliate_aut", "sbagliate_adhd", "sbagliate_pers", "sbagliate_ansia",
  "sbagliate_umore", "sbagliate_comp_al", "sbagliate_appr", "sbagliate_psicosi",
  "sbagliate_ocd", "sbagliate_ptsd", "sbagliate_dipendenza", "sbagliate_altro",
  "costo_aut", "costo_adhd", "costo_pers", "costo_ansia", "costo_umore",
  "costo_comp_al", "costo_appr", "costo_psicosi", "costo_ocd", "costo_ptsd",
  "costo_dipendenza", "costo_altro",
  "dove_aut", "dove_adhd", "dove_pers", "dove_ansia", "dove_umore",
  "dove_comp_al", "dove_appr", "dove_psicosi", "dove_ocd", "dove_ptsd",
  "dove_dipendenza", "dove_altro",
  "n_prof_aut", "n_prof_adhd", "n_prof_pers", "n_prof_ansia", "n_prof_umore",
  "n_prof_comp_al", "n_prof_appr", "n_prof_psicosi", "n_prof_ocd", "n_prof_ptsd",
  "n_prof_dipendenza", "n_prof_altro",
  "presa_carico_aut",
  "percorsi_aut___1", "percorsi_aut___2", "percorsi_aut___3", "percorsi_aut___4",
  "percorsi_aut___5", "percorsi_aut___6", "percorsi_aut___7", "percorsi_aut___8",
  "durata_percorsi", "sei_in_carico", "interruzione_percorso"
)

present_vars <- unique(df_raw$variabile)  # variables actually present in the CSV
# Use DB_VAR_ORDER for known variables; append any unknown variables at the end
all_vars <- c(
  intersect(DB_VAR_ORDER, present_vars),  # known variables in canonical order
  setdiff(present_vars, DB_VAR_ORDER)     # extra variables not in the canonical list
)

# ── HELPER: wrap long labels ───────────────────────────────────────────────────

# Wraps a character string to the given width by inserting newlines
wrap_str <- function(x, width = 28) {
  sapply(x, function(s) paste(strwrap(s, width), collapse = "\n"))
}

# ── HELPER: summary table grob ────────────────────────────────────────────────
# Builds a tableGrob for one variable with columns:
#   Val. | Etichetta | aut_tot | aut_lgbtq | aut_no_lgbtq |
#                      altra_tot | altra_lgbtq | altra_no_lgbtq |
#                      no_diag_tot | no_diag_lgbtq | no_diag_no_lgbtq |
#                      lgbtq_tot | no_lgbtq_tot
# Each cell shows: count\n(pct%)

make_table_grob <- function(var_name) {

  # ---- Crossed cells (6 combinations) ----
  cdf <- df_combo[df_combo$variabile == var_name, ]
  wide_combo <- cdf %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%  # format as "N\n(pct%)"
    select(valore, label, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")  # pivot to wide; fill missing with "—"

  # Ensure all 6 combination columns exist even if no data for that combo
  for (co in COMBO_ORDER) {
    if (!co %in% names(wide_combo)) wide_combo[[co]] <- "—"
  }

  # ---- Diagnosis marginals ----
  tdd <- df_tot_diag[df_tot_diag$variabile == var_name, ]
  wide_td <- tdd %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%
    select(valore, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")
  for (co in TOT_DIAGNOSI_ORDER) {
    if (!co %in% names(wide_td)) wide_td[[co]] <- "—"  # ensure all marginal columns exist
  }

  # ---- LGBTQ+ marginals ----
  tld <- df_tot_lgbt[df_tot_lgbt$variabile == var_name, ]
  wide_tl <- tld %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%
    select(valore, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")
  for (co in TOT_LGBT_ORDER) {
    if (!co %in% names(wide_tl)) wide_tl[[co]] <- "—"  # ensure all marginal columns exist
  }

  # ---- Merge all three by valore ----
  wide <- wide_combo %>%
    left_join(wide_td[, c("valore", TOT_DIAGNOSI_ORDER)], by = "valore") %>%  # add diagnosis marginal columns
    left_join(wide_tl[, c("valore", TOT_LGBT_ORDER)],     by = "valore")      # add LGBTQ+ marginal columns

  # Remove rows where no subject belongs to any of the 6 crossed cells
  combo_counts <- cdf %>%
    group_by(valore) %>%
    summarise(tot_count = sum(count), .groups = "drop")
  wide <- wide[wide$valore %in% combo_counts$valore[combo_counts$tot_count > 0], ]

  # Column order: Val | Label | [per each diagnosi: tot, lgbtq, no_lgbtq] | lgbtq_tot | no_lgbtq_tot
  INTERLEAVED_ORDER <- c(
    "aut_tot",            "aut_lgbtq",            "aut_no_lgbtq",
    "altra_diagnosi_tot", "altra_diagnosi_lgbtq", "altra_diagnosi_no_lgbtq",
    "no_diagnosi_tot",    "no_diagnosi_lgbtq",    "no_diagnosi_no_lgbtq",
    "lgbtq_tot",          "no_lgbtq_tot"
  )
  col_order <- c("valore", "label", INTERLEAVED_ORDER)
  for (co in col_order) {
    if (!co %in% names(wide)) wide[[co]] <- "—"  # add any missing column as "—"
  }
  wide <- wide[, col_order]  # reorder columns

  # Sort rows by numeric value where possible
  suppressWarnings({
    num_val <- as.integer(wide$valore)
    if (!any(is.na(num_val))) wide <- wide[order(num_val), ]  # sort only if all values are numeric
  })

  # Truncate labels longer than 35 characters for readability
  wide$label <- ifelse(
    nchar(wide$label) > 35,
    paste0(substr(wide$label, 1, 32), "..."),  # truncate and append ellipsis
    wide$label
  )

  # Combined label and colour maps for all 11 data columns
  ALL_LABELS <- c(TOT_DIAGNOSI_LABELS, TOT_LGBT_LABELS, COMBO_LABELS)
  ALL_COLORS <- c(TOT_DIAGNOSI_COLORS, TOT_LGBT_COLORS, COMBO_COLORS)

  # Set column header names (Val., Etichetta, then one header per group column)
  col_names <- c("Val.", "Etichetta", unname(ALL_LABELS[INTERLEAVED_ORDER]))
  names(wide) <- col_names

  # Header fill colours: dark for Val./Etichetta, group colour for data columns
  header_fills <- c("#2E4057", "#2E4057", unname(ALL_COLORS[INTERLEAVED_ORDER]))

  # Table theme: alternating white / light-blue rows; bold white header text
  tt <- ttheme_minimal(
    base_size = 5.5,
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5, cex = 0.70),  # centred cell text
      bg_params = list(
        fill = c("white", "#F0F4FA"),  # alternating row colours
        col  = "#CCCCCC", lwd = 0.4   # light grey cell borders
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontface = "bold",
                       hjust = 0.5, x = 0.5, cex = 0.65),  # white bold header text
      bg_params = list(fill = header_fills, col = "#CCCCCC", lwd = 0.4)  # group-coloured header backgrounds
    )
  )

  tableGrob(wide, rows = NULL, theme = tt)  # build and return the table grob
}

# ── HELPER: stacked bar chart ──────────────────────────────────────────────────
# One stacked bar per distinct value of the categorical variable (x-axis).
# Stack segments = the 6 crossed combinations (diagnosis × LGBTQ+).
# Y-axis = absolute count.  Segment label = within-bar percentage (hidden if < 3%).

make_bar_plot <- function(var_name) {

  plot_df <- df_combo[df_combo$variabile == var_name, ] %>%
    mutate(
      label_wrapped = wrap_str(label, 30),           # wrap long labels for x-axis
      combo         = factor(combo, levels = COMBO_ORDER)  # enforce stacking order
    )

  if (nrow(plot_df) == 0) return(ggplot() + theme_void())  # return empty plot if no data

  # Drop values where no group has any subjects
  plot_df <- plot_df %>%
    group_by(valore) %>%
    filter(sum(count) > 0) %>%
    ungroup()

  if (nrow(plot_df) == 0) return(ggplot() + theme_void())  # return empty plot if all zeroes

  # Sort x-axis categories: numeric order if all values are integer, otherwise as-is
  suppressWarnings(num_val <- as.integer(unique(plot_df$valore)))
  ref <- plot_df %>% distinct(valore, label_wrapped)
  if (!any(is.na(num_val))) {
    ref <- ref %>% mutate(n = as.integer(valore)) %>% arrange(n)  # numeric sort
  }
  x_levels <- ref$label_wrapped
  plot_df$label_wrapped <- factor(plot_df$label_wrapped, levels = x_levels)  # apply sort order

  # pct_fetta: share of each group within each x-axis category
  # = count / sum of all groups for that value × 100
  plot_df <- plot_df %>%
    group_by(label_wrapped) %>%
    mutate(pct_fetta = count / sum(count) * 100) %>%
    ungroup()

  ggplot(plot_df, aes(x = label_wrapped, y = count, fill = combo)) +
    geom_bar(
      stat      = "identity",  # use the count column directly
      position  = "stack",     # stack all 6 group segments
      width     = 0.65,
      colour    = "white",     # white separator between stacked segments
      linewidth = 0.25
    ) +
    geom_text(
      # Show percentage label only when the segment is wide enough (>= 3%)
      aes(label = ifelse(pct_fetta >= 3, paste0(sprintf("%.0f", pct_fetta), "%"), "")),
      position = position_stack(vjust = 0.5),  # centre label within segment
      size     = 2.2,
      colour   = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = COMBO_COLORS[COMBO_ORDER],  # apply group colours
      labels = COMBO_LABELS[COMBO_ORDER],  # apply group labels in legend
      name   = NULL,
      drop   = FALSE  # show all 6 combinations in legend even if some are absent
    ) +
    scale_y_continuous(
      labels = scales::comma,              # format y-axis with thousand separators
      expand = expansion(mult = c(0, 0.05))  # no gap below bars; small gap above
    ) +
    labs(x = NULL, y = "N rispondenti") +  # no x-axis title; Italian y-axis label
    theme_minimal(base_size = 8) +
    theme(
      legend.position    = "bottom",
      legend.key.size    = unit(0.30, "cm"),
      legend.text        = element_text(size = 6),
      legend.spacing.x   = unit(0.2, "cm"),
      axis.text.x        = element_text(size = 6.5, lineheight = 0.80,
                                        angle = 20, hjust = 1),  # angled x labels
      axis.text.y        = element_text(size = 6.5),
      panel.grid.major.x = element_blank(),                      # no vertical gridlines
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#DDDDDD", linewidth = 0.35),
      plot.margin        = margin(4, 8, 4, 4)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # legend in 2 rows for compactness
}

# ── GENERATE PDF (A4 landscape) ───────────────────────────────────────────────

cat(sprintf("Generating PDF for %d variables...\n", length(all_vars)))

pdf(OUTPUT_PDF, width = 11.69, height = 8.27, paper = "a4r")  # A4 landscape dimensions in inches

for (i in seq_along(all_vars)) {

  var_name <- all_vars[i]

  # Skip variables that have no rows in any of the three data frames
  has_data <- (var_name %in% df_combo$variabile) ||
              (var_name %in% df_tot_diag$variabile) ||
              (var_name %in% df_tot_lgbt$variabile)
  if (!has_data) next

  cat(sprintf("  [%d/%d] %s\n", i, length(all_vars), var_name))  # progress message

  n_values <- length(unique(df_raw$valore[df_raw$variabile == var_name]))  # number of distinct response values

  # Inner helper: draws a page with a thin title bar and a content area below
  draw_page <- function(title_panel, content_grob, content_height = 0.92) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(
      nrow    = 2,
      ncol    = 1,
      heights = unit(c(0.06, 0.94), "npc")  # title bar: 6% height; content: 94%
    )))
    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(title_panel)   # draw the coloured title bar
    popViewport()
    pushViewport(viewport(layout.pos.row = 2,
                          x = 0.5, y = 0.5, width = 0.98, height = content_height,
                          just = c("centre", "centre")))
    grid.draw(content_grob)  # draw the table or chart
    popViewport()
    popViewport()
  }

  # Build the title bar: white bold text on dark-blue background
  title_grob  <- textGrob(
    label = gsub("_", " ", toupper(var_name)),  # replace underscores with spaces and capitalise
    gp    = gpar(fontsize = 11, fontface = "bold", col = "white"),
    x = 0.02, hjust = 0  # left-aligned within the bar
  )
  title_bg    <- rectGrob(gp = gpar(fill = "#2E4057", col = NA))  # dark-blue rectangle
  title_panel <- grobTree(title_bg, title_grob)                   # combine background + text

  tbl_grob <- make_table_grob(var_name)              # build summary table
  bar_grob <- ggplotGrob(make_bar_plot(var_name))    # build stacked bar chart

  # Layout decision: >10 values → table and chart on separate pages;
  # ≤10 values → single page with table above and chart below
  if (n_values > 10) {
    draw_page(title_panel, tbl_grob, content_height = 0.92)  # page 1: table only
    draw_page(title_panel, bar_grob, content_height = 0.96)  # page 2: chart only
  } else {
    # Dynamic height split: table fraction grows with number of values (capped at 55%)
    table_frac <- min(0.55, max(0.28, 0.16 + n_values * 0.035))
    chart_frac <- 1 - 0.06 - table_frac  # remainder of page after title + table

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(
      nrow    = 3,
      ncol    = 1,
      heights = unit(c(0.06, table_frac, chart_frac), "npc")  # title | table | chart
    )))

    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(title_panel)  # draw title bar in the top slot
    popViewport()

    pushViewport(viewport(layout.pos.row = 2,
                          x = 0.5, y = 0.5, width = 0.98, height = 0.92,
                          just = c("centre", "centre")))
    grid.draw(tbl_grob)     # draw table in the middle slot
    popViewport()

    pushViewport(viewport(layout.pos.row = 3,
                          x = 0.5, y = 0.5, width = 0.96, height = 0.96,
                          just = c("centre", "centre")))
    grid.draw(bar_grob)     # draw chart in the bottom slot
    popViewport()

    popViewport()
  }
}

dev.off()  # close and finalise the PDF file

# ── COPY PDF TO SCRIPT DIRECTORY ─────────────────────────────────────────────
# Ensures the PDF lands next to this script file when run via source() or Rscript.

script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(normalizePath(rstudioapi::getSourceEditorContext()$path))  # RStudio: use editor path
  } else {
    frames <- sys.frames()
    ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))  # source() call frames
    if (length(ofiles) > 0) {
      dirname(normalizePath(ofiles[[length(ofiles)]]))  # use innermost sourced file path
    } else {
      args     <- commandArgs(trailingOnly = FALSE)
      file_arg <- args[grepl("^--file=", args)]  # find --file= argument when called via Rscript
      if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
    }
  }
}, error = function(e) getwd())  # fall back to working directory on any error

final_path <- file.path(script_dir, OUTPUT_PDF)  # target path in the script's directory
# Copy the PDF only if it was not already written there
if (normalizePath(OUTPUT_PDF, mustWork = FALSE) != normalizePath(final_path, mustWork = FALSE)) {
  file.copy(OUTPUT_PDF, final_path, overwrite = TRUE)
}

cat(sprintf("Done -> %s\n", final_path))  # confirm the final PDF location
