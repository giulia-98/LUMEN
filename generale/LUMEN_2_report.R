#Clear existing data and graphics
rm(list=ls())
graphics.off()

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(grid)

# ── CONFIG ────────────────────────────────────────────────────────────────────

INPUT_CSV  <- "groups_counts.csv"
OUTPUT_PDF <- "report.pdf"

# Ordine fisso diagnosi e lgbt
DIAGNOSI_ORDER <- c("aut", "altra_diagnosi", "no_diagnosi")
LGBT_ORDER     <- c("lgbtq", "no_lgbtq")

DIAGNOSI_LABELS <- c(
  aut            = "Autismo",
  altra_diagnosi = "Altra diagnosi",
  no_diagnosi    = "Nessuna diagnosi"
)

LGBT_LABELS <- c(
  lgbtq    = "LGBTQ+",
  no_lgbtq = "Non LGBTQ+"
)

# 6 combinazioni diagnosi x lgbt
COMBO_ORDER <- c(
  "aut_lgbtq",            "aut_no_lgbtq",
  "altra_diagnosi_lgbtq", "altra_diagnosi_no_lgbtq",
  "no_diagnosi_lgbtq",    "no_diagnosi_no_lgbtq"
)

COMBO_LABELS <- c(
  aut_lgbtq                = "Autismo · LGBTQ+",
  aut_no_lgbtq             = "Autismo · Non LGBTQ+",
  altra_diagnosi_lgbtq     = "Altra diagnosi · LGBTQ+",
  altra_diagnosi_no_lgbtq  = "Altra diagnosi · Non LGBTQ+",
  no_diagnosi_lgbtq        = "Nessuna diagnosi · LGBTQ+",
  no_diagnosi_no_lgbtq     = "Nessuna diagnosi · Non LGBTQ+"
)

# Colori combinazioni: tono pieno=lgbtq, tono pastello=no_lgbtq
COMBO_COLORS <- c(
  aut_lgbtq                = "#2255AA",
  aut_no_lgbtq             = "#7BAAE0",
  altra_diagnosi_lgbtq     = "#C05010",
  altra_diagnosi_no_lgbtq  = "#F0A87A",
  no_diagnosi_lgbtq        = "#2E7D32",
  no_diagnosi_no_lgbtq     = "#88CC8A"
)

# 5 gruppi marginali totali
TOT_DIAGNOSI_ORDER <- c("aut_tot", "altra_diagnosi_tot", "no_diagnosi_tot")
TOT_LGBT_ORDER     <- c("lgbtq_tot", "no_lgbtq_tot")

TOT_DIAGNOSI_LABELS <- c(
  aut_tot            = "Autismo (tot)",
  altra_diagnosi_tot = "Altra diag. (tot)",
  no_diagnosi_tot    = "Nessuna diag. (tot)"
)

TOT_LGBT_LABELS <- c(
  lgbtq_tot    = "LGBTQ+ (tot)",
  no_lgbtq_tot = "Non LGBTQ+ (tot)"
)

TOT_DIAGNOSI_COLORS <- c(
  aut_tot            = "#162E6E",   # blu molto scuro
  altra_diagnosi_tot = "#7A2E00",   # arancio molto scuro
  no_diagnosi_tot    = "#1A4D1C"    # verde molto scuro
)

TOT_LGBT_COLORS <- c(
  lgbtq_tot    = "#7B2D8B",   # viola scuro
  no_lgbtq_tot = "#B07CC6"    # viola chiaro
)

EXCLUDE_VARS <- c("consenso", "questionario_lumen_complete")

# ── LOAD DATA ─────────────────────────────────────────────────────────────────

df_raw <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)
df_raw$label <- ifelse(is.na(df_raw$label) | df_raw$label == "",
                       as.character(df_raw$valore), df_raw$label)
df_raw <- df_raw[!df_raw$variabile %in% EXCLUDE_VARS, ]

# Separa combinazioni standard dai gruppi totali
# Le righe marginali diagnosi hanno diagnosi in TOT_DIAGNOSI_ORDER e lgbt == "tot"
# Le righe marginali lgbt hanno diagnosi == "tot" e lgbt in TOT_LGBT_ORDER
df_combo <- df_raw[df_raw$diagnosi %in% DIAGNOSI_ORDER &
                   df_raw$lgbt     %in% LGBT_ORDER, ]

df_tot_diag <- df_raw[df_raw$diagnosi %in% TOT_DIAGNOSI_ORDER, ]
df_tot_lgbt <- df_raw[df_raw$lgbt     %in% TOT_LGBT_ORDER, ]

# Colonna combo per combinazioni standard
df_combo$combo <- paste(df_combo$diagnosi, df_combo$lgbt, sep = "_")
df_combo$combo <- factor(df_combo$combo, levels = COMBO_ORDER)

# Colonna combo per totali diagnosi e lgbt
df_tot_diag$combo <- df_tot_diag$diagnosi   # es. "aut_tot"
df_tot_lgbt$combo <- df_tot_lgbt$lgbt       # es. "lgbtq_tot"

# ordine visualizzazione variabili
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

present_vars <- unique(df_raw$variabile)
all_vars <- c(
  intersect(DB_VAR_ORDER, present_vars),
  setdiff(present_vars, DB_VAR_ORDER)
)

# ── HELPER: wrap long labels ───────────────────────────────────────────────────

wrap_str <- function(x, width = 28) {
  sapply(x, function(s) paste(strwrap(s, width), collapse = "\n"))
}

# ── HELPER: tabella riepilogativa ─────────────────────────────────────────────
# Colonne: Val | Etichetta | aut_tot | aut_lgbtq | aut_no_lgbtq | altra_tot | altra_lgbtq | altra_no_lgbtq | no_diag_tot | no_diag_lgbtq | no_diag_no_lgbtq | lgbtq_tot | no_lgbtq_tot

make_table_grob <- function(var_name) {

  # ---- combinazioni standard ----
  cdf <- df_combo[df_combo$variabile == var_name, ]
  wide_combo <- cdf %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%
    select(valore, label, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")

  for (co in COMBO_ORDER) {
    if (!co %in% names(wide_combo)) wide_combo[[co]] <- "—"
  }

  # ---- totali diagnosi ----
  tdd <- df_tot_diag[df_tot_diag$variabile == var_name, ]
  wide_td <- tdd %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%
    select(valore, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")
  for (co in TOT_DIAGNOSI_ORDER) {
    if (!co %in% names(wide_td)) wide_td[[co]] <- "—"
  }

  # ---- totali lgbt ----
  tld <- df_tot_lgbt[df_tot_lgbt$variabile == var_name, ]
  wide_tl <- tld %>%
    mutate(cell = paste0(count, "\n(", sprintf("%.1f", pct), "%)")) %>%
    select(valore, combo, cell) %>%
    pivot_wider(names_from = combo, values_from = cell, values_fill = "—")
  for (co in TOT_LGBT_ORDER) {
    if (!co %in% names(wide_tl)) wide_tl[[co]] <- "—"
  }

  # ---- unisci per valore ----
  wide <- wide_combo %>%
    left_join(wide_td[, c("valore", TOT_DIAGNOSI_ORDER)], by = "valore") %>%
    left_join(wide_tl[, c("valore", TOT_LGBT_ORDER)],     by = "valore")

  # rimuovi valori senza soggetti in nessun gruppo (count == 0 per tutti i 6 combo)
  combo_counts <- cdf %>%
    group_by(valore) %>%
    summarise(tot_count = sum(count), .groups = "drop")
  wide <- wide[wide$valore %in% combo_counts$valore[combo_counts$tot_count > 0], ]

  # ordina le colonne: Val | Label | per ogni diagnosi: tot, lgbtq, no_lgbtq | lgbtq_tot | no_lgbtq_tot
  INTERLEAVED_ORDER <- c(
    "aut_tot",            "aut_lgbtq",            "aut_no_lgbtq",
    "altra_diagnosi_tot", "altra_diagnosi_lgbtq", "altra_diagnosi_no_lgbtq",
    "no_diagnosi_tot",    "no_diagnosi_lgbtq",    "no_diagnosi_no_lgbtq",
    "lgbtq_tot",          "no_lgbtq_tot"
  )
  col_order <- c("valore", "label", INTERLEAVED_ORDER)
  for (co in col_order) {
    if (!co %in% names(wide)) wide[[co]] <- "—"
  }
  wide <- wide[, col_order]

  # ordina righe per valore numerico
  suppressWarnings({
    num_val <- as.integer(wide$valore)
    if (!any(is.na(num_val))) wide <- wide[order(num_val), ]
  })

  wide$label <- ifelse(
    nchar(wide$label) > 35,
    paste0(substr(wide$label, 1, 32), "..."),
    wide$label
  )

  ALL_LABELS <- c(TOT_DIAGNOSI_LABELS, TOT_LGBT_LABELS, COMBO_LABELS)
  ALL_COLORS <- c(TOT_DIAGNOSI_COLORS, TOT_LGBT_COLORS, COMBO_COLORS)

  col_names <- c("Val.", "Etichetta", unname(ALL_LABELS[INTERLEAVED_ORDER]))
  names(wide) <- col_names

  # colori header: grigio (2) | interleaved (11)
  header_fills <- c("#2E4057", "#2E4057", unname(ALL_COLORS[INTERLEAVED_ORDER]))

  tt <- ttheme_minimal(
    base_size = 5.5,
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5, cex = 0.70),
      bg_params = list(
        fill = c("white", "#F0F4FA"),
        col  = "#CCCCCC", lwd = 0.4
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontface = "bold",
                       hjust = 0.5, x = 0.5, cex = 0.65),
      bg_params = list(fill = header_fills, col = "#CCCCCC", lwd = 0.4)
    )
  )

  tableGrob(wide, rows = NULL, theme = tt)
}

# ── HELPER: istogramma stacked ────────────────────────────────────────────────
# Una sola barra per ogni valore della variabile categorica (asse X).
# Stack = 6 combo diagnosi x lgbt.
# Y = count (n assoluto), position = "stack".
# Etichetta fetta = count / sum(count per quel valore) * 100
#   → % di quel gruppo tra tutti i rispondenti con quella categoria.
# Le barre hanno altezze diverse (proporzionali al n di rispondenti).

make_bar_plot <- function(var_name) {

  plot_df <- df_combo[df_combo$variabile == var_name, ] %>%
    mutate(
      label_wrapped = wrap_str(label, 30),
      combo         = factor(combo, levels = COMBO_ORDER)
    )

  if (nrow(plot_df) == 0) return(ggplot() + theme_void())

  # rimuovi valori senza soggetti in nessun gruppo
  plot_df <- plot_df %>%
    group_by(valore) %>%
    filter(sum(count) > 0) %>%
    ungroup()

  if (nrow(plot_df) == 0) return(ggplot() + theme_void())

  # ordine categorie sull'asse x
  suppressWarnings(num_val <- as.integer(unique(plot_df$valore)))
  ref <- plot_df %>% distinct(valore, label_wrapped)
  if (!any(is.na(num_val))) {
    ref <- ref %>% mutate(n = as.integer(valore)) %>% arrange(n)
  }
  x_levels <- ref$label_wrapped
  plot_df$label_wrapped <- factor(plot_df$label_wrapped, levels = x_levels)

  # pct_fetta = count / totale per quel valore (somma dei 6 gruppi su quel valore)
  plot_df <- plot_df %>%
    group_by(label_wrapped) %>%
    mutate(pct_fetta = count / sum(count) * 100) %>%
    ungroup()

  ggplot(plot_df, aes(x = label_wrapped, y = count, fill = combo)) +
    geom_bar(
      stat      = "identity",
      position  = "stack",
      width     = 0.65,
      colour    = "white",
      linewidth = 0.25
    ) +
    geom_text(
      aes(label = ifelse(pct_fetta >= 3, paste0(sprintf("%.0f", pct_fetta), "%"), "")),
      position = position_stack(vjust = 0.5),
      size     = 2.2,
      colour   = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = COMBO_COLORS[COMBO_ORDER],
      labels = COMBO_LABELS[COMBO_ORDER],
      name   = NULL,
      drop   = FALSE
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = NULL, y = "N rispondenti") +
    theme_minimal(base_size = 8) +
    theme(
      legend.position    = "bottom",
      legend.key.size    = unit(0.30, "cm"),
      legend.text        = element_text(size = 6),
      legend.spacing.x   = unit(0.2, "cm"),
      axis.text.x        = element_text(size = 6.5, lineheight = 0.80,
                                        angle = 20, hjust = 1),
      axis.text.y        = element_text(size = 6.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#DDDDDD", linewidth = 0.35),
      plot.margin        = margin(4, 8, 4, 4)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

# ── GENERA PDF (A4 orizzontale) ───────────────────────────────────────────────

cat(sprintf("Generating PDF for %d variables...\n", length(all_vars)))

pdf(OUTPUT_PDF, width = 11.69, height = 8.27, paper = "a4r")

for (i in seq_along(all_vars)) {

  var_name <- all_vars[i]

  # verifica che la variabile esista in almeno uno dei dataset
  has_data <- (var_name %in% df_combo$variabile) ||
              (var_name %in% df_tot_diag$variabile) ||
              (var_name %in% df_tot_lgbt$variabile)
  if (!has_data) next

  cat(sprintf("  [%d/%d] %s\n", i, length(all_vars), var_name))

  n_values <- length(unique(df_raw$valore[df_raw$variabile == var_name]))

  # helper: disegna una pagina con titolo + contenuto
  draw_page <- function(title_panel, content_grob, content_height = 0.92) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(
      nrow    = 2,
      ncol    = 1,
      heights = unit(c(0.06, 0.94), "npc")
    )))
    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(title_panel)
    popViewport()
    pushViewport(viewport(layout.pos.row = 2,
                          x = 0.5, y = 0.5, width = 0.98, height = content_height,
                          just = c("centre", "centre")))
    grid.draw(content_grob)
    popViewport()
    popViewport()
  }

  # titolo
  title_grob  <- textGrob(
    label = gsub("_", " ", toupper(var_name)),
    gp    = gpar(fontsize = 11, fontface = "bold", col = "white"),
    x = 0.02, hjust = 0
  )
  title_bg    <- rectGrob(gp = gpar(fill = "#2E4057", col = NA))
  title_panel <- grobTree(title_bg, title_grob)

  tbl_grob <- make_table_grob(var_name)
  bar_grob <- ggplotGrob(make_bar_plot(var_name))

  # soglia: se n_values > 10 tabella e grafico su pagine separate
  if (n_values > 10) {
    draw_page(title_panel, tbl_grob, content_height = 0.92)
    draw_page(title_panel, bar_grob, content_height = 0.96)
  } else {
    # pagina unica: titolo 6% | tabella (dinamica) | grafico (resto)
    table_frac <- min(0.55, max(0.28, 0.16 + n_values * 0.035))
    chart_frac <- 1 - 0.06 - table_frac

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(
      nrow    = 3,
      ncol    = 1,
      heights = unit(c(0.06, table_frac, chart_frac), "npc")
    )))

    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(title_panel)
    popViewport()

    pushViewport(viewport(layout.pos.row = 2,
                          x = 0.5, y = 0.5, width = 0.98, height = 0.92,
                          just = c("centre", "centre")))
    grid.draw(tbl_grob)
    popViewport()

    pushViewport(viewport(layout.pos.row = 3,
                          x = 0.5, y = 0.5, width = 0.96, height = 0.96,
                          just = c("centre", "centre")))
    grid.draw(bar_grob)
    popViewport()

    popViewport()
  }
}

dev.off()

# Salva nella stessa cartella dello script
script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(normalizePath(rstudioapi::getSourceEditorContext()$path))
  } else {
    frames <- sys.frames()
    ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))
    if (length(ofiles) > 0) {
      dirname(normalizePath(ofiles[[length(ofiles)]]))
    } else {
      args     <- commandArgs(trailingOnly = FALSE)
      file_arg <- args[grepl("^--file=", args)]
      if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
    }
  }
}, error = function(e) getwd())

final_path <- file.path(script_dir, OUTPUT_PDF)
if (normalizePath(OUTPUT_PDF, mustWork = FALSE) != normalizePath(final_path, mustWork = FALSE)) {
  file.copy(OUTPUT_PDF, final_path, overwrite = TRUE)
}

cat(sprintf("Done -> %s\n", final_path))
