# Clear existing data and graphics
rm(list = ls())
graphics.off()

library(dplyr)
library(moments)
library(Hmisc)

# Carica dati con label e factor
source("load_lumen_data.R")
source("compute_subgroups.R")
data <- load_lumen_data()


# ==============================================================================
# PREPARAZIONE DATI
# ==============================================================================

# --- Selezione variabili prior_ric, prior_aut, prior_salute ---
cols <- grep("^prior_ric|^prior_aut|^prior_salute", names(data), value = TRUE)
prior_data <- data[, cols]

# --- Correzione prior_aut_sociale: se > 10 dividi per 10 ---
prior_data$prior_aut_sociale <- ifelse(
  is_valid(prior_data$prior_aut_sociale) & prior_data$prior_aut_sociale > 10,
  prior_data$prior_aut_sociale / 10,
  prior_data$prior_aut_sociale
)

# ==============================================================================
# DEFINIZIONE SOTTOGRUPPI
# Usa i filtri di compute_subgroups.R (DIAGNOSI_FILTERS, LGBT_FILTERS)
#
# Gruppi diagnosi (3):
#   aut            = ha diagnosi autismo
#   altra_diagnosi = altra diagnosi psichiatrica, no autismo
#   no_diagnosi    = nessuna diagnosi
#
# Gruppi lgbt (2):
#   lgbtq          = trans OR identità non binaria OR orientamento non etero
#   no_lgbtq       = cis AND identità binaria AND eterosessuale
#
# Gruppi marginali: aut_tot, altra_diagnosi_tot, no_diagnosi_tot,
#                   lgbtq_tot, no_lgbtq_tot
# ==============================================================================

SOTTOGRUPPI <- c(
  # marginali diagnosi
  list(
    aut            = DIAGNOSI_FILTERS$aut(data),
    altra_diagnosi = DIAGNOSI_FILTERS$altra_diagnosi(data),
    no_diagnosi    = DIAGNOSI_FILTERS$no_diagnosi(data),
    lgbtq          = LGBT_FILTERS$lgbtq(data),
    no_lgbtq       = LGBT_FILTERS$no_lgbtq(data)
  ),
  # 6 incroci diagnosi x lgbt
  list(
    aut_x_lgbtq          = DIAGNOSI_FILTERS$aut(data)            & LGBT_FILTERS$lgbtq(data),
    aut_x_no_lgbtq       = DIAGNOSI_FILTERS$aut(data)            & LGBT_FILTERS$no_lgbtq(data),
    altra_diag_x_lgbtq   = DIAGNOSI_FILTERS$altra_diagnosi(data) & LGBT_FILTERS$lgbtq(data),
    altra_diag_x_no_lgbtq= DIAGNOSI_FILTERS$altra_diagnosi(data) & LGBT_FILTERS$no_lgbtq(data),
    no_diag_x_lgbtq      = DIAGNOSI_FILTERS$no_diagnosi(data)    & LGBT_FILTERS$lgbtq(data),
    no_diag_x_no_lgbtq   = DIAGNOSI_FILTERS$no_diagnosi(data)    & LGBT_FILTERS$no_lgbtq(data)
  )
)

# ==============================================================================
# FUNZIONI
# ==============================================================================

get_gruppo_var <- function(col) {
  dplyr::case_when(
    startsWith(col, "prior_ric")    ~ "prior_ric",
    startsWith(col, "prior_aut")    ~ "prior_aut",
    startsWith(col, "prior_salute") ~ "prior_salute"
  )
}

calc_stats <- function(x) {
  # Valori salvati SENZA arrotondamento: l'arrotondamento avviene solo
  # al momento del display (tabella e CSV). Boxplot e tabella leggono
  # quindi esattamente gli stessi numeri.
  x_val <- x[is_valid(x)]
  q     <- quantile(x_val, probs = c(0.25, 0.75))
  data.frame(
    N_validi   = length(x_val),
    N_mancanti = sum(!is_valid(x)),
    Media      = mean(x_val),
    DS         = sd(x_val),
    Min        = if (length(x_val)) min(x_val) else NA_real_,
    Q1         = q[[1]],
    Mediana    = median(x_val),
    Q3         = q[[2]],
    Max        = if (length(x_val)) max(x_val) else NA_real_,
    IQR        = q[[2]] - q[[1]],
    Asimmetria = moments::skewness(x_val),
    Curtosi    = moments::kurtosis(x_val) - 3
  )
}

# ==============================================================================
# CALCOLO STATISTICHE — totale + tutti i sottogruppi
# ==============================================================================

# Aggiunge il campione totale in testa
tutti_gruppi <- c(list(totale = rep(TRUE, nrow(data))), SOTTOGRUPPI)

all_stats <- do.call(rbind, lapply(names(tutti_gruppi), function(grp_name) {
  mask <- tutti_gruppi[[grp_name]]
  sub  <- prior_data[mask, , drop = FALSE]

  do.call(rbind, lapply(cols, function(col) {
    s <- calc_stats(sub[[col]])
    cbind(
      data.frame(
        Sottogruppo = grp_name,
        Variabile   = col,
        Gruppo_var  = get_gruppo_var(col),
        stringsAsFactors = FALSE
      ),
      s
    )
  }))
}))

rownames(all_stats) <- NULL

# ==============================================================================
# SALVATAGGIO CSV
# Arrotondamento applicato solo qui, al momento del salvataggio —
# i valori in all_stats restano esatti per boxplot e tabella.
# ==============================================================================

all_stats_csv <- all_stats
round_cols <- c("Media", "DS", "Min", "Q1", "Mediana", "Q3", "Max",
                "IQR", "Asimmetria", "Curtosi")
all_stats_csv[round_cols] <- lapply(all_stats_csv[round_cols],
                                    function(x) round(x, 3))

write.csv(all_stats_csv, "statistiche_prior.csv", row.names = FALSE)

cat("Statistiche calcolate su", length(cols), "variabili x",
    length(tutti_gruppi), "sottogruppi =", nrow(all_stats), "righe totali.\n")
cat("File salvato: statistiche_prior.csv\n\n")

cat("N per sottogruppo:\n")
print(sapply(tutti_gruppi, sum))

# ==============================================================================
# PDF: tabelle + boxplot per variabile
# ==============================================================================

library(ggplot2)
library(gridExtra)
library(grid)

# Ordine fisso dei sottogruppi per tabella e boxplot
ORDINE_GRUPPI <- c(
  "totale",
  "aut",            "aut_x_lgbtq",       "aut_x_no_lgbtq",
  "altra_diagnosi", "altra_diag_x_lgbtq","altra_diag_x_no_lgbtq",
  "no_diagnosi",    "no_diag_x_lgbtq",   "no_diag_x_no_lgbtq",
  "lgbtq",          "no_lgbtq"
)

# Etichette leggibili per i sottogruppi
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

# Colori per i boxplot: 3 famiglie (aut, altra diag, no diag) + marginali lgbt
COL_GRUPPI <- c(
  totale                  = "#555555",
  aut                     = "#1a6faf",
  aut_x_lgbtq             = "#5aaee0",
  aut_x_no_lgbtq          = "#a8d4f5",
  altra_diagnosi          = "#b85c00",
  altra_diag_x_lgbtq      = "#e0883a",
  altra_diag_x_no_lgbtq   = "#f5c98a",
  no_diagnosi             = "#2e8b57",
  no_diag_x_lgbtq         = "#5dbf85",
  no_diag_x_no_lgbtq      = "#a8dfc0",
  lgbtq                   = "#7b2d8b",
  no_lgbtq                = "#c07fd4"
)

# ------------------------------------------------------------------------------
# Funzione: costruisce la tabella riassuntiva per una variabile
# ------------------------------------------------------------------------------
make_table_grob <- function(var_name, stats_df, ordine, label_map, col_map) {

  sub <- stats_df[stats_df$Variabile == var_name &
                    stats_df$Sottogruppo %in% ordine, , drop = FALSE]

  # Riordina secondo l'ordine dei gruppi
  sub <- sub[match(ordine[ordine %in% sub$Sottogruppo], sub$Sottogruppo), ]

  # Arrotonda al momento del display (i valori in all_stats restano esatti)
  tab <- data.frame(
    Gruppo   = label_map[sub$Sottogruppo],
    N_validi = sub$N_validi,
    Media    = round(sub$Media,   2),
    DS       = round(sub$DS,      2),
    Mediana  = round(sub$Mediana, 2),
    Q1       = round(sub$Q1,      2),
    Q3       = round(sub$Q3,      2),
    IQR      = round(sub$IQR,     2),
    stringsAsFactors = FALSE
  )

  # Colori di sfondo righe: stesso colore del boxplot, molto trasparente
  row_colors <- col_map[sub$Sottogruppo]
  fill_core  <- matrix(
    sapply(row_colors, function(col) adjustcolor(col, alpha.f = 0.18)),
    nrow = nrow(tab), ncol = ncol(tab)
  )

  tt <- ttheme_default(
    base_size = 7,
    core = list(
      bg_params = list(fill = fill_core, col = NA),
      fg_params = list(hjust = 1, x = 0.95)
    ),
    colhead = list(
      bg_params = list(fill = "grey92", col = NA),
      fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5)
    )
  )
  tableGrob(tab, rows = NULL, theme = tt)
}

# ------------------------------------------------------------------------------
# Funzione: costruisce il boxplot per una variabile.
# Legge Q1, Mediana, Q3, IQR, Min, Max DIRETTAMENTE da all_stats —
# gli stessi valori (non arrotondati) usati dalla tabella.
#
# NON usa geom_boxplot(stat="identity") che ha comportamento instabile con
# coord_flip. Usa invece primitivi espliciti:
#   geom_segment  → baffi (whisker inferiore e superiore)
#   geom_rect     → box (da Q1 a Q3)
#   geom_segment  → linea mediana
# In questo modo ogni elemento corrisponde esattamente a un valore di all_stats.
# ------------------------------------------------------------------------------
make_boxplot <- function(var_name, stats_df, ordine, label_map, col_map) {

  sub <- stats_df[stats_df$Variabile == var_name &
                    stats_df$Sottogruppo %in% ordine, ]

  if (nrow(sub) == 0) return(NULL)

  # Ordine gruppi presenti, dal basso verso l'alto (coord già orizzontale)
  grp_presenti <- ordine[ordine %in% sub$Sottogruppo]
  lev          <- label_map[grp_presenti]
  lev          <- lev[!is.na(lev)]

  # Costruisce il dataframe con valori esatti da all_stats
  n_grp   <- length(grp_presenti)
  plot_df <- do.call(rbind, lapply(seq_along(grp_presenti), function(i) {
    grp <- grp_presenti[i]
    row <- sub[sub$Sottogruppo == grp, ]
    q1  <- row$Q1
    q3  <- row$Q3
    iqr <- row$IQR
    data.frame(
      Gruppo  = factor(label_map[grp], levels = lev),
      colore  = col_map[grp],
      y_pos   = n_grp - i + 1,                 # invertito: primo gruppo in alto
      q1      = q1,
      mediana = row$Mediana,
      q3      = q3,
      w_inf   = max(row$Min, q1 - 1.5 * iqr),  # baffo inferiore
      w_sup   = min(row$Max, q3 + 1.5 * iqr),  # baffo superiore
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)

  half_w <- 0.3   # metà larghezza del box

  ggplot(plot_df) +
    # Baffo inferiore: da w_inf a Q1
    geom_segment(aes(x = w_inf, xend = q1,
                     y = y_pos, yend = y_pos,
                     colour = colore), linewidth = 0.6) +
    # Baffo superiore: da Q3 a w_sup
    geom_segment(aes(x = q3, xend = w_sup,
                     y = y_pos, yend = y_pos,
                     colour = colore), linewidth = 0.6) +
    # Tacca alle estremità dei baffi
    geom_segment(aes(x = w_inf, xend = w_inf,
                     y = y_pos - half_w * 0.5, yend = y_pos + half_w * 0.5,
                     colour = colore), linewidth = 0.5) +
    geom_segment(aes(x = w_sup, xend = w_sup,
                     y = y_pos - half_w * 0.5, yend = y_pos + half_w * 0.5,
                     colour = colore), linewidth = 0.5) +
    # Box da Q1 a Q3
    geom_rect(aes(xmin = q1, xmax = q3,
                  ymin = y_pos - half_w, ymax = y_pos + half_w,
                  fill = colore),
              colour = "grey40", linewidth = 0.3, alpha = 0.85) +
    # Linea mediana
    geom_segment(aes(x = mediana, xend = mediana,
                     y = y_pos - half_w, yend = y_pos + half_w,
                     colour = colore),
                 colour = "grey15", linewidth = 1.1) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    scale_y_continuous(
      breaks = rev(seq_along(grp_presenti)),
      labels = lev,
      limits = c(0.5, length(grp_presenti) + 0.5)
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y  = element_text(size = 7),
      axis.text.x  = element_text(size = 7),
      plot.margin  = margin(4, 6, 4, 4)
    )
}

# ------------------------------------------------------------------------------
# Titoli di sezione per i gruppi di variabili
# ------------------------------------------------------------------------------
SEZIONI <- list(
  prior_ric    = "Priorità di ricerca (prior_ric)",
  prior_aut    = "Priorità autismo (prior_aut)",
  prior_salute = "Priorità salute (prior_salute)"
)

# ------------------------------------------------------------------------------
# Genera PDF
# ------------------------------------------------------------------------------
pdf("prior_boxplot.pdf", width = 11, height = 8.5, onefile = TRUE)

sezione_corrente <- ""

for (var_name in cols) {

  tryCatch({

    sezione <- get_gruppo_var(var_name)

    # Pagina di intestazione di sezione al cambio gruppo
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#2c3e50", col = NA))
      grid.text(
        SEZIONI[[sezione]],
        x = 0.5, y = 0.5,
        gp = gpar(col = "white", fontsize = 22, fontface = "bold")
      )
    }

    # --- Boxplot (costruito dai valori precalcolati in all_stats) ---
    bp <- make_boxplot(var_name, all_stats,
                       ORDINE_GRUPPI, LABEL_GRUPPI, COL_GRUPPI)

    # --- Tabella ---
    tb <- make_table_grob(var_name, all_stats,
                          ORDINE_GRUPPI, LABEL_GRUPPI, COL_GRUPPI)

    # --- Layout: boxplot a sinistra (60%), tabella a destra (40%) ---
    grid.newpage()

    # Titolo
    grid.text(
      var_name,
      x = 0.02, y = 0.97, hjust = 0, vjust = 1,
      gp = gpar(fontsize = 11, fontface = "bold", col = "#2c3e50")
    )

    # Boxplot (viewport sinistra)
    if (!is.null(bp)) {
      vp_left <- viewport(x = 0.01, y = 0.03, width = 0.56, height = 0.91,
                          just = c("left", "bottom"))
      print(bp, vp = vp_left)
    }

    # Tabella (viewport destra)
    vp_right <- viewport(x = 0.59, y = 0.03, width = 0.40, height = 0.91,
                         just = c("left", "bottom"))
    pushViewport(vp_right)
    grid.draw(tb)
    popViewport()

  }, error = function(e) {
    message("Errore su variabile '", var_name, "': ", conditionMessage(e))
    # Pagina di errore nel PDF invece di bloccarsi
    grid.newpage()
    grid.text(
      paste0("Errore su: ", var_name, "\n", conditionMessage(e)),
      x = 0.5, y = 0.5,
      gp = gpar(fontsize = 10, col = "red")
    )
  })
}

dev.off()
cat("PDF salvato: prior_boxplot.pdf\n")

