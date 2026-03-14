# ==============================================================================
# LUMEN_4_tests.R
#
# Test non parametrici per confrontare i sottogruppi sulle variabili di
# priorità (prior_ric, prior_aut, prior_salute), tutte misurate su scala
# continua 0-10.
#
# SCELTA DEL TEST
# ---------------
# Le distribuzioni delle variabili prior mostrano forte asimmetria negativa
# (mediane alte, code verso il basso) e i gruppi più piccoli hanno n < 30.
# Per questo motivo si usano test non parametrici, che non assumono normalità
# né omoschedasticità.
#
# ------------------------------------------------------------------------------
# PASSO 1 — EFFETTO DIAGNOSI (prior_ric e prior_salute)
# ------------------------------------------------------------------------------
# Le variabili prior_aut sono escluse da questo passo perché hanno risposte
# quasi esclusivamente dal gruppo autistico: confrontare i 3 gruppi diagnosi
# non ha senso.
#
# Test: Kruskal-Wallis (omnibus)
#   Verifica se almeno un gruppo diagnosi (aut / altra_diagnosi / no_diagnosi)
#   differisce dagli altri nella distribuzione dei ranghi. È l'equivalente non
#   parametrico dell'ANOVA a una via.
#   H0: le distribuzioni dei tre gruppi sono identiche.
#   Statistica: H (chi-quadro approssimato), df = k-1 = 2.
#   Effect size: eta² = (H - k + 1) / (n - k)
#     eta² ~ 0.01 = piccolo, ~ 0.06 = medio, ~ 0.14 = grande (Cohen 1988)
#
# Post-hoc: Mann-Whitney U (se KW significativo)
#   Confronti pairwise tra le 3 coppie di gruppi diagnosi:
#     aut vs altra_diagnosi
#     aut vs no_diagnosi
#     altra_diagnosi vs no_diagnosi
#   H0: le distribuzioni dei due gruppi sono identiche.
#   Statistica: W (somma dei ranghi).
#   Effect size: r = |Z| / sqrt(n)
#     r ~ 0.10 = piccolo, ~ 0.30 = medio, ~ 0.50 = grande (Cohen 1988)
#
# Correzione per confronti multipli: FDR Benjamini-Hochberg applicata
#   separatamente al KW omnibus (42 variabili) e a ciascuna coppia post-hoc
#   (42 variabili × 3 coppie).
#
# ------------------------------------------------------------------------------
# PASSO 2 — EFFETTO LGBT
# ------------------------------------------------------------------------------
# Test: Mann-Whitney U
#   Confronto tra lgbtq e no_lgbtq.
#   Per prior_ric e prior_salute: confronto su tutto il campione con gruppo_lgbt.
#   Per prior_aut: confronto SOLO entro il gruppo autistico (aut_x_lgbtq vs
#   aut_x_no_lgbtq), perché le altre diagnosi non hanno risposte su queste
#   variabili.
#   Effect size: r = |Z| / sqrt(n)
#
# Correzione FDR su tutte le variabili (42 test).
#
# ------------------------------------------------------------------------------
# PASSO 3 — CONFRONTI PAIRWISE SUI 6 GRUPPI INCROCIATI (prior_ric, prior_salute)
# ------------------------------------------------------------------------------
# Le variabili prior_aut sono escluse (stesso motivo del passo 1).
#
# Test: Mann-Whitney U su tutte le 15 coppie possibili tra i 6 gruppi:
#   aut_x_lgbtq, aut_x_no_lgbtq,
#   altra_diagnosi_x_lgbtq, altra_diagnosi_x_no_lgbtq,
#   no_diagnosi_x_lgbtq, no_diagnosi_x_no_lgbtq
#
# Correzione FDR applicata per variabile (15 confronti per variabile).
#
# ------------------------------------------------------------------------------
# INTERPRETAZIONE DEI RISULTATI
# ------------------------------------------------------------------------------
# p e p_fdr:
#   Il p grezzo indica la probabilità di osservare una statistica almeno così
#   estrema sotto H0. Il p_fdr è il p corretto per confronti multipli con FDR
#   Benjamini-Hochberg: controlla il tasso atteso di falsi positivi tra i test
#   significativi (meno conservativo di Bonferroni, più appropriato per analisi
#   esplorative).
#   Soglie: p_fdr < 0.05 = significativo (*), < 0.01 (**), < 0.001 (***)
#   Nel PDF: sfondo rosso chiaro = significativo, arancio = tendenza (< 0.10)
#
# Effect size r (Mann-Whitney):
#   Misura la forza dell'effetto indipendentemente dall'n.
#   r = 0.10 piccolo | r = 0.30 medio | r = 0.50 grande
#   Un test può essere significativo con r piccolo se n è grande: valutare
#   sempre r insieme a p_fdr.
#
# Effect size eta² (Kruskal-Wallis):
#   Proporzione approssimativa di varianza spiegata dal gruppo.
#   eta² = 0.01 piccolo | 0.06 medio | 0.14 grande
#
# Attenzione ai gruppi piccoli:
#   I gruppi incrociati più piccoli (es. altra_diagnosi_x_no_lgbtq, n~25)
#   hanno potenza statistica ridotta: un test non significativo non implica
#   assenza di effetto. Interpretare con cautela.
#
# ------------------------------------------------------------------------------
# OUTPUT
# ------------------------------------------------------------------------------
#   test_diagnosi.csv  — KW omnibus + post-hoc pairwise diagnosi
#   test_lgbt.csv      — Mann-Whitney effetto LGBT
#   test_pairwise.csv  — Mann-Whitney pairwise 6 gruppi incrociati
#   test_risultati.csv — tutti i risultati in un unico file lungo
#   test_risultati.pdf — una pagina per variabile con tabelle dei risultati
# ==============================================================================

rm(list = ls())
graphics.off()

library(dplyr)
library(Hmisc)

source("load_lumen_data.R")
source("compute_subgroups.R")
data <- load_lumen_data()
data <- data[data$consenso == 1 & !is.na(data$consenso), ]

# ==============================================================================
# PREPARAZIONE DATI
# ==============================================================================

# Variabili per diagnosi + lgbt (prior_ric e prior_salute: tutti i gruppi)
cols_diag <- grep("^prior_ric|^prior_salute", names(data), value = TRUE)

# Variabili solo per lgbt entro autistici (prior_aut: solo gruppo aut risponde)
cols_aut  <- grep("^prior_aut", names(data), value = TRUE)

# Tutte le variabili (per pairwise 6 gruppi e per sezione 2 LGBT su prior_ric/salute)
cols <- c(cols_diag, cols_aut)

data$prior_aut_sociale <- ifelse(
  is_valid(data$prior_aut_sociale) & data$prior_aut_sociale > 10,
  data$prior_aut_sociale / 10,
  data$prior_aut_sociale
)

# ==============================================================================
# VETTORI GRUPPO
# ==============================================================================

diag_vec <- dplyr::case_when(
  DIAGNOSI_FILTERS$aut(data)            ~ "aut",
  DIAGNOSI_FILTERS$altra_diagnosi(data) ~ "altra_diagnosi",
  DIAGNOSI_FILTERS$no_diagnosi(data)    ~ "no_diagnosi",
  TRUE                                  ~ NA_character_
)

lgbt_vec <- dplyr::case_when(
  LGBT_FILTERS$lgbtq(data)    ~ "lgbtq",
  LGBT_FILTERS$no_lgbtq(data) ~ "no_lgbtq",
  TRUE                         ~ NA_character_
)

data$gruppo_diag <- factor(diag_vec,
  levels = c("aut", "altra_diagnosi", "no_diagnosi"))
data$gruppo_lgbt <- factor(lgbt_vec,
  levels = c("lgbtq", "no_lgbtq"))
data$gruppo6 <- factor(
  ifelse(!is.na(diag_vec) & !is.na(lgbt_vec),
         paste(diag_vec, lgbt_vec, sep = "_x_"), NA_character_),
  levels = c("aut_x_lgbtq", "aut_x_no_lgbtq",
             "altra_diagnosi_x_lgbtq", "altra_diagnosi_x_no_lgbtq",
             "no_diagnosi_x_lgbtq",    "no_diagnosi_x_no_lgbtq"))

cat("N per gruppo diagnosi:\n");       print(table(data$gruppo_diag, useNA = "ifany"))
cat("\nN per gruppo LGBT:\n");         print(table(data$gruppo_lgbt, useNA = "ifany"))
cat("\nN per 6 gruppi incrociati:\n"); print(table(data$gruppo6,     useNA = "ifany"))

# ==============================================================================
# FUNZIONI HELPER
# ==============================================================================

kw_eta2 <- function(H, k, n) round(max((H - k + 1) / (n - k), 0), 4)

mw_effect_r <- function(mw_obj, n) {
  z <- qnorm(mw_obj$p.value / 2)
  round(abs(z) / sqrt(n), 4)
}

safe_mw <- function(x, y) {
  if (length(x) < 3 || length(y) < 3) return(NULL)
  tryCatch(
    suppressWarnings(wilcox.test(x, y, exact = FALSE, conf.int = FALSE)),
    error = function(e) NULL
  )
}

# Formatta p-value per output:
# - usa notazione scientifica se p < 0.001 (evita "0.0000")
# - altrimenti arrotonda a 4 decimali
fmt_p <- Vectorize(function(p, digits = 4) {
  if (is.na(p)) return(NA_real_)
  if (p < 0.001) return(signif(p, 3))   # es. 2.34e-05
  round(p, digits)
})

# ==============================================================================
# 1. EFFETTO DIAGNOSI
# ==============================================================================

cat("\n\n--- 1. Kruskal-Wallis + post-hoc Mann-Whitney (diagnosi) ---\n")

coppie_diag <- list(
  c("aut",            "altra_diagnosi"),
  c("aut",            "no_diagnosi"),
  c("altra_diagnosi", "no_diagnosi")
)

# Solo prior_ric e prior_salute: prior_aut ha risposte solo dal gruppo aut
res_diagnosi <- do.call(rbind, lapply(cols_diag, function(col) {

  df  <- data[!is.na(data$gruppo_diag), ]
  x   <- df[[col]];  grp <- df$gruppo_diag
  ok  <- !is.na(x);  xv  <- x[ok]; gv <- droplevels(grp[ok])
  n   <- length(xv); k   <- nlevels(gv)

  # Controlla che ci siano almeno 2 gruppi con osservazioni valide
  grp_validi <- names(table(gv))[table(gv) >= 3]
  if (length(grp_validi) < 2) {
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

  kw   <- kruskal.test(xv ~ gv)
  eta2 <- kw_eta2(kw$statistic, k, n)

  row <- data.frame(
    Variabile = col, N = n,
    KW_H = round(kw$statistic, 3), KW_df = kw$parameter,
    KW_p = fmt_p(kw$p.value),   KW_eta2 = eta2,
    stringsAsFactors = FALSE
  )

  for (cp in coppie_diag) {
    nm <- paste(cp, collapse = "_vs_")
    g1 <- xv[gv == cp[1]]; g2 <- xv[gv == cp[2]]
    mw <- safe_mw(g1, g2)
    row[[paste0(nm, "_n1")]] <- length(g1)
    row[[paste0(nm, "_n2")]] <- length(g2)
    row[[paste0(nm, "_W")]]  <- if (!is.null(mw)) round(mw$statistic, 1)       else NA
    row[[paste0(nm, "_p")]]  <- if (!is.null(mw)) fmt_p(mw$p.value)         else NA
    row[[paste0(nm, "_r")]]  <- if (!is.null(mw)) mw_effect_r(mw, length(g1) + length(g2)) else NA
  }
  row
}))

res_diagnosi$KW_p_fdr <- fmt_p(p.adjust(res_diagnosi$KW_p, method = "BH"))
for (cp in coppie_diag) {
  nm <- paste(cp, collapse = "_vs_")
  pcol <- paste0(nm, "_p")
  res_diagnosi[[paste0(nm, "_p_fdr")]] <-
    fmt_p(p.adjust(res_diagnosi[[pcol]], method = "BH"))
}

write.csv(res_diagnosi, "test_diagnosi.csv", row.names = FALSE)
cat("Salvato: test_diagnosi.csv\n")

# ==============================================================================
# 2. EFFETTO LGBT
# ==============================================================================

cat("\n--- 2. Mann-Whitney (LGBT) ---\n")
# prior_ric e prior_salute: tutti i soggetti con gruppo_lgbt valido
# prior_aut: solo soggetti autistici (aut) con gruppo_lgbt valido

run_mw_lgbt <- function(col, df_base) {
  df  <- df_base[!is.na(df_base$gruppo_lgbt), ]
  x   <- df[[col]]; grp <- droplevels(df$gruppo_lgbt)
  ok  <- !is.na(x); xv  <- x[ok]; gv <- grp[ok]
  g1  <- xv[gv == "lgbtq"]; g2 <- xv[gv == "no_lgbtq"]
  mw  <- safe_mw(g1, g2)
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
    MW_r        = mw_effect_r(mw, length(g1) + length(g2)),
    stringsAsFactors = FALSE
  )
}

data_aut <- data[!is.na(data$gruppo_diag) & data$gruppo_diag == "aut", ]

res_lgbt <- rbind(
  # prior_ric e prior_salute: tutti i soggetti
  do.call(rbind, lapply(cols_diag, run_mw_lgbt, df_base = data)),
  # prior_aut: solo autistici
  do.call(rbind, lapply(cols_aut,  run_mw_lgbt, df_base = data_aut))
)

res_lgbt$MW_p_fdr <- fmt_p(p.adjust(res_lgbt$MW_p, method = "BH"))
write.csv(res_lgbt, "test_lgbt.csv", row.names = FALSE)
cat("Salvato: test_lgbt.csv\n")

# ==============================================================================
# 3. PAIRWISE 6 GRUPPI INCROCIATI
# ==============================================================================

cat("\n--- 3. Mann-Whitney pairwise (6 gruppi incrociati) ---\n")

coppie6 <- combn(levels(data$gruppo6), 2, simplify = FALSE)

# prior_aut escluse: il confronto pairwise 6 gruppi non ha senso
# perché solo il gruppo aut ha risposte su queste variabili
res_pw <- do.call(rbind, lapply(cols_diag, function(col) {
  do.call(rbind, lapply(coppie6, function(cp) {

    g1 <- data[[col]][!is.na(data$gruppo6) & data$gruppo6 == cp[1] & !is.na(data[[col]])]
    g2 <- data[[col]][!is.na(data$gruppo6) & data$gruppo6 == cp[2] & !is.na(data[[col]])]
    mw <- safe_mw(g1, g2)

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
      MW_r = mw_effect_r(mw, length(g1) + length(g2)),
      stringsAsFactors = FALSE
    )
  }))
}))

res_pw <- res_pw %>%
  group_by(Variabile) %>%
  mutate(MW_p_fdr = fmt_p(p.adjust(MW_p, method = "BH"))) %>%
  ungroup() %>%
  as.data.frame()

write.csv(res_pw, "test_pairwise.csv", row.names = FALSE)
cat("Salvato: test_pairwise.csv\n")

# ==============================================================================
# RIEPILOGO
# ==============================================================================

cat("\n\n======================================================\n")
cat("RIEPILOGO — variabili con effetto significativo (FDR < 0.05)\n")
cat("======================================================\n")

cat("\n1. Kruskal-Wallis diagnosi (omnibus):\n")
sig <- res_diagnosi[!is.na(res_diagnosi$KW_p_fdr) & res_diagnosi$KW_p_fdr < 0.05,
                    c("Variabile","KW_H","KW_df","KW_p","KW_p_fdr","KW_eta2")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

cat("\n2. Mann-Whitney LGBT:\n")
sig <- res_lgbt[!is.na(res_lgbt$MW_p_fdr) & res_lgbt$MW_p_fdr < 0.05,
                c("Variabile","N_lgbtq","N_no_lgbtq","MW_W","MW_p","MW_p_fdr","MW_r")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

cat("\n3. Pairwise 6 gruppi (coppie con FDR < 0.05):\n")
sig <- res_pw[!is.na(res_pw$MW_p_fdr) & res_pw$MW_p_fdr < 0.05,
              c("Variabile","Gruppo_1","Gruppo_2","N1","N2","MW_W","MW_p","MW_p_fdr","MW_r")]
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("  Nessuna\n")

# ==============================================================================
# CSV UNICO — tutte le sezioni in un solo file
# ==============================================================================

# Nota: prior_aut solo in sezione 2 (LGBT entro autistici), non in 1 e 3
# Sezione 1: KW omnibus (una riga per variabile, solo prior_ric e prior_salute)
csv_diag <- data.frame(
  Sezione   = "1_diagnosi_KW",
  Variabile = res_diagnosi$Variabile,
  Confronto = "omnibus",
  Gruppo_1  = NA_character_,
  Gruppo_2  = NA_character_,
  N1        = res_diagnosi$N,
  N2        = NA_integer_,
  Statistica = res_diagnosi$KW_H,
  df        = res_diagnosi$KW_df,
  p         = res_diagnosi$KW_p,
  p_fdr     = res_diagnosi$KW_p_fdr,
  effect    = res_diagnosi$KW_eta2,
  effect_label = "eta2",
  stringsAsFactors = FALSE
)

# Post-hoc diagnosi
csv_diag_ph <- do.call(rbind, lapply(coppie_diag, function(cp) {
  nm <- paste(cp, collapse = "_vs_")
  data.frame(
    Sezione   = "1_diagnosi_posthoc",
    Variabile = res_diagnosi$Variabile,
    Confronto = nm,
    Gruppo_1  = cp[1], Gruppo_2 = cp[2],
    N1        = res_diagnosi[[paste0(nm, "_n1")]],
    N2        = res_diagnosi[[paste0(nm, "_n2")]],
    Statistica = res_diagnosi[[paste0(nm, "_W")]],
    df        = NA_real_,
    p         = res_diagnosi[[paste0(nm, "_p")]],
    p_fdr     = res_diagnosi[[paste0(nm, "_p_fdr")]],
    effect    = res_diagnosi[[paste0(nm, "_r")]],
    effect_label = "r",
    stringsAsFactors = FALSE
  )
}))

# Sezione 2: LGBT
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

# Sezione 3: pairwise 6 gruppi
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

csv_all <- rbind(csv_diag, csv_diag_ph, csv_lgbt, csv_pw)
write.csv(csv_all, "test_risultati.csv", row.names = FALSE)
cat("Salvato: test_risultati.csv\n")

# ==============================================================================
# PDF — una pagina per variabile
# ==============================================================================

library(grid)
library(gridExtra)

# Colori per significatività
col_sig   <- "#c0392b"   # rosso  — FDR < 0.05
col_trend <- "#e67e22"   # arancio — FDR < 0.10
col_ns    <- "grey30"    # grigio  — non significativo

# Colora cella in base a FDR (usata solo nel PDF per sfondo tabella)
cell_color_p <- function(p, p_fdr) {
  if (is.na(p)) return(list(txt = "—", col = col_ns))
  txt <- formatC(p, format = "f", digits = 4)
  if (!is.na(p_fdr)) {
    if (p_fdr < 0.05)  txt <- paste0(txt, " *")
    if (p_fdr < 0.01)  txt <- paste0(txt, "*")
    if (p_fdr < 0.001) txt <- paste0(txt, "*")
  }
  col <- if (!is.na(p_fdr) && p_fdr < 0.05) col_sig else
         if (!is.na(p_fdr) && p_fdr < 0.10) col_trend else col_ns
  list(txt = txt, col = col)
}

# Costruisce un grob tabella con celle colorate per significatività
make_test_grob <- function(df_tab, title) {
  if (is.null(df_tab) || nrow(df_tab) == 0) return(NULL)

  n_col <- ncol(df_tab)
  n_row <- nrow(df_tab)

  # Colori di sfondo intestazione
  head_fill <- matrix("grey88", nrow = 1,   ncol = n_col)
  # Colori di sfondo dati: bianco/grigio alternati
  core_fill <- matrix(
    rep(c("white", "grey97"), length.out = n_row * n_col),
    nrow = n_row, ncol = n_col
  )

  # Colora colonne p e p_fdr in base alla significatività
  p_col_idx    <- which(names(df_tab) == "p")
  pfdr_col_idx <- which(names(df_tab) == "p_fdr")
  if (length(p_col_idx) && length(pfdr_col_idx)) {
    for (i in seq_len(n_row)) {
      pv   <- suppressWarnings(as.numeric(df_tab[i, p_col_idx]))
      pfdr <- suppressWarnings(as.numeric(df_tab[i, pfdr_col_idx]))
      clr  <- if (!is.na(pfdr) && pfdr < 0.05) "#fde8e8" else
              if (!is.na(pfdr) && pfdr < 0.10) "#fef3e2" else "white"
      core_fill[i, p_col_idx]    <- clr
      core_fill[i, pfdr_col_idx] <- clr
    }
  }

  tt <- ttheme_default(
    base_size = 6.5,
    core    = list(
      bg_params = list(fill = core_fill, col = "grey80"),
      fg_params = list(hjust = 1, x = 0.95, fontsize = 6.5)
    ),
    colhead = list(
      bg_params = list(fill = head_fill, col = "grey70"),
      fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5, fontsize = 6.5)
    )
  )

  grob <- tableGrob(df_tab, rows = NULL, theme = tt)

  # Aggiunge titolo sopra la tabella
  title_grob <- textGrob(title, gp = gpar(fontsize = 8, fontface = "bold",
                                           col = "#2c3e50"), hjust = 0, x = 0.01)
  arrangeGrob(title_grob, grob, ncol = 1, heights = unit(c(0.3, 1), c("cm", "null")))
}

# Genera una pagina PDF per una variabile
make_var_page <- function(col) {

  is_aut_var <- startsWith(col, "prior_aut")

  # ---- Tabella 1: KW omnibus (solo prior_ric e prior_salute) ----
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
    tab2_rows <- lapply(coppie_diag, function(cp) {
      nm <- paste(cp, collapse = "_vs_")
      data.frame(
        Confronto = paste(cp[1], "vs", cp[2]),
        N1  = row_kw[[paste0(nm, "_n1")]],
        N2  = row_kw[[paste0(nm, "_n2")]],
        W   = round(row_kw[[paste0(nm, "_W")]], 1),
        p   = fmt_p(row_kw[[paste0(nm, "_p")]]),
        p_fdr = fmt_p(row_kw[[paste0(nm, "_p_fdr")]]),
        r   = round(row_kw[[paste0(nm, "_r")]], 4),
        stringsAsFactors = FALSE
      )
    })
    tab2 <- do.call(rbind, tab2_rows)
  } else {
    tab1 <- NULL
    tab2 <- NULL
  }

  # ---- Tabella 3: LGBT ----
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

  # ---- Tabella 4: pairwise 6 gruppi (solo prior_ric e prior_salute) ----
  if (!is_aut_var) {
    pw_col <- res_pw[res_pw$Variabile == col,
                     c("Gruppo_1","Gruppo_2","N1","N2","MW_W","MW_p","MW_p_fdr","MW_r")]
    names(pw_col) <- c("Gruppo_1","Gruppo_2","N1","N2","W","p","p_fdr","r")
    pw_col$W     <- round(as.numeric(pw_col$W), 1)
    pw_col$p     <- sapply(pw_col$p,     function(x) fmt_p(as.numeric(x)))
    pw_col$p_fdr <- sapply(pw_col$p_fdr, function(x) fmt_p(as.numeric(x)))
    pw_col$r     <- round(as.numeric(pw_col$r), 4)
  } else {
    pw_col <- NULL
  }

  # Costruisce i grob (NULL se la sezione non si applica)
  g1 <- if (!is.null(tab1))   make_test_grob(tab1,   "1. Kruskal-Wallis omnibus (diagnosi)")  else NULL
  g2 <- if (!is.null(tab2))   make_test_grob(tab2,   "2. Post-hoc Mann-Whitney (diagnosi)")   else NULL
  g3 <-                        make_test_grob(tab3,   "3. Mann-Whitney effetto LGBT")
  g4 <- if (!is.null(pw_col)) make_test_grob(pw_col, "4. Mann-Whitney pairwise (6 gruppi)")   else NULL

  grobs  <- Filter(Negate(is.null), list(g1, g2, g3, g4))
  if (length(grobs) == 0) return(invisible(NULL))

  grid.newpage()

  # Intestazione fissa in cima (titolo + leggenda)
  pushViewport(viewport(x = 0, y = 0.88, width = 1, height = 0.12,
                         just = c("left", "bottom")))
  grid.text(col,
            x = 0.02, y = 0.85, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 12, fontface = "bold", col = "#2c3e50"))
  grid.text("* FDR<0.05  ** FDR<0.01  *** FDR<0.001  |  sfondo rosso = significativo, arancio = tendenza",
            x = 0.02, y = 0.30, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 6, col = "grey50"))
  popViewport()

  # Area contenuto sotto l'intestazione
  # Colonna sinistra: tabelle 1+2 (KW omnibus + post-hoc diagnosi)
  vp_left  <- viewport(x = 0.01, y = 0.02, width = 0.48, height = 0.85,
                        just = c("left", "bottom"))
  # Colonna destra: tabella 3 (LGBT) in alto, tabella 4 (pairwise) in basso
  # Altezze proporzionali: tabella 4 ha 15 righe, tabella 3 ne ha 1
  vp_right <- viewport(x = 0.51, y = 0.02, width = 0.48, height = 0.85,
                        just = c("left", "bottom"))

  pushViewport(vp_left)
  grid.draw(arrangeGrob(
    grobs[[1]],
    if (length(grobs) >= 2) grobs[[2]] else nullGrob(),
    ncol = 1, heights = unit(c(1, 3), "null")
  ))
  popViewport()

  pushViewport(vp_right)
  g3_grob <- if (length(grobs) >= 3) grobs[[3]] else nullGrob()
  g4_grob <- if (length(grobs) >= 4) grobs[[4]] else nullGrob()
  grid.draw(arrangeGrob(
    g3_grob, g4_grob,
    ncol = 1, heights = unit(c(1, 5), "null")
  ))
  popViewport()
}

# Genera il PDF
pdf("test_risultati.pdf", width = 11, height = 8.5, onefile = TRUE)

sezione_corrente <- ""
for (col in cols) {
  tryCatch({
    sezione <- dplyr::case_when(
      startsWith(col, "prior_ric")    ~ "Priorita di ricerca (prior_ric)",
      startsWith(col, "prior_aut")    ~ "Priorita autismo (prior_aut)",
      startsWith(col, "prior_salute") ~ "Priorita salute (prior_salute)"
    )
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#2c3e50", col = NA))
      grid.text(sezione, x = 0.5, y = 0.5,
                gp = gpar(col = "white", fontsize = 20, fontface = "bold"))
    }
    make_var_page(col)
  }, error = function(e) {
    message("Errore su ", col, ": ", conditionMessage(e))
    grid.newpage()
    grid.text(paste0("Errore su: ", col, "\n", conditionMessage(e)),
              x = 0.5, y = 0.5, gp = gpar(fontsize = 9, col = "red"))
  })
}

dev.off()
cat("Salvato: test_risultati.pdf\n")

# ==============================================================================
# PDF RISULTATI SIGNIFICATIVI — una pagina per variabile con effetti sig.
#
# Per ogni variabile con almeno un confronto significativo (FDR < 0.05)
# produce una pagina con:
#   - Tabella dei soli confronti significativi
#   - Testo automatico che descrive la direzione degli effetti
#   - Spazio per note manuali
# ==============================================================================

# ------------------------------------------------------------------------------
# Funzione: direzione dell'effetto (mediana gruppo 1 vs gruppo 2)
# Restituisce una stringa "G1 > G2", "G1 < G2" o "G1 = G2"
# ------------------------------------------------------------------------------
direzione_effetto <- function(col, grp1_nome, grp2_nome) {
  m1_mask <- !is.na(data$gruppo6) & data$gruppo6 == grp1_nome & !is.na(data[[col]])
  m2_mask <- !is.na(data$gruppo6) & data$gruppo6 == grp2_nome & !is.na(data[[col]])

  # Per confronti diagnosi usa gruppo_diag, per LGBT usa gruppo_lgbt
  if (grp1_nome %in% levels(data$gruppo_diag)) {
    m1_mask <- !is.na(data$gruppo_diag) & data$gruppo_diag == grp1_nome & !is.na(data[[col]])
    m2_mask <- !is.na(data$gruppo_diag) & data$gruppo_diag == grp2_nome & !is.na(data[[col]])
  }
  if (grp1_nome %in% c("lgbtq", "no_lgbtq")) {
    # Per prior_aut usa solo autistici
    base <- if (startsWith(col, "prior_aut")) {
      data[!is.na(data$gruppo_diag) & data$gruppo_diag == "aut", ]
    } else data
    m1_mask <- !is.na(base$gruppo_lgbt) & base$gruppo_lgbt == grp1_nome & !is.na(base[[col]])
    m2_mask <- !is.na(base$gruppo_lgbt) & base$gruppo_lgbt == grp2_nome & !is.na(base[[col]])
    med1 <- median(base[[col]][m1_mask], na.rm = TRUE)
    med2 <- median(base[[col]][m2_mask], na.rm = TRUE)
    sym  <- if (med1 > med2) ">" else if (med1 < med2) "<" else "="
    return(list(sym = sym, med1 = round(med1, 2), med2 = round(med2, 2)))
  }

  med1 <- median(data[[col]][m1_mask], na.rm = TRUE)
  med2 <- median(data[[col]][m2_mask], na.rm = TRUE)
  sym  <- if (med1 > med2) ">" else if (med1 < med2) "<" else "="
  list(sym = sym, med1 = round(med1, 2), med2 = round(med2, 2))
}

# Etichette leggibili per i nomi dei gruppi
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
# Funzione: genera il testo automatico per una variabile
# Raccoglie tutti i confronti significativi e li descrive
# ------------------------------------------------------------------------------
genera_testo <- function(col) {
  frasi <- c()
  is_aut_var <- startsWith(col, "prior_aut")

  # --- Effetto diagnosi (KW + post-hoc) ---
  if (!is_aut_var) {
    row_kw <- res_diagnosi[res_diagnosi$Variabile == col, ]
    kw_fdr <- as.numeric(row_kw$KW_p_fdr)
    if (!is.na(kw_fdr) && kw_fdr < 0.05) {
      frasi <- c(frasi, sprintf(
        "Effetto diagnosi significativo [KW: H=%.2f, p_fdr=%s, eta2=%.3f].",
        row_kw$KW_H, row_kw$KW_p_fdr, row_kw$KW_eta2))

      for (cp in coppie_diag) {
        nm    <- paste(cp, collapse = "_vs_")
        p_fdr <- as.numeric(row_kw[[paste0(nm, "_p_fdr")]])
        if (!is.na(p_fdr) && p_fdr < 0.05) {
          dir   <- direzione_effetto(col, cp[1], cp[2])
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

  # --- Effetto LGBT ---
  row_lgbt <- res_lgbt[res_lgbt$Variabile == col, ]
  lgbt_fdr <- as.numeric(row_lgbt$MW_p_fdr)
  if (!is.na(lgbt_fdr) && lgbt_fdr < 0.05) {
    dir   <- direzione_effetto(col, "lgbtq", "no_lgbtq")
    frasi <- c(frasi, sprintf(
      "Effetto LGBT+ significativo: LGBT+ (med=%.2f) %s No LGBT+ (med=%.2f)  [W=%s, r=%s, p_fdr=%s].",
      dir$med1, dir$sym, dir$med2,
      row_lgbt$MW_W, row_lgbt$MW_r, row_lgbt$MW_p_fdr))
  }

  # --- Pairwise 6 gruppi ---
  if (!is_aut_var) {
    pw_sig <- res_pw[res_pw$Variabile == col &
                       !is.na(res_pw$MW_p_fdr) &
                       as.numeric(res_pw$MW_p_fdr) < 0.05, ]
    if (nrow(pw_sig) > 0) {
      frasi <- c(frasi, "Confronti pairwise significativi (6 gruppi):")
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

  if (length(frasi) == 0) return(NULL)
  frasi
}

# ------------------------------------------------------------------------------
# Funzione: pagina PDF per variabile significativa
# ------------------------------------------------------------------------------
make_sig_page <- function(col) {
  frasi <- genera_testo(col)
  if (is.null(frasi)) return(invisible(NULL))

  is_aut_var <- startsWith(col, "prior_aut")

  grid.newpage()

  # Sfondo intestazione
  grid.rect(x = 0, y = 0.92, width = 1, height = 0.08,
            just = c("left", "bottom"),
            gp = gpar(fill = "#2c3e50", col = NA))
  grid.text(col, x = 0.02, y = 0.97, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 12, fontface = "bold", col = "white"))
  grid.text("Risultati statisticamente significativi (FDR < 0.05)",
            x = 0.99, y = 0.97, hjust = 1, vjust = 1,
            gp = gpar(fontsize = 7, col = "grey80"))

  # ---------- Tabella confronti significativi ----------
  # Costruisce la tabella con solo le righe sig.
  tab_sig_rows <- list()

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

  if (length(tab_sig_rows) > 0) {
    tab_sig <- do.call(rbind, tab_sig_rows)

    # Colori sfondo per significatività
    n_row <- nrow(tab_sig); n_col <- ncol(tab_sig)
    fill_core <- matrix("white", nrow = n_row, ncol = n_col)
    for (i in seq_len(n_row)) {
      clr <- if (tab_sig$p_fdr[i] < 0.001) "#fad4d4" else
             if (tab_sig$p_fdr[i] < 0.01)  "#fde0e0" else "#fff0f0"
      fill_core[i, ] <- clr
    }
    tab_sig$p_fdr <- fmt_p(tab_sig$p_fdr)

    tt <- ttheme_default(
      base_size = 7.5,
      core    = list(bg_params = list(fill = fill_core, col = "grey85"),
                     fg_params = list(hjust = 1, x = 0.95)),
      colhead = list(bg_params = list(fill = "grey88", col = "grey70"),
                     fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5))
    )
    tab_grob <- tableGrob(tab_sig, rows = NULL, theme = tt)

    vp_tab <- viewport(x = 0.01, y = 0.52, width = 0.98, height = 0.38,
                       just = c("left", "bottom"))
    pushViewport(vp_tab)
    grid.draw(tab_grob)
    popViewport()
  }

  # ---------- Testo automatico ----------
  grid.text("Interpretazione:",
            x = 0.02, y = 0.50, hjust = 0, vjust = 1,
            gp = gpar(fontsize = 10, fontface = "bold", col = "#2c3e50"))

  y_txt <- 0.45
  for (frase in frasi) {
    grid.text(frase, x = 0.03, y = y_txt, hjust = 0, vjust = 1,
              gp = gpar(fontsize = 9, col = "grey15"))
    y_txt <- y_txt - 0.048
  }
}

# ------------------------------------------------------------------------------
# Identifica le variabili con almeno un risultato significativo
# ------------------------------------------------------------------------------
vars_sig <- unique(c(
  # KW diagnosi significativo
  res_diagnosi$Variabile[!is.na(res_diagnosi$KW_p_fdr) &
                           as.numeric(res_diagnosi$KW_p_fdr) < 0.05],
  # Post-hoc diagnosi significativo
  unlist(lapply(coppie_diag, function(cp) {
    nm <- paste(cp, collapse = "_vs_")
    res_diagnosi$Variabile[!is.na(res_diagnosi[[paste0(nm, "_p_fdr")]]) &
                              as.numeric(res_diagnosi[[paste0(nm, "_p_fdr")]]) < 0.05]
  })),
  # LGBT significativo
  res_lgbt$Variabile[!is.na(res_lgbt$MW_p_fdr) &
                       as.numeric(res_lgbt$MW_p_fdr) < 0.05],
  # Pairwise significativo
  res_pw$Variabile[!is.na(res_pw$MW_p_fdr) &
                     as.numeric(res_pw$MW_p_fdr) < 0.05]
))
# Mantieni l'ordine originale delle variabili
vars_sig <- cols[cols %in% vars_sig]

cat(sprintf("\nVariabili con almeno un effetto significativo (FDR < 0.05): %d\n",
            length(vars_sig)))
cat(paste(vars_sig, collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# Genera PDF risultati significativi
# ------------------------------------------------------------------------------
pdf("test_significativi.pdf", width = 11, height = 8.5, onefile = TRUE)

# Pagina di copertina
grid.newpage()
grid.rect(gp = gpar(fill = "#2c3e50", col = NA))
grid.text("Risultati Significativi", x = 0.5, y = 0.60,
          gp = gpar(col = "white", fontsize = 24, fontface = "bold"))
grid.text(sprintf("%d variabili con FDR < 0.05", length(vars_sig)),
          x = 0.5, y = 0.50,
          gp = gpar(col = "grey80", fontsize = 14))
grid.text(paste(strwrap(paste(vars_sig, collapse = " | "), width = 90),
                collapse = "
"),
          x = 0.5, y = 0.35,
          gp = gpar(col = "grey70", fontsize = 7))
grid.text("Ogni pagina: tabella confronti sig. + interpretazione automatica + spazio note",
          x = 0.5, y = 0.20,
          gp = gpar(col = "grey60", fontsize = 8, fontface = "italic"))

sezione_corrente <- ""
for (col in vars_sig) {
  tryCatch({
    sezione <- dplyr::case_when(
      startsWith(col, "prior_ric")    ~ "Priorita di ricerca (prior_ric)",
      startsWith(col, "prior_aut")    ~ "Priorita autismo (prior_aut)",
      startsWith(col, "prior_salute") ~ "Priorita salute (prior_salute)"
    )
    if (sezione != sezione_corrente) {
      sezione_corrente <- sezione
      grid.newpage()
      grid.rect(gp = gpar(fill = "#34495e", col = NA))
      grid.text(sezione, x = 0.5, y = 0.5,
                gp = gpar(col = "white", fontsize = 18, fontface = "bold"))
    }
    make_sig_page(col)
  }, error = function(e) {
    message("Errore su ", col, ": ", conditionMessage(e))
    grid.newpage()
    grid.text(paste0("Errore su: ", col, "\n", conditionMessage(e)),
              x = 0.5, y = 0.5, gp = gpar(fontsize = 9, col = "red"))
  })
}

dev.off()
cat(sprintf("Salvato: test_significativi.pdf (%d pagine variabile + copertina + sezioni)\n",
            length(vars_sig)))
