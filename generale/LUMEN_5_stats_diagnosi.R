# ==============================================================================
# LUMEN_5_stats_diagnosi.R
#
# PURPOSE
# -------
# Testa se il gruppo diagnostico (aut vs altra_diagnosi) e lo stato LGBTQ+
# (lgbtq vs no_lgbtq) influenzano le variabili aggregate del percorso
# diagnostico:
#
#   valutazione  — setting diagnosi (pubblico / privato / entrambi)
#   invio        — chi ha inviato alla prima visita
#   prima_visita — tempo attesa prima visita
#   att_diagnosi — tempo attesa diagnosi dalla prima visita
#   sbagliate    — diagnosi sbagliate ricevute (0/1)
#   costo        — costo della diagnosi
#   dove         — luogo geografico della diagnosi
#   n_prof       — numero di professionisti consultati
#
# STRUTTURA DATASET
# -----------------
# Formato LUNGO: una riga per ogni coppia soggetto × diagnosi ricevuta (1/2/3).
# Il gruppo no_diagnosi è escluso perché non ha dati su queste variabili.
#
# NOTA METODOLOGICA
# -----------------
# I test (Kruskal-Wallis / Chi-quadro / Fisher) assumono indipendenza tra le
# osservazioni. Poiché uno stesso soggetto può contribuire con più righe (una
# per diagnosi ricevuta), le osservazioni non sono strettamente indipendenti.
# Questo può leggermente aumentare i falsi positivi. I risultati vanno
# interpretati con questa limitazione in mente.
#
# TEST
# ----
#   Variabili ordinali (prima_visita, att_diagnosi, costo, n_prof):
#     Kruskal-Wallis (3+ gruppi) o Mann-Whitney (2 gruppi)
#     Post-hoc: Dunn con Holm (solo se KW con 3+ gruppi significativo)
#
#   Variabili nominali (valutazione, invio, dove):
#     Chi-quadro se celle attese >= 5 in >80% dei casi e n >= 30
#     Fisher simulato (Monte Carlo B=5000) altrimenti
#
#   Variabile binaria (sbagliate, 0/1):
#     Chi-quadro / Fisher (come nominale)
#
# CONFRONTI (2 analisi separate)
#   1. gruppo_diagnosi: aut vs altra_diagnosi
#   2. lgbtq: lgbtq vs no_lgbtq (intera popolazione con dati)
#
# CORREZIONE MULTIPLA
#   FDR (BH) applicata separatamente per ciascuna delle 2 analisi
#
# TEST AGGIUNTIVO
#   n_diagnosi: Kruskal-Wallis (3 gruppi diagnosi incluso no_diagnosi) +
#               Mann-Whitney (lgbtq), dataset wide
#
# OUTPUT
#   stats_diagnosi_risultati.csv     — p-value LRT per ogni outcome × predittore
#   stats_n_diagnosi.csv             — test su n_diagnosi (KW + MW)
#   stats_n_diagnosi_posthoc.csv     — post-hoc Dunn per n_diagnosi (se sig.)
#   stats_diagnosi_dataset_lungo.csv — dataset lungo (una riga per diagnosi)
#   stats_diagnosi_sommario.pdf      — sommario PDF (A4 portrait)
#
# MODELLI
#   Ordinali (prima_visita, att_diagnosi, costo, n_prof):
#     clmm() — ordinal; y ~ gruppo_diagnosi + lgbtq + (1|record_id)
#   Nominali (valutazione, invio, dove):
#     mblogit() — mclogit; y ~ gruppo_diagnosi + lgbtq | (1|record_id)
#   Binaria (sbagliate):
#     glmer() — lme4; y ~ gruppo_diagnosi + lgbtq + (1|record_id), binomial
#   p-value: LRT (modello pieno vs senza il termine testato)
#   FDR (BH) separata per gruppo_diagnosi e lgbtq
#
# DIPENDENZE
#   ordinal, mclogit, lme4, dunn.test, dplyr, ggplot2, gridExtra, grid
# ==============================================================================

rm(list = ls())
graphics.off()

# ── DIPENDENZE ────────────────────────────────────────────────────────────────

pkgs <- c("ordinal", "mclogit", "lme4", "dunn.test",
          "dplyr", "ggplot2", "gridExtra", "grid")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

# ── CONFIG ────────────────────────────────────────────────────────────────────

INPUT_CSV    <- "LUMEN_DATA_processed.csv"
OUT_RESULTS  <- "stats_diagnosi_risultati.csv"
OUT_ND       <- "stats_n_diagnosi.csv"
OUT_ND_PH    <- "stats_n_diagnosi_posthoc.csv"
OUT_LONG     <- "stats_diagnosi_dataset_lungo.csv"
OUT_SUMMARY  <- "stats_diagnosi_sommario.pdf"
ALPHA        <- 0.05

DIAG_SPEC <- c("aut", "adhd", "pers", "ansia", "umore", "comp_al",
               "appr", "psicosi", "ocd", "ptsd", "dipendenza", "altro")

# Tipo di modello per ogni outcome
OUTCOMES <- list(
  valutazione  = "nominale",   # mblogit
  invio        = "nominale",   # mblogit
  prima_visita = "ordinale",   # clmm
  att_diagnosi = "ordinale",   # clmm
  sbagliate    = "binario",    # glmer binomial
  costo        = "ordinale",   # clmm
  dove         = "nominale",   # mblogit
  n_prof       = "ordinale"    # clmm
)

# ── CARICA DATI ───────────────────────────────────────────────────────────────

raw <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# Gruppi dal dataset (calcolati in LUMEN_1)
raw$gruppo_diagnosi <- dplyr::case_when(
  raw$aut_ind            %in% c("Sì","Si") ~ "aut",
  raw$altra_diagnosi_ind %in% c("Sì","Si") ~ "altra_diagnosi",
  raw$no_diagnosi_ind    %in% c("Sì","Si") ~ "no_diagnosi",
  TRUE                                     ~ NA_character_
)
raw$gruppo_lgbtq <- dplyr::case_when(
  raw$lgbtq %in% c("Sì","Si") ~ "lgbtq",
  raw$lgbtq == "No"            ~ "no_lgbtq",
  TRUE                         ~ NA_character_
)

data_wide <- raw[!is.na(raw$gruppo_diagnosi), ]
data_wide$gruppo_diagnosi <- factor(data_wide$gruppo_diagnosi,
  levels = c("aut", "altra_diagnosi", "no_diagnosi"))
data_wide$gruppo_lgbtq <- factor(data_wide$gruppo_lgbtq,
  levels = c("lgbtq", "no_lgbtq"))

cat(sprintf("N soggetti classificati: %d\n", nrow(data_wide)))
cat("Gruppi diagnosi:\n"); print(table(data_wide$gruppo_diagnosi, useNA="ifany"))
cat("Gruppi LGBTQ+:\n");   print(table(data_wide$gruppo_lgbtq,   useNA="ifany"))

# ── COSTRUZIONE DATASET LUNGO ─────────────────────────────────────────────────
# Una riga per soggetto × diagnosi ricevuta (valore 1/2/3).
#
# gruppo_diagnosi nella riga riflette la DIAGNOSI SPECIFICA di quella riga:
#   "aut"            — solo per le righe dove diagnosi_spec == "aut"
#   "altra_diagnosi" — per tutte le altre diagnosi (adhd, pers, ansia, ...)
#
# Questo significa che un soggetto con aut_ind=="Sì" e anche adhd ricevuto
# contribuisce con:
#   - una riga con gruppo_diagnosi="aut"       (per la diagnosi aut)
#   - una riga con gruppo_diagnosi="altra_diagnosi" (per la diagnosi adhd)
#
# n_diagnosi è una proprietà del soggetto (costante per tutte le sue righe).

costruisci_lungo <- function(df) {
  righe <- list()
  for (diag in DIAG_SPEC) {
    col_val  <- diag
    col_inv  <- if (diag == "pers") "invio_person" else paste0("invio_", diag)
    col_pv   <- paste0("prima_visita_", diag)
    col_att  <- paste0("att_diagnosi_", diag)
    col_sbag <- paste0("sbagliate_",    diag)
    col_cost <- paste0("costo_",        diag)
    col_dove <- paste0("dove_",         diag)
    col_prof <- paste0("n_prof_",       diag)

    if (!col_val %in% names(df)) next
    mask <- !is.na(df[[col_val]]) & df[[col_val]] %in% c(1, 2, 3)
    if (sum(mask) == 0) next
    sub <- df[mask, ]

    get_col <- function(col)
      if (col %in% names(sub)) as.integer(sub[[col]]) else rep(NA_integer_, nrow(sub))

    # gruppo_diagnosi della riga = "aut" se è la riga autismo, altrimenti "altra_diagnosi"
    grp_riga <- ifelse(diag == "aut", "aut", "altra_diagnosi")

    righe[[length(righe) + 1]] <- data.frame(
      record_id       = sub$record_id,
      gruppo_diagnosi = grp_riga,          # basato sulla diagnosi di questa riga
      gruppo_soggetto = as.character(sub$gruppo_diagnosi),  # gruppo clinico del soggetto
      lgbtq           = sub$gruppo_lgbtq,
      diagnosi_spec   = diag,
      n_diagnosi      = as.integer(sub$n_diagnosi),
      valutazione     = get_col(col_val),
      invio           = get_col(col_inv),
      prima_visita    = get_col(col_pv),
      att_diagnosi    = get_col(col_att),
      sbagliate       = get_col(col_sbag),
      costo           = get_col(col_cost),
      dove            = get_col(col_dove),
      n_prof          = get_col(col_prof),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, righe)
}

long <- costruisci_lungo(data_wide)
long$gruppo_diagnosi <- factor(long$gruppo_diagnosi,
  levels = c("aut", "altra_diagnosi"))
long$lgbtq <- factor(long$lgbtq, levels = c("lgbtq", "no_lgbtq"))

cat(sprintf("\nDataset lungo: %d righe, %d soggetti unici\n",
            nrow(long), length(unique(long$record_id))))
cat("gruppo_diagnosi (per riga diagnosi):\n")
print(table(long$gruppo_diagnosi, useNA = "ifany"))
cat("lgbtq:\n")
print(table(long$lgbtq, useNA = "ifany"))
cat("diagnosi_spec:\n")
print(table(long$diagnosi_spec, useNA = "ifany"))


# ── HELPER: LRT generico (logLik difference) ─────────────────────────────────

lrt_ll <- function(mod_full, mod_rid) {
  tryCatch({
    ll_f <- as.numeric(logLik(mod_full))
    ll_r <- as.numeric(logLik(mod_rid))
    ddf  <- attr(logLik(mod_full),"df") - attr(logLik(mod_rid),"df")
    stat <- 2*(ll_f - ll_r)
    if (ddf <= 0 || stat < 0) return(NA_real_)
    pchisq(stat, df=ddf, lower.tail=FALSE)
  }, error=function(e) NA_real_)
}

lrt_clmm   <- lrt_ll
lrt_glmer  <- lrt_ll
lrt_mblogit <- lrt_ll

# ── HELPER: accorpa categorie rare (nominali) ─────────────────────────────────

MIN_CAT <- 10L
accorpa_rare <- function(x, min_n=MIN_CAT, altro="altro") {
  x   <- as.character(x)
  tab <- table(x)
  x[x %in% names(tab)[tab < min_n]] <- altro
  factor(x)
}

# ── FUNZIONE PRINCIPALE: modello misto per un outcome ─────────────────────────

run_model <- function(out_name, tipo, df) {

  df_s <- df[!is.na(df[[out_name]]) &
             !is.na(df$gruppo_diagnosi) &
             !is.na(df$lgbtq), ]

  n      <- nrow(df_s)
  n_sogg <- length(unique(df_s$record_id))

  empty <- data.frame(outcome=out_name, tipo=tipo, n_obs=n, n_soggetti=n_sogg,
                      p_gruppo=NA_real_, p_lgbtq=NA_real_, note="",
                      stringsAsFactors=FALSE)

  if (n < 15 || n_sogg < 8) { empty$note <- "n insufficiente"; return(empty) }

  p_gruppo <- NA_real_; p_lgbtq <- NA_real_; note <- ""

  if (tipo == "ordinale") {

    df_s[[out_name]] <- ordered(df_s[[out_name]])
    mf <- tryCatch(clmm(as.formula(paste0(out_name," ~ gruppo_diagnosi + lgbtq + (1|record_id)")),
                   data=df_s, nAGQ=1), error=function(e) NULL)
    if (is.null(mf)) { empty$note <- "clmm non convergito"; return(empty) }
    m_ng <- tryCatch(clmm(as.formula(paste0(out_name," ~ lgbtq + (1|record_id)")),
                     data=df_s, nAGQ=1), error=function(e) NULL)
    m_nl <- tryCatch(clmm(as.formula(paste0(out_name," ~ gruppo_diagnosi + (1|record_id)")),
                     data=df_s, nAGQ=1), error=function(e) NULL)
    if (!is.null(m_ng)) p_gruppo <- lrt_ll(mf, m_ng)
    if (!is.null(m_nl)) p_lgbtq  <- lrt_ll(mf, m_nl)

  } else if (tipo == "binario") {

    df_s[[out_name]] <- as.integer(df_s[[out_name]])
    mf <- tryCatch(glmer(as.formula(paste0(out_name," ~ gruppo_diagnosi + lgbtq + (1|record_id)")),
                   data=df_s, family=binomial, control=glmerControl(optimizer="bobyqa")),
                   error=function(e) NULL)
    if (is.null(mf)) { empty$note <- "glmer non convergito"; return(empty) }
    m_ng <- tryCatch(glmer(as.formula(paste0(out_name," ~ lgbtq + (1|record_id)")),
                     data=df_s, family=binomial, control=glmerControl(optimizer="bobyqa")),
                     error=function(e) NULL)
    m_nl <- tryCatch(glmer(as.formula(paste0(out_name," ~ gruppo_diagnosi + (1|record_id)")),
                     data=df_s, family=binomial, control=glmerControl(optimizer="bobyqa")),
                     error=function(e) NULL)
    if (!is.null(m_ng)) p_gruppo <- lrt_ll(mf, m_ng)
    if (!is.null(m_nl)) p_lgbtq  <- lrt_ll(mf, m_nl)

  } else if (tipo == "nominale") {

    df_s[[out_name]] <- accorpa_rare(df_s[[out_name]])
    if (nlevels(df_s[[out_name]]) < 2) {
      empty$note <- "un solo valore"; return(empty)
    }
    mb_ctrl <- list(maxit=200, nlminb.control=list(iter.max=500, eval.max=1000))

    fit_nom <- function(fstr, dat) {
      frm <- as.formula(fstr)
      m <- tryCatch(mblogit(frm, random=~1|record_id, control=mb_ctrl, data=dat),
                    error=function(e) NULL)
      if (!is.null(m)) return(list(mod=m, tipo="mblogit"))
      m <- tryCatch(mclogit(frm, data=dat), error=function(e) NULL)
      if (!is.null(m)) return(list(mod=m, tipo="mclogit"))
      m <- tryCatch(multinom(frm, data=dat, trace=FALSE), error=function(e) NULL)
      if (!is.null(m)) return(list(mod=m, tipo="multinom"))
      list(mod=NULL, tipo=NA)
    }

    rf  <- fit_nom(paste0(out_name," ~ gruppo_diagnosi + lgbtq"), df_s)
    mf  <- rf$mod
    if (!is.na(rf$tipo) && rf$tipo != "mblogit")
      note <- paste0(note, "[", rf$tipo, "] ")
    if (is.null(mf)) { empty$note <- "non convergito"; return(empty) }

    rng <- fit_nom(paste0(out_name," ~ lgbtq"), df_s)
    rnl <- fit_nom(paste0(out_name," ~ gruppo_diagnosi"), df_s)
    if (!is.null(rng$mod)) p_gruppo <- lrt_ll(mf, rng$mod)
    if (!is.null(rnl$mod)) p_lgbtq  <- lrt_ll(mf, rnl$mod)
  }

  data.frame(outcome=out_name, tipo=tipo, n_obs=n, n_soggetti=n_sogg,
             p_gruppo=p_gruppo, p_lgbtq=p_lgbtq, note=note,
             stringsAsFactors=FALSE)
}

# ── ESEGUI MODELLI ────────────────────────────────────────────────────────────

cat("\nEsecuzione modelli misti...\n")
risultati_list <- lapply(names(OUTCOMES), function(out) {
  cat(sprintf("  → %-15s (%s)\n", out, OUTCOMES[[out]]))
  run_model(out, OUTCOMES[[out]], long)
})
risultati <- do.call(rbind, risultati_list)
rownames(risultati) <- NULL

# FDR separata per i due predittori
applica_fdr <- function(p_vec) {
  out <- rep(NA_real_, length(p_vec))
  ok  <- !is.na(p_vec)
  if (any(ok)) out[ok] <- p.adjust(p_vec[ok], method="BH")
  out
}

risultati$p_fdr_gruppo <- applica_fdr(risultati$p_gruppo)
risultati$p_fdr_lgbtq  <- applica_fdr(risultati$p_lgbtq)
risultati$sig_gruppo   <- ifelse(is.na(risultati$p_fdr_gruppo), NA,
                                  risultati$p_fdr_gruppo < ALPHA)
risultati$sig_lgbtq    <- ifelse(is.na(risultati$p_fdr_lgbtq), NA,
                                  risultati$p_fdr_lgbtq < ALPHA)

print(risultati[, c("outcome","tipo","n_obs","p_gruppo","p_fdr_gruppo",
                    "sig_gruppo","p_lgbtq","p_fdr_lgbtq","sig_lgbtq","note")])


# ── FIX: pacchetti aggiuntivi (nnet) ─────────────────────────────────────────
if (!requireNamespace("nnet", quietly=TRUE))
  install.packages("nnet", repos="https://cloud.r-project.org")
library(nnet)


# ── ESEGUI MODELLI ────────────────────────────────────────────────────────────

cat("\nEsecuzione modelli misti...\n")
risultati_list <- lapply(names(OUTCOMES), function(out) {
  cat(sprintf("  → %-15s (%s)\n", out, OUTCOMES[[out]]))
  res <- run_model(out, OUTCOMES[[out]], long)
  cat(sprintf("     p_gruppo=%-10s  p_lgbtq=%-10s  note=%s\n",
              ifelse(is.na(res$p_gruppo),"NA",formatC(res$p_gruppo,digits=4,format="f")),
              ifelse(is.na(res$p_lgbtq), "NA",formatC(res$p_lgbtq, digits=4,format="f")),
              res$note))
  res
})
risultati <- do.call(rbind, risultati_list)
rownames(risultati) <- NULL

applica_fdr <- function(p_vec) {
  out <- rep(NA_real_, length(p_vec))
  ok  <- !is.na(p_vec)
  if (any(ok)) out[ok] <- p.adjust(p_vec[ok], method="BH")
  out
}
risultati$p_fdr_gruppo <- applica_fdr(risultati$p_gruppo)
risultati$p_fdr_lgbtq  <- applica_fdr(risultati$p_lgbtq)
risultati$sig_gruppo   <- ifelse(is.na(risultati$p_fdr_gruppo), NA, risultati$p_fdr_gruppo < ALPHA)
risultati$sig_lgbtq    <- ifelse(is.na(risultati$p_fdr_lgbtq),  NA, risultati$p_fdr_lgbtq  < ALPHA)

print(risultati[, c("outcome","tipo","n_obs","p_gruppo","p_fdr_gruppo",
                    "sig_gruppo","p_lgbtq","p_fdr_lgbtq","sig_lgbtq","note")])

# ── TEST N_DIAGNOSI (dataset wide) ────────────────────────────────────────────
# Due t-test di Welch sulla media di n_diagnosi:
#   1. aut vs altra_diagnosi
#   2. lgbtq vs no_lgbtq

cat("\n── Test n_diagnosi ──\n")

nd    <- data_wide[!is.na(data_wide$n_diagnosi), ]
nd_lg <- nd[!is.na(nd$gruppo_lgbtq), ]
nd_2  <- nd[nd$gruppo_diagnosi %in% c("aut","altra_diagnosi"), ]
nd_2$gruppo_diagnosi <- factor(nd_2$gruppo_diagnosi, levels=c("aut","altra_diagnosi"))

tt_diag <- tryCatch(t.test(n_diagnosi ~ gruppo_diagnosi, data=nd_2, var.equal=FALSE),
                    error=function(e) NULL)
tt_lgbt <- tryCatch(t.test(n_diagnosi ~ gruppo_lgbtq,    data=nd_lg, var.equal=FALSE),
                    error=function(e) NULL)

if (!is.null(tt_diag))
  cat(sprintf("  Welch t-test aut vs altra_diagnosi : t=%.3f, df=%.1f, p=%.4f\n",
              tt_diag$statistic, tt_diag$parameter, tt_diag$p.value))
if (!is.null(tt_lgbt))
  cat(sprintf("  Welch t-test lgbtq vs no_lgbtq     : t=%.3f, df=%.1f, p=%.4f\n",
              tt_lgbt$statistic, tt_lgbt$parameter, tt_lgbt$p.value))

mean_diag <- tapply(nd_2$n_diagnosi,  nd_2$gruppo_diagnosi, mean, na.rm=TRUE)
mean_lgbt <- tapply(nd_lg$n_diagnosi, nd_lg$gruppo_lgbtq,   mean, na.rm=TRUE)
sd_diag   <- tapply(nd_2$n_diagnosi,  nd_2$gruppo_diagnosi, sd,   na.rm=TRUE)
sd_lgbt   <- tapply(nd_lg$n_diagnosi, nd_lg$gruppo_lgbtq,   sd,   na.rm=TRUE)

nd_risultati <- data.frame(
  test      = c("Welch t-test", "Welch t-test"),
  confronto = c("aut vs altra_diagnosi", "lgbtq vs no_lgbtq"),
  n         = c(nrow(nd_2), nrow(nd_lg)),
  t         = c(ifelse(is.null(tt_diag), NA, round(tt_diag$statistic, 3)),
                ifelse(is.null(tt_lgbt), NA, round(tt_lgbt$statistic,  3))),
  df        = c(ifelse(is.null(tt_diag), NA, round(tt_diag$parameter, 1)),
                ifelse(is.null(tt_lgbt), NA, round(tt_lgbt$parameter,  1))),
  p_value   = c(ifelse(is.null(tt_diag), NA, round(tt_diag$p.value, 4)),
                ifelse(is.null(tt_lgbt), NA, round(tt_lgbt$p.value,  4))),
  sig       = c(ifelse(is.null(tt_diag), NA, tt_diag$p.value < ALPHA),
                ifelse(is.null(tt_lgbt), NA, tt_lgbt$p.value  < ALPHA)),
  stringsAsFactors=FALSE)

# ── SALVA CSV ─────────────────────────────────────────────────────────────────

script_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))
  if (length(ofiles) > 0) dirname(normalizePath(ofiles[[length(ofiles)]]))
  else {
    args <- commandArgs(trailingOnly=FALSE)
    fa   <- args[grepl("^--file=", args)]
    if (length(fa)>0) dirname(normalizePath(sub("^--file=","",fa))) else getwd()
  }
}, error=function(e) getwd())

write.csv(risultati,    file.path(script_dir, OUT_RESULTS), row.names=FALSE)
write.csv(nd_risultati, file.path(script_dir, OUT_ND),      row.names=FALSE)
write.csv(long,         file.path(script_dir, OUT_LONG),    row.names=FALSE)
message("Risultati salvati.")

# ==============================================================================
# SOMMARIO PDF
# ==============================================================================

COL_AUT   <- "#2255AA"
COL_ALTRA <- "#7BAAE0"
COL_LGBT  <- "#7B2D8B"
COL_NOLG  <- "#C09DD4"
COL_HDR   <- "#2E4057"
COL_ND    <- "#2E7D32"

fmt_p   <- function(x) ifelse(is.na(x), "\u2014", formatC(as.numeric(x), format="f", digits=4))
fmt_sig <- function(x) ifelse(is.na(x), "", ifelse(x, "*", ""))

# Descrizione del test usato per ogni outcome (per la tabella di copertina)
TEST_DESC <- list(
  valutazione  = "mblogit (nominale) / fallback multinom",
  invio        = "mblogit (nominale) / fallback multinom",
  prima_visita = "clmm (ordinale)",
  att_diagnosi = "clmm (ordinale)",
  sbagliate    = "glmer binomiale",
  costo        = "clmm (ordinale)",
  dove         = "mblogit (nominale) / fallback multinom",
  n_prof       = "clmm (ordinale)"
)

VAR_LABELS <- list(
  valutazione  = c("1"="Pubblico","2"="Privato","3"="Entrambi"),
  invio        = c("1"="Autoinvio","2"="Genitore","3"="Amici/parenti",
                   "4"="Ist. scolastiche","5"="Medico curante",
                   "6"="Psicologo","7"="Psichiatra","8"="Altro"),
  prima_visita = c("1"="<3 mesi","2"="3-6 mesi","3"="6m-1a",
                   "4"="1-2 anni","5"="2-3 anni","6"=">3 anni"),
  att_diagnosi = c("1"="<3 mesi","2"="3-6 mesi","3"="6m-1a",
                   "4"="1-2 anni","5"="2-3 anni","6"=">3 anni"),
  sbagliate    = c("0"="No","1"="Si"),
  costo        = c("1"="Gratuita","2"="<150\u20ac","3"="150-300\u20ac",
                   "4"="301-500\u20ac","5"=">500\u20ac"),
  dove         = c("1"="Online","2"="Citta","3"="Provincia",
                   "4"="Regione","5"="Fuori regione"),
  n_prof       = c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7+")
)

VAR_TITOLI <- c(
  valutazione  = "VALUTAZIONE - Setting della diagnosi",
  invio        = "INVIO - Chi ha inviato alla prima visita",
  prima_visita = "PRIMA VISITA - Tempo di attesa",
  att_diagnosi = "ATTESA DIAGNOSI - Dalla prima visita alla diagnosi",
  sbagliate    = "DIAGNOSI SBAGLIATE - Ricevute prima della definitiva",
  costo        = "COSTO - Spesa per la diagnosi",
  dove         = "DOVE - Luogo geografico della diagnosi",
  n_prof       = "N. PROFESSIONISTI - Consultati per la diagnosi"
)

# ── tema ggplot comune ───────────────────────────────────────────────────────

theme_lumen <- function(base=7) {
  theme_minimal(base_size=base) +
  theme(
    plot.title        = element_text(size=base, face="bold", colour=COL_HDR,
                                     margin=margin(b=2)),
    axis.text.x       = element_text(size=base-1, angle=25, hjust=1,
                                     lineheight=0.8),
    axis.text.y       = element_text(size=base-1),
    axis.title        = element_text(size=base-1),
    panel.grid.major.x= element_blank(),
    panel.grid.minor  = element_blank(),
    panel.grid.major.y= element_line(colour="#DDDDDD", linewidth=0.3),
    legend.position   = "none",
    plot.margin       = margin(3,4,3,4)
  )
}

# ── helper: istogramma per un singolo gruppo ─────────────────────────────────
# Asse x = valore della variabile, asse y = N assoluto

make_hist_single <- function(df_sub, out_col, val_labels, titolo, fill_col) {
  d <- df_sub[!is.na(df_sub[[out_col]]), ]
  if (nrow(d) == 0)
    return(ggplot() + theme_void() + ggtitle(titolo) +
           theme(plot.title=element_text(size=7,face="bold",colour=COL_HDR)))

  d$val_lbl <- factor(
    ifelse(as.character(d[[out_col]]) %in% names(val_labels),
           val_labels[as.character(d[[out_col]])],
           as.character(d[[out_col]])),
    levels=val_labels)

  cnt <- as.data.frame(table(val=d$val_lbl), stringsAsFactors=FALSE)
  cnt$val <- factor(cnt$val, levels=val_labels)

  ggplot(cnt, aes(x=val, y=Freq)) +
    geom_bar(stat="identity", fill=fill_col, colour="white", linewidth=0.2,
             width=0.7) +
    geom_text(aes(label=Freq), vjust=-0.3, size=2.0, colour="#333333") +
    scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
    labs(title=titolo, x=NULL, y="N") +
    theme_lumen()
}

# ── helper: tabella grob con tema uniforme ───────────────────────────────────
# sig_rows: vettore di indici di riga (1-based) da evidenziare in rosso chiaro

COL_SIG_ROW <- "#FDDEDE"   # rosso chiaro per righe significative

make_tbl_grob <- function(df_tbl, hfill=COL_HDR, base_size=6.5, sig_rows=integer(0)) {
  n_rows <- nrow(df_tbl)
  row_fills <- c(rep(c("white","#F0F4FA"), ceiling(n_rows/2)))[seq_len(n_rows)]
  if (length(sig_rows) > 0)
    row_fills[sig_rows] <- COL_SIG_ROW
  tt <- ttheme_minimal(base_size=base_size,
    core    = list(fg_params=list(hjust=0, x=0.03, cex=0.82),
                   bg_params=list(fill=row_fills, col="#CCCCCC", lwd=0.3)),
    colhead = list(fg_params=list(col="white", fontface="bold",
                                  hjust=0, x=0.03, cex=0.78),
                   bg_params=list(fill=hfill, col="#CCCCCC", lwd=0.3)))
  tableGrob(df_tbl, rows=NULL, theme=tt)
}

# ── helper: tabella conteggi N (%) per valore, per un gruppo ─────────────────

make_counts_single <- function(df_sub, out_col, val_labels) {
  d <- df_sub[!is.na(df_sub[[out_col]]), ]
  d$val_lbl <- factor(
    ifelse(as.character(d[[out_col]]) %in% names(val_labels),
           val_labels[as.character(d[[out_col]])],
           as.character(d[[out_col]])),
    levels=val_labels)
  tot <- nrow(d)
  cnt <- as.data.frame(table(Valore=d$val_lbl), stringsAsFactors=FALSE)
  cnt$`N (%)`  <- sprintf("%d (%.1f%%)", cnt$Freq, cnt$Freq/tot*100)
  cnt$Freq     <- NULL
  cnt
}

# ── helper: tabella test LRT per un outcome ──────────────────────────────────
# Restituisce lista(tbl, sig_rows) per evidenziare righe significative

make_test_tbl <- function(out_name) {
  r <- risultati[risultati$outcome == out_name, ]
  if (nrow(r) == 0) return(NULL)
  tbl <- data.frame(
    Predittore = c("gruppo_diagnosi","lgbtq"),
    Modello    = r$tipo,
    N_obs      = r$n_obs,
    p_LRT      = c(fmt_p(r$p_gruppo),     fmt_p(r$p_lgbtq)),
    p_FDR      = c(fmt_p(r$p_fdr_gruppo), fmt_p(r$p_fdr_lgbtq)),
    `sig*`     = c(fmt_sig(r$sig_gruppo), fmt_sig(r$sig_lgbtq)),
    Note       = c(if(nchar(r$note)>0) r$note else "\u2014", "\u2014"),
    stringsAsFactors=FALSE, check.names=FALSE)
  sig_rows <- which(c(isTRUE(r$sig_gruppo), isTRUE(r$sig_lgbtq)))
  list(tbl=tbl, sig_rows=sig_rows)
}

# ── helper: titolo barra colorata ────────────────────────────────────────────

title_bar <- function(testo, col=COL_HDR, fontsize=9) {
  grobTree(
    rectGrob(gp=gpar(fill=col, col=NA)),
    textGrob(testo, x=0.015, hjust=0,
             gp=gpar(fontsize=fontsize, fontface="bold", col="white")))
}

# ── FUNZIONE PAGINA PER VARIABILE ─────────────────────────────────────────────
# Layout A4:
#   Row 1 (5%)  : barra titolo variabile
#   Row 2 (42%) : 4 istogrammi (aut | altra | lgbtq | no_lgbtq)
#   Row 3 (28%) : tabella conteggi (2 colonne: diagnosi | lgbtq)
#   Row 4 (25%) : tabella test LRT

draw_var_page <- function(out_name, val_labels, titolo) {
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=4, ncol=1,
    heights=unit(c(0.05,0.42,0.28,0.25),"npc"))))

  # ── Row 1: titolo ────────────────────────────────────────────────────────
  pushViewport(viewport(layout.pos.row=1))
  grid.draw(title_bar(titolo))
  popViewport()

  # ── Row 2: 4 istogrammi affiancati ───────────────────────────────────────
  pushViewport(viewport(layout.pos.row=2,
    x=0.5,y=0.5,width=0.98,height=0.96,just=c("centre","centre")))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4)))

  df_aut   <- long[long$gruppo_diagnosi=="aut",   ]
  df_altra <- long[long$gruppo_diagnosi=="altra_diagnosi", ]
  df_lgbt  <- long[!is.na(long$lgbtq) & long$lgbtq=="lgbtq",    ]
  df_nolg  <- long[!is.na(long$lgbtq) & long$lgbtq=="no_lgbtq", ]

  plots <- list(
    make_hist_single(df_aut,   out_name, val_labels, "Autismo",       COL_AUT),
    make_hist_single(df_altra, out_name, val_labels, "Altra diagnosi",COL_ALTRA),
    make_hist_single(df_lgbt,  out_name, val_labels, "LGBTQ+",        COL_LGBT),
    make_hist_single(df_nolg,  out_name, val_labels, "Non LGBTQ+",    COL_NOLG)
  )
  for (i in 1:4) {
    pushViewport(viewport(layout.pos.col=i))
    grid.draw(ggplotGrob(plots[[i]]))
    popViewport()
  }
  popViewport(); popViewport()

  # ── Row 3: tabelle conteggi affiancate (diagnosi | lgbtq) ────────────────
  pushViewport(viewport(layout.pos.row=3,
    x=0.5,y=0.5,width=0.98,height=0.96,just=c("centre","centre")))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

  # colonna sinistra: aut e altra affiancate in un unico df wide
  cnt_aut   <- make_counts_single(df_aut,   out_name, val_labels)
  cnt_altra <- make_counts_single(df_altra, out_name, val_labels)
  cnt_wide_d <- data.frame(
    Valore        = cnt_aut$Valore,
    `Autismo`     = cnt_aut$`N (%)`,
    `Altra diag.` = cnt_altra$`N (%)`,
    stringsAsFactors=FALSE, check.names=FALSE)

  cnt_lgbt  <- make_counts_single(df_lgbt, out_name, val_labels)
  cnt_nolg  <- make_counts_single(df_nolg, out_name, val_labels)
  cnt_wide_l <- data.frame(
    Valore       = cnt_lgbt$Valore,
    `LGBTQ+`     = cnt_lgbt$`N (%)`,
    `Non LGBTQ+` = cnt_nolg$`N (%)`,
    stringsAsFactors=FALSE, check.names=FALSE)

  pushViewport(viewport(layout.pos.col=1,
    x=0.5,y=0.5,width=0.95,height=0.92,just=c("centre","centre")))
  grid.draw(arrangeGrob(
    textGrob("Conteggi N (%) — Autismo vs Altra diagnosi",
             x=0, hjust=0, gp=gpar(fontsize=7,fontface="bold",col=COL_AUT)),
    make_tbl_grob(cnt_wide_d, hfill=COL_AUT),
    ncol=1, heights=unit(c(0.12,0.88),"npc")))
  popViewport()

  pushViewport(viewport(layout.pos.col=2,
    x=0.5,y=0.5,width=0.95,height=0.92,just=c("centre","centre")))
  grid.draw(arrangeGrob(
    textGrob("Conteggi N (%) — LGBTQ+ vs Non LGBTQ+",
             x=0, hjust=0, gp=gpar(fontsize=7,fontface="bold",col=COL_LGBT)),
    make_tbl_grob(cnt_wide_l, hfill=COL_LGBT),
    ncol=1, heights=unit(c(0.12,0.88),"npc")))
  popViewport()

  popViewport(); popViewport()

  # ── Row 4: tabella test LRT ───────────────────────────────────────────────
  pushViewport(viewport(layout.pos.row=4,
    x=0.5,y=0.5,width=0.98,height=0.90,just=c("centre","centre")))
  tbl_t <- make_test_tbl(out_name)
  lbl_mod <- paste0(
    "LRT modello misto (* = p_FDR < 0.05)  |  ",
    "clmm (ordinali) | glmer binomiale (sbagliate) | mblogit/multinom (nominali)  |  ",
    "FDR BH separata per predittore")
  grid.draw(arrangeGrob(
    textGrob(lbl_mod, x=0, hjust=0,
             gp=gpar(fontsize=6,col=COL_HDR)),
    if (!is.null(tbl_t))
      make_tbl_grob(tbl_t$tbl, sig_rows=tbl_t$sig_rows)
    else
      textGrob("(nessun risultato)", gp=gpar(fontsize=8,col="grey40")),
    ncol=1, heights=unit(c(0.13,0.87),"npc")))
  popViewport()
  popViewport()
}

# ── PAGINA N_DIAGNOSI ─────────────────────────────────────────────────────────
# Layout:
# Layout n_diagnosi:
#   Row 1 (5%)  : titolo
#   Row 2 (50%) : 4 istogrammi (aut | altra | lgbtq | no_lgbtq)
#   Row 3 (45%) : tabella 2 test Chi2

draw_nd_page <- function() {
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=3, ncol=1,
    heights=unit(c(0.05,0.50,0.45),"npc"))))

  # ── Row 1: titolo ────────────────────────────────────────────────────────
  pushViewport(viewport(layout.pos.row=1))
  grid.draw(title_bar("N_DIAGNOSI - Numero di diagnosi ricevute", col=COL_ND))
  popViewport()

  # ── Row 2: 4 istogrammi ──────────────────────────────────────────────────
  pushViewport(viewport(layout.pos.row=2,
    x=0.5,y=0.5,width=0.98,height=0.96,just=c("centre","centre")))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4)))

  nd_vals <- sort(unique(c(nd_2$n_diagnosi, nd_lg$n_diagnosi)))

  df_nd_aut   <- nd_2[nd_2$gruppo_diagnosi=="aut",            ]
  df_nd_altra <- nd_2[nd_2$gruppo_diagnosi=="altra_diagnosi", ]
  df_nd_lgbt  <- nd_lg[nd_lg$gruppo_lgbtq=="lgbtq",           ]
  df_nd_nolg  <- nd_lg[nd_lg$gruppo_lgbtq=="no_lgbtq",        ]

  make_nd_hist <- function(d, titolo, col) {
    if (nrow(d)==0) return(ggplot()+theme_void()+ggtitle(titolo))
    cnt <- as.data.frame(table(n=factor(d$n_diagnosi, levels=nd_vals)),
                         stringsAsFactors=FALSE)
    ggplot(cnt, aes(x=n, y=Freq)) +
      geom_bar(stat="identity", fill=col, colour="white",
               linewidth=0.2, width=0.7) +
      geom_text(aes(label=ifelse(Freq>0,Freq,"")), vjust=-0.3,
                size=2.0, colour="#333333") +
      scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
      labs(title=titolo, x="N diagnosi", y="N soggetti") +
      theme_lumen()
  }

  nd_plots <- list(
    make_nd_hist(df_nd_aut,   "Autismo",        COL_AUT),
    make_nd_hist(df_nd_altra, "Altra diagnosi", COL_ALTRA),
    make_nd_hist(df_nd_lgbt,  "LGBTQ+",         COL_LGBT),
    make_nd_hist(df_nd_nolg,  "Non LGBTQ+",     COL_NOLG)
  )
  for (i in 1:4) {
    pushViewport(viewport(layout.pos.col=i))
    grid.draw(ggplotGrob(nd_plots[[i]]))
    popViewport()
  }
  popViewport(); popViewport()

  # ── Row 3: tabella 2 test Chi2 ───────────────────────────────────────────
  pushViewport(viewport(layout.pos.row=3,
    x=0.5,y=0.5,width=0.70,height=0.88,just=c("centre","centre")))

  nd_tbl <- data.frame(
    Test      = nd_risultati$test,
    Confronto = nd_risultati$confronto,
    N         = nd_risultati$n,
    t         = as.character(nd_risultati$t),
    df        = ifelse(is.na(nd_risultati$df), "\u2014",
                       as.character(nd_risultati$df)),
    p         = fmt_p(nd_risultati$p_value),
    `sig*`    = fmt_sig(nd_risultati$sig),
    stringsAsFactors=FALSE, check.names=FALSE)

  med_txt <- sprintf(
    "Medie (SD)  aut=%.2f (%.2f) | altra=%.2f (%.2f) | lgbtq+=%.2f (%.2f) | no_lgbtq=%.2f (%.2f)",
    mean_diag["aut"],          sd_diag["aut"],
    mean_diag["altra_diagnosi"], sd_diag["altra_diagnosi"],
    mean_lgbt["lgbtq"],        sd_lgbt["lgbtq"],
    mean_lgbt["no_lgbtq"],     sd_lgbt["no_lgbtq"])

  grid.draw(arrangeGrob(
    textGrob("Welch t-test sulla media di n_diagnosi  (* = p < 0.05)",
             x=0, hjust=0, gp=gpar(fontsize=8, fontface="bold", col=COL_ND)),
    textGrob(med_txt, x=0, hjust=0, gp=gpar(fontsize=6.5, col="#444444")),
    make_tbl_grob(nd_tbl, hfill=COL_ND,
                  sig_rows=which(nd_risultati$sig == TRUE)),
    ncol=1, heights=unit(c(0.09,0.08,0.83),"npc")))
  popViewport()
  popViewport()
}

# ── APRI PDF ──────────────────────────────────────────────────────────────────

pdf(file.path(script_dir, OUT_SUMMARY), width=8.27, height=11.69, paper="a4")

# ==============================================================================
# PAGINA 1: COPERTINA
# Layout:
#   Row 1 (12%) : banner titolo
#   Row 2 (52%) : tabella dataset lungo (aut | altra | lgbtq | no_lgbtq)
#   Row 3 (36%) : tabella riepilogo test per variabile
# ==============================================================================

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=3, ncol=1,
  heights=unit(c(0.12,0.50,0.38),"npc"))))

# ── banner ───────────────────────────────────────────────────────────────────
pushViewport(viewport(layout.pos.row=1))
grid.draw(grobTree(
  rectGrob(gp=gpar(fill=COL_HDR,col=NA)),
  textGrob("LUMEN \u2014 Analisi statistica\nvariabili percorso diagnostico",
           x=0.5,y=0.60,hjust=0.5,
           gp=gpar(fontsize=14,fontface="bold",col="white",lineheight=1.4)),
  textGrob(format(Sys.time(),"%d-%m-%Y %H:%M"),
           x=0.5,y=0.18,hjust=0.5,
           gp=gpar(fontsize=8,col="#CCDDFF"))))
popViewport()

# ── tabella dataset lungo ────────────────────────────────────────────────────
pushViewport(viewport(layout.pos.row=2,
  x=0.5,y=0.5,width=0.82,height=0.92,just=c("centre","centre")))

long_aut   <- long[long$gruppo_diagnosi=="aut",            ]
long_altra <- long[long$gruppo_diagnosi=="altra_diagnosi", ]
long_lgbt  <- long[!is.na(long$lgbtq) & long$lgbtq=="lgbtq",    ]
long_nolg  <- long[!is.na(long$lgbtq) & long$lgbtq=="no_lgbtq", ]

build_count_row <- function(label, d, out_col, val_labels) {
  tot <- sum(!is.na(d[[out_col]]))
  if (tot == 0) return(data.frame(Variabile=label, Autismo="—",
    `Altra diag.`="—", `LGBTQ+`="—", `Non LGBTQ+`="—",
    stringsAsFactors=FALSE, check.names=FALSE))
  f <- function(df) {
    n <- sum(!is.na(df[[out_col]]))
    sprintf("%d", n)
  }
  data.frame(
    Variabile    = label,
    Autismo      = f(long_aut),
    `Altra diag.`= f(long_altra),
    `LGBTQ+`     = f(long_lgbt),
    `Non LGBTQ+` = f(long_nolg),
    stringsAsFactors=FALSE, check.names=FALSE)
}

# riga header con N totali (solo N righe totali)
cover_hdr <- data.frame(
  Variabile    = "N righe totali",
  Autismo      = as.character(nrow(long_aut)),
  `Altra diag.`= as.character(nrow(long_altra)),
  `LGBTQ+`     = as.character(nrow(long_lgbt)),
  `Non LGBTQ+` = as.character(nrow(long_nolg)),
  stringsAsFactors=FALSE, check.names=FALSE)

# righe per ogni variabile outcome: N osservazioni disponibili
cover_vars <- do.call(rbind, lapply(names(VAR_LABELS), function(v)
  build_count_row(VAR_TITOLI[v], long, v, VAR_LABELS[[v]])))
cover_vars[,2:5] <- lapply(cover_vars[,2:5], as.character)

cover_df <- rbind(cover_hdr, cover_vars)
cover_df$Variabile <- gsub(" - .*$","", cover_df$Variabile)  # accorcia titoli

grid.draw(arrangeGrob(
  textGrob("Composizione dataset lungo (una riga per diagnosi ricevuta)",
           x=0, hjust=0,
           gp=gpar(fontsize=8,fontface="bold",col=COL_HDR)),
  make_tbl_grob(cover_df, hfill=COL_HDR, base_size=7),
  ncol=1, heights=unit(c(0.06,0.94),"npc")))
popViewport()

# ── tabella test per variabile ────────────────────────────────────────────────
pushViewport(viewport(layout.pos.row=3,
  x=0.5,y=0.5,width=0.96,height=0.92,just=c("centre","centre")))

test_cover <- do.call(rbind, lapply(names(OUTCOMES), function(v) {
  r <- risultati[risultati$outcome == v, ]
  data.frame(
    Variabile  = v,
    Tipo       = r$tipo,
    Test       = TEST_DESC[[v]],
    N_obs      = r$n_obs,
    p_gruppo   = fmt_p(r$p_gruppo),
    pFDR_grp   = fmt_p(r$p_fdr_gruppo),
    sig_grp    = fmt_sig(r$sig_gruppo),
    p_lgbtq    = fmt_p(r$p_lgbtq),
    pFDR_lgbt  = fmt_p(r$p_fdr_lgbtq),
    sig_lgbt   = fmt_sig(r$sig_lgbtq),
    stringsAsFactors=FALSE)
}))
names(test_cover) <- c("Variabile","Tipo","Test usato","N",
                        "p gruppo","pFDR grp","*",
                        "p lgbtq","pFDR lgbt","*  ")

# righe significative: almeno uno dei due predittori sig
sig_cover <- which(apply(risultati[, c("sig_gruppo","sig_lgbtq")], 1,
                         function(x) any(isTRUE(x))))

grid.draw(arrangeGrob(
  textGrob(
    "Riepilogo test  (* = pFDR < 0.05  |  LRT: modello pieno vs senza predittore  |  FDR BH separata)",
    x=0, hjust=0, gp=gpar(fontsize=6.5,col=COL_HDR)),
  make_tbl_grob(test_cover, hfill=COL_HDR, base_size=6, sig_rows=sig_cover),
  ncol=1, heights=unit(c(0.07,0.93),"npc")))
popViewport()
popViewport()

# ==============================================================================
# PAGINE 2–9: una per ogni variabile
# ==============================================================================

for (out_name in names(OUTCOMES)) {
  draw_var_page(out_name, VAR_LABELS[[out_name]], VAR_TITOLI[[out_name]])
}

# ==============================================================================
# PAGINA 10: N_DIAGNOSI
# ==============================================================================

draw_nd_page()

dev.off()
message("Sommario PDF salvato in: ", OUT_SUMMARY)
cat("\nCompletato.\n")
