# ==============================================================================
# compute_subgroups.R
#
# Struttura a TRE LIVELLI: variabile categorica x diagnosi x lgbt
#
# Gruppi diagnosi (3): aut | altra_diagnosi | no_diagnosi
# Gruppi lgbt    (2): lgbtq | no_lgbtq
# → 6 combinazioni per ogni valore di ogni variabile
#
# Gruppi marginali aggiunti:
#   diagnosi: aut_tot | altra_diagnosi_tot | no_diagnosi_tot
#             (indipendentemente dal gruppo lgbt)
#   lgbt:     lgbtq_tot | no_lgbtq_tot
#             (indipendentemente dal gruppo diagnosi)
#
# CSV output colonne:
#   variabile, valore, label, diagnosi, lgbt, count,
#   totale_gruppo, totale_var, totale_soggetti, pct
#
# pct = count / totale_var * 100  (% sul gruppo che ha risposto; somma = 100%)
#
# Funzioni esportate:
#   compute_subgroups(data, cat_vars)
#   summary_subgroups(subgroups, data)
# ==============================================================================


# ------------------------------------------------------------------------------
# Helpers: controllo NA e valore valido
# ------------------------------------------------------------------------------

# Restituisce TRUE solo se x non è NA e soddisfa la condizione
is_valid <- function(x) !is.na(x)


# ------------------------------------------------------------------------------
# Filtri diagnosi  (data -> vettore logico)
# Ogni filtro:
#   1. controlla che la variabile rilevante non sia NA
#   2. verifica il valore atteso
# ------------------------------------------------------------------------------

DIAGNOSI_FILTERS <- list(

  # Autismo: ha diagnosi di autismo (pubblico / privato / entrambi)
  # aut %in% {1, 2, 3}  →  aut != 4 e non NA
  aut = function(data) {
    is_valid(data$aut) & data$aut != 4
  },

  # Altra diagnosi: NO autismo (aut == 4 o NA),
  # ma almeno una diagnosi tra le altre colonne diagnostiche != 4 e non NA
  altra_diagnosi = function(data) {
    diag_cols <- c("pers", "ansia", "umore", "comp_al", "appr",
                   "psicosi", "ocd", "ptsd", "dipendenza", "altro")

    # TRUE se almeno una colonna diagnostica (escluso aut) ha valore valido != 4
    ha_altra <- Reduce(`|`, lapply(diag_cols, function(col) {
      is_valid(data[[col]]) & data[[col]] != 4
    }))

    # Esclude chi ha autismo (aut != 4) o chi non ha aut valorizzato
    no_aut <- is_valid(data$aut) & data$aut == 4

    ha_altra & no_aut
  },

  # Nessuna diagnosi: diagnosi_si_no == 2 e non NA
  no_diagnosi = function(data) {
    is_valid(data$diagnosi_si_no) & data$diagnosi_si_no == 2
  }
)


# ------------------------------------------------------------------------------
# Filtri lgbt  (data -> vettore logico)
# Ogni filtro:
#   1. controlla che TUTTE le variabili rilevanti siano non NA
#   2. applica la logica di inclusione/esclusione
# ------------------------------------------------------------------------------

LGBT_FILTERS <- list(

  # lgbtq: almeno una delle seguenti condizioni (OR)
  #   - trans_cis == 1  (persona trans)
  #   - identita_genere non in {1, 2}  (non uomo/donna binari)
  #   - orientamento_sex != 1  (non eterosessuale)
  # Ogni condizione è valutata solo se la variabile non è NA
  lgbtq = function(data) {
    is_trans      <- is_valid(data$trans_cis)       & data$trans_cis == 1
    is_id_diversa <- is_valid(data$identita_genere)  & !(data$identita_genere %in% c(1, 2))
    is_or_diverso <- is_valid(data$orientamento_sex) & data$orientamento_sex != 1

    is_trans | is_id_diversa | is_or_diverso
  },

  # no_lgbtq: TUTTE le seguenti condizioni (AND)
  #   - cis: trans_cis == 2 e non NA
  #   - identità binaria: identita_genere in {1, 2} e non NA
  #   - eterosessuale: orientamento_sex == 1 e non NA
  # Se una qualsiasi delle tre variabili è NA → FALSE (non classificabile)
  no_lgbtq = function(data) {
    is_cis        <- is_valid(data$trans_cis)       & data$trans_cis == 2
    is_id_binaria <- is_valid(data$identita_genere)  & data$identita_genere %in% c(1, 2)
    is_etero      <- is_valid(data$orientamento_sex) & data$orientamento_sex == 1

    is_cis & is_id_binaria & is_etero
  }
)


# ------------------------------------------------------------------------------
# Variabili categoriche
# ------------------------------------------------------------------------------

ALL_CAT_VARS <- c(
  "consenso", "eta", "sesso_nascita", "trans_cis", "identita_genere",
  "orientamento_sex", "regione", "centro_abitato",
  "titolo_studio", "titolo_studio_genitore1", "titolo_studio_genitore2",
  "figli", "quanti_figli", "reddito",
  "servizi_salute_mentale", "diagnosi_si_no",
  "aut", "adhd", "pers", "ansia", "umore", "comp_al", "appr",
  "psicosi", "ocd", "ptsd", "dipendenza", "altro",
  "livello_aut",
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
  "durata_percorsi", "sei_in_carico", "interruzione_percorso",
  "questionario_lumen_complete"
)


# ------------------------------------------------------------------------------
# build_group_entries()
#
# Funzione interna: dato un filtro base (vettore logico) e un sottoinsieme
# di variabili categoriche, costruisce la lista annidata
# result[[var_name]][[valore]] = list(var, value, index, count)
#
# Solo i soggetti con risposta non nulla (is_valid) vengono conteggiati.
# ------------------------------------------------------------------------------

build_group_entries <- function(data, base_filter, cat_vars) {
  result <- list()

  for (var_name in cat_vars) {
    col <- data[[var_name]]

    # Soggetti nel gruppo CON risposta valida (non NA) per questa variabile
    valid_mask <- base_filter & is_valid(col)
    valori     <- sort(unique(col[valid_mask]))

    result[[var_name]] <- list()

    for (val in valori) {
      index <- valid_mask & col == val
      key   <- as.character(val)
      result[[var_name]][[key]] <- list(
        var   = var_name,
        value = val,
        index = index,
        count = sum(index)
      )
    }
  }

  result
}


# ------------------------------------------------------------------------------
# compute_subgroups()
#
# Restituisce lista annidata: result[[diagnosi]][[lgbt]][[variabile]][[valore]]
# Ogni foglia: list(var, value, index, count)
#
# Struttura:
#   6 combinazioni diagnosi x lgbt
#   + 3 marginali diagnosi (lgbt = "tot")
#   + 2 marginali lgbt     (diagnosi = "tot")
# ------------------------------------------------------------------------------

compute_subgroups <- function(data, cat_vars = NULL) {

  if (is.null(cat_vars)) cat_vars <- intersect(ALL_CAT_VARS, names(data))

  result <- list()

  # ------------------------------------------------------------------
  # 1. Combinazioni diagnosi x lgbt
  # ------------------------------------------------------------------
  for (d_nome in names(DIAGNOSI_FILTERS)) {
    d_filter <- DIAGNOSI_FILTERS[[d_nome]](data)
    result[[d_nome]] <- list()

    for (l_nome in names(LGBT_FILTERS)) {
      l_filter    <- LGBT_FILTERS[[l_nome]](data)
      base_filter <- d_filter & l_filter

      result[[d_nome]][[l_nome]] <- build_group_entries(data, base_filter, cat_vars)
    }
  }

  # ------------------------------------------------------------------
  # 2. Marginali diagnosi (lgbt = "tot")
  # ------------------------------------------------------------------
  for (d_nome in names(DIAGNOSI_FILTERS)) {
    d_filter <- DIAGNOSI_FILTERS[[d_nome]](data)
    d_key    <- paste0(d_nome, "_tot")

    result[[d_key]] <- list()
    result[[d_key]][["tot"]] <- build_group_entries(data, d_filter, cat_vars)
  }

  # ------------------------------------------------------------------
  # 3. Marginali lgbt (diagnosi = "tot")
  # ------------------------------------------------------------------
  if (is.null(result[["tot"]])) result[["tot"]] <- list()

  for (l_nome in names(LGBT_FILTERS)) {
    l_filter <- LGBT_FILTERS[[l_nome]](data)
    l_key    <- paste0(l_nome, "_tot")

    result[["tot"]][[l_key]] <- build_group_entries(data, l_filter, cat_vars)
  }

  return(result)
}


# ------------------------------------------------------------------------------
# summary_subgroups()
#
# Restituisce dataframe con colonne:
#   variabile, valore, label, diagnosi, lgbt, count,
#   totale_gruppo, totale_var, totale_soggetti, pct
#
# totale_gruppo   = n soggetti nel gruppo (indipendente dalla variabile)
# totale_var      = n soggetti nel gruppo con risposta valida (non NA) per
#                   quella variabile → denominatore di pct (somma = 100%)
# totale_soggetti = nrow(data)
# pct             = count / totale_var * 100
# ------------------------------------------------------------------------------

summary_subgroups <- function(subgroups, data) {

  totale_soggetti <- nrow(data)
  rows <- list()

  for (d_nome in names(subgroups)) {
    for (l_nome in names(subgroups[[d_nome]])) {

      # Ricostruisce la maschera base del gruppo
      if (d_nome == "tot") {
        # marginale lgbt: rimuove suffisso "_tot" per recuperare il filtro
        l_base      <- sub("_tot$", "", l_nome)
        gruppo_mask <- LGBT_FILTERS[[l_base]](data)

      } else if (grepl("_tot$", d_nome)) {
        # marginale diagnosi
        d_base      <- sub("_tot$", "", d_nome)
        gruppo_mask <- DIAGNOSI_FILTERS[[d_base]](data)

      } else {
        # combinazione incrociata
        gruppo_mask <- DIAGNOSI_FILTERS[[d_nome]](data) &
                       LGBT_FILTERS[[l_nome]](data)
      }

      totale_gruppo_base <- sum(gruppo_mask)

      for (var_name in names(subgroups[[d_nome]][[l_nome]])) {

        col        <- data[[var_name]]
        factor_col <- data[[paste0(var_name, ".factor")]]

        # Denominatore: soggetti nel gruppo con risposta valida (non NA)
        totale_var <- sum(gruppo_mask & is_valid(col))

        for (key in names(subgroups[[d_nome]][[l_nome]][[var_name]])) {
          entry <- subgroups[[d_nome]][[l_nome]][[var_name]][[key]]

          # Recupera label dal factor corrispondente
          label <- NA_character_
          if (!is.null(factor_col)) {
            idx <- which(is_valid(data[[var_name]]) &
                           as.character(data[[var_name]]) == key)[1]
            if (length(idx) > 0 && !is.na(idx))
              label <- as.character(factor_col[idx])
          }

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
            pct             = if (totale_var > 0)
                                round(entry$count / totale_var * 100, 1)
                              else NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  do.call(rbind, rows)
}
