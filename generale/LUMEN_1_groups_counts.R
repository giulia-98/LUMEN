# Clear existing data and graphics
rm(list = ls())
graphics.off()

library(Hmisc)

# Carica dati con label e factor
source("load_lumen_data.R")
source("compute_subgroups.R")
data <- load_lumen_data()

# Verifica consenso
data <- data[data$consenso == 1 & !is.na(data$consenso), ]

##################################################################################################################


##################################################################################################################

# Calcolo sottogruppi a TRE LIVELLI: variabile categorica x diagnosi x lgbt
#
# Struttura:  subgroups[[diagnosi]][[lgbt]][[variabile]][[valore]]
#
# Gruppi diagnosi (3): aut | altra_diagnosi | no_diagnosi
# Gruppi lgbt    (2): lgbtq | no_lgbtq
# → 6 combinazioni + gruppi marginali totali:
#   aut_tot, altra_diagnosi_tot, no_diagnosi_tot  (diagnosi indip. da lgbt)
#   lgbtq_tot, no_lgbtq_tot                       (lgbt indip. da diagnosi)
#
# CSV colonne:
#   variabile, valore, label, diagnosi, lgbt, count, totale_gruppo, totale_soggetti, pct
#
# pct = count / totale_soggetti * 100  (percentuale sul totale dei soggetti)

subgroups <- compute_subgroups(data)

df <- summary_subgroups(subgroups, data)

# Ordine gruppi: prima i totali marginali, poi le combinazioni diagnosi x lgbt
gruppi_totali_diagnosi <- c("aut_tot", "altra_diagnosi_tot", "no_diagnosi_tot")
gruppi_totali_lgbt     <- c("lgbtq_tot", "no_lgbtq_tot")

df_tot_diagnosi <- df[df$diagnosi %in% gruppi_totali_diagnosi, ]
df_tot_lgbt     <- df[df$lgbt     %in% gruppi_totali_lgbt, ]
df_combinazioni <- df[!(df$diagnosi %in% gruppi_totali_diagnosi) &
                      !(df$lgbt     %in% gruppi_totali_lgbt), ]

df_ordinato <- rbind(df_tot_diagnosi, df_tot_lgbt, df_combinazioni)


# Salva nella stessa cartella dello script
# Funziona sia con source() in RStudio sia con Rscript da terminale
script_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, function(f) f$ofile))
  if (length(ofiles) > 0) {
    dirname(normalizePath(ofiles[[length(ofiles)]]))
  } else {
    args     <- commandArgs(trailingOnly = FALSE)
    file_arg <- args[grepl("^--file=", args)]
    if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
  }
}, error = function(e) getwd())

output_path <- file.path(script_dir, "groups_counts.csv")
write.csv(df_ordinato, output_path, row.names = FALSE)
message("File salvato in: ", output_path)

