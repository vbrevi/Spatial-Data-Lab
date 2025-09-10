rm(list = ls())  # Clear all variables
graphics.off()   # Close all plots
cat("\014")      # Clear the console (Ctrl+L also works)

# Caricare la libreria necessarie
library(readxl)
library(openxlsx)
library(sf)

# CREATING ONE FILE 

# Impostare la directory di lavoro
setwd(" SET DIRECTORY ")
gwd <- getwd()

# Caricare i dataset
veicoli <- read_excel(paste0(gwd[1], "/veicoli_pulito.xlsx"))
statocivile <- read_excel(paste0(gwd[1], "/statocivile_pulito.xlsx"))
altimetria <- read_excel(paste0(gwd[1], "/altimetrie_pulito.xlsx"))

# Visualizzare le prime righe e le colonne dei dataset per verificarne il contenuto
head(veicoli)
head(statocivile)
head(altimetria)

# Unire i due dataset sulla base della colonna "Comune"
# Prima unisci veicoli e statocivile
veicoli_statocivile <- merge(veicoli, statocivile, by = "Comune", all = TRUE)

# Poi unisci il risultato con altimetria
dataset_unito <- merge(veicoli_statocivile, altimetria, by = "Comune", all = TRUE)

# Rimuovere le righe con i comuni specificati
dataset_unito <- dataset_unito[!(dataset_unito$Comune %in% c("Prestine")), ]


# Visualizzare il nuovo dataset unito
head(dataset_unito)

# Mostrare classi
sapply(dataset_unito, class)

# Convertire tutte le colonne tranne la prima da character a numeric
dataset_unito[, -1] <- lapply(dataset_unito[, -1], function(x) as.numeric(as.character(x)))

# Verifica dei tipi dopo la conversione
str(dataset_unito)

# Creiamo metriche/nuove colonne interessanti 
dataset_unito <- dataset_unito %>%
  mutate(
    densita_pop = Pop_tot / Superficie,
    densita_auto = Autovetture / Pop_tot,
    veicoli_per_altitudine = Tot_veicoli / Alt_media,
    popolazione_per_altitudine = Pop_tot / Alt_media,
    Quota_elettriche = Auto_elettriche / Autovetture
  )

# Salva i dati puliti in un nuovo file Excel chiamato 'veicoli_pulito.xlsx'
write.xlsx(dataset_unito, "dataset_unito.xlsx")

# Verifica che il file sia stato creato nel percorso di lavoro
cat("File 'dataset_unito.xlsx' è stato salvato correttamente.")

# ---------------------------------------------------------------------------------------------------------

# CREATING RDA

# Seleziona solo le colonne desiderate
dati_finali <- dati_completi %>%
  dplyr::select(
    Comune,
    Nubile_celibe = `Nubile/celibe`,
    Coniugato = `Coniugata/o`,
    Divorziato = `Divorziata/o`,
    Vedovo = `Vedova/o`,
    Pop_tot,
    Autovetture,
    Tot_veicoli,
    Auto_elettriche,
    Superficie,
    Alt_media
  ) %>%
  # Converti tutte le colonne tranne Comune in numerico
  mutate(across(
    -Comune,
    ~ as.numeric(gsub(",", "", .))  # Rimuove eventuali virgole nei numeri
  ))

dati_finali <- dati_finali %>%
  mutate(
    densita_pop = ifelse(is.na(Pop_tot) | is.na(Superficie), NA, Pop_tot / Superficie),
    densita_auto = ifelse(is.na(Autovetture) | is.na(Pop_tot), NA, Autovetture / Pop_tot),
    veicoli_per_altitudine = ifelse(is.na(Tot_veicoli) | is.na(Alt_media), NA, Tot_veicoli / Alt_media),
    popolazione_per_altitudine = ifelse(is.na(Pop_tot) | is.na(Alt_media), NA, Pop_tot / Alt_media),
    Quota_elettriche = ifelse(is.na(Auto_elettriche) | is.na(Autovetture), NA, Auto_elettriche / Autovetture)
  )

colnames(dati_finali)[colnames(dati_finali) == "Comune"] <- "COMUNE"


save(dati_finali, file = file.path(base_path, "unito.rda"))

# ---------------------------------------------------------------------------------------------------------
# SHAPEFILE RDA

shp_path <- " SET DIRECTORY "

shapefilecom <- st_read(file.path(shp_path, "Com01012023_g_WGS84.shp"))

save(shapefilecom, file = file.path(shp_path, "shapefilecom.rda"))

# Carica e salva ciascun shapefile
aeroporto <- st_read(file.path(shp_path, "area di rispetto aeroporto montichiari.shp"))
save(aeroporto, file = "aeroporto.rda")

fascia_rispetto <- st_read(file.path(shp_path, "fascia_rispetto.shp"))
save(fascia_rispetto, file = "fascia_rispetto.rda")

interscambi <- st_read(file.path(shp_path, "interscambi.shp"))
save(interscambi, file = "interscambi.rda")

linee_dirette <- st_read(file.path(shp_path, "linee_dirette.shp"))
save(linee_dirette, file = "linee_dirette.rda")

opere_prog <- st_read(file.path(shp_path, "opere da programmare.shp"))
save(opere_prog, file = "opere_prog.rda")

opere_prog_es <- st_read(file.path(shp_path, "opere esistenti e programmate.shp"))
save(opere_prog_es, file = "opere_prog_es.rda")

mob_punt <- st_read(file.path(shp_path, "sistema della mobilità puntuale.shp"))
save(mob_punt, file = "mob_punt.rda")

# Carica e salva ciascun shapefile
aree_ed <- st_read(file.path(shp_path, "aree_edificate.shp"))
save(aree_ed, file = "aree_ed.rda")

confine <- st_read(file.path(shp_path, "confine_ambito.shp"))
save(confine, file = "confine_ambito.rda")

trasf <- st_read(file.path(shp_path, "trasformazioni_condizionali.shp"))

save(trasf, file = "trasf.rda")
