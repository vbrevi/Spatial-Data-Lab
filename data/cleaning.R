# PULIZIA DATI

rm(list = ls()) 
graphics.off()   
cat("\014")      

# Caricare le librerie necessarie
library(readxl)
library(openxlsx)
library(dplyr)

# ALTIMETRIA

# Impostare la directory di lavoro
setwd(" SET DIRECTORY ")
gwd <- getwd()

# Leggere il file Excel, saltando la prima riga 
df <- read_excel(paste0(gwd[1], "Elab_Altimetrie_DEM.xlsx"), skip = 1)

# Visualizzare le prime righe e le colonne del dataset per avere un'idea del formato
head(df)
names(df)

# Cambio nome colonne
colnames(df) <- c("COD_REG", "COD_PRO", "PRO_COM", "Comune", "Superficie",
                  "Alt_min", "Alt_max", "Range_alt", "Alt_media", "Alt_mediana", "Alt_std")

# Visualizzare le prime righe e le colonne del dataset per avere un'idea del formato
head(df)
names(df)

# Mantieni solo righe per comuni di Brescia
df_filtrato <- df[df$COD_PRO == 17, ]

# Cambia nome "Tremosine e Puegnago
df_filtrato$Comune[df_filtrato$Comune == "Tremosine"] <- "Tremosine sul Garda"
df_filtrato$Comune[df_filtrato$Comune == "Puegnago sul Garda"] <- "Puegnago del Garda"

# Elimina prime colonne
df_filtrato <- df_filtrato[, -c(1:3)]

# Salva i dati puliti in un nuovo file Excel chiamato 'altimetrie_pulito.xlsx'
write.xlsx(df_filtrato, "altimetrie_pulito.xlsx")

# Verifica che il file sia stato creato nel percorso di lavoro
cat("File 'altimetrie_pulito.xlsx' è stato salvato correttamente.")

# STATO CIVILE

rm(list = ls()) 
graphics.off()   
cat("\014") 

# Impostare la directory di lavoro
setwd(" SET DIRECTORY ")
gwd <- getwd()

# Leggere il file Excel, saltando la prima riga (che contiene intestazioni inutili)
df <- read_excel(paste0(gwd[1], "Tutti i comuni per stato civile (IT1,22_289_DF_DCIS_POPRES1_25,1.0).xlsx"), skip = 1)

# Visualizzare le prime righe e le colonne del dataset per avere un'idea del formato
head(df)
names(df)

# Pulire il dataset: rimuovere le righe 1, 2, 3, 4,5,7 (intestazioni o righe non utili)
statocivile <- df %>%
  slice(-c(1, 2, 3, 4, 5, 7))  # Elimina le righe 1, 2, 3, 4, 5, 7

# Visualizzare le prime righe e le colonne del dataset per avere un'idea del formato
head(statocivile)
names(statocivile)

# Impostare la prima riga come intestazione delle colonne
colnames(statocivile) <- statocivile[1, ]

# Rimuovere la prima riga, che ora è diventata ridondante come intestazione
statocivile <- statocivile[-1, ]

# Rinominare la prima colonna in 'Comune' per una maggiore chiarezza
colnames(statocivile)[1] <- "Comune"

# Visualizzare le prime righe e le colonne del dataset dopo le modifiche
head(statocivile)
names(statocivile)

# Rimuovere le colonne con i nomi specificati
statocivile <- statocivile %>%
  select(-`Unito/a civilmente`, 
         -`Già in unione civile (per decesso del partner)`, 
         -`Già in unione civile (per scioglimento unione)`)

# Rinominare la colonna in 'Pop_tot' per una maggiore chiarezza
colnames(statocivile)[6] <- "Pop_tot"

# Visualizza le prime righe per assicurarti che le colonne siano state eliminate
head(statocivile)

# Salva i dati puliti in un nuovo file Excel chiamato 'veicoli_pulito.xlsx'
write.xlsx(statocivile, "statocivile_pulito.xlsx")

# Verifica che il file sia stato creato nel percorso di lavoro
cat("File 'statocivile_pulito.xlsx' è stato salvato correttamente.")

# VEICOLI

rm(list = ls()) 
graphics.off()   
cat("\014") 

# Impostare la directory di lavoro
setwd(" YOUR DIRECTORY ")
gwd <- getwd()

# Leggere il file Excel, saltando la prima riga (che contiene intestazioni inutili)
df <- read_excel(paste0(gwd[1], "Veicoli - Pubblico registro automobilistico - comuni (IT1,41_993_DCIS_VEICOLIPRA_COM_1,1.0).xlsx"), skip = 1)

# Visualizzare le prime righe e le colonne del dataset per avere un'idea del formato
head(df)
names(df)

# Pulire il dataset: rimuovere le righe 1, 2, 3, 5 (intestazioni o righe non utili)
veicoli <- df %>%
  slice(-c(1, 2, 3, 5))  # Elimina le righe 1, 2, 3, 5

# Visualizzare le prime righe e le colonne del dataset pulito
head(veicoli)
names(veicoli)

# Impostare la prima riga come intestazione delle colonne
colnames(veicoli) <- veicoli[1, ]

# Rimuovere la prima riga, che ora è diventata ridondante come intestazione
veicoli <- veicoli[-1, ]

# Rinominare la prima colonna in 'Comune' per una maggiore chiarezza
colnames(veicoli)[1] <- "Comune"
colnames(veicoli)[17] <- "Tot_veicoli"

# Visualizzare le prime righe e le colonne del dataset dopo le modifiche
head(veicoli)
names(veicoli)

# Convertire la colonna 'Autovetture' in numerico, se necessario
veicoli$Autovetture <- as.numeric(veicoli$Autovetture)

# Calcolare il numero di auto elettriche (11,5 ogni 1000 autovetture)
veicoli <- veicoli %>%
  mutate(Auto_elettriche = round(Autovetture * 11.5 / 1000, 0))

# Visualizzare le prime righe e le colonne del dataset dopo le modifiche
head(veicoli)
names(veicoli)

# Salva i dati puliti in un nuovo file Excel chiamato 'veicoli_pulito.xlsx'
write.xlsx(veicoli, "veicoli_pulito.xlsx")

# Verifica che il file sia stato creato nel percorso di lavoro
cat("File 'veicoli_pulito.xlsx' è stato salvato correttamente.")