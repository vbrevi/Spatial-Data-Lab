library(sf)
library(fields)
library(RColorBrewer)
library(classInt)
library(rmapshaper)
library(tmap)
library(tmaptools)
library(cartogram)
library(ggplot2)
library(spdep)
library(spatialreg)
library(dplyr)
library(stargazer)
library(spgwr)
library(GWmodel)
library(plotly)
library(readxl)
library(scales)
library(car)
library(shinydashboard)
library(leaflet)

load("unito.rda")

load("shapefilecom.rda") 

# Lista dei file .rda da caricare
rda_files <- c(
  "aeroporto.rda",
  "fascia_rispetto.rda",
  "interscambi.rda",
  "linee_dirette.rda",
  "opere_prog.rda",
  "opere_prog_es.rda",
  "mob_punt.rda",
  "aree_ed.rda",
  "confine_ambito.rda",
  "trasf.rda"
)

# Carica tutti i file .rda
for (file in rda_files) {
  load(file.path( file))
  #file <- st_set_crs(file,3003)
}



#shapefilecom <- st_transform(shapefilecom, 3003)          
#shapefile_strade <- lapply(shapefile_strade, st_transform, 3003)
#shapefile_edifici <- lapply(shapefile_edifici, st_transform, 3003)


