# 📂 Data Folder

This folder contains the datasets and scripts used to prepare the inputs for the Shiny app.

---

## 📜 Files

### 1. `cleaning.R`
- Loads three datasets from **ISTAT**:  
  - **Altimetry** (municipal elevation data)  [https://www.istat.it/classificazione/principali-statistiche-geografiche-sui-comuni/]
  - **Civil status** (population by marital status)  [https://esploradati.istat.it/databrowser/#/it/dw/categories/IT1,POP,1.0/POP_POPULATION/DCIS_POPRES1/IT1,22_289_DF_DCIS_POPRES1_25,1.0]
  - **Vehicles** (vehicle registration data)  [https://www.istat.it/comunicato-stampa/indicatori-del-parco-veicolare-anno-2023/]
- Performs initial cleaning and preprocessing steps.  
- Saves the cleaned outputs as `name_pulito` files in this directory.  

---

### 2. `rda_creation.R`
- Loads the cleaned datasets (`*_pulito`).  
- Merges them into a single dataset called **`dataset_unito`**.  
- Imports shapefiles containing:  
  - **Municipal boundaries** [https://sit.provincia.brescia.it/download/cartografia-provinciale]
  - **Roads and buildings**  [https://sit.provincia.brescia.it/download/shape-file-ptcp-provincia-brescia]
- Creates `.Rda` files to be used by the Shiny app.  
