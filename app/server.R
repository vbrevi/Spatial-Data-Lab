library(shiny)
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
library(scales) 
library(spdep)

# ---- Funzione per mappa ggplotly interattiva ----
map_interactive <- function(data, fill_var, tooltip_col, fill_label, fill_trans = "log", 
                            palette = "plasma", title = NULL, border_size = 0.3) {
  p <- ggplot(data, aes(fill = .data[[fill_var]], text = .data[[tooltip_col]])) +
    geom_sf(color = "black", linewidth = border_size) +
    scale_fill_viridis_c(name = fill_label, option = palette, trans = fill_trans,
                         labels = scales::label_number()) +
    labs(title = title) +
    theme_void() +
    theme(panel.background = element_rect(fill = "lightblue", colour = "white"),
          plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = "text", width = 500, height = 500)
}

form <- function(indep, dep) {
  nr <- dim(indep)[2]-1
  if (nr==0) {
    beta0 <- names(indep)
  } else {
    beta <- 0
    for(i in 1:nr) {
      beta[i]<-paste(names(indep)[i],"+")
    }
    beta <- paste(beta, collapse=" ")
    beta0<-paste(beta,names(indep)[nr+1]) 
  }
  beta1 <- paste(names(dep), "~", beta0)
}

server <- function(input, output, session) {
  
  # ---- Caricamento dati e pre-processing all'avvio della sessione ----
  load("unito.rda")
  load("shapefilecom.rda") 
  
  dati_finali <- dati_finali %>%
    dplyr::rename(
      Area = Superficie,
      Altitude = Alt_media,
      Population = Pop_tot,
      Single = Nubile_celibe,
      Married = Coniugato,
      Divorced = Divorziato,
      Widowed = Vedovo,
      Cars = Autovetture,
      Vehicles = Tot_veicoli,
      Electric_vehicles = Auto_elettriche,
      Population_density = densita_pop,
      Car_density = densita_auto,
      Vehicles_per_altitude = veicoli_per_altitudine,
      Population_per_altitude = popolazione_per_altitudine,
      Electric_vehicle_ratio = Quota_elettriche
    )
  
  indep_r <- reactive({
    indep <- dati_finali %>% dplyr::select(input$indep)
    
    if (input$exclude_brescia_reg) {
      indep <- indep[dati_finali$COMUNE != "Brescia", , drop = FALSE]
    }
    indep
  })
  
  dep_r <- reactive({
    dep <- dati_finali %>% dplyr::select(input$dep)
    
    if (input$exclude_brescia_reg) {
      dep <- dep[dati_finali$COMUNE != "Brescia", , drop = FALSE]
    }
    dep
  })
  
  RE <- reactive({
    # attendi che esistano sia indep che dep
    req(input$indep, input$dep)
    indep <- indep_r()
    dep <- dep_r()
    # controlla che esista almeno una colonna indipendente
    if (ncol(indep) == 0) return(NULL)
    datt_m <- cbind.data.frame(dep, indep)
    beta1 <- form(indep, dep)
    # proteggi lm con try() per non far cadere l'app in caso di errore
    fit <- try(lm(beta1, data = datt_m), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)
    fit
  })
  
  # ---- Reactive: dati per comune selezionato ----
  dati_comune <- reactive({
    req(input$comuni)
    dati_finali[dati_finali$COMUNE == input$comuni, ]
  })
  
  # ---- Value boxes ----
  output$boxPop <- renderValueBox({
    valueBox(format(dati_comune()$Population, big.mark = ","), "Population", icon = icon("users"), color = "blue")
  })
  
  output$boxAlt <- renderValueBox({
    valueBox(format(dati_comune()$Altitude, big.mark = ","), "Altitude", icon = icon("mountain"), color = "teal")
  })
  
  output$boxVeicoli <- renderValueBox({
    valueBox(format(dati_comune()$Vehicles, big.mark = ","), "Total Vehicles", icon = icon("car"), color = "purple")
  })
  
  output$boxElettrici <- renderValueBox({
    valueBox(format(dati_comune()$Electric_vehicles, big.mark = ","), "Electric Vehicles", icon = icon("bolt"), color = "green")
  })
  
  # ---- Mappa ggplotly ----
  output$mapVarie <- renderPlotly({
    req(input$variabile_mappa)
    
    shapefile_brescia <- shapefilecom[shapefilecom$COD_PROV == 17, ]
    mappa_dati <- merge(shapefile_brescia, dati_finali, by = "COMUNE", all.x = TRUE)
    
    # Colonna tooltip dinamica
    mappa_dati$tooltip_text <- paste0("Municipality: ", mappa_dati$COMUNE, "\n",
                                      input$variabile_mappa, ": ", mappa_dati[[input$variabile_mappa]])
    
    # Titolo dinamico
    titolo <- paste("Distribution of", input$variabile_mappa)
    
    # Chiamo la funzione
    map_interactive(
      data = mappa_dati,
      fill_var = input$variabile_mappa,
      tooltip_col = "tooltip_text",
      fill_label = input$variabile_mappa,
      title = titolo,
      border_size = 0.3
    )
  })
  
  # ---- Density ----
  output$hist_f <- renderPlotly({
    req(input$variabile_mappa)
    x <- dati_finali[[input$variabile_mappa]]
    x <- x[!is.na(x)]
    dens <- density(x)
    
    plot_ly() %>%
      add_trace(x = dens$x, y = dens$y, type = "scatter", mode = "lines", fill = "tozeroy",
                line = list(color = "skyblue"), name = "Density") %>%
      layout(title = paste("Density of", input$variabile_mappa),
             xaxis = list(title = input$variabile_mappa),
             yaxis = list(title = "Density"),
             autosize = FALSE,
             width = 500,
             height = 500)
  })
  
  # ---- Mappa Leaflet vuota ----
  # Mappa iniziale
  output$mappa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10.5, lat = 45.5, zoom = 8)
  })
  
  # Utility: porta in WGS84 se serve (evita crash con CRS proiettati)
  to_wgs84 <- function(x) {
    stopifnot(inherits(x, "sf"))
    
    if (is.na(st_crs(x))) {
      message("CRS mancante: assegno 3003 come default")
      st_crs(x) <- 3003
    }
    
    if (sf::st_is_longlat(x)) return(x)   # già lon/lat
    st_transform(x, 4326)                 # trasformo in WGS84
  }
  
  add_sf_layer <- function(proxy, x, group_name) {
    x <- to_wgs84(x)
    
    geom_type <- unique(as.character(st_geometry_type(x)))
    if (any(geom_type %in% c("MULTIPOINT"))) {
      x <- st_cast(x, "POINT")
      geom_type <- unique(as.character(st_geometry_type(x)))
    }
    
    if (any(geom_type %in% c("POINT"))) {
      proxy <- proxy %>% addCircleMarkers(data = x, radius = 5, group = group_name)
    } else if (any(geom_type %in% c("LINESTRING","MULTILINESTRING"))) {
      proxy <- proxy %>% addPolylines(data = x, weight = 2, group = group_name)
    } else {
      x <- sf::st_make_valid(x)
      proxy <- proxy %>% addPolygons(data = x, weight = 1, fillOpacity = 0.5, group = group_name)
    }
    
    proxy  # ritorna il proxy aggiornato
  }
  
  # Quando cambiano i checkbox, ricostruisci i layer selezionati
  observeEvent(input$category_selector, ignoreInit = TRUE, {
    sel <- input$category_selector           # vettore dei NOMI OGGETTO (es. "aeroporto")
    proxy <- leafletProxy("mappa")
    
    # Cancella tutti i layer dinamici in un colpo solo
    proxy <- proxy %>% clearGroup("selected_layers")
    
    if (length(sel) == 0) return(invisible())
    
    # Per ogni nome oggetto selezionato, prendi l'oggetto e aggiungilo
    for (obj_name in sel) {
      if (exists(obj_name, inherits = TRUE)) {
        x <- get(obj_name, inherits = TRUE)
        if (inherits(x, "sf")) {
          proxy <- add_sf_layer(proxy, x, group_name = "selected_layers")  # <--- qui
        } else {
          warning(sprintf("Object '%s' is not an sf.", obj_name))
        }
      } else {
        warning(sprintf("Object '%s' not found.", obj_name))
      }
    }
    
  })
  
  # ---- Scatter plot ----
  output$plot_scatter <- renderPlotly({
    # Applico filtro su Brescia in base al checkbox
    datt <- dati_finali
    if (!input$include_brescia) {
      datt <- datt[datt$COMUNE != "Brescia", ]
    }
    
    frmm <- as.formula(paste0(input$var_y, " ~ ", input$var_x))
    m <- lm(frmm, data = datt)
    
    fig <- broom::augment(m, se_fit = TRUE) %>%
      plot_ly(x = datt[[input$var_x]], showlegend = FALSE) %>%
      add_markers(y = datt[[input$var_y]], color = I("black")) %>%
      add_ribbons(
        ymin = ~.fitted - 1.96 * .se.fit,
        ymax = ~.fitted + 1.96 * .se.fit,
        color = I("gray80")
      ) %>%
      add_lines(y = ~.fitted, color = I("steelblue")) %>%
      layout(
        title = "Correlation between selected variables",
        xaxis = list(showgrid = FALSE, title = input$var_x),
        yaxis = list(showgrid = FALSE, title = input$var_y)
      )
    
    fig
  })
  
  # --- Inizializza selectize una sola volta, DOPO che la UI è pronta ---
  
  
  # --- Output per regressione: gestisci il caso NULL del modello ---
  output$coefregressionRE <- renderPlotly({
    m <- RE()
    validate(need(!is.null(m), "Select dependent and at least one independent variable"))
    broom::tidy(m) %>% 
      mutate(term = forcats::fct_reorder(term, estimate)) %>%
      plot_ly(x = ~estimate, y = ~term) %>%
      add_markers(
        error_x = ~list(value = std.error), 
        color = I("blue"),
        hoverinfo = "x"
      )
  })
  
  output$lm1 <- renderUI({
    m <- RE()
    if (is.null(m)) return(HTML("<p>Please select variables for the regression.</p>"))
    
    HTML(paste0(
      '<div style="width:100%; overflow-x:auto;">',
      '<style>table {width:100%; table-layout:fixed;} td, th {word-wrap: break-word;}</style>',
      stargazer(m, type="html"),
      '</div>'
    ))
  })
  
  # Grafico a torta / donut della popolazione
  output$pop_pie <- renderPlotly({
    req(input$comune_sel)
    df <- dati_finali %>%
      dplyr::filter(COMUNE == input$comune_sel)
    
    vals <- c(df$Single, df$Married, df$Divorced, df$Widowed)
    labels <- c("Single", "Married", "Divorced", "Widowed")
    df_pie <- data.frame(labels, vals)
    
    plot_ly(
      data = df_pie,
      labels = ~labels,
      values = ~vals,
      type = "pie",
      textinfo = "label+percent",
      hole = 0.3,
      marker = list(colors = c("#1f77b4", "#2ca02c", "#ff7f0e", "#d62728"))
    ) %>%
      layout(
        title = paste("Population Composition in", df$COMUNE),
        height="500px")
  })
  
  # Grafico a torta dei veicoli
  output$vehicles_pie <- renderPlotly({
    req(input$comune_sel)
    df <- dati_finali %>%
      dplyr::filter(COMUNE == input$comune_sel)
    
    vals <- c(df$Cars, df$Vehicles, df$Electric_vehicles)
    labels <- c("Total Cars", "Total Vehicles", "Electric Vehicles")
    
    plot_ly(
      labels = labels,
      values = vals,
      type = "pie",
      textinfo = "label+percent",
      hole = 0.3,  # se vuoi un donut, altrimenti rimuovi
      marker = list(colors = c("blue", "purple", "green"))
    ) %>%
      layout(
        title = paste("Vehicle Distribution in", df$COMUNE),
        height = 500
      )
  })
  
}
