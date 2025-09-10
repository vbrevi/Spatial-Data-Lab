# ui.R
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly) 

# Liste principali dei "temi"
main_categories <- setNames(
  c("aeroporto", "fascia_rispetto", "interscambi", "linee_dirette",
    "opere_prog", "opere_prog_es", "mob_punt", "aree_ed",
    "confine", "trasf"),
  c("Airport safety area", "Buffer zone", "Interchanges", "Direct lines",
    "Projects to be planned", "Existing and planned works", "Point-based mobility system",
    "Built-up areas", "Boundary of area", "Conditional transformations")
)


# Definisci le scelte delle variabili in un unico posto
variable_choices <- c(
  "Area" = "Area",
  "Altitude" = "Altitude",
  "Population" = "Population",
  "Single" = "Single",
  "Married" = "Married",
  "Divorced" = "Divorced",
  "Widowed" = "Widowed",
  "Cars" = "Cars",
  "Vehicles" = "Vehicles",
  "Electric Vehicles" = "Electric_vehicles",
  "Population Density" = "Population_density",
  "Car Density" = "Car_density",
  "Vehicles per Altitude" = "Vehicles_per_altitude",
  "Population per Altitude" = "Pop_per_altitude",
  "Electric Vehicle Ratio" = "Electric_vehicle_ratio"
)

# Definisci le scelte delle variabili in un unico posto per la y di regressione
variable_choices_reg <- c(
  "Electric vehicles" = "Electric_vehicles",
  "Electric vehicle ratio" = "Electric_vehicle_ratio"
)

ui <- dashboardPage(
  dashboardHeader(
    title = 'SPATIAL DATA LAB - ELECTRIC VEHICLE IN THE PROVINCE OF BRESCIA',
    titleWidth = 800
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "Descriptive", icon = icon("dashboard")),
      menuItem("Composition", tabName = "Composition", icon = icon("chart-pie")),
      menuItem("Visualization", tabName = "Visual", icon = icon("map")),
      menuItem("Analysis by variable", tabName = "Analysis", icon = icon("bar-chart")),
      menuItem("Regression Analysis", tabName = "Regression", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Descriptive",
        fluidRow(
          box(
            title = "Select a municipality",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            status = "primary",
            column(
              5,
              selectizeInput(
                inputId = "comuni",
                label = "Select a municipality",
                choices = unique(dati_finali$COMUNE),
                selected = "Acquafredda",
                multiple = FALSE
              )
            )
          ),
          box(
            title = "Legend",
            width = 6,
            background = "light-blue",
            collapsible = TRUE,
            status = "primary",
            "You can select a municipality to view its statistical profile, and then select a variable to visualize its distribution across municipalities on the map, along with the corresponding density plot."
          )
        ),
        fluidRow(
          valueBoxOutput("boxPop", width = 3),
          valueBoxOutput("boxAlt", width = 3),
          valueBoxOutput("boxVeicoli", width = 3),
          valueBoxOutput("boxElettrici", width = 3)
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              selectInput(
                inputId = "variabile_mappa",
                label = "Select variable to show on map",
                choices = variable_choices,
                selected = "Area"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            tabBox(
              width = NULL,
              title = "Density map",
              id = "tabset1",
              tabPanel(
                plotlyOutput("mapVarie", height = "500px")
              )
            )
          ),
          column(
            width = 6,
            tabBox(
              width = NULL,
              title = "Density distribution",
              id = "tabset2",
              tabPanel(
                plotlyOutput("hist_f", height = "500px")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "Visual",
        fluidRow(
          box(
            title = "Select Layers",
            solidHeader = TRUE,
            status = "primary",
            checkboxGroupInput(
              inputId = "category_selector",
              label = "Choose one or more layers:",
              choices = main_categories
            )
          ),
          box(
            title = "Legend",
            width = 6,
            background = "light-blue",
            collapsible = TRUE,
            status = "primary",
            tagList(
              p("In this section, you can visualize the municipal boundaries, road networks, buildings, and specific infrastructures on the map."),
              p("To display these data layers, select the corresponding checkboxes in the 'Select Layers' panel. The chosen layers will then be loaded and displayed on the map below."),
              p(HTML("---")),
              p(HTML("<b>Note:</b> This function does not work exactly as expected. Some layers take a little time to load, so selecting an option may not immediately display anything on the map."))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            leafletOutput("mappa", height = 600)
          )
        )
      ),
      
      tabItem(
        tabName = "Analysis",
        
        # Prima riga: menu + checkbox a sinistra, legenda a destra
        fluidRow(
          box(
            title = "Data & Brescia Filter",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            # Menu a tendina per X e Y
            column(
              6,
              selectizeInput(
                inputId = "var_x",
                label = "Select X variable",
                choices = variable_choices,
                multiple = FALSE,
                selected = "Superficie"
              ),
              selectizeInput(
                inputId = "var_y",
                label = "Select Y variable",
                choices = variable_choices,
                multiple = FALSE,
                selected = "Population"
              ),
              # Checkbox per Brescia sotto i menu
              checkboxInput(
                inputId = "include_brescia",
                label = "Include Brescia in scatterplot",
                value = TRUE
              )
            )
          ),
          
          # Box legenda affiancata
          box(
            title = "Legend",
            background = "light-blue",
            status = "primary",
            width = 6,
            collapsible = TRUE,
            tagList(
              p(HTML("
                  This is a scatterplot showing the relationship between the variables you select from the dropdown menus.<br>
                  The plot may appear visually skewed because Brescia is much larger and more populous than the other municipalities.<br>
                  Removing Brescia can make the patterns in the other municipalities easier to see.<br>
                  Use the checkbox to include or exclude the municipality of Brescia.
              ")),
              br(), br()
            )
          )
        ),
        fluidRow(
          box(
            title = "Scatterplot",
            plotlyOutput("plot_scatter"),
            collapsible = TRUE,
            width = 12
          )
        ),
        fluidRow(
          tags$head(
            tags$style(
              HTML("
                  .shiny-output-error-validation {
                    color: #ff0000;
                    font-weight: bold;
                    font-size: 22px;
                  }
              ")
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'Regression',
        fluidRow(
          box(
            title = "Variable selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            column(
              6,
              style = 'padding:1px;',
              selectizeInput(
                inputId = "dep",
                label = "Dependent variable",
                choices = variable_choices_reg,
                multiple = FALSE,
                selected = "Electric vehicle ratio"
              )
            ),
            column(
              6,
              selectizeInput(
                inputId = "indep",
                label = "Independent variables",
                choices = variable_choices,
                multiple = TRUE,
                selected = c("Population", "Altitude", "Area")
              )
            ),
            column(
              6,
              checkboxInput(
                inputId = "exclude_brescia_reg",
                label = "Exclude Brescia from regression",
                value = FALSE
              )
            )
          ),
          box(
            title = "Legend",
            background = "light-blue",
            collapsible = TRUE,
            width = 6,
            status = "primary",
            "Regression analysis correlates a set of X independent variables to a dependent variable Y:",
            br(),
            withMathJax(
              paste0("\\( Y = \\alpha + \\beta X + \\epsilon \\) ")
            ),
            br(),
            "where", "\u03B1", "is the intercept,", "\u03B2", "is a vector of coefficients associated to the vector of X independent variables,",
            "and,", "\u03B5", "is the error terms - those part of the dependent variable Y not explained by the X independent variables.",
            br(),
            "Each coefficient is tested in order to check if it is statistically significant.",
            "If the probability of being zero is lower than 5% (p-value<0.05), conventionally represented by two asterisks **, ",
            "then a coefficient is considered statistically significant."
          )
        ),
        
        fluidRow(
          box(
            title = "Regression coefficients (OLS)",
            collapsible = TRUE,
            width = 6,
            plotlyOutput("coefregressionRE")
          ),
          box(
            title = "Regression table",
            collapsible = TRUE,
            width = 6,
            column(
              12,
              align = "center",
              uiOutput("lm1")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "Composition",
        fluidRow(
          box(
            title = "Select Municipality",
            width = 6,
            selectizeInput(
              inputId = "comune_sel",
              label = "Select a municipality",
              choices = unique(dati_finali$COMUNE),
              selected = "Acquafredda",
              multiple = FALSE
            )
          ),
          box(
            title = "Legend",
            width = 6,
            background = "light-blue",
            "The charts below show the demographic composition and vehicle distribution for the selected municipality."
          )
        ),
        fluidRow(
          box(
            title = "Population Composition",
            plotlyOutput("pop_pie", height = 500),
            width = 6,
            solidHeader = TRUE
          ),
          box(
            title = "Vehicles Distribution",
            plotlyOutput("vehicles_pie", height = 500),
            width = 6,
            solidHeader = TRUE
          )
        )
      )
    )
  )
)