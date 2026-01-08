
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(janitor)
library(sf)
library(scales)
library(FactoMineR) 
library(factoextra)

# load the dataframes
krankenhäuser <- readxl::read_xlsx("data/krankenhäuser.xslx") %>% 
  mutate(across(everything(), ~ replace(., is.na(.), 0)))
bezirke_wien <- st_read("data/geojson/bezirke_95_geo.json") %>% 
  filter(grepl("^Wien.*,", name)) %>% 
  mutate(
    Bezirk = name %>% str_extract("(?<=,).*$"),
    Bezirk_Nr = name %>% str_extract("(?<=Wien)\\s*\\d+(?=.)") %>% as.numeric(),
    Bezirk_Nr_Name=paste0("(", sprintf("%02d", Bezirk_Nr), ") ", Bezirk)
  ) %>% 
  arrange(Bezirk_Nr) 

bezirke_wien %>% writexl::write_xlsx("data/bezirke_wien.xslx")

krankenhäuser <- krankenhäuser %>% 
  mutate(
    patienten_pro_arzt = Stationäre.Patientinnen.und.Patienten / Ärztinnen.und.Ärzte,
    patienten_pro_bett = Stationäre.Patientinnen.und.Patienten / Systematisierte.Betten,
  ) %>% 
  right_join(bezirke_wien) %>% 
  mutate(across(everything(), ~ replace(., is.na(.), 0)))
  
# krankenhäuser %>% View()


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Hospital capacities in Vienna"),
      p("This dashboard shows the usage of the capacities of hospitals in Vienna."),
      p("The choropleth (top-left) shows the number of stationed patients per district.
        The scatterplots show the relationship between the number of patients 
        and number of stationed doctors (bottom-left) / number of available beds (top-right) 
        / number of general staff (bottom-right). The points are drawn as the
        hospitals' respective districts."),
      p("The gray diagonals are the regression lines between the two variables.
        The red observations are considered as anomalous, due to their residuals 
        being larger than 1.5 times the standard deviation of the residuals."),
      h4("Linear model summary:"),
      verbatimTextOutput("summary"),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(5,
               fluidRow(plotOutput("map")),
               fluidRow(plotOutput("scatter_ärzte")),
               ),
        column(5,
               fluidRow(plotOutput("scatter_betten")),
               fluidRow(plotOutput("scatter_staff")),
               ),
        column(2,
                  h3("Applicable filters"),
                  p("By unchecking some districts or single hospitals, the 
                    model is retrained and the plots are updated."),
                 # Picker 1: District Filter
                 checkboxGroupInput(
                   inputId = "district_selection",
                   label = "District Filter",
                   choices = bezirke_wien$Bezirk_Nr_Name,
                   selected = bezirke_wien$Bezirk_Nr_Name
                 ),
                 
                 # Picker 2: Hospital filter
                 # hospital_filter_options <- krankenhäuser$Krankenanstalt
                 pickerInput(
                   inputId = "hospital_selection",
                   label = "Hospital filter",
                   choices = split(krankenhäuser$Krankenanstalt, krankenhäuser$Bezirk_Nr_Name),
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE, 
                     `live-search` = TRUE
                   ),
                   selected = krankenhäuser$Krankenanstalt
                 ),
        ),
      ),
    )
  )
)

server <- function(input, output, session) {
  
  df_filtered <- reactive({
    req(input$hospital_selection)
    req(input$district_selection)
    krankenhäuser %>% 
      # apply filters
      filter(Krankenanstalt %in% input$hospital_selection) %>% 
      filter(Bezirk_Nr_Name %in% input$district_selection)
  })

  lm_model <- reactive({
    req(df_filtered())
    # perform the linear regression
    df_filtered() %>% 
      lm(Stationäre.Patientinnen.und.Patienten ~ 
           Systematisierte.Betten + Ärztinnen.und.Ärzte + 
           Personen.in.nicht.ärztlichen.Gesundheitsberufen, data = .)
  })
    

  df <- reactive({
    req(df_filtered())
    req(lm_model())
    df_filtered <- df_filtered()
    df_filtered$residuals <- residuals(lm_model()) # extract the residuals from model training
    # if the residuals are larger than 1.5 * standard dev of all residuals
    # it counts as anomalous
    df_filtered$efficiency_flag <- ifelse(df_filtered$residuals > sd(df_filtered$residuals)*1.5, 
                                            "Overloaded", "Normal")
    df_filtered
  })
  
  
  text_size = 3
  
  # Example plots
  output$scatter_ärzte <- renderPlot({
    df() %>% 
      ggplot(aes(x = Ärztinnen.und.Ärzte, 
                 y = Stationäre.Patientinnen.und.Patienten)) +
      geom_text(aes(label=Bezirk_Nr, color = efficiency_flag), 
                vjust = 0, hjust = 0, size = text_size, key_glyph = "point" 
      ) +
      geom_smooth(method = "lm", se = FALSE, color="grey") +
      scale_color_manual(values = c("Normal" = "steelblue", "Overloaded" = "red")) +
      labs(
        # title = "Hospital Workload Regression",
        x = "Number of doctors",
        y = "Number of patients",
        color = "Workload status"
      ) +
      theme_minimal() +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme(
        panel.grid = element_blank(),
        legend.position="none"
      )
    })
  output$scatter_betten <- renderPlot({
    df() %>% 
      left_join(bevölkerung) %>% 
      ggplot(aes(x = Systematisierte.Betten, 
                 y = Stationäre.Patientinnen.und.Patienten)) +
      geom_text(aes(label=Bezirk_Nr, color = efficiency_flag), 
                vjust = 0, hjust = 0, size = text_size, key_glyph = "point" 
      ) +
      geom_smooth(method = "lm", se = FALSE, color="grey") +
      scale_color_manual(values = c("Normal" = "steelblue", "Overloaded" = "red")) +
      labs(
        # title = "Hospital Workload Regression",
        x = "Number of available beds",
        y = "Number of patients",
        color = "Workload status"
      ) +
      theme_minimal() +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme(
        panel.grid = element_blank(),
        legend.position="none"
      )
  })
  output$scatter_staff <- renderPlot({
    df() %>% 
      left_join(bevölkerung) %>% 
      ggplot(aes(x = Personen.in.nicht.ärztlichen.Gesundheitsberufen, 
                 y = Stationäre.Patientinnen.und.Patienten)) +
      geom_text(aes(label=Bezirk_Nr, color = efficiency_flag), 
                vjust = 0, hjust = 0, size = text_size, key_glyph = "point" 
      ) +
      geom_smooth(method = "lm", se = FALSE, color="grey") +
      scale_color_manual(values = c("Normal" = "steelblue", "Overloaded" = "red")) +
      labs(
        # title = "Hospital Workload Regression",
        x = "Number of general staff",
        y = "Number of patients",
        color = "Workload status"
      ) +
      theme_minimal() +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme(
        panel.grid = element_blank(),
        legend.position="none"
      )
  })
  
  output$map <- renderPlot({
    req(input$district_selection)
    krankenhäuser %>% 
      group_by(Bezirk_Nr, geometry, iso, Bezirk_Nr_Name) %>% 
      summarise(n = sum(Stationäre.Patientinnen.und.Patienten)) %>% 
      mutate(n = ifelse(Bezirk_Nr_Name %in% input$district_selection, n, 0)) %>% 
      ggplot(aes(geometry=geometry)) +
        geom_sf(aes(fill = n), color = "white") +
        scale_fill_viridis_c(option = "plasma") +
        labs(
          title = paste("Stationed patients per district", year),
          fill = ""
        ) +
        theme_minimal() +
        scale_fill_gradientn(colors = c("white", "lightblue1", "steelblue",  "blue4"),
                             values = scales::rescale(c(0, 1, 80)),) +
        theme(
          axis.text = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, vjust = -2),
          panel.grid = element_blank(),
          aspect.ratio = .8 # Plot Seitenverhältnisse ändern
        )
  })
  
  # Example text
  
  output$summary <- renderText({
    req(lm_model())
    paste(capture.output(summary(lm_model())), collapse = "\n")
  })

  
  }

shinyApp(ui, server)
