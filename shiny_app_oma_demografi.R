
# Bib
library(shiny)
library(tidyverse, warn.conflicts = F)
library(highcharter, warn.conflicts = F )

## Data import
Sys.setlocale("LC_CTYPE")
data <- vroom::vroom("data/figurdata 4.1.csv") |>
    dplyr::mutate(versjon = ifelse(versjon == "Høy nasjonal vekst", "Hoy nasjonal vekst", versjon)
    ) |>
    dplyr::distinct() |>
    na.omit()

# Server: Med sidemeny hvor man kan trekke ned region.
server <- function(input, output, session) {
    
}
ui <- fluidPage(
    titlePanel("Figur med nedtrekksvalg"),
    #fluidRow(
    sidebarLayout(
        sidebarPanel(
            selectInput('omrade', 'Geografi', names(omrade), width = "80%" )
        ),
        mainPanel(
            highchartOutput("plot", width = "110%", height = "800px")
            
        )
    )
)


# Server
server <- function(input, output, session) {
    
    # Data: SSBs befolkningsframskriving
    Sys.setlocale("LC_CTYPE")
    dataset <- reactive({
        subset_data <- data[data$omrade == input$omrade, ]
        subset_data <- na.omit(subset_data)
        subset_data
    })  
    # Plottet
    output$plot <- renderHighchart({
    
    highchart() |>
        hc_add_series( data = dataset(),
                       type = "line",
                       hcaes(y = value,
                             x = ar,
                             group = factor(versjon)
                       )
        )  |> 
        hc_xAxis(title = list(text = "")) |> 
        hc_yAxis(title = list(text = "Befolkningsantall")
        ) |> 
        hc_plotOptions( series = list( marker = list(enabled = F) )
        )
    }
    )
}


shinyApp(ui, server)
