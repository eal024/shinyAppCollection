

## Bibliotek  
library(tidyverse)
library(highcharter)
library(shiny)

# Data kapittel Politikk. Figur med nedtrekksvalg
Sys.setlocale("LC_CTYPE")
data <- vroom::vroom("data/figurdata_politikk10.1_v3.csv") 

data_ref <- data |> filter( lokasjon == "Total")  |> arrange(name)

# 
data1 <- data |> 
    #filter( lokasjon != "Totalt")  %>% 
    left_join(data_ref |>
                  mutate( index = 1:n(), .by = name) %>% 
                  filter( index == 1) %>% 
                  select( name, ref_verdi = value)
              , join_by(name)
    )  %>% 
    mutate(      name = fct_reorder( factor(name), ref_verdi),
                 kategori2 = paste0(kategori, "\n", lokasjon)
    )


## Navn til nedtrekksmeny
# Geografi
Sys.setlocale("LC_CTYPE")
X <- c(
    "Aldring av befolkningen",
    "Økt arbeidsinnvandring",
    "Flere flyktninger",
    "Grønt skifte og klimautfordringer",
    "Manglende etterspurt kompetanse blant arbeidssøkere",
    "Økt omstillingstakt i arbeidslivet",
    "Flere med fysiske og psykiske lidelser",
    "Flere unge i utenforskap",
    "Flere fattige",
    "Innstramming/effektivisering av off. sektor",
    "Lavere tillit til offentlig sektor",
    "Økte forventninger til brukermedvirkning",
    "Digitaliseringen av offentlige tjenester øker",
    "Økte forventninger til samarbeid på tvers av sektorer",
    "Sentralisering av offentlige tjenester",
    "Flere internasjonale konflikter/kriger",
    "Økte forventninger til at off. tjenester har effekt",
    "Økte forv. til at det off. kan håndtere uforutsette situasjoner",
    "Økt mangel på arbeidskraft",
    "Økende politisk polarisering og konfliktnivå i Norge"
)


Sys.setlocale("LC_CTYPE")
lokasjon <- setNames(unique(data1$lokasjon), unique(data1$lokasjon) ) #

data1 |> filter( lokasjon == lokasjon[[1]] )

ui <- fluidPage(
    titlePanel("Figur med nedtrekksvalg"),
    #fluidRow(
    sidebarLayout(
        sidebarPanel(
            selectInput('lokasjon', 'Geografi', names(lokasjon), width = "80%" )
        ),
        mainPanel(
            highchartOutput("plot", width = "110%", height = "800px")
            
        )
    )
)


server <- function(input, output, session) {
    Sys.setlocale("LC_CTYPE")
    dataset <- reactive({
        data1 |> 
            filter( lokasjon ==input$lokasjon | str_detect(lokasjon, "Tota"))
    })
    
    
    output$plot <- renderHighchart(
        {
            highchart() %>%
                hc_add_series(type = "bar",
                              data = dataset(),
                              hcaes(y = value,
                                    x = name,
                                    group = factor(kategori2) )
                ) %>%
                hc_xAxis(title = list(text = ""), categories = X ) %>%
                hc_yAxis(labels = list(formatter = JS("function() { 
                                             if (this.value == 1) {
                                               return '1 Lite';
                                             } else if (this.value == 2) {
                                               return '2 Noe';
                                             } else if (this.value == 3) {
                                               return '3 Mye';
                                             } else {
                                               return ' ';
                                             }
                                           }")),
                         min = 1,
                         title = list(text = "" )
                ) 
        }
    )
}

shinyApp(ui, server)
