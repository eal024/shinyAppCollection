


library(tidyverse)
library(shiny)
source( here::here("2022-02-02 soa_utregning.R") )

# User interface
ui <- fluidPage(
    titlePanel("Gevinstanalyse"),
    helpText("Analysen er basert på prinsipper for samfunnsøkonomisk analyse (Direktoratet for økonomistyring , 2018)"),    
    
    
    sidebarPanel( 
        
        numericInput("tiltaks_kost", "Tiltakskostnader:", 200000),
        
        numericInput("numeric_prop", "Sannsynlighetsrate for overgang til arbeid:", 0.5),
        
        sliderInput("numeric_inntekt", "Inntekt under ordinære betingelser", min = 2*108287, max = 6*108287, value = 448965),
        helpText("Inntekt: Median årsinntekt i 2020, for personer med grunnskole som høyeste fullførte utdanning, var i følge SSB 423 480 kroner"),
        
        
        
        # Input
        sliderInput("integer_age", "Alder ved start tiltak: ", min = 20, max = 65, value = 24),
        
        numericInput("pensj_alder", "Pensjonsalder:", 65),
        
        # Include clarifying text ----
        helpText("Info: I følge Bjørnstad (2019) er gjennsnittlig pensjonsalder i Norge 65 år."),
        
        sliderInput("integer_arbGiverAvg", "Prosent arbeidsgiveravgfit og sosiale kostnader samlet: ", min = .15, max = .30, value = 0.2),
        
          
        numericInput("numeric_soshjelp", "Gjennomsnittlig årlig økonomisk sosialhjelp per mottaker i 2020, var 130 000 kroner.", value = 130000),
        
        sliderInput("integer_r", "Diskonteringsrente: ", min = 0, max = 0.06, value = 0.04)
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
    # Helpertext
        withMathJax(),
        helpText('Metodisk summeres gevinster og kostnader etter nåverdiprinsippet: 
                 $$NV = \\sum_{i = 0}^{År}\\frac{gevinster_{i}-kostnader_{i}}{(1+r)^r}$$'),    
        
    # Output: Table summarizing the values entered ----
        tableOutput("values"),

    # Output: Grafisk presentasjon av gevinstene ----
        plotOutput("graf"),
    )
)


##
server <- function(input, output, session) {
    
    sliderValues <- reactive({
        
        soa_utregning(start         = input$integer_age, slutt = input$pensj_alder,
                      inntekt       = input$numeric_inntekt,
                      arb_giver_og_sosiale_kost  = input$integer_arbGiverAvg ,
                      #so_kost_pst   = input$integer_sosialeKost,
                      tiltakskost   = input$tiltaks_kost,
                      r             = input$integer_r,
                      sannsynlig_arbeid = input$numeric_prop,
                      sosialhjelp = input$numeric_soshjelp
        )
        
    })
    
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
        sliderValues()
    })
    
    
    ## Grafisk framstilling ----
    output$graf <- renderPlot({
        ggplot( data = as_tibble(sliderValues() ) ,
                aes(
                    y      = `Mill. kroner`,
                    x      = fct_reorder(Forklart, `Mill. kroner`),
                    fill   = fct_reorder(Forklart, `Mill. kroner`),
                    color  = fct_reorder(Forklart, `Mill. kroner`)
                )
        ) + 
            geom_col() +
            theme_light() + 
            ylim( c( ifelse( -input$tiltaks_kost*2 > -3, -input$tiltaks_kost*2, -3  ) ,10)) +
            theme( legend.position = "none", 
                   axis.text.x = element_text(  angle = 90,
                                                size = 16
                   ),
                   axis.text.y = element_text(size = 16)
            ) +
            labs( x = NULL, y = "Mill. kroner")
        
    })
}

shinyApp(ui, server)




