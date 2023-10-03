


library(shiny)

ui <- fluidPage(
    
    
    
    helpText("Summeringskalkulator. Til hjelp for dere i departementet."),  

    sidebarPanel(    
        numericInput("x", "Tast inn verdi for X", 10),
    
        sliderInput("y", "Tast inn verdi for Y", min = 0, max = 1000, value = 50),
    
    ),
        
    mainPanel(
        
        textOutput("resultat")
        
    )
)

server <- function(input, output, session) {
    
    
    output$resultat <- renderText({
        paste0("Her kommer den summerte verdien: ", (input$x + input$y) )
    })
      
}

shinyApp(ui, server)
