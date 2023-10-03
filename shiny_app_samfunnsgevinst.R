

# App som regner ut samfunnsgevinst ved at en person kommer i arbeid fremfor uføretrygd.
# Basert på artikkel: https://www.nav.no/no/nav-og-samfunn/kunnskap/analyser-fra-nav/arbeid-og-velferd/arbeid-og-velferd/arbeid-og-velferd-nr.2-2021/mulig-samfunnsgevinst-av-arbeid-fremfor-uforetrygd


# Bibliotek
library(tidyverse)
library(shiny)
Sys.setlocale("LC_CTYPE")


# Funksjon som regner ut gevinst fra gitt inntekt
soa_utregning <- function( start = 24, slutt = 65, inntekt = 4.566*108287, arb_kost_pst, so_kost_pst, sosialhjelp,  tiltakskost, r = 0.04, sannsynlig_arbeid = 0.5 ){
    #
    start       <- start
    slutt       <- slutt
    lengde      <- length( c(start:slutt) )
    inntekt     <- inntekt/108287
    arb_avg_pst <- arb_kost_pst
    so_kost_pst <- so_kost_pst
    kost_sos_hj <- sosialhjelp/108287
    kost        <- -tiltakskost/108287
    r           <- r
    R           <- (1/(1+r)^c(0:(lengde-1) ) )
    Prop        <- sannsynlig_arbeid
    
    # Tibble
    df <- tibble(  tid         = seq(from = 0, to = (lengde- 1), length.out = lengde),
                   R           = R,
                   inntekt     = c(0, rep(x = inntekt, times = (lengde-1) ) )*Prop,
                   arb_avg     = arb_avg_pst*inntekt,
                   soskost     = so_kost_pst*inntekt,
                   skattred    =  kost_sos_hj*0.2,
                   kost        = c( kost, rep(0, times = (lengde-1) )  )
    ) %>%
        rowwise() %>%
        # Må inkludere alle komponeneter (tibble over)
        mutate( disk_sum =  R*sum(inntekt ,arb_avg, soskost, kost,skattred) ) %>%
        ungroup()
    
    
    # Total gevinst diskontert
    tot_gevinst <- df %>%  summarise( d = sum(disk_sum)*108287/10^6) %>% pull(d)
    #tot_gevinst
    
    df1 <- df %>%
        ungroup() %>%
        summarise(
            `Inntekt diskontert`            =               sum(R*inntekt)*108287/10^6,
            `Arbeidsgiveravgift diskontert` =               sum(R*arb_avg)*108287/10^6,
            `Sosialekostnader   diskontert` =               sum(R*soskost)*108287/10^6,
            `Tiltakskost diskontert`        =               sum(R*kost)*108287/10^6,
            `Reduksjon i skattefinansiering`=               sum(R*skattred)*108287/10^6, 
            `Total gevinst`                 =    tot_gevinst
        )
    # 
    # 
    #df
    
    df1 %>% pivot_longer(everything(),names_to =  "Forklart", values_to = "Mill. kroner")
    #     
    
}





ui <- fluidPage(
    titlePanel("Gevinstanalyse"),
    
    sidebarPanel( 
        
        
        numericInput("tiltaks_kost", "Tiltakskostnader :", 200000),
        
        numericInput("numeric_prop", "Sannsynlighetsrate for overgang til arbeid:", 0.5),
        
        sliderInput("numeric_inntekt", "Inntekt under ordinære betingelser", min = 2*108287, max = 6*108287, value = 4.57*108287),
        helpText("Inntekt: Gjennomsnittlig årsinntekt i 2020, for personer med grunnskole som hoyeste fullførte utdanning, var i følge SSB 493 000 kroner"),
        
        
        # Input
        sliderInput("integer_age", "Alder ved start tiltak: ", min = 20, max = 65, value = 24),
        
        numericInput("pensj_alder", "Pensjonsalder:", 65),
        
        # Include clarifying text ----
        helpText("Info: I følge Bjørnstad (2019) er gjennsnittlig pensjonsalder i Norge 65 år."),
        
        sliderInput("integer_arbGiverAvg", "Prosentandel arbeidsgiver avgfit: ", min = .10, max = .30, value = 0.2),
        
        sliderInput("integer_sosialeKost", "Prosent sosiale kostnader: ", min = .05, max = 0.15, value = .13),
        
        numericInput("numeric_soshjelp", "Gjennomsnittlig årlig økonomisk sosialhjelp (for mottakere) i 2020, var 130 000 kroner.", value = 130000),
        
        sliderInput("integer_r", "Diskonteringsrente: ", min = 0, max = 0.08, value = 0.04)
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        # Output: Table summarizing the values entered ----
        tableOutput("values"),
        
        # Output: Grafisk presentasjon av gevinstene ----
        plotOutput("graf")
        
    )
)



server <- function(input, output, session) {
    
    sliderValues <- reactive({
        
        soa_utregning(start       = input$integer_age, slutt = input$pensj_alder,
                      inntekt     = input$numeric_inntekt,
                      arb_kost_pst = input$integer_arbGiverAvg ,
                      so_kost_pst = input$integer_sosialeKost,
                      tiltakskost = input$tiltaks_kost,
                      r           = input$integer_r,
                      sannsynlig_arbeid = input$numeric_prop,
                      sosialhjelp = input$numeric_soshjelp
        )
        
    })
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
        sliderValues()
    })
    
    # # Grafisk framstilling ----
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
            ylim( c( ifelse( -input$tiltaks_kost*2 > -3,-input$tiltaks_kost,-3  ) ,10)) +
            theme( legend.position = "none", 
                   axis.text.x = element_text(  angle = 90,
                                                size = 16
                   )
            ) +
            labs( x = NULL, y = "Mill. kroner")
        
    })
    
}

shinyApp(ui, server)




