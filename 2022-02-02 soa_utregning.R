


# start <- 0
# end   <- 10
# end_tiltak <- 2
# tiltakskost <- 100000
# 
# tibble( ar     = seq( from = start, to = (end-1), by = 1),
#         tiltak = c( rep(   x = 1, times = (end_tiltak+1) ),
#                     rep(   x = 0, times = (end - length(c(0:end_tiltak))) )
#         )*tiltakskost
# )
# 
# 
# soa_utregning(
#     start = 24,
#     slutt = 65,
#     inntekt = 400000,
#     arb_giver_og_sosiale_kost = 0.2,
#     sosialhjelp = 130000,
#     tiltakskost = 100000,
#     r = 0.04,
#     sannsynlig_arbeid = 0.5
# )
# 
# 

##
soa_utregning <- function( start = 24, slutt = 65, inntekt = 4.566*108287, arb_giver_og_sosiale_kost , sosialhjelp,  tiltakskost, r = 0.04, sannsynlig_arbeid = 0.5 ){
    #
    
    G <- 108287
    
    start              <- start
    slutt              <- slutt
    lengde             <- length( c(start:slutt) )
    inntekt            <- inntekt/G
    arb_og_sos_avg_pst <- arb_giver_og_sosiale_kost
    # so_kost_pst <- so_kost_pst
    kost_sos_hj        <- sosialhjelp/G
    kost               <- -tiltakskost/G
    r                  <- r
    R                  <- (1/(1+r)^c(0:(lengde-1) ) )
    Prop               <- sannsynlig_arbeid
    
    # Tibble
    df <- tibble(  tid         = seq(from = 0, to = (lengde- 1), length.out = lengde),
                   R           = R,
                   inntekt     = c(0, rep(x = inntekt, times = (lengde-1) ) )*Prop,
                   arb_avg_sos_kost     = arb_og_sos_avg_pst*inntekt,
                   #soskost     = so_kost_pst*inntekt,
                   skattred    =  kost_sos_hj*0.2,
                   kost        = c( kost, rep(0, times = (lengde-1) )  )
    ) %>%
        rowwise() %>%
        # Må inkludere alle komponeneter (tibble over)
        mutate( disk_sum =  R*sum(inntekt ,arb_avg_sos_kost, kost,skattred) ) %>%
        ungroup()
    
    
    # Total gevinst diskontert
    tot_gevinst <- df %>%  summarise( d = sum(disk_sum)*G/10^6) %>% pull(d)
    #tot_gevinst
    
    df1 <- df %>%
        ungroup() %>%
        summarise(
            `Inntekt diskontert`                          =                         sum(R*inntekt )*G/10^6,
            `Arbg. og sosiale avg.` =               sum(R*arb_avg_sos_kost )*G/10^6,
            #`Sosialekostnader   diskontert`               =               sum(R*soskost )*G/10^6,
            `Tiltakskost diskontert`                      =               sum(R*kost    )*G/10^6,
            `Reduksjon i skattefinansiering`              =               sum(R*skattred)*G/10^6, 
            `Total gevinst`                               =    tot_gevinst
        )
    # 
    # 
    #df
    
    df1 %>% pivot_longer(everything(),names_to =  "Forklart", values_to = "Mill. kroner")
    #     
    
}


