



library(tidyverse)


# inntekt = did + inntekt som ufør
samfunnsgevinst <-  bind_rows(
    sos_gevinst( inntekt = (2.23+0.186),  inntekt_uft = 0.186 ,    vekst_inntekt = 0.00, vekst_antall_ar   = 10, gj_alder = 25.4 , pensjonsalder = 65, kategori_navn = "Jobb, 20-29 år"),
    sos_gevinst( inntekt = (2.525+0.515), inntekt_uft = 0.515 ,    vekst_inntekt = 0.00, vekst_antall_ar   = 10, gj_alder = 46.6 , pensjonsalder = 65, kategori_navn = "Jobb, alle") #,
    # sos_gevinst( inntekt = (0.64+0.186),  inntekt_uft = 0.186 ,    vekst_inntekt = 0.00, vekst_antall_ar   = 10, gj_alder = 26 ,   pensjonsalder = 65, kategori_navn = "avslag uføre, 20-29 år"),
    # sos_gevinst( inntekt = (1.1+0.552),   inntekt_uft = 0.552 ,    vekst_inntekt = 0.00, vekst_antall_ar   = 10, gj_alder = 43 ,   pensjonsalder = 65  ,   kategori_navn = "avslag uføre, alle")
)

samfunnsgevinst %>% filter( category == "totalt")

soa_wf <- samfunnsgevinst %>% filter( category != "totalt") #%>% mutate( value = value*100853)
#  ------------------------------------------------------------------------

soa_wf %>% distinct( k)

# Case 1: -----------------------------------------------------------------

# Gi levels til faktor
soa_wf_0 <-  soa_wf[,1:3] %>% filter( k %in% c("Jobb, 20-29 år"))

soa_wf_0 <- soa_wf_0 %>% add_row(soa_wf_0 %>% group_by(k) %>% summarise( category = "totalt", value = -sum(value), .groups = "drop"))

levels <- soa_wf_0$category

soa_wf_facet <- soa_wf[,1:3] %>%
    add_row(soa_wf[,1:3] %>% group_by(k) %>% summarise( category = "totalt", value = -sum(value), .groups = "drop")) %>% 
    arrange( k)

# Sum 0
soa_wf_0 %>% summarise( round(sum(value),-1))


# Datapasting
# soa_wf_facet %>% distinct(k) %>% pull() %>% datapasta::vector_paste()
level_k <- c("Jobb, alle", "Jobb, 20-29 år" #, "avslag uføre, alle", "avslag uføre, 20-29 år" 
)


soa_wf_facet_1 <- soa_wf_facet %>%
    # ta med paste::lag_vector
    mutate( k = factor(k, levels = level_k ),
            k_organisere = k,
            # Omgjøre value til grunnbeløp
            value = value*100000/10^6
    )%>%
    mutate( ) %>% 
    mutate( kat_2 = case_when( str_detect(str_to_lower(k) ,"avslag") ~ "Avslag uføre",
                               T ~ "jobb") %>% as.factor() %>% fct_rev() ) %>% 
    # filter( k == "20 år") %>% 
    group_by( k) %>% 
    mutate( category = factor(category, levels = levels),
            ymin = round( cumsum(value), 3),
            ymax = lag(cumsum(value), default = 0),
            xmin = c(head(category, -1), NA),
            xmax = c(tail(category, -1), NA),
            impact = ifelse( category %in% c(
                # as.character(df$category[1]),
                as.character( soa_wf_facet$category[nrow(soa_wf_facet)])),
                "totalt",
                ifelse( value > 0, "Pluss", "Minus")) ) %>% 
    mutate( k = str_c( k, "\n",
                       glue::glue("Totalgevinst: { format( -round( value[category == 'totalt'],2), digits = 3) } mill.kr")  )) %>% 
    
    # mutate( k = str_c( k, "\n",
    #                    glue::glue("Totalgevinst: { format( -round( value[category == 'totalt'],0)/10^6, digits = 2) } mill.")  )) %>% 
    mutate( bidrag =  ifelse(  category != "totalt", (ymin-ymax)  ,0) ) %>% 
    filter( ! is.na(value)) 

soa_wf_facet_1

graf <- soa_wf_facet_1 %>% 
    mutate( k =   k %>% as.factor() %>% fct_rev()   ) %>% 
    ggplot( ) +
    theme_bw() +
    geom_rect( aes( xmin = as.integer(category) - w/2,
                    xmax = as.integer(category) +w/2,
                    ymin = ymin,
                    ymax = ymax,
                    fill = impact),
               colour = "black",alpha = 0.9
    ) +
    scale_x_discrete(limits = levels ) +
    scale_fill_manual(values = (c("Minus" = "blue", "Pluss" = "orange", "totalt" = "black"))) +
    facet_wrap( ~ fct_rev(k), nrow =1)

graf
w <- 0.8
snitt <- -round(soa_wf_facet_1$value[soa_wf_facet_1$category == "totalt" & str_detect(soa_wf_facet_1$k, "alle") & !str_detect(soa_wf_facet_1$k, "avslag") ],1)

windowsFonts(Times=windowsFont("TT Times New Roman"))

graf_endelig <- graf +
    geom_segment(data = soa_wf_facet_1[1:(nrow(soa_wf_facet_1) -1),],aes(x = xmin,
                                                                         xend = xmax,
                                                                         y = ymin,
                                                                         yend = ymin)) +
    coord_cartesian( ylim= c(0, max(soa_wf_facet_1$ymax)+ .025 ) ) +
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_text( 
        data = soa_wf_facet_1,
        aes(y = ymin, x = category ,
            label = ifelse( bidrag == 0, "", format(round( bidrag ,2)/1, digits = 1, scientific = F)  ) ,vjust = ifelse(value < 0, +1,-0.1) ),
        hjust = .5, color = "black", size = 3.5
    ) +
    #facet_grid(  kat_2 ~ k, nrow =  2 ) +
    theme_light() +
    theme(legend.position = "none",
          panel.grid.major.x =  element_blank(), 
          panel.grid.minor.x =   element_blank(),
          strip.text = element_text( color = "black", family = "Times", size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95,  family =  "Times", size = 14,face = "bold" )
    ) +
    labs( y = "Mill. kroner", x = "")  +
    annotate( "point", x = "totalt", y = snitt, colour = "red", size = 2.5) +
    #coord_cartesian(ylim = c(0, 7*10^1) ) +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = 20) ) +
    scale_y_continuous( labels = function(x) format(x/1, digits = 0 ),
                        #breaks = seq(from = 0, to = 7*10^6, by = 2*10^6)
    )

graf_endelig
#ggsave( plot =  graf_endelig, device = "svg", path = "plot", width = 9, height = 5, "waterfall_test.svg")

# PP
ggsave( plot =  graf_endelig, device = "png", path = "plot/pp", width = 5.5, height = 6, "waterfall_jobb_gruppen.png")

