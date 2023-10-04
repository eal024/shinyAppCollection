# shinyAppCollection

Dette prosjektet inneholer noen eksempler på shiny-apper. Data er fra åpne kilder.   

###  Eksempel 1: Utregning av samfunnsgevinst

Dette er en kalkulator som regner ut samfunnsgevinst fra at en person kommer i arbeid, fremfor å havne på uføretrygd. Kalkulateren baserer seg paa en  artikkel i [Arbeid og velferd:](https://www.nav.no/no/nav-og-samfunn/kunnskap/analyser-fra-nav/arbeid-og-velferd/arbeid-og-velferd/arbeid-og-velferd-nr.2-2021/mulig-samfunnsgevinst-av-arbeid-fremfor-uforetrygd).<br>   

Lenker:<br>
Her er [appen](https://eirik-andre-lamy.shinyapps.io/shinyAppCollection/)<br>og her er [R-koden](https://github.com/eal024/shinyAppCollection/blob/main/shiny_app_samfunnsgevinst.R).<br>
Basert på artikkelen i [Arbeid og velferd:](https://www.nav.no/no/nav-og-samfunn/kunnskap/analyser-fra-nav/arbeid-og-velferd/arbeid-og-velferd/arbeid-og-velferd-nr.2-2021/mulig-samfunnsgevinst-av-arbeid-fremfor-uforetrygd)<br> 

###  Eksempel 2: Linjegraf med nedtrekksvalg

I forbindelse med [Navs Omverdensanalyse](https://data.nav.no/omverdensanalyse/index.html) presenterte vi dataene med interaktive figurer. Dette med bruk av [Highchart](https://www.highcharts.com/) og Quarto. Vi forsøkte å legge til et par ytterligere funksjoner, men fikk ikke løst utfordringer med intern server (shinyapps.io-ble ikke godkjent til bruk). Appen hvor man kunne velge befolkningsframskrivningen (til SSB) for by og fylke, ble derfor ikke med i nettversjonen.         

[Her er appen -- slik den ville ha fungert hvis vi hadde tatt den med. Satser paa at den er med til neste ar](https://eirik-andre-lamy.shinyapps.io/omverdensanalyse/)

Lenker:<br>
Her er [appen](https://eirik-andre-lamy.shinyapps.io/omverdensanalyse/)<br>og her er [R-koden](https://github.com/eal024/shinyAppCollection/blob/main/shiny_app_oma_demografi.R).<br>
