library(shiny)
library(shinydashboard)
library(httr)
library(dplyr)
library(tidyr)
library(plotly)
library(incidence)
library(EpiEstim)

#Function
## Get The Data
covid_data <- function(response){
    cov_raw <- content(response, as='parsed', simplifyVector = TRUE)
    cov <- cov_raw$list_perkembangan
    new_cov <- 
        cov %>%
        select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
        select(-starts_with("AKUMULASI")) %>% 
        dplyr::rename(
            kasus_baru = KASUS,
            meninggal = MENINGGAL,
            sembuh = SEMBUH
        ) %>%
        dplyr::mutate(
            tanggal = as.POSIXct(tanggal/1000, origin = '1970-01-01'),
            tanggal = as.Date(tanggal)
        )
    return(new_cov)
}

#Make Incidence Object
i.prov <- function(data_covid){
    data_covid2 <- data_covid %>%
        uncount(kasus_baru)
    return(incidence(data_covid2$tanggal))
}

#Database
##DKI Jakarta
link_DKI <- 'https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json'
resp_DKI <- GET(link_DKI)
DKI <- covid_data(resp_DKI)
i.DKI <- i.prov(DKI)

##Jawa Barat
link_jabar <- 'https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json'
resp_jabar <- GET(link_jabar)
jabar <- covid_data(resp_jabar)
i.jabar <- i.prov(jabar)

##Jawa Tengah
link_jateng <- 'https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json'
resp_jateng <- GET(link_jateng)
jateng <- covid_data(resp_jateng)
i.jateng <- i.prov(jateng)

##Jawa Timur
link_jatim <- 'https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json'
resp_jatim <- GET(link_jatim)
jatim <- covid_data(resp_jatim)
i.jatim <- i.prov(jatim)

##BALI
link_bali <- 'https://data.covid19.go.id/public/api/prov_detail_BALI.json'
resp_bali <- GET(link_bali)
bali <- covid_data(resp_bali)
i.bali <- i.prov(bali)

##BANTEN
link_banten <- 'https://data.covid19.go.id/public/api/prov_detail_BANTEN.json'
resp_banten <- GET(link_banten)
banten <- covid_data(resp_banten)
i.banten <- i.prov(banten)

##DIY
link_DIY <- 'https://data.covid19.go.id/public/api/prov_detail_DAERAH_ISTIMEWA_YOGYAKARTA.json'
resp_DIY <- GET(link_DIY)
DIY <- covid_data(resp_DIY)
i.DIY <- i.prov(DIY)

##NTB
link_NTB <- 'https://data.covid19.go.id/public/api/prov_detail_NUSA_TENGGARA_BARAT.json'
resp_NTB <- GET(link_NTB)
NTB <- covid_data(resp_NTB)
i.NTB <- i.prov(NTB)

##NTT
link_NTT <- 'https://data.covid19.go.id/public/api/prov_detail_NUSA_TENGGARA_TIMUR.json'
resp_NTT <- GET(link_NTT)
NTT <- covid_data(resp_NTT)
i.NTT <- i.prov(NTT)

##ACEH
link_aceh <- 'https://data.covid19.go.id/public/api/prov_detail_ACEH.json'
resp_aceh <- GET(link_aceh)
aceh <- covid_data(resp_aceh)
i.aceh <- i.prov(aceh)

##Sumatera Barat
link_sumbar <- 'https://data.covid19.go.id/public/api/prov_detail_SUMATERA_BARAT.json'
resp_sumbar <- GET(link_sumbar)
sumbar <- covid_data(resp_sumbar)
i.sumbar <- i.prov(sumbar)

##Sumatera Utara
link_sumut <- 'https://data.covid19.go.id/public/api/prov_detail_SUMATERA_UTARA.json'
resp_sumut <- GET(link_sumut)
sumut <- covid_data(resp_sumut)
i.sumut <- i.prov(sumut)

##Riau
link_riau <- 'https://data.covid19.go.id/public/api/prov_detail_RIAU.json'
resp_riau <- GET(link_riau)
riau <- covid_data(resp_riau)
i.riau <- i.prov(riau)

##Kepulauan Riau
link_kepri <- 'https://data.covid19.go.id/public/api/prov_detail_KEPULAUAN_RIAU.json'
resp_kepri <- GET(link_kepri)
kepri <- covid_data(resp_kepri)
i.kepri <- i.prov(kepri)

##Jambi
link_jambi <- 'https://data.covid19.go.id/public/api/prov_detail_JAMBI.json'
resp_jambi <- GET(link_jambi)
jambi <- covid_data(resp_jambi)
i.jambi <- i.prov(jambi)

##Bengkulu
link_bengkulu <- 'https://data.covid19.go.id/public/api/prov_detail_BENGKULU.json'
resp_bengkulu <- GET(link_bengkulu)
bengkulu <- covid_data(resp_bengkulu)
i.bengkulu <- i.prov(bengkulu)

##Sumatera Selatan
link_sumsel <- 'https://data.covid19.go.id/public/api/prov_detail_SUMATERA_SELATAN.json'
resp_sumsel <- GET(link_sumsel)
sumsel <- covid_data(resp_sumsel)
i.sumsel <- i.prov(sumsel)

##Kepulauan Bangka Belitung
link_babel <- 'https://data.covid19.go.id/public/api/prov_detail_KEPULAUAN_BANGKA_BELITUNG.json'
resp_babel <- GET(link_babel)
babel <- covid_data(resp_babel)
i.babel <- i.prov(babel)

##Lampung
link_lampung <- 'https://data.covid19.go.id/public/api/prov_detail_LAMPUNG.json'
resp_lampung <- GET(link_lampung)
lampung <- covid_data(resp_lampung)
i.lampung <- i.prov(lampung)

##Kalimantan Barat
link_kalbar <- 'https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_BARAT.json'
resp_kalbar <- GET(link_kalbar)
kalbar <- covid_data(resp_kalbar)
i.kalbar <- i.prov(kalbar)

##Kalimantan Selatan
link_kalsel <- 'https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_SELATAN.json'
resp_kalsel <- GET(link_kalsel)
kalsel <- covid_data(resp_kalsel)
i.kalsel <- i.prov(kalsel)

##Kalimantan Tengah
link_kalteng <- 'https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TENGAH.json'
resp_kalteng <- GET(link_kalteng)
kalteng <- covid_data(resp_kalteng)
i.kalteng <- i.prov(kalteng)

##Kalimantan Timur
link_kaltim <- 'https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TIMUR.json'
resp_kaltim <- GET(link_kaltim)
kaltim <- covid_data(resp_kaltim)
i.kaltim <- i.prov(kaltim)

##Kalimantan Utara
link_kalut <- 'https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_UTARA.json'
resp_kalut <- GET(link_kalut)
kalut <- covid_data(resp_kalut)
i.kalut <- i.prov(kalut)

##Gorontalo
link_gorontalo <- 'https://data.covid19.go.id/public/api/prov_detail_GORONTALO.json'
resp_gorontalo <- GET(link_gorontalo)
gorontalo <- covid_data(resp_gorontalo)
i.gorontalo <- i.prov(gorontalo)

##Sulawesi Barat
link_sulbar <- 'https://data.covid19.go.id/public/api/prov_detail_SULAWESI_BARAT.json'
resp_sulbar <- GET(link_sulbar)
sulbar <- covid_data(resp_sulbar)
i.sulbar <- i.prov(sulbar)

##Sulawesi Selatan
link_sulsel <- 'https://data.covid19.go.id/public/api/prov_detail_SULAWESI_SELATAN.json'
resp_sulsel <- GET(link_sulsel)
sulsel <- covid_data(resp_sulsel)
i.sulsel <- i.prov(sulsel)

##Sulawesi Tenggara
link_sultra <- 'https://data.covid19.go.id/public/api/prov_detail_SULAWESI_TENGGARA.json'
resp_sultra <- GET(link_sultra)
sultra <- covid_data(resp_sultra)
i.sultra <- i.prov(sultra)

##Sulawesi Tengah
link_sulteng <- 'https://data.covid19.go.id/public/api/prov_detail_SULAWESI_TENGAH.json'
resp_sulteng <- GET(link_sulteng)
sulteng <- covid_data(resp_sulteng)
i.sulteng <- i.prov(sulteng)

##Sulawesi Utara
link_sulut <- 'https://data.covid19.go.id/public/api/prov_detail_SULAWESI_UTARA.json'
resp_sulut <- GET(link_sulut)
sulut <- covid_data(resp_sulut)
i.sulut <- i.prov(sulut)

##Maluku
link_maluku <- 'https://data.covid19.go.id/public/api/prov_detail_MALUKU.json'
resp_maluku <- GET(link_maluku)
maluku <- covid_data(resp_maluku)
i.maluku <- i.prov(maluku)

##Maluku Utara
link_malut <- 'https://data.covid19.go.id/public/api/prov_detail_MALUKU_UTARA.json'
resp_malut <- GET(link_malut)
malut <- covid_data(resp_malut)
i.malut <- i.prov(malut)

##Papua
link_papua <- 'https://data.covid19.go.id/public/api/prov_detail_PAPUA.json'
resp_papua <- GET(link_papua)
papua <- covid_data(resp_papua)
i.papua <- i.prov(papua)

##Papua Barat
link_pabar <- 'https://data.covid19.go.id/public/api/prov_detail_PAPUA_BARAT.json'
resp_pabar <- GET(link_pabar)
pabar <- covid_data(resp_pabar)
i.pabar <- i.prov(pabar)


##Shiny Dashboard
header <- dashboardHeader(title = strong('SIAR COVID : Sistem Informasi Angka Reproduksi COVID-19 Indonesia'), titleWidth = 750)
sidebar <- dashboardSidebar(disable = T
)
body <- dashboardBody(
    fluidRow(
        box(width = 4,
            title = 'Pilih Provinsi',
            status = 'primary',
            selectInput(inputId = 'prov',
                        label = '',
                        choices = c('DKI Jakarta', 'Jawa Barat', 'Jawa Tengah', 'Jawa Timur', 'Bali', 
                                    'Banten', 'DI Yogyakarta', 'Nusa Tenggara Timur', 'Nusa Tenggara Barat', 'Nangroe Aceh Darussalam',
                                    'Sumatera Barat', 'Sumatera Utara', 'Riau', 'Kepulauan Riau', 'Jambi',
                                    'Bengkulu', 'Sumatera Selatan', 'Kepulauan Bangka Belitung', 'Lampung',
                                    'Kalimantan Barat', 'Kalimantan Selatan', 'Kalimantan Tengah', 'Kalimantan Timur', 'Kalimantan Utara',
                                    'Gorontalo', 'Sulawesi Barat', 'Sulawesi Selatan', 'Sulawesi Tenggara', 'Sulawesi Tengah', 'Sulawesi Utara',
                                    'Maluku', 'Maluku Utara', 'Papua', 'Papua Barat'),
                        selected = 'DKI Jakarta',
                        multiple = F)),
        box(width = 8,
            title = "Distribusi Serial Interval pada Model Cori",
            status = 'primary',
            column(width = 5,
                   helpText('Note : Harap berkonsultasi dengan ahli saat mengisinya. Default parameter menggunakan penelitian Li et al, 2020 di Tiongkok.')
            ),
            column(width = 3,
                   numericInput(inputId = 'mean_si',
                                label = 'SI Mean',
                                value = 7.5,
                                min = 0,
                                max = 10)
            ),
            column(width = 3,
                   numericInput(inputId = 'std_si',
                                label = 'SI Standard Deviation',
                                value = 3.4,
                                min = 0,
                                max = 10))
            ),
    ),
    fluidRow(
        box(width = 12, title = textOutput('title'),
            valueBoxOutput('kasus', width = 3),
            valueBoxOutput('meninggal', width = 3),
            valueBoxOutput('sembuh', width = 3),
            valueBoxOutput('rt', width = 3))
    ),
    fluidRow(
        box(title = 'Perkembangan Kasus COVID-19', solidHeader = T, plotlyOutput(outputId = "Plot1")),
        box(title = 'Perkembangan Angka Reproduksi Rt', solidHeader = T, plotlyOutput(outputId = "Plot2"))
    ),
    fluidRow(
        infoBoxOutput('saran', width = 12)
    )
)

# Create the UI using the header, sidebar, and body
ui <- tagList(dashboardPage(header, sidebar, body, skin = 'black'))

server <- function(input, output) {
    output$Plot1 <- renderPlotly({
        if(input$prov == 'Jawa Barat'){
            data_p1 <- jabar
        }else if(input$prov == 'DKI Jakarta'){
            data_p1 <- DKI
        }else if(input$prov == 'Jawa Tengah'){
            data_p1 <- jateng 
        }else if(input$prov == 'Jawa Timur'){
            data_p1 <- jatim
        }else if(input$prov == 'Bali'){
            data_p1 <- bali    
        }else if(input$prov == 'Banten'){
            data_p1 <- banten
        }else if(input$prov == 'DI Yogyakarta'){
            data_p1 <- DIY
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p1 <- NTT
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p1 <- NTB
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p1 <- aceh
        }else if(input$prov == 'Sumatera Barat'){
            data_p1 <- sumbar
        }else if(input$prov == 'Sumatera Utara'){
            data_p1 <- sumut
        }else if(input$prov == 'Riau'){
            data_p1 <- riau
        }else if(input$prov == 'Kepulauan Riau'){
            data_p1 <- kepri
        }else if(input$prov == 'Jambi'){
            data_p1 <- jambi
        }else if(input$prov == 'Bengkulu'){
            data_p1 <- bengkulu
        }else if(input$prov == 'Sumatera Selatan'){
            data_p1 <- sumsel
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p1 <- babel
        }else if(input$prov == 'Lampung'){
            data_p1 <- lampung
        }else if(input$prov == 'Kalimantan Barat'){
            data_p1 <- kalbar
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p1 <- kalsel
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p1 <- kalteng
        }else if(input$prov == 'Kalimantan Timur'){
            data_p1 <- kaltim
        }else if(input$prov == 'Kalimantan Utara'){
            data_p1 <- kalut
        }else if(input$prov == 'Gorontalo'){
            data_p1 <- gorontalo
        }else if(input$prov == 'Sulawesi Barat'){
            data_p1 <- sulbar
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p1 <- sulsel
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p1 <- sultra
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p1 <- sulteng
        }else if(input$prov == 'Sulawesi Utara'){
            data_p1 <- sulut
        }else if(input$prov == 'Maluku'){
            data_p1 <- maluku
        }else if(input$prov == 'Maluku Utara'){
            data_p1 <- malut
        }else if(input$prov == 'Papua'){
            data_p1 <- papua
        }else if(input$prov == 'Papua Barat'){
            data_p1 <- pabar
        }    
            
        p1 <- plot_ly(data_p1, x = ~tanggal, y = ~kasus_baru, name = 'Kasus Baru', type = 'scatter', mode = 'lines',
                      line = list(color = 'deepskyblue3', width = 2))
        p1 <- p1 %>% add_trace(y = ~meninggal, name = 'Kasus Meninggal', type = 'scatter', mode = 'lines',
                               line = list(color = 'firebrick3', width = 2))
        p1 <- p1 %>% add_trace(y = ~sembuh, name = 'Kasus Sembuh', type = 'scatter', mode = 'lines',
                               line = list(color = 'aquamarine3', width = 2))
        p1 <- p1 %>% layout(xaxis = list(title = "Tanggal"),
                            yaxis = list (title = "Jumlah"),
                            hovermode = 'x unified')
        p1
    })
    
    output$Plot2 <- renderPlotly({
        if(input$prov == 'Jawa Barat'){
            data_p2 <- i.jabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DKI Jakarta'){
            data_p2 <- i.DKI
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Tengah'){
            data_p2 <- i.jateng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Timur'){
            data_p2 <- i.jatim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bali'){
            data_p2 <- i.bali
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Banten'){
            data_p2 <- i.banten
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DI Yogyakarta'){
            data_p2 <- i.DIY
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p2 <- i.NTT
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p2 <- i.NTB
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p2 <- i.aceh
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Barat'){
            data_p2 <- i.sumbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13
        }else if(input$prov == 'Sumatera Utara'){
            data_p2 <- i.sumut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Riau'){
            data_p2 <- i.riau
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Riau'){
            data_p2 <- i.kepri
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jambi'){
            data_p2 <- i.jambi
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bengkulu'){
            data_p2 <- i.bengkulu
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Selatan'){
            data_p2 <- i.sumsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p2 <- i.babel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Lampung'){
            data_p2 <- i.lampung
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Barat'){
            data_p2 <- i.kalbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p2 <- i.kalsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p2 <- i.kalteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Timur'){
            data_p2 <- i.kaltim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Utara'){
            data_p2 <- i.kalut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Gorontalo'){
            data_p2 <- i.gorontalo
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Barat'){
            data_p2 <- i.sulbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p2 <- i.sulsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p2 <- i.sultra
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p2 <- i.sulteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Utara'){
            data_p2 <- i.sulut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku'){
            data_p2 <- i.maluku
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku Utara'){
            data_p2 <- i.malut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua'){
            data_p2 <- i.papua
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua Barat'){
            data_p2 <- i.pabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }
        
        prov_estimate <- estimate_R(data_p2, method = 'parametric_si', config = 
                                        make_config(list(
                                            mean_si = input$mean_si,
                                            std_si = input$std_si,
                                            t_start = t_start,
                                            t_end = t_end
                                        )))
        rt <- data.frame(tanggal = prov_estimate$dates[15:length(prov_estimate$dates)],
                         kasus = prov_estimate$R$`Mean(R)`,
                         lower = prov_estimate$R$`Quantile.0.025(R)`,
                         upper = prov_estimate$R$`Quantile.0.975(R)`)
        
        hline <- function(y = 1, color = "orange") {
            list(
                type = "line", 
                x0 = 0, 
                x1 = 1, 
                xref = "paper",
                y0 = y, 
                y1 = y, 
                line = list(color = color)
            )
        }
        
        p2 <- plot_ly(rt, x = ~tanggal, y = ~upper, type = 'scatter', mode = 'lines',
                      line = list(color = 'transparent'),
                      showlegend = FALSE, name = 'Upper 95% CI') 
        p2 <- p2 %>% add_trace(y = ~lower, type = 'scatter', mode = 'lines',
                               fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                               showlegend = FALSE, name = 'Lower 95% CI') 
        p2 <- p2 %>% add_trace(x = ~tanggal, y = ~kasus, type = 'scatter', mode = 'lines',
                               line = list(color='rgb(0,100,80)'),
                               name = 'Average') 
        p2 <- p2 %>% layout(
                            xaxis = list(title = "Waktu"),
                            yaxis = list(title = "Nilai Rt"),
                            shapes = hline(1))
        
    })
    
    output$kasus <- renderValueBox({
        if(input$prov == 'Jawa Barat'){
            data_p1 <- jabar
        }else if(input$prov == 'DKI Jakarta'){
            data_p1 <- DKI
        }else if(input$prov == 'Jawa Tengah'){
            data_p1 <- jateng 
        }else if(input$prov == 'Jawa Timur'){
            data_p1 <- jatim
        }else if(input$prov == 'Bali'){
            data_p1 <- bali    
        }else if(input$prov == 'Banten'){
            data_p1 <- banten
        }else if(input$prov == 'DI Yogyakarta'){
            data_p1 <- DIY
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p1 <- NTT
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p1 <- NTB
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p1 <- aceh
        }else if(input$prov == 'Sumatera Barat'){
            data_p1 <- sumbar
        }else if(input$prov == 'Sumatera Utara'){
            data_p1 <- sumut
        }else if(input$prov == 'Riau'){
            data_p1 <- riau
        }else if(input$prov == 'Kepulauan Riau'){
            data_p1 <- kepri
        }else if(input$prov == 'Jambi'){
            data_p1 <- jambi
        }else if(input$prov == 'Bengkulu'){
            data_p1 <- bengkulu
        }else if(input$prov == 'Sumatera Selatan'){
            data_p1 <- sumsel
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p1 <- babel
        }else if(input$prov == 'Lampung'){
            data_p1 <- lampung
        }else if(input$prov == 'Kalimantan Barat'){
            data_p1 <- kalbar
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p1 <- kalsel
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p1 <- kalteng
        }else if(input$prov == 'Kalimantan Timur'){
            data_p1 <- kaltim
        }else if(input$prov == 'Kalimantan Utara'){
            data_p1 <- kalut
        }else if(input$prov == 'Gorontalo'){
            data_p1 <- gorontalo
        }else if(input$prov == 'Sulawesi Barat'){
            data_p1 <- sulbar
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p1 <- sulsel
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p1 <- sultra
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p1 <- sulteng
        }else if(input$prov == 'Sulawesi Utara'){
            data_p1 <- sulut
        }else if(input$prov == 'Maluku'){
            data_p1 <- maluku
        }else if(input$prov == 'Maluku Utara'){
            data_p1 <- malut
        }else if(input$prov == 'Papua'){
            data_p1 <- papua
        }else if(input$prov == 'Papua Barat'){
            data_p1 <- pabar
        }    
        
        valueBox(data_p1$kasus_baru[length(data_p1$kasus_baru)], 'Penambahan Kasus', icon = icon('plus-circle'), color = 'blue')
    })
    
    output$meninggal <- renderValueBox({
        if(input$prov == 'Jawa Barat'){
            data_p1 <- jabar
        }else if(input$prov == 'DKI Jakarta'){
            data_p1 <- DKI
        }else if(input$prov == 'Jawa Tengah'){
            data_p1 <- jateng 
        }else if(input$prov == 'Jawa Timur'){
            data_p1 <- jatim
        }else if(input$prov == 'Bali'){
            data_p1 <- bali    
        }else if(input$prov == 'Banten'){
            data_p1 <- banten
        }else if(input$prov == 'DI Yogyakarta'){
            data_p1 <- DIY
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p1 <- NTT
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p1 <- NTB
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p1 <- aceh
        }else if(input$prov == 'Sumatera Barat'){
            data_p1 <- sumbar
        }else if(input$prov == 'Sumatera Utara'){
            data_p1 <- sumut
        }else if(input$prov == 'Riau'){
            data_p1 <- riau
        }else if(input$prov == 'Kepulauan Riau'){
            data_p1 <- kepri
        }else if(input$prov == 'Jambi'){
            data_p1 <- jambi
        }else if(input$prov == 'Bengkulu'){
            data_p1 <- bengkulu
        }else if(input$prov == 'Sumatera Selatan'){
            data_p1 <- sumsel
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p1 <- babel
        }else if(input$prov == 'Lampung'){
            data_p1 <- lampung
        }else if(input$prov == 'Kalimantan Barat'){
            data_p1 <- kalbar
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p1 <- kalsel
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p1 <- kalteng
        }else if(input$prov == 'Kalimantan Timur'){
            data_p1 <- kaltim
        }else if(input$prov == 'Kalimantan Utara'){
            data_p1 <- kalut
        }else if(input$prov == 'Gorontalo'){
            data_p1 <- gorontalo
        }else if(input$prov == 'Sulawesi Barat'){
            data_p1 <- sulbar
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p1 <- sulsel
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p1 <- sultra
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p1 <- sulteng
        }else if(input$prov == 'Sulawesi Utara'){
            data_p1 <- sulut
        }else if(input$prov == 'Maluku'){
            data_p1 <- maluku
        }else if(input$prov == 'Maluku Utara'){
            data_p1 <- malut
        }else if(input$prov == 'Papua'){
            data_p1 <- papua
        }else if(input$prov == 'Papua Barat'){
            data_p1 <- pabar
        }    
        
        valueBox(data_p1$meninggal[length(data_p1$meninggal)], 'Kasus Meninggal', icon = icon('minus-circle'), color = 'orange')
    })
    
    output$sembuh <- renderValueBox({
        if(input$prov == 'Jawa Barat'){
            data_p1 <- jabar
        }else if(input$prov == 'DKI Jakarta'){
            data_p1 <- DKI
        }else if(input$prov == 'Jawa Tengah'){
            data_p1 <- jateng 
        }else if(input$prov == 'Jawa Timur'){
            data_p1 <- jatim
        }else if(input$prov == 'Bali'){
            data_p1 <- bali    
        }else if(input$prov == 'Banten'){
            data_p1 <- banten
        }else if(input$prov == 'DI Yogyakarta'){
            data_p1 <- DIY
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p1 <- NTT
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p1 <- NTB
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p1 <- aceh
        }else if(input$prov == 'Sumatera Barat'){
            data_p1 <- sumbar
        }else if(input$prov == 'Sumatera Utara'){
            data_p1 <- sumut
        }else if(input$prov == 'Riau'){
            data_p1 <- riau
        }else if(input$prov == 'Kepulauan Riau'){
            data_p1 <- kepri
        }else if(input$prov == 'Jambi'){
            data_p1 <- jambi
        }else if(input$prov == 'Bengkulu'){
            data_p1 <- bengkulu
        }else if(input$prov == 'Sumatera Selatan'){
            data_p1 <- sumsel
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p1 <- babel
        }else if(input$prov == 'Lampung'){
            data_p1 <- lampung
        }else if(input$prov == 'Kalimantan Barat'){
            data_p1 <- kalbar
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p1 <- kalsel
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p1 <- kalteng
        }else if(input$prov == 'Kalimantan Timur'){
            data_p1 <- kaltim
        }else if(input$prov == 'Kalimantan Utara'){
            data_p1 <- kalut
        }else if(input$prov == 'Gorontalo'){
            data_p1 <- gorontalo
        }else if(input$prov == 'Sulawesi Barat'){
            data_p1 <- sulbar
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p1 <- sulsel
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p1 <- sultra
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p1 <- sulteng
        }else if(input$prov == 'Sulawesi Utara'){
            data_p1 <- sulut
        }else if(input$prov == 'Maluku'){
            data_p1 <- maluku
        }else if(input$prov == 'Maluku Utara'){
            data_p1 <- malut
        }else if(input$prov == 'Papua'){
            data_p1 <- papua
        }else if(input$prov == 'Papua Barat'){
            data_p1 <- pabar
        }    
        
        valueBox(data_p1$sembuh[length(data_p1$sembuh)], 'Kasus Sembuh', icon = icon('heart'), color = 'olive')
    })
    
    output$rt <- renderValueBox({
        if(input$prov == 'Jawa Barat'){
            data_p2 <- i.jabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DKI Jakarta'){
            data_p2 <- i.DKI
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Tengah'){
            data_p2 <- i.jateng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Timur'){
            data_p2 <- i.jatim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bali'){
            data_p2 <- i.bali
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Banten'){
            data_p2 <- i.banten
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DI Yogyakarta'){
            data_p2 <- i.DIY
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p2 <- i.NTT
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p2 <- i.NTB
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p2 <- i.aceh
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Barat'){
            data_p2 <- i.sumbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13
        }else if(input$prov == 'Sumatera Utara'){
            data_p2 <- i.sumut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Riau'){
            data_p2 <- i.riau
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Riau'){
            data_p2 <- i.kepri
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jambi'){
            data_p2 <- i.jambi
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bengkulu'){
            data_p2 <- i.bengkulu
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Selatan'){
            data_p2 <- i.sumsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p2 <- i.babel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Lampung'){
            data_p2 <- i.lampung
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Barat'){
            data_p2 <- i.kalbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p2 <- i.kalsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p2 <- i.kalteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Timur'){
            data_p2 <- i.kaltim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Utara'){
            data_p2 <- i.kalut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Gorontalo'){
            data_p2 <- i.gorontalo
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Barat'){
            data_p2 <- i.sulbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p2 <- i.sulsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p2 <- i.sultra
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p2 <- i.sulteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Utara'){
            data_p2 <- i.sulut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku'){
            data_p2 <- i.maluku
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku Utara'){
            data_p2 <- i.malut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua'){
            data_p2 <- i.papua
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua Barat'){
            data_p2 <- i.pabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }
        
        prov_estimate <- estimate_R(data_p2, method = 'parametric_si', config = 
                                        make_config(list(
                                            mean_si = input$mean_si,
                                            std_si = input$std_si,
                                            t_start = t_start,
                                            t_end = t_end
                                        )))
        valueBox(round(prov_estimate$R$`Mean(R)`[length(prov_estimate$R$`Mean(R)`)],2), 'Nilai Rt', icon = icon('users'), color = 'yellow')
    })
    
    output$saran <- renderInfoBox({
        if(input$prov == 'Jawa Barat'){
            data_p2 <- i.jabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DKI Jakarta'){
            data_p2 <- i.DKI
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Tengah'){
            data_p2 <- i.jateng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jawa Timur'){
            data_p2 <- i.jatim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bali'){
            data_p2 <- i.bali
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Banten'){
            data_p2 <- i.banten
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'DI Yogyakarta'){
            data_p2 <- i.DIY
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p2 <- i.NTT
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p2 <- i.NTB
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p2 <- i.aceh
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Barat'){
            data_p2 <- i.sumbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13
        }else if(input$prov == 'Sumatera Utara'){
            data_p2 <- i.sumut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Riau'){
            data_p2 <- i.riau
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Riau'){
            data_p2 <- i.kepri
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Jambi'){
            data_p2 <- i.jambi
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Bengkulu'){
            data_p2 <- i.bengkulu
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sumatera Selatan'){
            data_p2 <- i.sumsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p2 <- i.babel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Lampung'){
            data_p2 <- i.lampung
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Barat'){
            data_p2 <- i.kalbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p2 <- i.kalsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p2 <- i.kalteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Timur'){
            data_p2 <- i.kaltim
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Kalimantan Utara'){
            data_p2 <- i.kalut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Gorontalo'){
            data_p2 <- i.gorontalo
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Barat'){
            data_p2 <- i.sulbar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p2 <- i.sulsel
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p2 <- i.sultra
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p2 <- i.sulteng
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Sulawesi Utara'){
            data_p2 <- i.sulut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku'){
            data_p2 <- i.maluku
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Maluku Utara'){
            data_p2 <- i.malut
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua'){
            data_p2 <- i.papua
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }else if(input$prov == 'Papua Barat'){
            data_p2 <- i.pabar
            t_start <- seq(2, nrow(data_p2$counts)-13)   
            t_end <- t_start + 13 
        }
        
        prov_estimate <- estimate_R(data_p2, method = 'parametric_si', config = 
                                        make_config(list(
                                            mean_si = input$mean_si,
                                            std_si = input$std_si,
                                            t_start = t_start,
                                            t_end = t_end
                                        )))
        if(prov_estimate$R$`Mean(R)`[length(prov_estimate$R$`Mean(R)`)]> 1){
            value <- 'Kebijakan penanggulangan belum berjalan efektif. Saran yang diberikan : 1] Memperketat kebijakan PSBB, 2]Menghimbau masyarakat agar mematuhi protokol kesehatan, dan 3]Menutup tempat yang mengundang kerumunan massal'
        }else{
            value <- 'Kebijakan penanggulangan dapat dikatakan efektif. Saran yang diberikan : 1] Tetap menjaga agar tidak terjadi kelonjakan kasus'
        }
        
        infoBox(title = 'Kesimpulan dan Saran', value = value, icon = icon('key'), color = 'navy')
        
    })
    
    output$title <- renderText({
        if(input$prov == 'Jawa Barat'){
            data_p1 <- jabar
        }else if(input$prov == 'DKI Jakarta'){
            data_p1 <- DKI
        }else if(input$prov == 'Jawa Tengah'){
            data_p1 <- jateng 
        }else if(input$prov == 'Jawa Timur'){
            data_p1 <- jatim
        }else if(input$prov == 'Bali'){
            data_p1 <- bali    
        }else if(input$prov == 'Banten'){
            data_p1 <- banten
        }else if(input$prov == 'DI Yogyakarta'){
            data_p1 <- DIY
        }else if(input$prov == 'Nusa Tenggara Timur'){
            data_p1 <- NTT
        }else if(input$prov == 'Nusa Tenggara Barat'){
            data_p1 <- NTB
        }else if(input$prov == 'Nangroe Aceh Darussalam'){
            data_p1 <- aceh
        }else if(input$prov == 'Sumatera Barat'){
            data_p1 <- sumbar
        }else if(input$prov == 'Sumatera Utara'){
            data_p1 <- sumut
        }else if(input$prov == 'Riau'){
            data_p1 <- riau
        }else if(input$prov == 'Kepulauan Riau'){
            data_p1 <- kepri
        }else if(input$prov == 'Jambi'){
            data_p1 <- jambi
        }else if(input$prov == 'Bengkulu'){
            data_p1 <- bengkulu
        }else if(input$prov == 'Sumatera Selatan'){
            data_p1 <- sumsel
        }else if(input$prov == 'Kepulauan Bangka Belitung'){
            data_p1 <- babel
        }else if(input$prov == 'Lampung'){
            data_p1 <- lampung
        }else if(input$prov == 'Kalimantan Barat'){
            data_p1 <- kalbar
        }else if(input$prov == 'Kalimantan Selatan'){
            data_p1 <- kalsel
        }else if(input$prov == 'Kalimantan Tengah'){
            data_p1 <- kalteng
        }else if(input$prov == 'Kalimantan Timur'){
            data_p1 <- kaltim
        }else if(input$prov == 'Kalimantan Utara'){
            data_p1 <- kalut
        }else if(input$prov == 'Gorontalo'){
            data_p1 <- gorontalo
        }else if(input$prov == 'Sulawesi Barat'){
            data_p1 <- sulbar
        }else if(input$prov == 'Sulawesi Selatan'){
            data_p1 <- sulsel
        }else if(input$prov == 'Sulawesi Tenggara'){
            data_p1 <- sultra
        }else if(input$prov == 'Sulawesi Tengah'){
            data_p1 <- sulteng
        }else if(input$prov == 'Sulawesi Utara'){
            data_p1 <- sulut
        }else if(input$prov == 'Maluku'){
            data_p1 <- maluku
        }else if(input$prov == 'Maluku Utara'){
            data_p1 <- malut
        }else if(input$prov == 'Papua'){
            data_p1 <- papua
        }else if(input$prov == 'Papua Barat'){
            data_p1 <- pabar
        }
        
        paste("Statistik Terbaru", '(', as.character(format(data_p1$tanggal[length(data_p1$tanggal)], "%d %B %Y")), ')')    
    })
}

shinyApp(ui, server)