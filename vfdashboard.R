##Rodando Todas as bibliotecas

library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
# install.packages('bootstrap')
library(bootstrap)
#install.packages('lubridate')
library(lubridate)
#install.packages("reprex")
library(reprex)
library(stringr)
library(leaflet)
library(plotly)
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(sf)
library(geobr)
library(magrittr)
library(colorspace)
#nstall.packages('ggspatial')
library(ggspatial)
library(gganimate)
#install.packages('gifski')
library(gifski)
#install.packages('av')
library(av)

# Chamando todos os dados

d2020 <- read.csv("C:\\Users\\zabuz\\OneDrive\\Área de Trabalho\\Dashboard - VED\\Dados\\datatran2020.csv", TRUE, ';')

d2019 <- read.csv("C:\\Users\\zabuz\\OneDrive\\Área de Trabalho\\Dashboard - VED\\Dados\\datatran2019.csv", TRUE, ';')

d2018 <- read.csv("C:\\Users\\zabuz\\OneDrive\\Área de Trabalho\\Dashboard - VED\\Dados\\datatran2018.csv", TRUE, ';')

# Remanejando e arrumando os dados

df1 <- merge(d2020, d2019, all =T)
df <- merge(df1, d2018, all = T)
df <- data.frame(df)
df$data_inversa = as.Date(df$data_inversa, format = "%Y-%m-%d")
## Mapa com os estados
br1=read_state()
# View(df)
## Função para obter as modas

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




ui <- dashboardPage( skin = 'red',
                     #cabeçalho
                     dashboardHeader(
                       title = 'Acidentes de Trânsito',
                       dropdownMenu(type = "messages",
                                    messageItem(
                                      from = 'Polícia Rodoviária',
                                      message= 'Sem celular no trânsito',
                                      icon= icon("user"),
                                      time = "Agora"
                                    ) ),
                       #Menu(Canto superior direito)
                       dropdownMenu(type = "notification",
                                    notificationItem(
                                      text='Direção sem distração',
                                      icon= icon("mobile")
                                    ),
                                    notificationItem(
                                      text='Use o cinto de segurança',
                                      icon= icon("user-shield")
                                    ),
                                    notificationItem(
                                      text='Não ultrapasse a velocidade máxima',
                                      icon= icon("exclamation-triangle")
                                    )
                       ),
                       dropdownMenu(type = "tasks",
                                    taskItem(
                                      value = 90,
                                      color = "green",
                                      "Chegar seguro ao destino"
                                    )
                       )
                     ),
                     #Opções lado esquerdo
                     dashboardSidebar(
                       sidebarMenu(
                         dateRangeInput(inputId = "data",
                                        label = h3("Data"),
                                        start = min(df$data_inversa),
                                        end = max(df$data_inversa),
                                        min= min(df$data_inversa),
                                        max= max(df$data_inversa),
                                        language = "pt",
                                        format ="yyyy-mm-dd",
                                        separator = "-"),
                         
                         
                         
                         menuItem("Mapa",
                                  tabName = "Mapa",
                                  icon = icon("map-marked")),
                         
                         menuItem("Gráficos",
                                  tabName = "Gráficos",
                                  icon = icon("chart-bar")),
                         
                         menuItem("Estatísticas",
                                  tabName = "stat",
                                  icon = icon("table")),
                         
                         
                         menuItem('Apresentação', icon = icon("youtube"), href="https://youtube.com"),
                         
                         menuItem('Git',icon = icon("file-code"), href="https://github.com/PedroMaiorano/Dashboard-Interativo"),
                         
                         menuItem('Banco de Dados',icon = icon("file-csv"),href="https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-acidentes"),
                         
                         menuItem("Sobre",
                                  tabName = "Sobre",
                                  icon = icon("info"))
                         
                       )),
                     
                     #Corpo, (O que vai aparecer conforme muda as opções do lado esquerdo)
                     dashboardBody(
                       tabItems(
                         
                         tabItem(
                           tabName = "Mapa", h2('Mapa'),
                           box(
                             title = "Mapa com acidentes",
                             status = "danger",
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             width = 15,
                             height = 800,
                             leafletOutput("map",height = 740))
                         ),
                         
                         
                         tabItem(
                           tabName = 'Gráficos', h2("Gráficos"),
                           box(
                             title = "Acidentes por estado",
                             status = "danger",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("plotuf", width = '100%')
                           ),
                           
                           box(
                             
                             title = "As 10 maiores causas de acidente de trânsito",
                             status = "danger",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("plotcausa",width = '100%')),
                           
                           box(
                             title = "Classificação do acidente conforme a situação das vítimas",
                             status = "danger",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("plotclass", height = "350px")),
                           
                           
                           box(
                             title = "Série temporal de acidentes",
                             status = "danger",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("plotr", height = "350px")),
                         ),
                         
                         
                         
                         tabItem(
                           tabName = 'stat', h2("Dados estatísticos sobre os acidentes"),
                           fluidRow(
                             valueBox(
                               uiOutput("acidentes"),"Acidentes", color = "green",width = 5),
                             
                             valueBox(
                               uiOutput("fasedia"),"Média de acidentes diários", color = "lime",width = 5),
                             
                             valueBox(
                               uiOutput("acidentesf"),"Acidentes Fatais", color = "blue",width = 5),
                             
                             valueBox(
                               uiOutput("cmetere"),"Média de acidentes fatais diários", color = "light-blue",width = 5),
                             
                             valueBox(
                               uiOutput("feridosl"),"Feridos Leves", color = "yellow",width = 5),
                             
                             valueBox(
                               uiOutput("tipoacidente"),"Média de feridos leves diários", color = "orange",width = 5),
                             
                             valueBox(
                               uiOutput("feridosg"),"Feridos Graves", color = "navy",width = 5),
                             
                             valueBox(
                               uiOutput("macausa"),"Média de feridos graves diários", color = "teal",width = 5),
                             
                             valueBox(
                               uiOutput("mortes"),"Mortes", color = "black",width = 5),
                             
                             valueBox(
                               uiOutput("mecausa"),"Média de mortes por dia", color = "black",width = 5),
                             
                             valueBox(
                               uiOutput("fday"),"É a fase do dia com mais acidentes", color = "red",width = 5),
                             valueBox(
                               uiOutput("dday"),"É o dia da semana com mais acidentes", color = "fuchsia",width = 5)
                             
                             
                           )
                         ),
                         
                         tabItem(
                           tabName = "Sobre", h2("Desenvolvedores"),
                           fluidRow(
                             box(paste("Nome: Pedro Henrique Freitas Maiorano </br>
                                       E-mail: pedro.maiorano@usp.br </br>
                                       Github: https://github.com/PedroMaiorano" )%>% lapply(htmltools::HTML))),
                           fluidRow(
                             box(paste('Nome: Wemerson Mauricio dos Santos </br> 
                                       E-mail: wemersonmauricio@usp.br </br>
                                       Github: https://github.com/wemersonmauricio ' )%>% lapply(htmltools::HTML)))))))


# server
server <- function(input, output, session){
  ##  b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2]) 
  #filtra dentro dos dados mediante a data e retorna como um data frame b
  
  
  #Mapa
  output$map <- renderLeaflet({
    #Preparando os dados
    b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
    c = (b[b$classificacao_acidente =="Com Vítimas Fatais",])
    cc= b %>% group_by(uf) %>% summarise(feridos=sum(feridos))
    d = b %>% group_by(uf) %>% summarise(mortos=sum(mortos))
    #Juntando os dados com o mapa
    f = full_join(br1,table(b$uf),by=c("abbrev_state"="Var1"), copy= TRUE)
    g = full_join(br1,table(c$uf),by=c("abbrev_state"="Var1"), copy= TRUE)
    gg = full_join(br1,cc,by=c("abbrev_state"="uf"), copy= TRUE)
    h = full_join(br1,d,by=c("abbrev_state"="uf"), copy= TRUE)
    #paleta de cores
    pal= colorBin("YlOrRd",domain = f$Freq)
    #O que aparece quando passar o mouse
    lab= sprintf(
      "<strong>%s<strong></br>%g Acidentes </br> %g Acidentes Fatais</br>%g Feridos</br>%g Mortos",
      f$name_state, f$Freq,g$Freq,gg$feridos,h$mortos ) %>% lapply(htmltools::HTML)
    
    
    #Juntando tudo para fazer o mapa
    leaflet(f, height = "100%") %>% addTiles() %>% addPolygons(
      fillColor = ~pal(Freq),
      weight=2,
      opacity = 1,
      color='black',
      dashArray = '1',
      fillOpacity = 0.9,
      
      
      highlight = highlightOptions(
        weight=6,
        color="red",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE),
      label = lab,
      labelOptions = labelOptions(
        style = list("font-weight" = 'normal', padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%  
      addLegend(pal = pal,
                values = ~Freq,
                opacity =0.9,
                title = "Número de acidentes",
                position = "bottomleft")
    
    
  })
  
  #Gráficos
  
  output$plotuf <- renderPlotly({
    df1 = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
    chartpar <- data.frame(table(df1$uf))
    chartpar <-chartpar[order(chartpar$Freq,decreasing = TRUE),]
    chartpar$Var1 <- factor(chartpar$Var1,levels=chartpar$Var1)
    chartpar$acumulado <- cumsum(chartpar$Freq)
    chartpar$freqrela <- round((chartpar$Freq/sum(chartpar$Freq))*100,2)
    chartpar$freqrelaacu <-round(cumsum(chartpar$freqrela),2)
    
    paretochart<-ggplot(chartpar) +
      aes(x = Var1, fill = Var1, weight = freqrela) +
      geom_bar() +
      geom_point(aes(y=freqrelaacu))+
      geom_path(aes(y =freqrelaacu,group=1))+
      scale_fill_hue(direction = 1) +
      theme_minimal()+
      xlab("Estado")+
      ylab ("Frequência relativa de acidentes (%)")+
      labs(title = "Acidentes por estado")+
      theme(legend.position = "none")
    ggplotly(paretochart)
    
  })
  
  output$plotcausa <- renderPlotly({
    
    df2 = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
    chartbar <- data.frame(table(df2$causa_acidente))
    idx <- order(chartbar$Freq,decreasing = FALSE)
    levels <- chartbar$Var1[idx]
    chartbar$Var1 <- factor(chartbar$Var1,levels = levels,ordered = TRUE)
    chartbar <- chartbar[order(chartbar$Freq,decreasing = TRUE),]
    colnames(chartbar)<-c("Causas","Freq")
    barchart <- ggplot(chartbar[1:10,],aes(x= Causas,y=Freq, fill= Causas))+
      geom_bar(stat="identity",width = 1)+
      coord_flip()+
      xlab("")+
      ylab ("Acidentes")+
      labs(title = "Top 10")+
      theme(legend.position = "none")
    ggplotly(barchart)
  })
  
  output$plotclass <- renderPlotly({
    df3 = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
    classificacao <- data.frame(table(df3$classificacao_acidente))
    colnames(classificacao)<- c("Clas", "freq")
    fig<-plot_ly(classificacao, labels = ~Clas, values = ~freq, type = 'pie')
    fig<- fig %>% layout(title="Classificação de acidentes de trânsito",legend=list(title=list(text='<b> Legenda </b>')))
    fig
  })
  
  output$plotr <- renderPlotly({ 
    df4 = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
    acdiarios <- data.frame(table(df4$data_inversa))
    acdiarios$Var1 <-as.Date(acdiarios$Var1)
    colnames(acdiarios)<- c("data","acidentes")
    linechart<-ggplot(acdiarios,aes(x = data, y = acidentes)) +
      geom_line(size = 0.5, colour = "#112446") +
      labs(
        x = "Data",
        y = "Acidentes",
        title = "Acidentes ao longo do tempo"
      ) +
      ggthemes::theme_gdocs()
    ggplotly(linechart)
    
  })
  #Estatísticas
  ##Lado Esquerdo
  output$acidentes <- renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  nrow(b)})
  
  output$acidentesf <- renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  c <- (b[b$classificacao_acidente =="Com Vítimas Fatais",])
  nrow(c)})
  
  output$feridosl <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  sum(b$feridos_leves)})
  
  output$feridosg <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  sum(b$feridos_graves)})
  
  output$mortes <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  sum(b$mortos)})
  
  
  ##Lado Direto
  
  output$fasedia <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  round(length(b$feridos_leves)/length(unique(b$data_inversa)),2)})
  
  output$cmetere <-renderText({b=subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  c=(b[b$classificacao_acidente =="Com Vítimas Fatais",])
  round(length(c$data_inversa)/length(unique(c$data_inversa)),2)})
  
  output$tipoacidente <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  round(sum(b$feridos_leves)/length(unique(b$data_inversa)),2)})
  
  output$macausa <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  round(sum(b$feridos_graves)/length(unique(b$data_inversa)),2)})
  
  output$mecausa <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  round(sum(b$mortos)/length(unique(b$data_inversa)),2)})
  
  output$fday <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  getmode(b$fase_dia)
  })
  
  output$dday <-renderText({b = subset(df, df$data_inversa>= input$data[1] & df$data_inversa<= input$data[2])
  getmode(b$dia_semana)})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
