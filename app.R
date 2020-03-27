# Load packages -----------------------------------------------------
library(shiny)
library(plotly)
library(ggplot2)

# Load data ---------------------------------------------------------

#data y para primer plot
data <- read.csv("data.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data2 <- read.csv("data2.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data3 <- read.csv("data3.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data4 <- read.csv("total_tabla.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data5 <- read.csv("df_t.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data6 <- read.csv("data_casos_totales2.csv", header = T, sep = ",", fileEncoding = "UTF-8")

# Define UI ---------------------------------------------------------

library(shiny)
library(plotly)

ui <- fluidPage(
    
    headerPanel(title = HTML("COVID-19"),
                windowTitle="COVID-19"),
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Información App", icon = icon("exclamation"),
                             h1("Bienvenido!", align = "center"),
                             p("La siguiente app permite visualizar el crecimiento de la enfermedad COVID 19."),
                             strong("Se recomienda usar Google Chrome."),
                             br(),
                             p("Pestañas:"),
                             p(strong("- Grafico 30 dias: "),"En esta pestaña se encontrara la grafica por
                               dia del crecimiento de enfermedad por pais, unicamente los primeros 20 dias."), 
                             p(strong("- Grafico Total: "),"En esta pestaña se encontrara la grafica por
                               dia del crecimiento de enfermedad por pais."),
                             p(strong("- Grafico Porcentaje: "),"En esta pestaña se encontrara la grafica 
                               por dia de casos de enfermedad sobre el total de la poblacion del pais."),
                             p(strong("- Casos resumen: "),"En esta pestaña se encontrara un grafico de barras
                               el cual sirve para comparar el numero de casos de la enfermedad por pais."),
                             p(strong("- Enfermos - Muertos - Curados: "),"En esta pestaña se encontrara los datos de 
                               Diagnosticados, Muertos y Curados."),
                             p(strong("- Mapa: "),"En esta pestaña se encontrara un mapa descriptivo."),
                             p(strong("- Tabla de Datos: "),"En esta pestaña se encontrara el ultimo registro de un
                               caso por pais."),
                             p(strong("- FAQ ")),
                             p(strong("- Actualizaciones")),
                             br(),
                             fluidRow(
                                 column(1,
                                        br(),br()
                                 ),
                                 column(4,
                                        imageOutput("escudo1")
                                 ),
                                 column(3,
                                        br(),br(),
                                        imageOutput("engrane1")
                                 )
                             )
                    ),
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Grafico 30 dias", icon = icon("medkit"),
                             h1("Grafico de Enfermedad", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais", 
                                     label = "Seleccione pais", 
                                     choices = unique(data$Country), 
                                     selected = c( "Argentina", "Colombia"),
                                     multiple = TRUE
                                 ),
                                 
                                 plotlyOutput(outputId = "p")
                             )
                    ), 
                    
                    
                    
                    
                    
                    
                    tabPanel("Grafico Total", icon = icon("hospital"),
                             h1("Grafico de Enfermedad Total", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais2", 
                                     label = "Seleccione pais", 
                                     choices = unique(data2$Country), 
                                     selected = c( "Argentina", "Colombia"),
                                     multiple = TRUE
                                 ),
                                 plotlyOutput(outputId = "p2")
                             )
                    ), 
                    
                    
                    
                    
                    
                    
                    tabPanel("Grafico Porcentaje", icon = icon("ambulance"),
                             h1("Porcentaje de casos respecto a la poblacion", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais3", 
                                     label = "Seleccione pais", 
                                     choices = unique(data3$Country), 
                                     selected = c( "Argentina", "Colombia"),
                                     multiple = TRUE
                                 ),
                                 plotlyOutput(outputId = "p3")
                             )
                    ), 
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Casos resumen", icon = icon("chart-bar"),
                             h1("Casos Resumen", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais5", 
                                     label = "Seleccione pais", 
                                     choices = unique(data4$Country), 
                                     selected = c("Argentina", "Bolivia", "Brasil", "Chile", 
                                                  "Colombia", "Ecuador", "Paraguay", "Peru", 
                                                  "Uruguay", "Venezuela"),
                                     multiple = TRUE
                                 ),
                                 plotlyOutput(outputId = "p4")
                             )
                    ),                     
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Enfermos - Muertos - Curados", icon = icon("chart-bar"),
                             h1("Resumen", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Country1", 
                                     label = "Seleccione pais", 
                                     choices = unique(data5$Country), 
                                     selected = c("España", "Francia", "Italia"),
                                     multiple = FALSE
                                 ),
                                 plotlyOutput(outputId = "p5")
                             )
                    ),  
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Mapa", icon = icon("binoculars"),
                             h1("Mapa", align = "center"),
                             
                               plotlyOutput(outputId = "p6")
                             
                    ), 
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Tabla de Datos", icon = icon("bell"),
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais4", 
                                     label = "Seleccione pais", 
                                     choices = unique(data4$Country), 
                                     selected = c("Argentina", "Bolivia", "Brasil", "Chile", 
                                                  "Colombia", "Ecuador", "Paraguay", "Peru", 
                                                  "Uruguay", "Venezuela"),
                                     multiple = TRUE
                                 ),
                                 
                                 DT::dataTableOutput("estadisticas")
                             )
                             
                    ), 
                    
                    
                    
                    
                    
                    

                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("FAQ", icon = icon("paperclip"),
                             h1("FAQ", align = "center"),
                             p(strong("- Fuente 1:"), "Datos de la enfermedad tomados de elpais.com periodico de españa.", 
                               em("Tomado de: "), 
                               tags$a(href="https://elpais.com/sociedad/2020/03/09/actualidad/1583748887_173685.html", "ELPAIS.COM")),
                             
                             hr(),
                             
                             p(strong("- Fuente 2 :"), "Datos de la poblacion tomados de wikipedia.com periodico de españa.", 
                               em("Tomado de: "), 
                               tags$a(href="https://wikipedia.com", "WIKIPEDIA.COM")),
                             
                             hr(),
                             
                             fluidRow(
                                 column(3,
                                        br(),br()
                                 ),
                                 column(4,
                                        plotOutput("plot3",
                                                   width='350px',
                                                   height='350px')
                                 )
                             )
                    ),
                    
                    
                    
                    tabPanel("Updates", icon = icon("wrench"),
                             h1("Actualizaciones", align = "center"),
                             strong("Marzo 19, 2020"),
                             p("Creacion de App."),
                             hr(),
                             strong("Marzo 21, 2020"),
                             p("Actualizacion de datos."),
                             hr(),
                             strong("Marzo 23, 2020"),
                             p("Ingreso de beta."),
                             hr(),
                             strong("Marzo 26, 2020"),
                             p("Creacion de mapa."),
                             hr(),
                             fluidRow(
                                 column(4,
                                        br(),br()
                                 ),
                                 column(4,
                                        imageOutput("gancho1")
                                 )
                             )
                    )
        
                    
                    
                    
                    
                    
                    
                    
                    
                    
        )
    )
)

server <- function(input, output, ...) {
    output$p <- renderPlotly({
        plot_ly(data, x = ~Dia, y = ~Casos, color = ~Country, template = 'plotly_dark') %>%
            filter(Country %in% input$Pais) %>%
            group_by(Country) %>%
            add_lines()
    })
    
    
    
    
    # grafico total
    output$p2 <- renderPlotly({
        plot_ly(data2, x = ~Dia, y = ~Casos, color = ~Country, template = 'plotly_dark') %>%
            filter(Country %in% input$Pais2) %>%
            group_by(Country) %>%
            add_lines()
    })
    
    
    
    
    
    # grafico porcentaje
    output$p3 <- renderPlotly({
        plot_ly(data3, x = ~Dia, y = ~Porcentaje , color = ~Country, template = 'plotly_dark') %>%
            filter(Country %in% input$Pais3) %>%
            group_by(Country) %>%
            add_lines()  %>%
            layout(yaxis = list(title = 'Casos / poblacion total', zeroline = TRUE))
    })
    
    
    
    



    # grafico enfermos - muertos - curados
    output$p5 <- renderPlotly({
        
        fig <- plot_ly(data5, x = ~Dia)  %>%
            filter(Country %in% input$Country1)
        fig <- fig %>% add_lines(y = ~Casos, color = I('orange'), name = 'Cases')
        fig <- fig %>% add_lines(y = ~Deaths, color = I('red'), name = 'Deaths') 
        fig <- fig %>% add_lines(y = ~Recovered, color = I('green'), name = "Recovered")
        
        fig

    })
    
    
    
    
    
    
    # mapa
    output$p6 <- renderPlotly({
         
        #data6 %>%
        #    ggplot(aes(x=Poblacion, y=Dias_transcurridos, size=Casos.Totales, color=Continente)) +
        #    geom_point(alpha=0.5) + scale_size(range = c(.1, 24), name="Population (M)")
        
        
        plot_ly(data6,
                lat = ~Latitud,
                lon = ~Longitud,
                type = 'scattermapbox',
                hovertext = paste(" Pais :", data6[,"Country"],
                                  "<br> Poblacion :", data6[,"Poblacion"],
                                  "<br> Dias Transcurridos :", data6[,"Dias_transcurridos"],
                                  "<br> Casos Totales :", data6[,"Casos.Totales"]))  %>%
            layout(mapbox = list(style = 'open-street-map', zoom = 0, center = list(lon = -60, lat = -17)))
        
     })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # bar plot 
     output$p4 <- renderPlotly({
         datas <- subset(data4, Country %in% input$Pais5)
         datas[] <- lapply(datas, function(x) if(is.factor(x)) factor(x) else x)
         plot_ly(data = datas, x = ~Country, y = ~Casos.Totales, type = "bar", 
                 marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)',
                                                                       width = 1.5))) %>%
             layout(yaxis = list(title = 'Casos', zeroline = TRUE))
     })    
    
    
    
     
     
     
     
     
    ## tabla de estadisticos 
    
    
    output$estadisticas <- DT::renderDataTable({
        DT::datatable(data4 %>%
                          filter(Country %in% input$Pais4))
    })
    
    
    
    
    
    
    
    #imagenes
    
    
    output$engrane1 <- renderImage({
        return(list(
            src = "engranaje.gif", height = "150px"
        ))
    }, deleteFile = F)

    
}

shinyApp(ui, server)