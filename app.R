# Load packages -----------------------------------------------------
library(shiny)
library(plotly)
library(ggplot2)

# Load data ---------------------------------------------------------

#data y para primer plot
data <- read.csv("data.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data2 <- read.csv("data2.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data3 <- read.csv("data3.csv", header = T, sep = ",", fileEncoding = "UTF-8")
data4 <- read.csv("data_casos_totales.csv", header = T, sep = ",", fileEncoding = "UTF-8")

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
                             p(strong("- Grafico 20 dias: "),"En esta pestaña se encontrara la grafica por
                               dia del crecimiento de enfermedad por pais, unicamente los primeros 20 dias."), 
                             p(strong("- Grafico Total: "),"En esta pestaña se encontrara la grafica por
                               dia del crecimiento de enfermedad por pais."),
                             p(strong("- Grafico Porcentaje: "),"En esta pestaña se encontrara la grafica 
                               por dia de casos de enfermedad sobre el total de la poblacion del pais."),
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
                                 ),
                                 column(3,
                                        br(),br(),
                                        imageOutput("minas1")
                                 )
                             )
                    ),
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("Grafico 20 dias", icon = icon("medkit"),
                             h1("Grafico de Enfermedad", align = "center"),
                             
                             
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais", 
                                     label = "Seleccione pais", 
                                     choices = unique(data$Country), 
                                     selected = "Colombia",
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
                                     selected = "Colombia",
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
                                     selected = "Colombia",
                                     multiple = TRUE
                                 ),
                                 plotlyOutput(outputId = "p3")
                             )
                    ), 
                    
                    
                    
                    
                    
                    
                    
                    
                    tabPanel("News", icon = icon("bell"),
                             fluidRow(
                                 selectizeInput(
                                     inputId = "Pais4", 
                                     label = "Seleccione pais", 
                                     choices = unique(data4$Country), 
                                     selected = "Colombia",
                                     multiple = TRUE
                                 ),
                                 
                                 DT::dataTableOutput("estadisticas")
                             )
                             
                    ), 
                    
                    
                    
                    
                    
                    
                    tabPanel("FAQ", icon = icon("paperclip"),
                             h1("FAQ", align = "center"),
                             p(strong("- Fuente :"), "Datos de la enfermedad tomados de elpais.com periodico de españa.", 
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
        plot_ly(data3, x = ~Dia, y = ~porc, color = ~Country, template = 'plotly_dark') %>%
            filter(Country %in% input$Pais3) %>%
            group_by(Country) %>%
            add_lines()  %>%
            layout(yaxis = list(title = 'Casos / poblacion total',
                                zeroline = TRUE))
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
    
    output$como_vamos <- renderImage({
        return(list( src = "como_vamos.jpeg"))
        }, deleteFile = F)
    
}

shinyApp(ui, server)