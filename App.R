############################################################
#                 DEPTO. DE ESTADISTICA                    #
#                         IHSS                             #
#             CALCULO DE GASTO DIRECTO 2022                #
#            AUTOR : ELMER DAVID ZELAYA CRUZ               #
###########################################################

# ++++++++++++++++++++++++++++++++++++++++++++ Librerias 
library(shinydashboard)
library(DT)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(fresh)
library(dplyr)
library(lubridate)
library(tibble)
library(openxlsx)
library(readxl)
# ++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++ Comienzo Titulo 
{
  titulo <- dashboardHeader(title =  ('Gasto 2022'), 
                            titleWidth = 225)
  # ++++++++++++++++++++++++++++++++++++++++++++ fin titulo
  
  # ++++++++++++++++++++++++++++++++++++++++++++ Comienzo Menu
  
  menu <- dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(strong(h4('Inicio')),tabName = 'inicio'),
      menuItem(strong(h4('Gasto por Centro de Costo ')), tabName = 'gxcc'),
      menuItem(strong(h4('Gasto por Unidad')), tabName = 'gxu'),
      menuItem(strong(h4("Gasto por Emisor - Receptor", br(), "(Unidad)")), tabName = 'gxure'),
      menuItem(strong(h4("Gasto por Emisor - Receptor", br(), "(Subunidad)")), tabName = 'gxsure')
      
    )
  )
  # ++++++++++++++++++++++++++++++++++++++++++++ Fin Menu
  
  # ++++++++++++++++++++++++++++++++++++++++++++ Inicio del Cuerpo de Aplicacion 
  cuerpo <- dashboardBody(
    # ++++++++++++++++++++++++++++++++++++++++++++ Comienzo de Items 
    tabItems(
      
      # ++++++++++++++++++++++++++++++++++++++++++++ Comienzo item 1
      tabItem( 
        tabName = 'inicio', 
        box(status ="primary",
            solidHeader = 0,
            collapsible = 0,
            width = 12,
            fluidRow(column(width = 12, 
                            h1(strong("Gasto 2022"),align = 'center'), 
                            h3("La aplicacion ayudará con el cálculo de los Gastos del año 2022,
                          estos serán determinados Mensuales y anuales considerando los Centro de Costo, Unidades, Emisores - Receptores y Subunidades." )
            )
            )
        ),
        box(width = 12,status = 'primary', solidHeader = TRUE,
            style = "height:550px; overflow-y: scroll;overflow-x: scroll;",
            fileInput("File", h2("Cargar Data")),
            
            column(
              DT::dataTableOutput("sample_table"),width = 12)
        )
      ), 
      # ++++++++++++++++++++++++++++++++++++++++++++ Fin item 1
      
      # ++++++++++++++++++++++++++++++++++++++++++++ Comienzo item 2 
      tabItem(
        tabName = 'gxcc',
        tabsetPanel(header = NULL,
                    br(),
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ comienzo primer tabpanel
                    
                    tabPanel(("MENSUAL POR CÓDIGO SAP"),fluidRow(box(width = 12, solidHeader = TRUE,
                                                                     selectizeInput("mcs1", h3("Ingrese código SAP"), choices = NULL)),
                                                                 box(width = 12, solidHeader = TRUE,
                                                                     selectizeInput("mes1", h3("Seleccione el mes"),choices = NULL)),
                                                                 valueBoxOutput(width = 2, ''), align = 'center',
                                                                 valueBoxOutput(width = 8, "gastoxmcs1"))
                             
                    ), 
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ fin del primer tabPanel
                    
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo segundo tabPanel
                    
                    tabPanel("MENSUAL POR DESCRIPCIÓN CECO",fluidRow(box(width = 12, solidHeader = TRUE,
                                                                         selectizeInput("mceco1", h3("Ingrese Descripción de CECO"), choices = NULL)),
                                                                     box(width = 12, solidHeader = TRUE,
                                                                         selectInput("mes2",h3("Seleccione el mes"),choices = NULL)),
                                                                     valueBoxOutput(width = 2, ''), align = 'center',
                                                                     valueBoxOutput(width = 8, "gastoxmceco1"))
                    ),
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ fin del segundo tabPanel
                    
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del tercer tabPanel
                    tabPanel("ANUAL POR CÓDIGO SAP",fluidRow(box(width = 12, solidHeader = TRUE,
                                                                 selectizeInput("acs1", h3("Ingrese código SAP"), choices = NULL)),
                                                             valueBoxOutput(width = 2, ''), align = 'center',
                                                             valueBoxOutput(width = 8, "gastoxacs1"))
                    ),
                    # ++++++++++++++++++++++++++++++++++++++++++++++++ Fin del tercer tabPanel
                    
                    # +++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del cuarto tabPanel
                    tabPanel("ANUAL POR DESCRIPCIÓN CECO",fluidRow(box(width = 12, solidHeader = TRUE,
                                                                       selectizeInput("aceco1", h3("Ingrese código Descripción CECO"), choices = NULL)),
                                                                   valueBoxOutput(width = 2, ''), align = 'center',
                                                                   valueBoxOutput(width = 8, "gastoxaceco1"))
                    )
                    #++++++++++++++++++++++++++++++++++++++++++++++++++ Fin del cuarto tabPanel 
        )
      ),
      # ++++++++++++++++++++++++++++++++++++++++++++++++ fin segundo tabItem 
      
      
      # +++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del tercer item
      tabItem(
        tabName = 'gxu',
        tabsetPanel(
          br(),
          # +++++++++++++++++++++++++++++++++++++++++++++++++ inicio primer tabpanel 
          tabPanel("MENSUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("mu1", h3("Ingrese La Unidad"), choices = NULL)),
                            box(width = 12, solidHeader = TRUE,
                                selectInput("mes5",h3("Seleccione el mes"),choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxmu1"))
          ),
          # ++++++++++++++++++++++++++++++++++++++++++++++++ fin primer tabsetpanel
          
          #+++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del segundo tabPanel       
          tabPanel("ANUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("au1", h3("Ingrese La Unidad"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxau1"))
          )
          # ++++++++++++++++++++++++++++++++++++++++++++++++ Fin del segundo tabPanel
        )
      ),
      # ++++++++++++++++++++++++++++++++++++++++++++++++ Fin de Tercer item
      
      # ++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del cuarto item
      tabItem(
        tabName = 'gxure',
        tabsetPanel(
          br(),
          # +++++++++++++++++++++++++++++++++++++++++++++++ comienzo primer tabPanel
          tabPanel("RECEPTORA MENSUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("mur1", h3("Ingrese La Unidad"), choices = NULL)),
                            box(width = 12, solidHeader = TRUE,
                                selectInput("mes7", h3("Seleccione el mes"),choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxmur1"))
          ),
          # +++++++++++++++++++++++++++++++++++++++++++++++ Fin del primer tabPanel
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ comienzo del segundo tabPanel
          tabPanel("EMISORA MENSUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("mue1", h3("Ingrese La Unidad"), choices = NULL)),
                            box(width = 12, solidHeader = TRUE,
                                selectInput("mes8", h3("Seleccione el mes"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxmue1"))
          ),
          # ++++++++++++++++++++++++++++++++++++++++++++++ Fin del segundo tabPanel
          
          # ++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del tercer tabPanel
          tabPanel("RECEPTORA ANUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("aur1", h3("Ingrese La Unidad"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxaur1"))
          ),
          # ++++++++++++++++++++++++++++++++++++++++++++++ Fin del tercer tabPanel
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del cuarto tabPanel
          tabPanel("EMISORA ANUAL POR UNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("aue1", h3("Ingrese La Unidad"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxaue1"))
          )
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ fin del caurto tabPanel
          
        )
      ),
      # ++++++++++++++++++++++++++++++++++++++++++++++++ Fin del cuarto item
      
      # ++++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del quinto Item
      tabItem(
        tabName = 'gxsure',
        tabsetPanel(
          br(),
          # +++++++++++++++++++++++++++++++++++++++++++++++ comienzo primer tabPanel
          tabPanel("RECEPTORA MENSUAL POR SUBUNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("msur1", h3("Ingrese La Subunidad"), choices = NULL)),
                            box(width = 12, solidHeader = TRUE,
                                selectInput("mes11", h3("Seleccione el mes"),choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxmsur1"))
          ),
          # +++++++++++++++++++++++++++++++++++++++++++++++ Fin del primer tabPanel
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ comienzo del segundo tabPanel
          tabPanel("EMISORA MENSUAL POR SUBUNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("msue1", h3("Ingrese La Subunidad"), choices = NULL)),
                            box(width = 12, solidHeader = TRUE,
                                selectInput("mes12", h3("Seleccione el mes"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxmsue1"))
          ),
          # ++++++++++++++++++++++++++++++++++++++++++++++ Fin del segundo tabPanel
          
          # ++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del tercer tabPanel
          tabPanel("RECEPTORA ANUAL POR SUBUNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("asur1", h3("Ingrese La Subunidad"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxasur1"))
          ),
          # ++++++++++++++++++++++++++++++++++++++++++++++ Fin del tercer tabPanel
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del cuarto tabPanel
          tabPanel("EMISORA ANUAL POR SUBUNIDAD",
                   fluidRow(box(width = 12, solidHeader = TRUE,
                                selectizeInput("asue1", h3("Ingrese La Subunidad"), choices = NULL)),
                            valueBoxOutput(width = 2, ''), align = 'center',
                            valueBoxOutput(width = 8, "gastoxasue1"))
          )
          
          # +++++++++++++++++++++++++++++++++++++++++++++++ fin del caurto tabPanel
          
        )
      )
      # ++++++++++++++++++++++++++++++++++++++++++++++++ Fin del quinto Item
      
    )
    # ++++++++++++++++++++++++++++++++++++++++++++++++ fin de items
  ) 
  # +++++++++++++++++++++++++++++++++++++++++++++++ fin Cuerpo de la aplicacion
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++ comienzo de la Interfaz de Usuario 
  Ui <-  dashboardPage(titulo, menu, cuerpo)
  # +++++++++++++++++++++++++++++++++++++++++++++++ Fin de la interfaz de usuario
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo Tamaño de Archivo 
  options(shiny.maxRequestSize = 50*1024^2)
  options(shiny.reactlog = TRUE)
  options(shiny.usecairo = TRUE)
  # +++++++++++++++++++++++++++++++++++++++++++++++ Fin tamaño de archivo
  
  
}

# +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo del Server
{
  Server <- function(input, output, session) {
    
    
    # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo de cargar datos 
    df_products_upload <- reactive({
      inFile <- input$File
      if (is.null(inFile)){
        return(NULL)
      }else{
        df <- read_excel(inFile$datapath)
        df <- df %>% mutate(Mes = toupper(Mes))
        return(df)}
    })
    # +++++++++++++++++++++++++++++++++++++++++++++++ Fin cargar datos
    
    # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo Leer Datos 
    
    output$sample_table <- DT::renderDataTable({
      df <- df_products_upload()
      DT::datatable(df)
    })
    # +++++++++++++++++++++++++++++++++++++++++++++++ Fin leer Datos  
    
    # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo de la lista selectize  
    {
      mcs1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes1", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "mcs1", choices = unique(Gasto_Unif_2022$`Centro de coste...1`), server = T)
      })
      
      mceco1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes2", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "mceco1", choices = unique(Gasto_Unif_2022$`DESCRIPCION CECO`), server = T)
      })
      
      acs1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "acs1", choices = unique(Gasto_Unif_2022$`Centro de coste...1`), server = T) 
      })
      
      aceco1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "aceco1", choices = unique(Gasto_Unif_2022$`DESCRIPCION CECO`), server = T) 
      })
      
      mu1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes5", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "mu1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      au1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "au1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      mur1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes7", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "mur1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      mue1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes8", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "mue1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      aur1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "aur1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      aue1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "aue1", choices = unique(Gasto_Unif_2022$UNIDAD), server = T) 
      })
      
      msur1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes11", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "msur1", choices = unique(Gasto_Unif_2022$supradescripcion), server = T) 
      })
      
      msue1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "mes12", choices = unique(toupper(Gasto_Unif_2022$Mes)), server = T)
        updateSelectizeInput(session, "msue1", choices = unique(Gasto_Unif_2022$supradescripcion), server = T) 
      })
      
      asur1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "asur1", choices = unique(Gasto_Unif_2022$supradescripcion), server = T) 
      })
      
      asue1 <- observe({
        Gasto_Unif_2022 <- df_products_upload()
        updateSelectizeInput(session, "asue1", choices = unique(Gasto_Unif_2022$supradescripcion), server = T) 
      })
      
    }
    
    
    # +++++++++++++++++++++++++++++++++++++++++++++++ Fin de la lista selectize
    
    # +++++++++++++++++++++++++++++++++++++++++++++++ Comienzo de la filtración
    
    {
      gastoxmcs1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s1<-Gasto_Unif_2022%>%filter(`Centro de coste...1` == input$mcs1, Mes == input$mes1)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s1)                    
      })
      
      gastoxmceco1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s2<-Gasto_Unif_2022%>%filter(`DESCRIPCION CECO` == input$mceco1, Mes == input$mes2)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s2)                    
      })
      
      gastoxaceco1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s3<-Gasto_Unif_2022%>%filter(`DESCRIPCION CECO` == input$aceco1)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s3)                    
      })
      
      gastoxacs1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s4<-Gasto_Unif_2022%>%filter(`Centro de coste...1` == input$acs1)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s4)                    
      })
      
      gastoxmu1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s5<-Gasto_Unif_2022%>%filter(UNIDAD == input$mu1, Mes == input$mes5)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s5)                    
      })
      
      gastoxau1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s6<-Gasto_Unif_2022%>%filter(UNIDAD == input$au1)%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s6)                    
      })
      
      gastoxmur1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s7<-Gasto_Unif_2022%>%filter(UNIDAD == input$mur1, Mes == input$mes7, `Emisora/Receptora` == "RECEPTORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits= 2, big.mark = ","))
        return(s7)                    
      })
      
      gastoxmue1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s8<-Gasto_Unif_2022%>%filter(UNIDAD == input$mue1, Mes == input$mes8, `Emisora/Receptora`== "EMISORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s8)                    
      })
      
      gastoxaur1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s9<-Gasto_Unif_2022%>%filter(UNIDAD == input$aur1,`Emisora/Receptora` == "RECEPTORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s9)                    
      })
      
      gastoxaue1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s10<-Gasto_Unif_2022%>%filter(UNIDAD == input$aue1,`Emisora/Receptora` == "EMISORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s10)                    
      })
      
      gastoxmsur1 <- reactive({ 
        Gasto_Unif_2022 <- df_products_upload()
        s11<-Gasto_Unif_2022%>%filter(supradescripcion == input$msur1, Mes == input$mes11, `Emisora/Receptora` == "RECEPTORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s11)                    
      })
      
      gastoxmsue1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s12<-Gasto_Unif_2022%>%filter(supradescripcion == input$msue1, Mes == input$mes12, `Emisora/Receptora` == "EMISORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits= 2, big.mark = ","))
        return(s12)                    
      })
      
      gastoxasur1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s13<-Gasto_Unif_2022%>%filter(supradescripcion == input$asur1,`Emisora/Receptora` == "RECEPTORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s13)                    
      })
      
      gastoxasue1 <- reactive({
        Gasto_Unif_2022 <- df_products_upload()
        s14<-Gasto_Unif_2022%>%filter(supradescripcion == input$asue1,`Emisora/Receptora` == "EMISORA")%>%
          summarize(formatC(sum(`Impte.moneda local`), format = "f",digits = 2, big.mark = ","))
        return(s14)                    
      })
      
    }  
    # ++++++++++++++++++++++++++++++++++++++++++++++ Fin de la filtración
    
    # ++++++++++++++++++++++++++++++++++++++++++++++ Inicio de la impresion de totales  
    
    {}
    output$gastoxmcs1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmcs1()), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxmceco1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmceco1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxacs1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxacs1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxaceco1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxaceco1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxmu1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmu1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxau1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxau1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxmur1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmur1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxmue1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmue1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxaur1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxaur1()), h2('Total de Gasto Directo'),
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxaue1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxaue1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    ###########
    output$gastoxmsur1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmsur1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxmsue1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxmsue1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxasur1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxasur1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
    
    output$gastoxasue1 <- renderValueBox({
      if(is.null(df_products_upload())){
        valueBox(
          paste0('-'), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }else{
        valueBox(
          paste0(gastoxasue1()), h2('Total de Gasto Directo'), 
          icon = icon("coins"),
          color = 'light-blue'
        )
      }
    })
  }  
  
  # +++++++++++++++++++++++++++++++++++ Fin de la impresión de totales  
}

shinyApp(ui = Ui, server = Server)




