## app.R ##
if(!require("shiny")){
  install.packages("shiny")
  library('shiny')
}


if(!require(shinydashboard)){
  install.packages("shinydashboard")
  library('shinydashboard')
}

if(!require(plotly)){
  install.packages("plotly")
  library('plotly')
}


if(!require(tidyverse)){
  install.packages("tidyverse")
  library('tidyverse')
}




#####################################################################
###Finale Datensatz laden
#####################################################################
load("VariantData.RData")


#####################################################################
###globale Variablen
#####################################################################

isInit<-FALSE
minDay<-min(Datensatz_Final$Produktionsdatum)
maxDay<-max(Datensatz_Final$Produktionsdatum)


#####################################################################
###globale Funktion
#####################################################################


#####################################################################
###header
#####################################################################
               
header<-dashboardHeader( 
  title = tags$img(src="logo.png", height = '100%', width = '200px',
                   style = 'position: absolute; opacity: 1;'),
  titleWidth = 400

)



#####################################################################
###sidebar
#####################################################################
sidebar<-dashboardSidebar(
  # zeitinterval für Produktionsdatum eingeben
  dateRangeInput("Days",
                 label = h3("Produktionsdatum "),
                 min = minDay,
                 max = maxDay,
                 start = "2008-12-02", 
                 end = "2015-02-03"),
  
  # Copy the chunk below to make a group of checkboxes
  checkboxGroupInput("Fahrzeug_Typ:", label = h3("Fahrzeug Typ"), 
                     choices = list("Typ 11" = 11,
                                    "Typ 12" = 12,
                                    "Typ 21" = 21,
                                    "Typ 22" = 22),
                     selected = 11)
  
  
  # actionButton("login", "Log in"),
  # textInput('userid','User id:',value=' definitely not Florian')
  #fluidRow(column(10,h3("Fahrzeug Typ"), verbatimTextOutput("value")))
)


#####################################################################
###body
#####################################################################
body<-dashboardBody(  

  mainPanel(
    tabsetPanel(
      
      id = "id_tabset",
      
      tabPanel(
        title = "Fehlerhäufigkeit",
        id = "tab1",
        plotly::plotlyOutput("Balkendiagramm")
      ),
      
      tabPanel(
        title = "Zeitinterval",
        id = "tab2",
        plotly::plotlyOutput("Boxplot")
      ),
      tabPanel(
        title = "Datensatz",
        id = "tab3",
        DT::DTOutput("data_to_show")
      )
      
    )
    
  )

)

#####################################################################
###ui
#####################################################################
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "red"
  

)
#####################################################################
###server
#####################################################################




server <- function(input, output) {

    #get data:
    #         Datensatz anhand Kombination von Eingaben abwandeln
    get_data_interested <- reactive({
      
      data <- Datensatz_Final%>%
        dplyr::filter(Produktionsdatum>=input$Days[1],
                      Produktionsdatum<=input$Days[2],)%>%
        dplyr::filter(Fahrzeug_Typ %in% input$Fahrzeug_Typ)
      
      return(data)

      
    })


    
    # observeEvent(input$Interval_Produktion, {
    #   showModal(modalDialog(
    #     title = "Somewhat important message",
    #     paste0(minDay,"  ",maxDay),
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # 
    # 
    # })
    
    #render Balkendiagramm
    output$Balkendiagramm<- plotly::renderPlotly({
      Data_Balken<-get_data_interested()%>%
        dplyr::select(Fehlerhaft,Fahrzeug_Typ,Motor_Typ)%>%
        dplyr::group_by(Fahrzeug_Typ,Motor_Typ)%>%
        dplyr::summarise(n=n(),f_rate=sum(Fehlerhaft==TRUE)/n)%>%
        tidyr::spread(Motor_Typ,f_rate)
        
      plot_ly(Data_Balken, x = ~Fahrzeug_Typ, y = ~Benzinmotor, type = 'bar', name = 'Benzinmotor') %>%
        add_trace(y = ~Dieselmotor, name = 'Dieselmotor') %>%
        layout(yaxis = list(title = 'Fehlerhäufigkeit'), barmode = 'group')

      
    })
    
    
    
    #render Boxplot
    output$Boxplot <- plotly::renderPlotly({
      Data_Boxplot<-get_data_interested()%>%
        dplyr::filter(Fehlerhaft==TRUE)%>%
        dplyr::select(Lebensdauer,Fahrzeug_Typ,Motor_Typ)
        
      plot_ly(Data_Boxplot, x = ~Fahrzeug_Typ, y = ~Lebensdauer, color = ~Motor_Typ, type = "box")%>%
        layout(boxmode = "group")
    })
    
    
    #render kompletten Datensatz
    output$data_to_show <- DT::renderDT({
      Datensatz_Final
      
    })
    
    
}


#####################################################################
###App laufen
#####################################################################
shinyApp(ui, server)