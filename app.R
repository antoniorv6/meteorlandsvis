library(shiny)
library(reshape2)
library(dplyr)
library(xts)
library(dygraphs)
library(rbokeh)
library(shinythemes)
library(ggplot2)
library(plotly)
library(scales)
library(leaflet)
library(leaflet.providers)
library(ggmap)
library(leaflet.extras)
library(magrittr)
library(leafsync)
library(knitr)


# Define UI for app that draws a histogram ----
ui <- fluidPage( theme = shinytheme("flatly"),
    tags$head(
  # Css un poco guarro para  ajustar los parámetros por defecto de Bootstrap que quedan feos
  tags$style("body{background-color: #ffffff !important} .container-fluid{padding: 0 !important;} 
             .navbar-header{margin-left: 1em !important} 
             .navbar{margin:0 !important}"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")),
  # App title ----
  navbarPage("Meteorite Landing Visualization",
             tabPanel("Summary",
                      sidebarPanel(selectInput("n_breaks", label = "Recclass:",
                                               choices = unique(meteor.val$recclass), selected = 20),
                                   dateRangeInput("daterange1", "Rango de fechas:",
                                                  start = "1975-01-01",
                                                  end   = "2013-12-31")),
                      mainPanel(dygraphOutput(outputId = "distPlot")
                      )),
             tabPanel("Typology"
                      
                      ),
             tabPanel("Maps",
                      column(2, class = "futurepanel",style="z-index:10",
                        fluidRow(style="padding:1em",
                               selectInput("selClass", h3("Class"), ,choices = rbind("All",unique(meteor.val$recclass)), selected = 1)),
                        fluidRow(style="padding:1em", 
                               checkboxGroupInput("checkGroup", 
                                                  h3("Fall"), 
                                                  choices = list("Found" = "Found","Fell" = "Fell"),
                                                  selected = "Found")),
                        fluidRow(style="padding:1em",
                               h3("Discovery year"),
                               sliderInput("sli1", "",
                                           min = 1800, max = 2021, value = c(1800, 2021))
                        ),
                        fluidRow(style="padding:1em",
                               h3("Weight range"),
                               sliderInput("sli2", "",min = 0, max = 70000, value = c(0, 70000))
                        )),
                      
                      leafletOutput("map", height="900"),
                     ),
             tabPanel("3D Vis",
                    htmlOutput("show3D"))
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  meteor <- read.csv("Data/meteorite-landings.csv", sep = ",", header = TRUE)
  # limpiamos
  meteor.val <- meteor[!is.na(meteor$year) & !is.na(meteor$mass),]
  # obtenemos los cuartiles
  meteor.type.range <- c(quantile(meteor.val$mass[is.na(meteor.val$mass) == FALSE], 0.25),
                         quantile(meteor.val$mass[is.na(meteor.val$mass) == FALSE], 0.50),
                         quantile(meteor.val$mass[is.na(meteor.val$mass) == FALSE], 0.75))
  # clasificamos por tamaño según los cuartiles
  meteor.val$size <- ifelse(meteor.val$mass < meteor.type.range[1], "pequeño",
                            ifelse(meteor.val$mass < meteor.type.range[2], "mediano",
                                   ifelse(meteor.val$mass < meteor.type.range[3], "grande",
                                          "muy grande"
                                   )
                            )
  )
  # Agrupamos la informacion para crear el xts
  meteor.df <- meteor.val[,c("year","size")]
  meteor.df.group <- meteor.df %>% group_by(year, size, .add = TRUE)
  # meteor.df.group %>% group_vars()
  meteor.df.count <- meteor.df.group %>% summarise(n = n())
  # meteor.df.count %>% group_vars()
  meteor.df.pivot <- meteor.df.count %>% dcast(year ~ size, fill=0)
  meteor.xts <- xts(x = meteor.df.pivot[,-1], order.by = as.Date(paste(meteor.df.pivot$year,"-01-01", sep="")))
  
  
  
  output$distPlot = renderDygraph({
    meteor.xts.graph <- meteor.xts[index(meteor.xts) >= as.Date(input$daterange1[1]) & index(meteor.xts) < as.Date(input$daterange1[2])]
    dygraph(meteor.xts.graph, main = verbatimTextOutput("dateRangeText"))
  })
  
  output$show3D=renderUI({includeHTML("www/index.html")})
  
  
  ### Mapa 
  
  df <- meteor
  df$mass <- df$mass/1000
  
  # define center of map
  lat_center <- c(40.48250014304902)
  long_center <- c(-28.383444587235175)
  
  
  pal3 <- colorNumeric(
    palette = colorRamp(c("#fff76a", "#ff2828"), interpolate="spline"),
    domain = df$total_oc)
  
  output$map <- renderLeaflet({
    index_fall<-which(df$fall==input$checkGroup)  # if found or fell
    df<-df[index_fall,]
    
    if(input$selClass!="All"){
      index_class <- which(df$recclass==input$selClass)
      df <- df[index_class,]
    }
    
    index_year<-which(df$year>=input$sli1[1] & df$year<=input$sli1[2]) 
    df <- df[index_year,]
    
    index_mass <- which(df$mass>=input$sli2[1] & df$mass<=input$sli2[2]) 
    df <- df[index_mass,]
    
    mapa <- leaflet(df,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
      addCircles(lng = ~reclong, lat = ~reclat, weight = 1,  color = ~pal3(mass),
                 radius = ~ mass*3, fillOpacity = 0.5, popup = paste("<strong>Nombre:</strong>", df$name, "<br/>", "<strong>Masa:</strong>", df$mass,"kg","<br/>", "<strong>Tipo:</strong>",df$recclass,"<br/>","<strong>Año:</strong>",df$year))  %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(long_center,lat_center,3) %>%
      addLegend(pal = pal3, values = ~ mass, opacity = 1, title = "Masa(kg)")
  })
}

shinyApp(ui = ui, server = server)