library(shiny)
library(reshape2)
library(dplyr)
library(xts)
library(dygraphs)
library(rbokeh)
library(shinythemes)


# Define UI for app that draws a histogram ----
ui <- fluidPage( theme = shinytheme("flatly"),
  # Css un poco guarro para  ajustar los parámetros por defecto de Bootstrap que quedan feos
  tags$style("body{background-color: #ffffff !important} .container-fluid{padding: 0 !important;} 
             .navbar-header{margin-left: 1em !important} 
             .navbar{margin:0 !important}"),
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
             tabPanel("Maps"),
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
  
}

shinyApp(ui = ui, server = server)