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
             tabPanel("Typology", style="height = 100%",
                      column(9, style = "background-color: none;",
                             h3("Mass by class"),
                             fluidRow(plotlyOutput("classBox")),
                             column(6,
                             h3("Class of meteorites found"),
                            fluidRow(plotlyOutput("percent"))),
                            column(6, div(img(src="./chon.png", height="80%", width="80%", align="center")),
                            h4("Chondryte meteor, from Sahara desert")
                            )
                          ),
                      column(3,
                             h3("Classification"),
                             h4("Ordinary chondrites"),
                             p("They are classfied by iron cuantity. His origin is from little asteroids. x is the texture number, indicates the evololution of father's body."),
                             tags$ul(
                               tags$li("Lx : Low iron"), 
                               tags$li("LLx: Very low iron"), 
                               tags$li("Hx: High iron")
                             ),
                             h4("Carbonaceus chondrites"),
                             p("It contains up to 5% of its weight in carbon. Their main metals are olivine and serpentine, along with Fe and Ni. They can contain up to 20% water and up to 7% organic compounds. They come from asteroids and perhaps comets.")
                             ,tags$ul(
                               tags$li("CM: They contain 15% less chondrules"), 
                               tags$li("CO: They contain 35-40% chondrules")
                             ),
                             h4("Metallic"),
                             p("They generally come from large asteroids. They are characterized by being composed of more than 90% metal.")
                             ,tags$ul(
                               tags$li("IIAB: Medium to coarse octahedrite. They present troilite and graphite nodules, with a rare presence of silicates.")
                             ),
                             h4("Achondrites"),
                             p("Achondrites are igneous rocks, like volcanic rocks. Its initial content has been completely transformed due to high heat. They are characterized by having little metal (less than 1%) and are classified according to their origin and calcium level.")
                             ,tags$ul(
                               tags$li("Ureilite: They are the achondrites poor in calcium. They are the rarest meteorites of all, rich in graphite, clinobronzite, olivines, diamonds and silicates.")
                             )
                      )),
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
  
  
  # Typology page
  
  ## box plt
  datos_by_class <- meteor %>% group_by(recclass)  %>%
    count(recclass, sort = TRUE)
  
  datos_by_class <- datos_by_class[1:15,]
  datos_by_class <- transform(datos_by_class, recclass = as.character(recclass))
  index <- which(meteor$recclass %in% datos_by_class[,1])
  
  datclass <- meteor[index,]
  
  g <-ggplot(datclass, aes(x=recclass, y= log(mass), fill=recclass)) +
    geom_boxplot() +
    xlab("Class") + ylab("Log(Mass) Kg") +
    scale_fill_manual(values=c('#8b4513','#9f6934','#5421d3','#002b73','#3c2ac5','#0c203d','#002861','#a9a9a9','#6c0de0','#232eb6','#00244f','#092c69','#002e84','#0030a6','#ffc0cb'))
  
  output$classBox <-renderPlotly({ggplotly(g)})
  
  ## pie chart
  datos_by_class1 <- meteor  %>% group_by(recclass)  %>%
    count(recclass, sort = TRUE)
  
  other <- list("recclass"="Other", "n"=sum(datos_by_class1[-c(1:15), 2]))
  datos_by_class2 <- rbind(datos_by_class1[1:15,] , other)

  datos_by_class2 <- datos_by_class2 %>% 
    arrange(desc(recclass)) %>%
    mutate(prop = n / sum(datos_by_class2$n) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop ) 
  
  datos_by_class2  <- datos_by_class2[order(datos_by_class2$prop),] 

  
  p <- plot_ly(datos_by_class2, labels = ~recclass, values = ~n, type = 'pie',textposition = 'outside',textinfo = 'label+percent', marker = list(colors = c( '#a9a9a9','#ffc0cb','#9f6934','#4f30bf','#7523d6','#8b4513','#622ccb','#2e33a4','#203396','#143187','#0c2f78','#092c69','#0c2959','#10254a','#E7E7D0','#5421d3'))) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  output$percent <-renderPlotly({ggplotly(p)})
}

shinyApp(ui = ui, server = server)