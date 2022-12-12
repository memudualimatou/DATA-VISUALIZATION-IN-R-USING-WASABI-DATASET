




setwd("C:/Users/Alimat sadia/Desktop/MSC DS &  AI/Data visualization/Work project")
library(shiny)
library(highcharter)
library(dplyr)
library(ggplot2)
library(plotly)

my_data <- read.csv('final_data.csv')


wmap <- hcmap(showInLegend = FALSE)


ui <- fluidPage(
  titlePanel("     "),
  sidebarLayout(
    sidebarPanel( h3(strong("Features Setting"),height=400),  
                  
                  sliderInput(inputId = "period", label = "Range of Year: ", min = 1980, max = 2015,
                              value = c(1980,2015)),
                  
                  selectInput(inputId="genre",label="Choose the Genre of Album:",
                              choices = c("Country","Folk", "Hip Hop","Jazz","Metal", "Others","Pop","R&B","Rock","Unknown"),
                              selected = "Rock",multiple = T),
                  
                  radioButtons(inputId = "gender", "Select Artist Gender: ",
                               choices = c('Male','Female' , 'Unknown'), selected = "Unknown"),
                  
                  checkboxGroupInput(inputId = "artist_lifespan", "Check Artist Ended Life Span",
                                     choices = c(TRUE,FALSE), selected = FALSE),
                  
                  selectInput(inputId = "bubble", "Check Repartition of : ",
                               choices = c('Albums'= 'count_album','Songs' = 'count_song', 
                                           "Both" = 'both'), selected = "count_song"),
                  
                  h5(strong("Total Number of Songs")),
                  textOutput("total_number_song"),
                  
                  h5(strong("Total Number of Artist")),
                  textOutput("total_number_artist"),
                  
                  h5(strong("Total Number of Albums")),
                  textOutput("total_number_album")
                  ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
      tabPanel(title="Bubble Map",
      highchartOutput('eqmap',height = "670px"),
      verbatimTextOutput("Unknowns")),
      
      tabPanel(title="Stacked Bar Graph",
               uiOutput("barplot")))
  )
)

)

server <- function(input, output) { 
  
  data <- reactive(my_data %>% filter(count_song >= 1) %>% filter(year >= input$period[1]) %>%
            filter(year <= input$period[2] & gender==input$gender & lifeSpan.ended==input$artist_lifespan & genre==input$genre)
            %>% group_by(country, lat, lon, continent))

  observe(if (input$bubble == 'count_song') {
    data_1 <- reactive(data() %>% rename(z = input$bubble) %>% summarise(z = sum(z),count_artist=sum(count_artist), .groups="keep") %>% filter(z >= 1))
    
    bubble_size <- reactive(data_1()$z)
    
    output$eqmap <-renderHighchart(
      
      wmap %>% 
        hc_add_series(data = subset(data_1(), continent == "Africa"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#3D7749", name = "Africa",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Songs : {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Europe"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F77099", name = "Europe",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Songs :  {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Asia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F5F508", name = "Asia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist},Number of Songs :  {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Central America" | continent == "North America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#10EEBC", name = "North America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Songs :  {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "South America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#E633FF", name = "South America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Songs : d: {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Australia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#FF5733", name = "Australia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Songs :  {point.z}')) %>%
        
        hc_legend(align = "right", layout = "horizontal", horizontalAlign = "bottom",
                  floating = TRUE ,valueDecimals = 0,
                  symbolHeight = 10, symbolWidth = 10, symbolRadius = 0,
                  
                  bubbleLegend = list(
                    enabled = TRUE,
                    borderColor = 'black',
                    borderWidth = 1,
                    color = 'white',
                    connectorColor = '#000000',
                    sizeBy = bubble_size())) %>%
        
        hc_tooltip() %>%
        
        hc_title(text = "<b> WORLDWIDE REPARTITION OF WASABI DATASET</b> ",margin = 10,
                 style = list(color = "#6495ED",fontSize="30px" ,useHTML = TRUE)) %>%
        
        hc_mapNavigation(enabled = T)%>% 
        hc_exporting(enabled = TRUE) 
    )
  })

  observe(if (input$bubble == 'count_album') {
    data_1 <- reactive(data() %>% rename(z = count_album) %>% summarise(z = sum(z),count_artist=sum(count_artist), .groups="keep") %>% filter(z >= 1))

    bubble_size <- reactive(data_1()$z)
    
    output$eqmap <-renderHighchart(
      
      wmap %>% 
        hc_add_series(data = subset(data_1(), continent == "Africa"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#3D7749", name = "Africa",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Albums : {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Europe"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F77099", name = "Europe",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Albums : {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Asia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F5F508", name = "Asia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Albums :  {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Central America" | continent == "North America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#10EEBC", name = "North America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Albums :  {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "South America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#E633FF", name = "South America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist}, Number of Albums : {point.z}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Australia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#FF5733", name = "Australia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.count_artist},Number of Albums :  {point.z}')) %>%
        
        hc_legend(align = "right", layout = "horizontal", horizontalAlign = "bottom",
                  floating = TRUE ,valueDecimals = 0,
                  symbolHeight = 10, symbolWidth = 10, symbolRadius = 0,
                  
                  bubbleLegend = list(
                    enabled = TRUE,
                    borderColor = 'black',
                    borderWidth = 1,
                    color = 'white',
                    connectorColor = '#000000',
                    sizeBy = bubble_size())) %>%
        
        hc_tooltip() %>%
        
        hc_title(text = "<b> WORLDWIDE REPARTITION OF WASABI DATASET</b> ",margin = 10,
                 style = list(color = "#6495ED",fontSize="30px" ,useHTML = TRUE)) %>%
        hc_mapNavigation(enabled = T)%>% 
        hc_exporting(enabled = TRUE) 
    )
  }) 
  
  observe(if (input$bubble == "both") {

    data_1 <- reactive(data() %>% rename(z = count_album, z1 = count_song,z2=count_artist) %>% summarise(z = sum(z), z1 = sum(z1),z2 = sum(z2), .groups="keep"))

    bubble_size_alb <- reactive(data_1()$z+data_1()$z1)
    
    output$eqmap <-renderHighchart(
      
      wmap %>% 
        hc_add_series(data = subset(data_1(), continent == "Africa"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#3D7749", name = "Africa", 
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2}, Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>% 
        
        hc_add_series(data = subset(data_1(), continent == "Europe"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F77099", name = "Europe",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2},  Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Asia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#F5F508", name = "Asia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2}, Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Central America" | continent == "North America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#10EEBC", name = "North America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2},  Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "South America"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#E633FF", name = "South America",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2},  Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>%
        
        hc_add_series(data = subset(data_1(), continent == "Australia"), type = "mapbubble", minSize = "1.5%", maxSize = '15%', showInLegend = TRUE, color = "#FF5733", name = "Australia",
                      tooltip = list(pointFormat = 'Location: {point.country}, Number of Artists : {point.z2},  Number of Albums :  {point.z}, Number of Songs : {point.z1}')) %>%
        
      
        
        hc_legend(align = "right", layout = "horizontal", horizontalAlign = "bottom",
                  floating = TRUE ,valueDecimals = 0,
                  symbolHeight = 10, symbolWidth = 10, symbolRadius = 0,
                  
                  bubbleLegend = list(
                    enabled = TRUE,
                    borderColor = 'black',
                    borderWidth = 1,
                    color = 'white',
                    connectorColor = '#000000',
                    sizeBy = bubble_size_alb())) %>%
        
        hc_tooltip() %>%
        
        hc_title(text = "<b> WORLDWIDE REPARTITION OF WASABI DATASET</b> ",margin = 10,
                 style = list(color = "#6495ED",fontSize="30px" ,useHTML = TRUE))%>%
        hc_mapNavigation(enabled = T)
    )
    
  })
  
  
  dart1<-reactive(data() %>% summarise(x=sum(count_artist), .groups="keep"))
  output$total_number_artist<-reactive(sum(dart1()$x))
  
  dart2<-reactive(data() %>% summarise(x=sum(count_song), .groups="keep"))
  output$total_number_song<-reactive(sum(dart2()$x))
  
  dart3<-reactive(data() %>% summarise(x=sum(count_album), .groups="keep"))
  output$total_number_album<-reactive(sum(dart3()$x))
  

  output$barplot <- renderUI({
   p<-data() %>% na.omit() %>% group_by(continent,year)%>%summarize(deezer=sum(deezer_fans), .groups="keep") %>% ggplot(aes(x=cut_number(year,10),y=deezer))+geom_col(aes(fill=continent), width = 0.8) + coord_flip()+labs(x="Year",y="Number of Deezer Fans",title="NUMBER OF DEEZER FANS PER CONTINENT \n")+ scale_fill_manual(values = c( "#3D7749","#F5F508", "#FF5733","#F77099","#10EEBC","#E633FF"))+  scale_y_continuous(labels = scales::comma)+ theme(plot.title = element_text(size = 20, face = "bold",color = "#6495ED"))
   
   ggplotly(p,height=670,width = 1100) 
    
  
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
