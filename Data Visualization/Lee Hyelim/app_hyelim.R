
library(shiny)
library(dplyr)
library(highcharter)
library(hpackedbubble)
library(purrr)

wasabi <- read.csv("final_data0.csv") 
cont <- unique(wasabi$country)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(h1(strong("Preference of Songs by country"), align = "center")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel( h4(strong("Features Setting"),align = 'center'),  
                  selectInput(inputId = "country", "Click the country",
                              choices = cont, selected = "World"),
                  
                  sliderInput(inputId = "period", label = "Range of Year: ", min = 1980, max = 2015,
                               value = c(1980,2015)),
                  
                  radioButtons(inputId = "circle", "Info of Circle",
                                      choices = c('Publicated songs' = 'count_song',
                                                  'Available countries' = 'average_availableCountries', 
                                                  'Fans' = 'average_fans'), selected = "count_song"),
                  width = 2),
 
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    highchartOutput('Circle_world',height = "500px"),
                    hpackedbubbleOutput('Circle_country',height = "500px"))
      ),
      width = 8)
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({wasabi %>% filter(year >= input$period[1] & year <= input$period[2]) %>%
                     select(continent,country,genre,count_song,average_availableCountries,average_fans)})
  data2 <- reactive({wasabi %>% filter(year >= input$period[1] & year <= input$period[2]) %>%
      filter(country == input$country) %>% 
      select(country,genre,count_song,average_availableCountries,average_fans) %>% 
      group_by(country,genre) %>% 
      summarise(count_song = sum(count_song, na.rm=T),
                average_availableCountries = mean(average_availableCountries, na.rm=T),
                average_fans = mean(average_fans, na.rm=T))
    })

  observe(if (input$circle == 'count_song') {
    tmp <- reactive({data() %>% group_by(continent,country)  %>% 
        summarise(count = sum(count_song)) %>% filter(continent != "NA") })
    tmp2 <- reactive({data2() %>% select(country,genre,count_song)})
    
    data_node <- reactive({as.Node(tmp2())})
    
    dt_africa <-  map(seq(nrow(tmp() %>% filter(continent=='Africa'))), function(x) {
      list(name = tmp() %>% filter(continent=='Africa') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Africa') %>% .[x,3] %>% pull(),
           description = 'Africa')})
    dt_europe <-  map(seq(nrow(tmp() %>% filter(continent=='Europe'))), function(x) {
      list(name = tmp() %>% filter(continent=='Europe') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Europe') %>% .[x,3] %>% pull(),
           description = 'Europe')})
    dt_asia <-  map(seq(nrow(tmp() %>% filter(continent=='Asia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Asia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Asia') %>% .[x,3] %>% pull(),
           description = 'Asia')})
    dt_north <-  map(seq(nrow(tmp() %>% filter(continent=='North America'))), function(x) {
      list(name = tmp() %>% filter(continent=='North America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='North America') %>% .[x,3] %>% pull(),
           description = 'North America')})
    dt_south <-  map(seq(nrow(tmp() %>% filter(continent=='South America'))), function(x) {
      list(name = tmp() %>% filter(continent=='South America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='South America') %>% .[x,3] %>% pull(),
           description = 'South America')})
    dt_aust <-  map(seq(nrow(tmp() %>% filter(continent=='Australia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Australia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Australia') %>% .[x,3] %>% pull(),
           description = 'Australia')})
    output$Circle_world <- renderHighchart({
      highchart() %>% 
        hc_chart(type = 'packedbubble', height = '100%') %>% 
        hc_tooltip(useHTML = T, pointFormat = 'Location: {point.name}, Number of Songs : {point.value}') %>% 
        hc_title(text = "Proportion by contry in the world",margin = 10,
                 style = list(color = "#6495ED",fontSize="20px" ,useHTML = TRUE)) %>%
        hc_plotOptions(packedbubble = list(minSize= '20%',maxSize= '100%',zMin = 0,zMax = 1000,
          layoutAlgorithm = list(gravitationalConstant=0.05,splitSeries=T,seriesInteraction=F,dragBetweenSeries=T,parentNodeLimit=T))) %>%
        hc_add_series(name = 'Africa', data = dt_africa ) %>%
        hc_add_series(name = 'Europe', data = dt_europe ) %>%
        hc_add_series(name = 'Asia', data = dt_asia) %>%
        hc_add_series(name = 'North America', data = dt_north ) %>%
        hc_add_series(name = 'South America', data = dt_south ) %>%
        hc_add_series(name = 'Australia', data = dt_aust )
      })
    output$Circle_country <- renderHpackedbubble({
      hpackedbubble(tmp2()$country,tmp2()$genre, tmp2()$count_song,
                    title = paste0("Proportion by genre of ",input$country),
                    titleSize = "20px",
                    titleColor = "#6495ED",
                    pointFormat = "Genre : {point.name}, Number of Songs : {point.value}",
                    dataLabelsFilter = 100,
                    packedbubbleMinSize = "20%",
                    packedbubbleMaxSize = "90%",
                    theme = "sunset",
                    packedbubbleZMin = 0,
                    packedbubbleZmax = 1000, split = 1,
                    gravitational = 0.02,
                    parentNodeLimit = 1,
                    dragBetweenSeries = 0,
                    seriesInteraction = 0,
                    width = "90%")})
    })
  
  observe(if (input$circle == 'average_availableCountries') {
    tmp <- reactive({data() %>% group_by(continent,country)  %>% 
        summarise(count = mean(average_availableCountries)) %>% filter(continent != "NA") })
    tmp2 <- reactive({data2() %>% select(country,genre,average_availableCountries)})
    
    dt_africa <-  map(seq(nrow(tmp() %>% filter(continent=='Africa'))), function(x) {
      list(name = tmp() %>% filter(continent=='Africa') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Africa') %>% .[x,3] %>% pull(),
           description = 'Africa')})
    dt_europe <-  map(seq(nrow(tmp() %>% filter(continent=='Europe'))), function(x) {
      list(name = tmp() %>% filter(continent=='Europe') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Europe') %>% .[x,3] %>% pull(),
           description = 'Europe')})
    dt_asia <-  map(seq(nrow(tmp() %>% filter(continent=='Asia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Asia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Asia') %>% .[x,3] %>% pull(),
           description = 'Asia')})
    dt_north <-  map(seq(nrow(tmp() %>% filter(continent=='North America'))), function(x) {
      list(name = tmp() %>% filter(continent=='North America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='North America') %>% .[x,3] %>% pull(),
           description = 'North America')})
    dt_south <-  map(seq(nrow(tmp() %>% filter(continent=='South America'))), function(x) {
      list(name = tmp() %>% filter(continent=='South America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='South America') %>% .[x,3] %>% pull(),
           description = 'South America')})
    dt_aust <-  map(seq(nrow(tmp() %>% filter(continent=='Australia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Australia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Australia') %>% .[x,3] %>% pull(),
           description = 'Australia')})
    output$Circle_world <- renderHighchart({
      highchart() %>% 
        hc_chart(type = 'packedbubble', height = '100%') %>% 
        hc_tooltip(useHTML = T, pointFormat = 'Location: {point.name}, Average of Available countries : {point.value}') %>% 
        hc_title(text = "Proportion by contry in the world",margin = 10,
                 style = list(color = "#6495ED",fontSize="20px" ,useHTML = TRUE)) %>%
        hc_plotOptions(packedbubble = list(minSize= '20%',maxSize= '100%',zMin = 0,zMax = 1000,
                                           layoutAlgorithm = list(gravitationalConstant=0.05,splitSeries=T,seriesInteraction=F,dragBetweenSeries=T,parentNodeLimit=T))) %>%
        hc_add_series(name = 'Africa', data = dt_africa ) %>%
        hc_add_series(name = 'Europe', data = dt_europe ) %>%
        hc_add_series(name = 'Asia', data = dt_asia ) %>%
        hc_add_series(name = 'North America', data = dt_north ) %>%
        hc_add_series(name = 'South America', data = dt_south ) %>%
        hc_add_series(name = 'Australia', data = dt_aust )
    })
    output$Circle_country <- renderHpackedbubble({
      hpackedbubble(tmp2()$country,tmp2()$genre, tmp2()$average_availableCountries,
                    title = paste0("Proportion by genre of ",input$country),
                    titleSize = "20px",
                    titleColor = "#6495ED",
                    pointFormat = "Genre : {point.name}, Average of Available countries : {point.value}",
                    dataLabelsFilter = 100,
                    packedbubbleMinSize = "20%",
                    packedbubbleMaxSize = "90%",
                    theme = "sunset",
                    packedbubbleZMin = 0,
                    packedbubbleZmax = 1000, split = 1,
                    gravitational = 0.02,
                    parentNodeLimit = 1,
                    dragBetweenSeries = 0,
                    seriesInteraction = 0,
                    width = "90%")})
  })
  
  observe(if (input$circle == 'average_fans') {
    tmp <- reactive({data() %>% group_by(continent,country)  %>% 
        summarise(count = mean(average_fans)) %>% filter(continent != "NA") })
    tmp2 <- reactive({data2() %>% select(country,genre,average_fans)})
    
    dt_africa <-  map(seq(nrow(tmp() %>% filter(continent=='Africa'))), function(x) {
      list(name = tmp() %>% filter(continent=='Africa') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Africa') %>% .[x,3] %>% pull(),
           description = 'Africa')})
    dt_europe <-  map(seq(nrow(tmp() %>% filter(continent=='Europe'))), function(x) {
      list(name = tmp() %>% filter(continent=='Europe') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Europe') %>% .[x,3] %>% pull(),
           description = 'Europe')})
    dt_asia <-  map(seq(nrow(tmp() %>% filter(continent=='Asia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Asia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Asia') %>% .[x,3] %>% pull(),
           description = 'Asia')})
    dt_north <-  map(seq(nrow(tmp() %>% filter(continent=='North America'))), function(x) {
      list(name = tmp() %>% filter(continent=='North America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='North America') %>% .[x,3] %>% pull(),
           description = 'North America')})
    dt_south <-  map(seq(nrow(tmp() %>% filter(continent=='South America'))), function(x) {
      list(name = tmp() %>% filter(continent=='South America') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='South America') %>% .[x,3] %>% pull(),
           description = 'South America')})
    dt_aust <-  map(seq(nrow(tmp() %>% filter(continent=='Australia'))), function(x) {
      list(name = tmp() %>% filter(continent=='Australia') %>% .[x,2] %>% pull(),
           value = tmp() %>% filter(continent=='Australia') %>% .[x,3] %>% pull(),
           description = 'Australia')})
    output$Circle_world <- renderHighchart({
      highchart() %>% 
        hc_chart(type = 'packedbubble', height = '100%') %>% 
        hc_tooltip(useHTML = T, pointFormat = 'Location: {point.name}, Average of Fans : {point.value}') %>%
        hc_title(text = "Proportion by contry in the world",margin = 10,
                 style = list(color = "#6495ED",fontSize="20px" ,useHTML = TRUE)) %>%
        hc_plotOptions(packedbubble = list(minSize= '20%',maxSize= '100%',zMin = 0,zMax = 1000,
                                           layoutAlgorithm = list(gravitationalConstant=0.05,splitSeries=T,seriesInteraction=F,dragBetweenSeries=T,parentNodeLimit=T))) %>%
        hc_add_series(name = 'Africa', data = dt_africa ) %>%
        hc_add_series(name = 'Europe', data = dt_europe ) %>%
        hc_add_series(name = 'Asia', data = dt_asia ) %>%
        hc_add_series(name = 'North America', data = dt_north ) %>%
        hc_add_series(name = 'South America', data = dt_south ) %>%
        hc_add_series(name = 'Australia', data = dt_aust )
    })
    
    output$Circle_country <- renderHpackedbubble({
      hpackedbubble(tmp2()$country,tmp2()$genre, tmp2()$average_fans,
                    title = paste0("Proportion by genre of ",input$country),
                    titleSize = "20px",
                    titleColor = "#6495ED",
                    pointFormat = "Genre : {point.name}, Average of Fans : {point.value}",
                    dataLabelsFilter = 100,
                    packedbubbleMinSize = "20%",
                    packedbubbleMaxSize = "90%",
                    theme = "sunset",
                    packedbubbleZMin = 0,
                    packedbubbleZmax = 1000, split = 1,
                    gravitational = 0.02,
                    parentNodeLimit = 1,
                    dragBetweenSeries = 0,
                    seriesInteraction = 0,
                    width = "90%")})
  })
}
    




# Run the application 
shinyApp(ui = ui, server = server)
