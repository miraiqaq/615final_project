#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Read and Clean Data
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
if (!require(tmap)) install.packages('tmap')
library(tmap)
if (!require(maps)) install.packages('maps')
library(maps)
rawdata <- read.csv("video_game_sales.csv")
mydata <- rawdata

#Remove observations without "Name"
mydata$Name <- as.character(mydata$Name)
mydata <- filter(mydata, Name!="")

#Remove game released after 2017 and NA
mydata$Year_of_Release <- as.numeric(as.character(mydata$Year_of_Release))
mydata <- filter(mydata,Year_of_Release<2017,Year_of_Release>1982)

byPlatform <- mydata[,c("Year_of_Release","Platform","NA_Sales","EU_Sales","JP_Sales","Other_Sales")]
platform_count <- byPlatform %>%
  group_by(Year_of_Release,Platform) %>%
  summarise(NA_Sales=sum(NA_Sales),EU_Sales=sum(EU_Sales),JP_Sales=sum(JP_Sales),Other_Sales=sum(Other_Sales))
new <- gather(platform_count,area,sales,NA_Sales,EU_Sales,JP_Sales,Other_Sales)
new$Platform <- as.character(new$Platform)


library(shiny)
library(shinydashboard)
library(plotly)
# Define UI 
year_list <- c(unique(as.character(new$Year_of_Release)))
genre_list <- c(unique(as.character(mydata$Genre)))

ui <- dashboardPage(
  skin = "black",
  # Application title
  dashboardHeader(title = "Video Game Sales"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Sales by Platform", tabName = "a", icon = icon("globe")),
      menuItem("Sales by Genre", tabName = "b", icon = icon("bar-chart-o")),
      menuItem("Benford Law Test", tabName = "c", icon = icon("stethoscope")),
      menuItem("Word Cloud", tabName = "d", icon = icon("wordpress")),
      menuItem("Top 10 Games", tabName = "e", icon = icon("thumbs-up", lib = "glyphicon")),
      menuItem("Raw Data Explore", tabName = "f", icon = icon("table")))
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "a",
      fluidRow(column(width = 8,
                      box(title = "Global Sales Map by Platform and Year",width = NULL, solidHeader = TRUE,
                          plotlyOutput("map", height = 500))),
               column(width = 4,
                      box(width = NULL, status = "warning",
                          selectInput("selectyear1",label = "Select Year:",choices=year_list),
                          selectInput("selectplatform1",label="Select Platform:",choices="")),
                      box(title = "Pie chart of genres",width = NULL, solidHeader = TRUE,
                          plotlyOutput("pie", height = 310)))
               )
    ),
    tabItem(
      tabName = "b",
      fluidRow(column(width = 12,
                      box(width = NULL,title="Sales by Genre",solidHeader = TRUE,
                          selectInput("selectgenre1",label="Select Genre:",choices=genre_list),
                          plotlyOutput("bar", height = 500)))
               )
    ),
    tabItem(
      tabName = "c",
      fluidRow(column(width = 12,
                      box(title = "Benford Law Test",width = NULL,solidHeader = TRUE,
                          plotOutput("benford",height = 400))),
               column(width = 12,
                      infoBoxOutput("benfordresult",width = NULL))
              )
    ),
    tabItem(
      tabName = "d",
      fluidRow(column(width = 6,
                      box(width = NULL,height = 550,
                          selectInput("selectyear2",label="Select Year:",choices=year_list),
                          sliderInput("freq","Minimum Frequency:",min = 1,  max = 10, value = 5),
                          sliderInput("max","Maximum Number of Words:",min = 1,  max = 300, value = 150))),
               column(width = 6,
                      box(width = NULL,title="Word Cloud",solidHeader = TRUE,
                          plotOutput("word",height = 500)))
               )
    ),
    tabItem(
      tabName = "e",
      fluidRow(column(width = 6,
                      box(width = NULL,title = "TOP 10 GAMES",solidHeader = TRUE,status = "primary",
                          selectInput("selectyear3",label="Select Year:",choices=year_list),
                          tableOutput("table1")))
               )
    ),
    tabItem(
      tabName = "f",
      numericInput("maxrows", "Rows to show:", 25),
      box(title = "Raw Dataset", collapsible = TRUE,
          solidHeader = TRUE, DT::dataTableOutput("table2"), width = 12, height = 450),
      downloadButton("downloadCsv", "Download as CSV")
    )
    
  ))
)

# Define server 
server <- function(input, output,session) {
  observeEvent(input$selectyear1,{
    updateSelectInput(session,'selectplatform1',
                      choices=c("choose one"="",unique(new[as.character(new$Year_of_Release)==as.character(input$selectyear1),"Platform"])))
  }) 
  
  #Tab 1 map
  output$map <- renderPlotly({
    ex_map_data <- new %>% 
      filter(Year_of_Release==input$selectyear1,Platform==input$selectplatform1)
    
    library(tmap)
    data(World)
    wm <- map_data("world")
    cc <- raster::ccodes()
    mappings <- c("North America"="NA_Sales", "Europe"="EU_Sales")
    cc$MYCONTINENTS <- mappings[cc$continent]
    cc$MYCONTINENTS <- ifelse(cc$NAME=="Japan","JP_Sales",cc$MYCONTINENTS)
    cc$MYCONTINENTS <- ifelse(is.na(cc$MYCONTINENTS),"Other_Sales",cc$MYCONTINENTS)
    cc <- left_join(cc, ex_map_data, by = c("MYCONTINENTS"="area"))
    wm$region %>% 
      unique %>% 
      setdiff(cc$NAME)
    ## UK is called United Kingdom in cc:
    unique(grep("Kingdom", cc$NAME, value=T, ignore.case=T))
    mappings <- c("UK"="United Kingdom", "USA"="United States")
    cc$NAME[match(mappings, cc$NAME)] <- names(mappings)
    wm <- left_join(wm, cc[,c("NAME","MYCONTINENTS", "sales")], by=c("region"="NAME"))
    
    p <- ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group, fill=sales), wm, colour = NA) +
      coord_quickmap() +
      scale_fill_gradient(low="light blue", high="red") +
      theme_void()
    library(plotly)
    ggplotly(p)
  })
  
  #Tab 1 pie chart
  output$pie <- renderPlotly({
    if (input$selectplatform1 != "") {
    genre_count <- mydata %>%
      select(Year_of_Release,Platform,Genre) %>%
      filter(Year_of_Release== input$selectyear1& Platform== input$selectplatform1) %>%
      group_by(Genre) %>%
      summarise(count=n())
    
    genre_pie <- plot_ly(genre_count, labels = ~Genre, values = ~count, type = 'pie',textposition = 'inside',textinfo = 'percent+label') %>%
      layout(showlegend = FALSE)
    
    ggplotly(genre_pie)
    }
  })
  
  #Tab 2 bar plot
  output$bar <- renderPlotly({
    mydata$Genre <- as.character(mydata$Genre)
    games_y1<-mydata %>%
      filter(Genre == input$selectgenre1) %>%
      select(Year_of_Release,EU_Sales,JP_Sales,NA_Sales,Other_Sales)
    games_long<-gather(games_y1,Region,TotalSales,EU_Sales:Other_Sales)
    
    q <- ggplot(games_long,aes(x=Year_of_Release,y=TotalSales,fill=Region))+
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(q)
  })
  
  #Tab 3 Benford
  output$benford <- renderPlot({
    library(benford.analysis)
    bfd <- benford(mydata$Global_Sales)
    plot(bfd)
  })
  
  #Tab 3 Result
  myresult <- "The p-value of Euclidean Distance Test is smaller than 0.05 so that we reject the null hypothesis. Therefore, the goodness-of-fit test based on the Euclidean distance between the first digits' distribution and Benford's distribution shows the data does not conform to Benford's law very well."
  output$benfordresult <- renderInfoBox({
    infoBox(
      "Result", myresult, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  #Tab 4 Word Cloud
  output$word <- renderPlot({
    library(tidytext)
    library(dplyr)
    text <- mydata %>%
      filter(Year_of_Release == input$selectyear2) %>%
      select(Name) %>%
      unnest_tokens(word, Name)
    
    data(stop_words)
    tidy_text <- text %>%
      anti_join(stop_words)
    
    library(wordcloud)
    tidy_text%>%
      count(word) %>%
      with(wordcloud(word, n, min.freq = input$freq,max.words = input$max,colors=brewer.pal(8, "Dark2")))
  })
  
  #Tab 5 Top 10 Games
  output$table1 <- renderTable({
    toptable <- mydata %>%
      select(Name,Global_Sales,Year_of_Release) %>%
      filter(Year_of_Release==input$selectyear3) %>%
      arrange(desc(Global_Sales)) %>%
      distinct(Name,Global_Sales) %>%
      head(10)
  },digits = 2)
  
  #Tab 6 Raw Data Table
  output$table2<- DT::renderDataTable({
    tabledata <- rawdata[1:input$maxrows,]
    DT::datatable(tabledata, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  #Tab 6 csv file
  output$downloadCsv <- downloadHandler(
    filename = "rawdata.csv",
    content = function(file) {
      write.csv(rawdata, file)
    },
    contentType = "text/csv"
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

