library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(shinyjs)
library(sodium)
library(data.table)
library(TTR)
library(forecast)
library(party)
library(randomForest)
theme_set(theme_classic())

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: rasmila  Password: rasmila"),
                     br(),
                     tags$code("Username: myuser  Password: mypass")
                   ))
)

credentials = data.frame(
  username_id = c("rasmila", "myuser"),
  passod   = sapply(c("rasmila", "mypass"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)



# function


detail_df <- read.csv("RestaurantSalesDetail.csv")
master_df <- read.csv("RestaurantSalesMaster.csv")


Get_RestaurantSales <- function(resturantName){
  master_df$BillDate <- as.Date(master_df$BillDate)
  restaurantSales <- master_df %>% filter(Name %in% resturantName)
  return(restaurantSales)
}
Get_RestaurantSalesDetail <- function(resturantName){
  detail_df$Date <- as.Date(detail_df$Date)
  restaurantSalesDetail <- detail_df %>% filter(Name %in% resturantName)
  return(restaurantSalesDetail)
}

Get_ResturantName <- function(){
  master_df$BillDate <- as.Date(master_df$BillDate)
  detail_df$Date <- as.Date(detail_df$Date)
  head <- master_df %>% 
    select(Name) %>%
    distinct()
  return(head)
}

Get_Item_data <- function(itemName, resturantName){
  data <-  detail_df %>% filter(Name %in% resturantName)
  data <- data %>% filter(ItemName %in% itemName)
  data$Date <- as.Date(data$Date)
  return(data)
}

Get_TopNumber <- function(resturantName, date){
  date <- as.Date(date)
  master_df$BillDate <- as.Date(master_df$BillDate)
  main <- master_df %>% filter(Name %in% resturantName)
  main <- main %>% filter(BillDate %in% date)%>% select(NetAmount,TotalDiscount,BasicAmount,ServiceCharge,Vat)
  main <- main %>%
    group_by(main$BillDate) %>%
    summarise(NetAmount = sum(NetAmount)
              ,TotalDiscount = sum(TotalDiscount)
              ,BasicAmount = sum(BasicAmount)
              ,ServiceCharge = sum(ServiceCharge)
              ,Vat = sum(Vat)) 
  return(main)
}



Get_Top10List_date <- function(resturantName, date){
  date <- as.Date(date)
  detail_df$Date <- as.Date(detail_df$Date)
  
  top10data <- detail_df %>% filter(Name %in% resturantName)
  top10data <- top10data %>% filter(Date %in% date) %>% select(ItemName,Quantity,Rate,Total)
  
  top10data <- top10data %>%
    group_by(ItemName) %>%
    summarise(Quantity = sum(Quantity)
              ,Rate = sum(Rate)
              ,Total = sum(Total)) 
  top10data <- top10data[order(-top10data$Quantity),] %>%
    slice(1:10)
  
  return(top10data)
}


Get_ItemName <- function(resturantName){
  itemName <- detail_df %>% filter(Name %in% resturantName)
  itemName <- itemName %>% 
    select(ItemName) %>%
    distinct()
  return(itemName)
}

GetSales <- function(date){
  date <- as.Date(date)
  nextdate <- as.Date(date + 6);
  master_df$BillDate <- as.Date(master_df$BillDate)
  sales <- master_df 
  sales <- sales %>% filter(BillDate >= date & BillDate <= nextdate)
  return(sales)
}

GetMonthSales <- function(date){
  date <- as.Date(date)
  nextdate <- lubridate::month(date)
  year <- lubridate::year(date)

  master_df$BillDate <- as.Date(master_df$BillDate)
  mm <- master_df 
  mm$BillDate <- as.Date(mm$BillDate)
  mm$year = lubridate::year(mm$BillDate)
  mm$month = lubridate::month(mm$BillDate)
  
  mm <- mm %>% filter(month == nextdate & year == year)
  return(mm)
  
}
Get_PredictItem_data <- function(itemName, resturantName){
  detail_df$Date<- as.Date(detail_df$Date)
  dat <-  detail_df %>% filter(Name %in% resturantName)
  dat <- dat %>% filter(ItemName %in% itemName)
  dat$year = lubridate::year(dat$Date)
  dat$yday = yday(dat$Date)
  dat$quarter = quarter(dat$Date)
  dat$month = lubridate::month(dat$Date)
  dat$day = lubridate::day(dat$Date)
  dat$weekdays = weekdays(dat$Date)
  glimpse(dat)
  
  dat = as.data.table(dat)
  dat$month = as.factor(dat$month)
  dat$weekdays = factor(dat$weekdays,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday",'Sunday'))
  dat[weekdays %in% c("Saturday",'Sunday'),weekend:=1]
  dat[!(weekdays %in% c("Saturday",'Sunday')),weekend:=0]
  dat$weekend = as.factor(dat$weekend)
  dat$year = as.factor(dat$year)
  dat$quarter = as.factor(dat$quarter)
  dat$week = format(dat$Date, "%V")
  dat = as.data.frame(dat)
  dat$week = as.integer(dat$week)
  glimpse(dat)
  dat <- dat %>% filter(ItemName %in% itemName)
  
  return(dat)
}

header <- dashboardHeader( title = "Sales Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }
    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Sales",
                 tabName = "resturant_tab",
                 icon = icon("dashboard")),
        menuItem("Product",
                 icon = icon("dashboard"),
                 menuSubItem("Restaurant1",
                             tabName = "rest1_tab",
                             icon = icon("dashboard")),
                 menuSubItem("Restaurant2",
                             tabName = "rest2_tab",
                             icon = icon("dashboard")),
                 menuSubItem("Restaurant3",
                             tabName = "rest3_tab",
                             icon = icon("dashboard"))
        ),
        menuItem("Report",
                 tabName = "report_tab",
                 icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        tabItem(tabName = "resturant_tab",
                fluidRow(box( selectInput(inputId = "resturantName"
                                          , label = "Select Resturant Name"
                                          , choices = c(Get_ResturantName())))
                         
                         ,box(  
                           dateInput("date", label = "Date input", value = "2018-09-01")
                         )
                ),
                
                fluidRow(
                  box(h2("TOP Numbers") 
                      ,tableOutput("topNumber"))
                ),
                fluidRow(box(h2("TOP Item Pie chart") 
                             ,plotOutput("top_10_pie"))
                         ,box(h2("TOP  Item Table") 
                              ,tableOutput("top_10_table"))
                )
                ,
                fluidRow(box(h2("TOP Item Bar chart") 
                             ,plotOutput("top_10_bar"))
                         ,box(h2("TOP Item Box plot") 
                              ,plotOutput("top_10_scatter"))
                )
        )
        
        , tabItem(tabName = "rest1_tab",
                  fluidRow(box( selectInput(inputId = "rest1ItemName"
                                            , label = "Select ItemName"
                                            , choices = c(Get_ItemName('Restaurant1')))),
                           box( selectInput(inputId = "rest1Variable"
                                            , label = "Select Variables"
                                            , choices =   c("Quantity" = "Quantity",
                                                            "Rate" = "Rate",
                                                            "Total" = "Total"))),
                  ),
                  
                  fluidRow(box(h4("Bar Plot")
                               ,plotOutput("rest1BarChart"))
                           ,box(h4("Prediction")
                                ,plotOutput("PredItemRest1"))
                           
                  ),
                  fluidRow(box(h4("Box Plot")
                               ,plotOutput("rest1BoxPlot"))
                           ,box(h4("Density  Plot")
                                ,plotOutput("rest1DensityChart"))
                  ),
                  fluidRow(box(h4("TimeSeries Plot")
                               ,plotOutput("rest1TimeSeriesPlot"))
                           ,box(h4("Scatter  Plot")
                                ,plotOutput("rest1LineChart"))
                           
                  )
        )
        , tabItem(tabName = "rest2_tab",
                  fluidRow(box( selectInput(inputId = "rest2ItemName"
                                            , label = "Select ItemName"
                                            , choices = c(Get_ItemName('Restaurant2')))),
                           box( selectInput(inputId = "rest2Variable"
                                            , label = "Select Variables"
                                            , choices =   c("Quantity" = "Quantity",
                                                            "Rate" = "Rate",
                                                            "Total" = "Total"))),
                  ),
                  
                  fluidRow(box(h4("Bar Plot")
                               ,plotOutput("rest2BarChart"))
                           ,box(h4("Prediction")
                                ,plotOutput("PredItemRest2"))
                           
                  ),
                  fluidRow(box(h4("Box Plot")
                               ,plotOutput("rest2BoxPlot"))
                           ,box(h4("Density  Plot")
                                ,plotOutput("rest2DensityChart"))
                  ),
                  fluidRow(box(h4("TimeSeries Plot")
                               ,plotOutput("rest2TimeSeriesPlot"))
                           ,box(h4("Scatter  Plot")
                                ,plotOutput("rest2LineChart"))
                           
                  )
        )
        
        , tabItem(tabName = "rest3_tab",
                  fluidRow(box( selectInput(inputId = "rest3ItemName"
                                            , label = "Select ItemName"
                                            , choices = c(Get_ItemName('Restaurant3')))),
                           box( selectInput(inputId = "rest3Variable"
                                            , label = "Select Variables"
                                            , choices =   c("Quantity" = "Quantity",
                                                            "Rate" = "Rate",
                                                            "Total" = "Total"))),
                  ),
                  
                  fluidRow(box(h4("Bar Plot")
                               ,plotOutput("rest3BarChart"))
                           ,box(h4("Prediction")
                                ,plotOutput("PredItemRest3"))
                           
                  ),
                  fluidRow(box(h4("Box Plot")
                               ,plotOutput("rest3BoxPlot"))
                           ,box(h4("Density  Plot")
                                ,plotOutput("rest3DensityChart"))
                  ),
                  fluidRow(box(h4("TimeSeries Plot")
                               ,plotOutput("rest3TimeSeriesPlot"))
                           ,box(h4("Scatter  Plot")
                                ,plotOutput("rest3LineChart"))
                  )
        )
        ,tabItem(tabName = "report_tab",
                 fluidRow(box(
                   dateInput("weekdate", label = "Date input", value = "2018-12-02")
                 )
                 
                 ),
                 
                 fluidRow(
                   box(h2("Week Sales") 
                       ,plotOutput("weekSales")),
                   box(h2("Month Sales") 
                       ,plotOutput("monthSales"))
                   
                 )
        )
      )
      
    }
    else {
      loginpage
    }
  })
  
  #tab-1
  output$topNumber <- renderTable(
    { Get_TopNumber(input$resturantName, input$date)
    }
  )
  
  # top_10_pie
  output$top_10_pie <- renderPlot(
    { 
      if(length((Get_Top10List_date(input$resturantName,input$date)$ItemName) != 0))
      {
        ggplot(Get_Top10List_date(input$resturantName,input$date), aes(x="", y=Quantity, fill=ItemName)) +
          geom_bar(stat="identity", width=1, show.legend = FALSE) +
          coord_polar("y", start=0)+
          scale_fill_brewer(palette="Dark2")
        
      }
        
    }
  )
  
  # top_10_table
  output$top_10_table <- renderTable(
    { Get_Top10List_date(input$resturantName,input$date)
    }
  )
  
  # top_10_bar
  output$top_10_bar <- renderPlot(
    { 
      g <- ggplot(Get_Top10List_date(input$resturantName,input$date), aes(ItemName, Quantity, fill = ItemName))
      g + geom_bar(stat="identity", width = 0.5) + 
        labs() +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
    }
  )
  # top_10_scatter
  output$top_10_scatter <- renderPlot(
    { 
      ggplot(Get_Top10List_date(input$resturantName,input$date), aes(x = ItemName, y = Quantity)) + 
        geom_point(shape=23, fill="blue", color="darkred", size=5)+
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
      
    }
  )
  
  
  # tab-2 -1
  # rest1BarChart
  output$rest1BarChart = renderPlot(
    {
      
      if (input$rest1Variable == "Total")
      {
        ggplot(data=Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Date, y=Total)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
      }
      
      else if (input$rest1Variable == "Quantity"){
        
        ggplot(data=Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Date, y=Quantity)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(data=Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Date, y=Rate)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest1LineChart
  output$rest1LineChart = renderPlot(
    {
      if (input$rest1Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest1ItemName,'Restaurant1'), aes(x = Date, y = Total)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else if (input$rest1Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest1ItemName,'Restaurant1'), aes(x = Date, y = Quantity)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x = Date, y = Rate)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest1BoxPlot
  output$rest1BoxPlot = renderPlot(
    {
      
      if (input$rest1Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x = factor(ItemName), y = Total)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else if (input$rest1Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x = factor(ItemName), y = Quantity)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x = factor(ItemName), y = Rate)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
    }
  )
  
  # rest1DensityChart
  output$rest1DensityChart = renderPlot(
    {
      
      if (input$rest1Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Total)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else if (input$rest1Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Quantity)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Rate)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
    }
  )
  
  # rest1TimeSeriesPlot
  output$rest1TimeSeriesPlot = renderPlot(
    {
      
      if (input$rest1Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest1ItemName,'Restaurant1'), aes(x=Date, y=Total)) + 
          geom_line(color = "878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else if (input$rest1Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Date, y=Quantity)) + 
          geom_line(color = "878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else {
        ggplot(Get_Item_data(input$rest1ItemName, 'Restaurant1'), aes(x=Date, y=Rate)) + 
          geom_line(color = "878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
    }
  )
  
  # PredItemRest1
  output$PredItemRest1 = renderPlot(
    {
      rest1 <- Get_PredictItem_data(input$rest1ItemName, 'Restaurant1')
      rf = randomForest(Total ~ Quantity + year + yday + quarter + month + day + weekend + week, data = rest1)
     
      if (input$rest1Variable == "Total")
      {
        predicted_df <- data.frame(Total = predict(rf, rest1), Date=rest1$Date)
        ggplot(data = rest1, aes(x = Date , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Total))
      }
      
      else if (input$rest1Variable == "Quantity"){
        
        predicted_df <- data.frame(Total = predict(rf, rest1), Quantity=rest1$Quantity)
        
        ggplot(data = rest1, aes(x = Quantity , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Quantity, y=Total))
      }
      
      else {
        predicted_df <- data.frame(Rate = predict(rf, rest1), Date=rest1$Date)
        ggplot(data = rest1, aes(x = Date , y = Rate)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Rate))
      }
    }
  )
  # tab-2 -2
  # rest2BarChart
  output$rest2BarChart = renderPlot(
    {
      
      if (input$rest2Variable == "Total")
      {
        ggplot(data=Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Date, y=Total)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
      }
      
      else if (input$rest2Variable == "Quantity"){
        
        ggplot(data=Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Date, y=Quantity)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(data=Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Date, y=Rate)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest2LineChart
  output$rest2LineChart = renderPlot(
    {
      if (input$rest2Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest2ItemName,'Restaurant2'), aes(x = Date, y = Total)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else if (input$rest2Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest2ItemName,'Restaurant2'), aes(x = Date, y = Quantity)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x = Date, y = Rate)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest2BoxPlot
  output$rest2BoxPlot = renderPlot(
    {
      
      if (input$rest2Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x = factor(ItemName), y = Total)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else if (input$rest2Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x = factor(ItemName), y = Quantity)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x = factor(ItemName), y = Rate)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
    }
  )
  
  # rest2DensityChart
  output$rest2DensityChart = renderPlot(
    {
      
      if (input$rest2Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Total)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else if (input$rest2Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Quantity)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Rate)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
    }
  )
  
  # rest2TimeSeriesPlot
  output$rest2TimeSeriesPlot = renderPlot(
    {
      
      if (input$rest2Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest2ItemName,'Restaurant2'), aes(x=Date, y=Total)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else if (input$rest2Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Date, y=Quantity)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else {
        ggplot(Get_Item_data(input$rest2ItemName, 'Restaurant2'), aes(x=Date, y=Rate)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
    }
  )
  
  # PredItemRest2
  output$PredItemRest2 = renderPlot(
    {
      rest2 <- Get_PredictItem_data(input$rest2ItemName, 'Restaurant2')
      rf = randomForest(Total ~ Quantity + year + yday + quarter + month + day + weekend + week, data = rest2)
      
      if (input$rest2Variable == "Total")
      {
        predicted_df <- data.frame(Total = predict(rf, rest2), Date=rest2$Date)
        ggplot(data = rest2, aes(x = Date , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Total))
      }
      
      else if (input$rest2Variable == "Quantity"){
        
        predicted_df <- data.frame(Total = predict(rf, rest2), Quantity=rest2$Quantity)
        
        ggplot(data = rest2, aes(x = Quantity , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Quantity, y=Total))
      }
      
      else {
        predicted_df <- data.frame(Rate = predict(rf, rest2), Date=rest2$Date)
        ggplot(data = rest2, aes(x = Date , y = Rate)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Rate))
      }
    }
  )
  # tab-2 -3
  # rest3BarChart
  output$rest3BarChart = renderPlot(
    {
      
      if (input$rest3Variable == "Total")
      {
        ggplot(data=Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Date, y=Total)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
      }
      
      else if (input$rest3Variable == "Quantity"){
        
        ggplot(data=Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Date, y=Quantity)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(data=Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Date, y=Rate)) + 
          geom_bar(stat="identity", fill = "#FF6666")+ 
          labs() +
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest3LineChart
  output$rest3LineChart = renderPlot(
    {
      if (input$rest3Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest3ItemName,'Restaurant3'), aes(x = Date, y = Total)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else if (input$rest3Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest3ItemName,'Restaurant3'), aes(x = Date, y = Quantity)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
      
      else {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x = Date, y = Rate)) + 
          geom_point(shape=23, fill="blue", color="darkred", size=5)+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))
      }
    }
  )
  
  # rest3BoxPlot
  output$rest3BoxPlot = renderPlot(
    {
      
      if (input$rest3Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x = factor(ItemName), y = Total)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else if (input$rest3Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x = factor(ItemName), y = Quantity)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
      
      else {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x = factor(ItemName), y = Rate)) + 
          geom_boxplot(color="red", fill="orange", alpha=0.2)
      }
    }
  )
  
  # rest3DensityChart
  output$rest3DensityChart = renderPlot(
    {
      
      if (input$rest3Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Total)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else if (input$rest3Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Quantity)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
      
      else {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Rate)) + 
          geom_density(color="darkblue", fill="lightblue")
      }
    }
  )
  
  # rest3TimeSeriesPlot
  output$rest3TimeSeriesPlot = renderPlot(
    {
      
      
      if (input$rest3Variable == "Total")
      {
        ggplot(Get_Item_data(input$rest3ItemName,'Restaurant3'), aes(x=Date, y=Total)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else if (input$rest3Variable == "Quantity"){
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Date, y=Quantity)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
      
      else {
        ggplot(Get_Item_data(input$rest3ItemName, 'Restaurant3'), aes(x=Date, y=Rate)) + 
          geom_line(color = "#878f99", size = 1) + scale_x_date(date_labels = "%Y-%m-%d")
      }
    }
  )
  
  # PredItemRest3
  output$PredItemRest3 = renderPlot(
    {
      rest3 <- Get_PredictItem_data(input$rest3ItemName, 'Restaurant3')
      rf = randomForest(Total ~ Quantity + year + yday + quarter + month + day + weekend + week, data = rest3)
      
      if (input$rest3Variable == "Total")
      {
        predicted_df <- data.frame(Total = predict(rf, rest3), Date=rest3$Date)
        ggplot(data = rest3, aes(x = Date , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Total))
      }
      
      else if (input$rest3Variable == "Quantity"){
        
        predicted_df <- data.frame(Total = predict(rf, rest3), Quantity=rest3$Quantity)
        
        ggplot(data = rest3, aes(x = Quantity , y = Total)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Quantity, y=Total))
      }
      
      else {
        predicted_df <- data.frame(Rate = predict(rf, rest3), Date=rest3$Date)
        ggplot(data = rest3, aes(x = Date , y = Rate)) + 
          geom_point(color='blue') +
          geom_line(color='red',data = predicted_df, aes(x=Date, y=Rate))
      }
    }
  )
  # tab-3
  
  
  # tab-4
  # weekSales
  output$weekSales = renderPlot(
    {
      
      the_plot <-   ggplot(GetSales(input$weekdate),aes(x=BillDate, y=NetAmount, col=Name, group=Name)) + 
        geom_line(size = 2)
      the_plot + scale_x_date(date_labels = "%Y-%m-%d")
      
    }
  )
  
  output$monthSales = renderPlot(
    {
      
      ggplot(GetMonthSales(input$weekdate),aes(x=BillDate, y=NetAmount, col=Name, group=Name)) + 
        geom_line(size = 1)+
        scale_x_date(date_labels = "%Y-%m-%d")
    }
  )
 
  
 
  
  
  
}
runApp(list(ui = ui, server = server))

