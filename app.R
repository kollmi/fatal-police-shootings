library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)

data <- read.csv("fatal-police-shootings-data_10.11.21.csv")
data$race <- dplyr::recode(dplyr::na_if(data$race,""), B = "Black", A = "Asian", H = "Hispanic", W = "White", N = "Native American",
                    O = "Other", .missing = "Unknown")
data$gender <- dplyr::recode(dplyr::na_if(data$gender,""), M = "Male", F = "Female", .missing = "Unknown")
`%!in%` = Negate(`%in%`)


ui <- dashboardPage(
  dashboardHeader(title = "Fatal Shootings by Police in the U.S. (2015-2021)", titleWidth = 500),
  dashboardSidebar(collapsed = TRUE,
                   tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))),
                   #sidebarMenu(menuItem("Fatal Shootings", tabName = "first"))),
  # tags$head(
  #   tags$style(HTML())
  # )),
  dashboardBody(
      # custom CSS to the dashboard look pretty
      tags$head(tags$style(HTML('
        /* logo */
         .skin-blue .main-header .logo {
                               background-color: #FAE9DA;
                               color: black;
                               font-weight: bold;
         }
         /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #FAE9DA;
                              }
         /* navbar (rest of the header) */
         .skin-blue .main-header .navbar {
                               background-color: #FAE9DA;
         }
                               .content-wrapper, .right-side {
                                background-color: #ECDBCD;
                               }
                               
                                .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#666666
                    }
          .box.box-solid.box-primary{
          border-bottom-color:#666666;
          border-left-color:#666666;
          border-right-color:#666666;
          border-top-color:#666666;
}    '))),
    fluidRow(
        column(width = 3,
               selectizeInput("name", "Name", choices = c("", unique(data$name))),
               selectizeInput("state", "State", choices = c("",sort(unique(data$state)))),
               selectizeInput("race", "Race", choices = unique(data$race), multiple = TRUE),
               selectizeInput("armed", "Armed?", choices = c("", "Unarmed", "Armed", "Undetermined")),
               selectizeInput("millness", "Signs of Mental Illness?", choices = c("","Yes", "No")),
               sliderInput("age", "Age", min = 6, max = 91, value = c(6,91), step = 1),
               dateRangeInput("date", "Date Range", min = as.Date(min(data$date)), max = as.Date(max(data$date)),
                              start = as.Date(min(data$date)), end = as.Date(max(data$date))),
               radioButtons("legend", "Legend Type",choices = c("Race", "Gender", "Presence of Body Camera", "Signs of Mental Illness")),
               actionButton("search", "Search"),
               actionButton("reset", "Reset Filters")),
        column(width = 9,
               box(width = NULL, solidHeader = TRUE, status = "primary", height = "425px",
                   leafletOutput("map", height = 350), title = "Map", 
                   htmlOutput("text")),
               box(width = NULL, solidHeader = TRUE, status = "primary", title = "Monthly Fatality Count", 
                   height = "375px",plotly::plotlyOutput("plot", height = 300)))
      )
    )
  )
#)

server <- function(input,output,session){
  race_pal <- colorFactor(pal = c("#F9A7B0", "#4B0082", "#800517", "#48CCCD", "#deb76f", "#566d7e", "#728c00"), 
                     domain = c("Asian", "Black", "Hispanic","Native American", "Other", "Unknown","White"))
  gender_pal <- colorFactor(pal = c("peru", "forestgreen", "gray"), domain = c("Male", "Female", "Unknown"))
  camera_pal <- colorFactor(pal = c("antiquewhite4", "thistle"), domain = c("False", "True"))
  # armed_pal <- colorFactor()
  millness_pal <- colorFactor(pal = c("antiquewhite4", "thistle"), domain = c("False", "True"))
  m <- eventReactive(input$search,{
    if (input$state != ""){data <- data %>% filter(state == input$state)}
    if (input$name != ""){data <- data %>% filter(name == input$name)}
    if (length(input$race) != 0){data <- data %>% filter(race %in% input$race)}
    if (input$armed != ""){
      if (input$armed == "Unarmed"){data <- data %>% filter(armed == "unarmed")}
      if (input$armed == "Undetermined"){data <- data %>% filter(armed %in% c("", "undetermined"))}
      if (input$armed == "Armed"){data <- data %>% filter(armed %!in% c("unarmed", "", "undetermined"))}
    }
    if (input$millness != ""){
      if (input$millness == "Yes"){data <- data %>% filter(signs_of_mental_illness == "True")}
      if (input$millness == "No"){data <- data %>% filter(signs_of_mental_illness == "False")}
    }
    data <- data %>% filter(age <= input$age[2] & age >= input$age[1])
    data <- data %>% filter(date <= as.character(input$date[2]) & date >= as.character(input$date[1]))
    plant <- function(var, pal){
      leaflet(data) %>% 
        # fitBounds(lat1 = 19.501, lat2 = 64.856,
        #                        lng1 = -161.75583, lng2 = -68.01197) %>% 
        setView(lng = -93, lat = 38.5, zoom = 3.5) %>% 
      addCircleMarkers(lng = ~longitude, lat = ~latitude, opacity = 50, radius = 2, color = ~pal(var),
                       popup = paste(paste('<b>Name:</b>',data$name),
                                     paste('<b>Age:</b>',data$age),
                                     paste('<b>Gender:</b>',data$gender),
                                     paste('<b>Race:</b>',data$race),
                                     paste('<b>Location:</b>', data$city, data$state),
                                     paste('<b>Body Camera?</b>',data$body_camera),
                                     paste('<b>Armed?</b>',data$armed),
                                     paste('<b>Signs of Mental Illness?</b>',data$signs_of_mental_illness),
                                     paste('<b>Date:</b>',data$date),
                                     sep = '<br/>')) %>%
      addTiles() %>% 
      addLegend(pal = pal, values = var) 
      # %>% 
      #   fitBounds(map = .,lat1 = 19.501, lat2 = 64.856,
      #             lng1 = -161.75583, lng2 = -68.01197)
    }
    if(input$legend == "Race"){plant(data$race, race_pal)}
    else if(input$legend == "Presence of Body Camera"){plant(data$body_camera, camera_pal)}
    else if(input$legend == "Gender"){plant(data$gender, gender_pal)}
    else{plant(data$signs_of_mental_illness, millness_pal)}
    
  })
  n <- eventReactive(input$search,{
    if (input$state != ""){data <- data %>% filter(state == input$state)}
    if (input$name != ""){data <- data %>% filter(name == input$name)}
    if (length(input$race) != 0){data <- data %>% filter(race %in% input$race)}
    if (input$armed != ""){
      if (input$armed == "Unarmed"){data <- data %>% filter(armed == "unarmed")}
      if (input$armed == "Undetermined"){data <- data %>% filter(armed %in% c("", "undetermined"))}
      if (input$armed == "Armed"){data <- data %>% filter(armed %!in% c("unarmed", "", "undetermined"))}
    }
    if (input$millness != ""){
      if (input$millness == "Yes"){data <- data %>% filter(signs_of_mental_illness == "True")}
      if (input$millness == "No"){data <- data %>% filter(signs_of_mental_illness == "False")}
    }
    data <- data %>% filter(age <= input$age[2] & age >= input$age[1])
    data <- data %>% filter(date <= as.character(input$date[2]) & date >= as.character(input$date[1]))
    
    date_tbl <- as.data.frame(table(data$date))
    date_tbl$Var1 <- as.Date(date_tbl$Var1)
    
    date_plt <- date_tbl %>% 
      group_by(month = lubridate::floor_date(Var1, "month")) %>%
      summarize(fatality_count = sum(Freq)) 
    date_plt <- date_plt %>% slice(-nrow(date_plt))
    
    g <- ggplot(date_plt, aes(x = month, y = fatality_count)) + geom_line() + theme_minimal() +
      labs(x = "Month", y = "Fatality Count")
    
    plotly::ggplotly(g)
    
    
  }
  )
  output$map <- renderLeaflet({m()})
  output$plot <- plotly::renderPlotly({n()})
  
  #reset button
  observe({
    input$reset
    updateSelectizeInput(session, "name", selected = "")
    updateSelectizeInput(session, "state", selected = "")
    updateSelectizeInput(session, "race", selected = "")
    updateSelectizeInput(session, "armed", selected = "")
    updateSelectizeInput(session, "millness", selected = "")
    updateSliderInput(session, "age", value = c(6,91))
    updateDateRangeInput(session, "date", min = as.Date(min(data$date)), max = as.Date(max(data$date)), 
                         start = as.Date(min(data$date)), end = as.Date(max(data$date)))
    updateNumericInput(session, "mynumber", value = 20)
    updateTextInput(session, "mytext", value = "test")
    updateRadioButtons(session,"legend", selected = "Race")
  })
  
  url <- a("Police Shootings Database", href="https://github.com/washingtonpost/data-police-shootings")
  output$text <- renderUI({
      tagList("This plot was generated with data from The Washington Post's", url,"(updated as of 9/27/21)")
    })
 
}

shinyApp(ui = ui, server = server)
