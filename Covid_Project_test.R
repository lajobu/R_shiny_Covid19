#rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(zoo)
library(tools)
library(shiny)
library(xtable)
library(shinydashboard)
library(colourpicker)
library(dashboardthemes)
library(devtools)


data <-
  read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

unique(data$location)

choosen_countries <-
  sort(c(unique(data$location)))

data <- data %>%
  filter(location %in% choosen_countries)

#Function:

an_fun <- function(x, y, y1, z, z1) {
  #x variable input$var
  #y country1 input$country
  #y1 country2 input$country
  #z date1 input$dtr[1]
  #z1 date2 input$dtr[2]
  a <- data %>%
    select(x, "location", "date") %>%
    filter(location == y &
             date %in%  c(seq(as.Date(z), as.Date(z1), "days"))) %>%
    select(x, "date")
  colnames(a) <- c(paste(y, sep = ""), "Date")
  b <- data %>%
    select(x, "location", "date") %>%
    filter(location == y1 &
             date %in%  c(seq(as.Date(z), as.Date(z1), "days"))) %>%
    select(x, "date")
  colnames(b) <- c(paste(y1, sep = ""), "Date")
  c <- merge(a, b, all = TRUE, order = TRUE)
  c[order(as.Date(c$Date, format = "%Y/%m/%d"), decreasing = TRUE),] %>%
    DT::datatable(rownames = FALSE, options = list(
      iDisplayLength = 5,
      aLengthMenu = c(5, 10, 15),
      dom = 'ltp'
    )) %>%
    DT::formatStyle(-5, color = '#742448', fontWeight = 'bold')
}

an_fun_var <- function(x) {
  #x country input$country or input$country2
  a <- data %>%
    select("total_cases",
           "new_cases",
           "total_deaths",
           "new_deaths",
           "location",
           "date") %>%
    filter(location == x) %>%
    select("date",
           "total_cases",
           "new_cases",
           "total_deaths",
           "new_deaths")
  b <- a[dim(a)[1], ] %>%
    data.frame()
  b$date <- as.character(b$date)
  b$total_cases <- as.character(b$total_cases)
  b$new_cases <- as.character(b$new_cases)
  b$total_deaths <- as.character(b$total_deaths)
  b$new_deaths <- as.character(b$new_deaths)
  colnames(b) <-
    c("Date",
      "Total cases:",
      "New cases:",
      "Total deaths:",
      "New deaths:")
  b$Date <- as.character(b$Date)
  xtable(b)
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    width = "420px",
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Created by Jorge Bueno Perez & Noam Shmuel",
        icon = icon("info"),
        tabName = "about"
      ),
      menuItem("Data source - Our World in Data", 
               icon = icon("github"), 
               href = "https://github.com/owid/covid-19-data/tree/master/public/data")
    ),
    
    #Type of analysis:
    selectInput(
      "type.an",
      "Select type of graph:",
      c("Total selected countries",
        "Comparison selected countries"),
      selected = "Israel"
    ),
    
    #Variable:
    selectInput("var", "Variable", names(data)[5:15], selected = "new_cases"),
    
    #Date range:
    dateRangeInput(
      inputId = "dtr",
      label = "Date interval:",
      start = "2020-03-01",
      end = max(data$date),
      min = min(data$date),
      max = max(data$date),
      format = "yyyy-mm-dd",
      startview = "year",
      weekstart = 1,
      language = "en",
      separator = "to",
      autoclose = TRUE
    ),
    
    #Country 1:
    selectInput("country",
                "Select country 1", choosen_countries, selected = "Poland"),
    
    
    uiOutput("t_deaths.contry1"),
    
    #Country 2:
    selectInput("country2",
                "Select country 2",
                choosen_countries,
                selected = "Israel"),
    
    
    uiOutput("t_deaths.contry2"),
    
    DT::dataTableOutput("filt.an")
  ),
  
  dashboardBody(
    plotOutput("plot1"),
    plotOutput("plot2"),
    tags$head(tags$style(
      HTML(
        "
          .content-wrapper {
          background-color: white !important;
          }
          
          .main-header .logo {
          font-family: 'Lobster';
          font-weight: 600;
          color: #742448 !important;
          background-color: white !important;
          }

          .main-header .navbar {
          background-color: white !important;
          }

          .main-sidebar {
          background-color: white !important;
          }

          .main-sidebar .sidebar {
          color: #742448 !important;
          }

        "
      )
    ))
  )
) #Final ui

server <- function(input, output, session) {
  #sideBarPanel:
  
  #t_deaths.contry1
  
  data_gen_an1 <- reactive({
    an_fun_var(input$country)
  })
  output$t_deaths.contry1 <- renderTable({
    data_gen_an1()
  })
  
  #t_deaths.contry2
  
  data_gen_an2 <- reactive({
    an_fun_var(input$country2)
  })
  output$t_deaths.contry2 <- renderTable({
    data_gen_an2()
  })
  
  #mainPanel:
  
  selectedData <- reactive({
    data %>%
      filter(location == input$country &
               date >= input$dtr[1] & date <= input$dtr[2])
  })
  
  selectedData1 <- reactive({
    data %>%
      filter(location == input$country2 &
               date >= input$dtr[1] & date <= input$dtr[2])
  })
  
  selectedData2 <- reactive({
    data %>%
      filter(
        location %in% c(input$country, input$country2) &
          date >= input$dtr[1] & date <= input$dtr[2]
      )
  })
  
  selectedMAdays <- reactive({
    return(input$ma)
  })
  
  selectedLabels <- reactive({
    namcol <- input$var
    namcol <- str_replace_all(namcol, "_", " ")
    namcol <- tools::toTitleCase(namcol)
    return(namcol)
  })
  
  #c("Graphical - Total sel. countries", "Graphical - Comparison sel. countries", "Numerical - Analysis of data"),
  
  graph_filter1 <- reactive({
    #First graph
    
    #Graphical - Total sel. countries"
    if (input$type.an == "Total selected countries") {
      #plot 1.a - Graphical - Total sel. country1
      ggplot(data = selectedData(), aes_string(x = "date", y = input$var)) +
        geom_bar(
          stat = "identity",
          color = '#742448',
          fill = "#742448",
          width = .75
        ) +
        geom_text(
          aes_string(label = input$var),
          hjust = 1.1,
          vjust = +0.39,
          angle = 90,
          size = 3.2,
          color = "#ffffff"
        ) + #color = "#ffffff"
        geom_smooth(
          method = "loess",
          se = F,
          color = 'black',
          size = 0.80,
          alpha = 1
        ) +
        ylab(selectedLabels()) +
        xlab("Date") +
        theme(
          axis.text.y = element_text(colour = "black", size = 12),
          axis.text.x = element_text(colour = "black"),
          axis.title.y = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          axis.title.x = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          title = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            color = '#742448'
          )
        ) +
        
        scale_x_date(
          date_breaks = "weeks",
          date_labels = "%b %e",
          date_minor_breaks = "1 days"
        ) +
        labs(
          subtitle = str_c("From ", input$dtr[1], " to ", input$dtr[2]),
          title = str_c(selectedLabels(), "in", input$country, sep = " ")
        )
      
      #"Graphical - Comparison sel. countries"
    } else if (input$type.an == "Comparison selected countries") {
      #Plot 2.a - Graphical - Comparison sel. countries
      ggplot(data = selectedData2(),
             aes_string(
               x = "date",
               y = input$var,
               color = "location"
             )) +
        geom_line(alpha = 1, size = 2) +
        #geom_text(aes_string(label= input$var), vjust=-0.5, size = 3.9, color = "#552583") +
        #geom_line(aes_string(x = "date", y = zoo::rollmean(selectedData()[,input$var], selectedMAdays()  ,na.pad=TRUE)),size = 1.2, alpha = 0.3,color = '#552583') +
        ylab(selectedLabels()) +
        xlab("Date") +
        theme(
          axis.text.y = element_text(colour = 'black', size = 12),
          axis.text.x = element_text(colour = 'black'),
          axis.title.y = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          axis.title.x = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          title = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            color = '#742448'
          )
        ) +
        
        scale_x_date(date_breaks = "weeks", date_labels = "%b %e") +
        labs(
          subtitle = str_c("From ", input$dtr[1], " to ", input$dtr[2]),
          title = str_c(
            selectedLabels(),
            "in",
            input$country,
            "vs",
            input$country2,
            sep = " "
          )
        ) +
        scale_color_manual(values = c("black", "#742448"))
    }
  })
  
  graph_filter2 <- reactive ({
    #Second graph
    
    #Graphical - Total sel. countries" -
    if (input$type.an == "Total selected countries") {
      #plot 1.b - Graphical - Total sel. country2
      ggplot(data = selectedData1(), aes_string(x = "date", y = input$var)) +
        geom_bar(
          stat = "identity",
          color = 'black',
          fill = 'black',
          width = .75
        ) +
        geom_text(
          aes_string(label = input$var),
          hjust = 1.1,
          vjust = +0.39,
          angle = 90,
          size = 3.2,
          color = "#ffffff"
        ) + #color = "#ffffff"
        geom_smooth(
          method = "loess",
          se = F,
          color = '#742448',
          size = 0.80,
          alpha = 1
        ) +
        ylab(selectedLabels()) +
        xlab("Date") +
        theme(
          axis.text.y = element_text(colour = 'black', size = 12),
          axis.text.x = element_text(colour = 'black'),
          axis.title.y = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          axis.title.x = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          title = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            color = '#742448'
          )
        ) +
        
        scale_x_date(
          date_breaks = "weeks",
          date_labels = "%b %e",
          date_minor_breaks = "1 days"
        ) +
        labs(
          subtitle = str_c("From ", input$dtr[1], " to ", input$dtr[2]),
          title = str_c(selectedLabels(), "in", input$country2, sep = " ")
        )
      
      #"Graphical - Comparison sel. countries"
    } else if (input$type.an == "Comparison selected countries") {
      #Plot 2.b - Graphical - Comparison sel. countries
      ggplot(data = selectedData2(),
             aes_string(
               x = "date",
               y = input$var,
               fill = "location"
             )) +
        geom_bar(stat = "identity", color = "white") +
        ylab(selectedLabels()) +
        xlab("Date") +
        theme(
          axis.text.y = element_text(colour = 'black', size = 12),
          axis.title.y = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          axis.title.x = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            colour = '#742448'
          ),
          title = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 0.2,
            color = '#742448'
          )
        ) +
        
        scale_x_date(
          date_breaks = "weeks",
          date_labels = "%b %e",
          date_minor_breaks = "weeks"
        ) +
        labs(
          subtitle = str_c("in ", selectedLabels()),
          title = str_c(
            selectedLabels(),
            " in ",
            input$country,
            " vs ",
            input$country2,
            " - shown in separate graphs",
            sep = ""
          )
        ) +
        facet_wrap( ~ selectedData2()$location) +
        scale_fill_manual(values = c("black", "#742448"))
    }
  })
  
  output$plot1 <- renderPlot({
    graph_filter1()
  })
  
  output$plot2 <- renderPlot({
    graph_filter2()
  })
  
  #filt_an
  
  filt_an <- reactive ({
    an_fun(input$var,
           input$country,
           input$country2,
           input$dtr[1],
           input$dtr[2])
  })
  
  output$filt.an <- DT::renderDT({
    filt_an()
  })
  
} #Final server

shiny::shinyApp(ui, server)