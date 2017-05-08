library(shiny)
library(ggplot2)
library(data.table)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Residential Fire Accidents with Fatalities"),
  dashboardSidebar(
    menuItem("Tables", tabName = "TABLE", icon = icon("table")),
    menuItem("Maps", tabName = "MAP", icon = icon("map"))
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "TABLE",
              fluidPage(
                flowLayout(
                selectInput(inputId = "year", 
                            label = "Choose A Year",
                            c("2007","2008","2009","2010","2011","2012","2013","2014")))),
              fluidPage(
                  dataTableOutput("table07"),
                  dataTableOutput("table08"),
                  dataTableOutput("table09"),
                  dataTableOutput("table10"),
                  dataTableOutput("table11"),
                  dataTableOutput("table12"),
                  dataTableOutput("table13"),
                  dataTableOutput("table14"))),
      
      tabItem(tabName = "MAP",
              fluidPage(
                flowLayout(
                  selectInput(inputId = "year1",
                              label = "Choose a Year",
                              c("US MAP", "2007","2008","2009","2010","2011","2012","2013","2014")))),
                 fluidPage(
                   splitLayout(
                   plotOutput("map"),plotOutput("map07")),
                   splitLayout(plotOutput("map08"),
                          plotOutput("map09")),
                   splitLayout(plotOutput("map10"),plotOutput("map11")),
                   splitLayout(
                          plotOutput("map12"),plotOutput("map13"),plotOutput("map14")))
              )
            )
      )
  )

  
 

server <- function(input, output) { 
  output$table07 <- renderDataTable({
    if (input$year == "2007") {return(fatalres_07)}
    else{
      return()
    }
  })
  output$table08 <- renderDataTable({
    if (input$year == "2008") {return(fatalres_08)}
    else{
      return()
    }
  })
  output$table09 <- renderDataTable({
    if (input$year == "2009") {return(fatalres_09)}
    else{
      return()
    }
  })
  output$table10 <- renderDataTable({
    if (input$year == "2010") {return(fatalres_10)}
    else{
      return()
    }
  })
  output$table11 <- renderDataTable({
    if (input$year == "2011") {return(fatalres_11)}
    else{
      return()
    }
  })
  output$table12 <- renderDataTable({
    if (input$year == "2012") {return(fatalres_12)}
    else{
      return()
    }
  })
  output$table13 <- renderDataTable({
    if (input$year == "2013") {return(fatalres_13)}
    else{
      return()
    }
  })
  output$table14 <- renderDataTable({
    if (input$year == "2014") {return(fatalres_14)}
    else{
      return()
    }
  })
  
  output$map <- renderPlot({ 
    if (input$year1 == "US MAP"){return(ggmap(map))}
    else{return()}
  })
  
  output$map07 <- renderPlot({     
    if (input$year1 == "2007"){return(map07)}
    else{return()}
  })
  
  
  output$map08 <- renderPlot({
    if (input$year1 == "2008"){return(map08)}
    else{return()}
  })
  
  output$map09 <- renderPlot({
    if (input$year1 == "2009"){return(map09)}
    else{return()}
  })  
  
  output$map10 <- renderPlot({
    if (input$year1 == "2010"){return(map10)}
    else{return()}
  })  
  
  output$map11 <- renderPlot({
    if (input$year1 == "2011"){return(map11)}
    else{return()}
  })  
  
  output$map12 <- renderPlot({
    if (input$year1 == "2012"){return(map12)}
    else{return()}
  })  
  
  output$map13 <- renderPlot({
    if (input$year1 == "2013"){return(map13)}
    else{return()}
  })  
  
  output$map14 <- renderPlot({
    if (input$year1 == "2014"){return(map14)}
    else{return()}
  })  
  }

shinyApp(ui, server)