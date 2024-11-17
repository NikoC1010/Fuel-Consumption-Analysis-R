library(shiny)
library(shinydashboard)
library(wordcloud2)
library(jiebaR)

header<-dashboardHeader(title = 'Group 8')

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Continuous variables", tabName = "continuous_variables", icon = icon("stream")),
    menuItem("Discrete variable", tabName = "discrete_variable", icon = icon("trophy")),
    menuItem("WordCloud", tabName = "wordcloud", icon = icon("dashboard")),
    menuItem("Regression", tabName = "regression", icon = icon("bus"))
  )
)

body<-dashboardBody(
  tabItems(
    tabItem(
      tabName = "continuous_variables",
      fluidRow(
        box(
          plotOutput('plot1',height = 250)
        ),
        box(
          radioButtons("Var1", "Variable type:",
                       c("Highwaympg" = 'highway_mpg',
                         "Enginesize" = "engine_size"))
        )
       ), 
      fluidRow(
        box(
          verbatimTextOutput('summary')
        )
      )
    ),
    tabItem(
      tabName = 'discrete_variable',
      fluidRow(
        box(
          plotOutput('plot2',height = 250)
        ),
        box(
          selectInput("Var2", "Variable type:",
                      c("FuelType" = "fuel_type",
                        "FuelSystem" = "fuel_system"))
        )
      ),
      fluidRow(
        box(
          tableOutput("freq")
        )
      )
    ),
    tabItem(
      tabName = 'wordcloud',
      fluidRow(
        box(
          wordcloud2Output('plot3')
        ),
        box(
          sliderInput('size','The font size:',
                      min = 0.05,max = 0.5,
                      value = 0.09)
          
        )
      )
    ),
    tabItem(
      tabName = 'regression',
      fluidRow(
        box(
          plotOutput('plot4',height = 250)
        ),
        box(
          numericInput('n','Number of sets of data:',
                       min = 1,max = 205,
                       value = 200)
        )
      )
    )
  )
)

ui <- dashboardPage(header,sidebar,body)


server <- function(input, output){
  
  #1
  num<- reactive({
    Variable1 <- switch(input$Var1,
                   highway_mpg = c_data$highwaympg,
                   engine_size = c_data$enginesize
                   )
    
  })

  output$plot1 <- renderPlot({
    Variable1 <- input$Variable1
    hist(num(),
         xlab = 'Numerical interval',
         main = paste0('Histogram of ',input$Var1),
         col = "sky blue", border = "white")
  })
  

  output$summary <- renderPrint({
    summary(num())
  })
  
  #2
  type<- reactive({
    Variable2 <- switch(input$Var2,
                        fuel_type = c_data$fueltype,
                        fuel_system = c_data$fuelsystem
    )
    
  })
  
  output$plot2<-renderPlot({
    fre_tab<-table(type())
    barplot(fre_tab,
            xlab = 'The element type',
            ylab = 'Frequency',
            main = paste0('BarPlot of ',input$Var2),
            col = "sky blue", border = "white")
  })
  output$freq<-renderTable({
    table(type())
  })
  #3
  output$plot3<-renderWordcloud2({
    name_data<-names(c_data)
    name_data<-freq(name_data)
    wordcloud2(name_data,
              size = input$size,
              color = 'random-light',
              backgroundColor = 'white',
              minRotation = -pi/4,
              maxRotation = pi/4,
              rotateRatio = 0.4,
              shape = 'circle')
  })
  #4
  output$plot4<-renderPlot({
    
    plot(c_data$highwaympg[1:input$n]~c_data$enginesize[1:input$n],
         xlab = 'Engine Size',
         ylab = 'Highway Mpg',
         main = 'Scatterplot',
         data = c_data)
  })
}
shinyApp(ui = ui, server = server)
