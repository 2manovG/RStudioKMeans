# k-means sample
library(shiny)


# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(iris), "Species")

ui <- fluidPage(
  titlePanel('Пример k-means'),
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'Ось X', vars),
      selectInput('ycol', 'Ось Y', vars, selected = vars[[2]]),
      numericInput('clusters', 'Количество кластеров', 5, min = 1, max = 9),
      textInput('testi', 'Строка под графиком', 'строка')
    ),
    mainPanel(
      p('График:'),
      plotOutput('plot1'),
      textOutput('testo'),
    )
  )
)

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("red", "blue", "green", "purple",
                       "orange", "yellow", "brown", "pink", "gray"))
                       
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$testo <- renderText({paste("Строка под графиком: ", input$testi)})
}

shinyApp(ui = ui, server = server)