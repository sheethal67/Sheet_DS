library(shiny)
library(r2d3)
library(jsonlite)

irisData <- read.csv("Iris.csv")
toJSON(irisData)


ui <- fluidPage(
  title = "Scatter Chart",
  h4("D3 based visualization using RShiny and R2D3", align = "center"),
  
  fluidRow(
      column(6, d3Output("d3SWSL")),
      column(6, d3Output("d3SWPW"))
  ),
  
  fluidRow(
      column(6, d3Output("d3PWPL")),
      column(6, d3Output("d3SLPL"))
  )  
)

server <- function(input, output) {
  output$d3SWSL <- renderD3({
    r2d3(
      options = list(names=c("Sepal_Width","Sepal_Length"),
                     axislabels=c("Sepal.Width","Sepal.Length")),
      data = irisData,
      script = "scatterPlot.js" 
    )
  })
  
  output$d3PWPL <- renderD3({
    r2d3(
      options = list(names=c("Petal_Width","Petal_Length"),
                     axislabels=c("Petal.Width","Petal.Length")),
      data = irisData,
      script = "scatterPlot.js" 
    )
  })

  output$d3SWPW <- renderD3({
    r2d3(
      options = list(names=c("Sepal_Width","Petal_Width"),
                     axislabels=c("Sepal.Width","Petal.Width")),
      data = irisData,
      script = "scatterPlot.js" 
    )
  })
  
  output$d3SLPL <- renderD3({
    r2d3(
      options = list(names=c("Sepal_Length","Petal_Length"),
                     axislabels=c("Sepal.Length","Petal.Length")),
      data = irisData,
      script = "scatterPlot.js" 
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
