library(shiny)
library(shinythemes)
library(HistData)
library(dplyr)
library(ggplot2)
library(plotly)
# Define UI for app
ui <- fluidPage(
  navbarPage("Predict Your Child Height"),
  theme = shinytheme("sandstone"),
  #titlePanel("Predict Child Height"),
  h6("This app uses the GaltonFamilies parent-child height data to fit a linear model to predict child's height with parent's height and the child's Gender as predictors. Based on the model fit, this app predicts the height of a child, when adult."),
  h6("Let's find out how tall your child is likely to be at age 18!"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "Gender",
        label = h5("Child's Gender"),
        choices = list("male" , "female"),
        selected = NULL),
      numericInput(
        "MotherH",
        label = h5("Mother's Height In Inches:"),
        min = 40,
        max = 90,
        value = 60
      ),
      numericInput(
        "FatherH",
        label = h5("Father's Height In Inches:"),
        min = 40,
        max = 90,
        value = 60
      ),
      actionButton("button", "Predict Height")
    ),
  # Main panel for displaying outputs ----
  mainPanel(tabsetPanel(
    tabPanel(
      "Summary",
      h4("Input Values:"),
      verbatimTextOutput("Gender"),
      verbatimTextOutput("MotherH"),
      verbatimTextOutput("FatherH"),
      h4("Predicted Height:"),
      verbatimTextOutput("ChildH")
    ),
    tabPanel("Plot",
             h4("Plot Parents Height vs Child Height:"),
             plotlyOutput("plot1",height = "480px"),
             verbatimTextOutput("hover"),
             verbatimTextOutput("click"),
             h4("Plot Child Height GenderWise With the Current Data:"),        
             plotOutput("plot2"),
             h5("Hmmmp!It seems like Mean Height of boys is more than than of girls!"))
  ))
)
)
# R Code Start Here
# Load Library
library(HistData)
library(dplyr)
# R Code Ends Here
# Define server logic
server <- function(input, output)
{
  nGender <- eventReactive(input$button, {
    input$Gender
  })
  output$Gender <-
    renderText({
      paste("Gender of the Child is:", nGender())
    })
  
  nFatherH <- eventReactive(input$button, {
    input$FatherH
  })
  output$FatherH <-
    renderText({
      paste("Father's Height is:", nFatherH())
    })
  nMotherH <- eventReactive(input$button, {
    input$MotherH
  })
  output$MotherH <-
    renderText({
      paste("Mother's Height is:", nMotherH())
    })
  
  nFatherH <- eventReactive(input$button, {
    input$FatherH
  })
  output$FatherH <-
    renderText({
      paste("Father's Height is:", nFatherH())
    })
  
  height_data <- GaltonFamilies
  model1 <-
    lm(childHeight ~ father + mother + gender , data = height_data)
  test_data <-
    reactive({
      data.frame(
        father = input$FatherH,
        mother = input$MotherH,
        gender = input$Gender
      )
    })
  pred <- reactive({
    predict(model1, test_data())
  })
  
  nChildH <- eventReactive(input$button, {
    round(pred())
  })
  output$ChildH <-
    renderText({
      paste("Child's Predicted Height in Inches is:", nChildH())
    })
  
  output$plot1 <- renderPlotly({
    plot_ly(height_data,
            x = ~father, 
            y = ~mother, 
            z = ~as.numeric(childHeight), 
            color = ~gender,
            type = "scatter3d") %>%
      layout(scene = list(xaxis = list(title = 'FatherH'),
                          yaxis = list(title = 'MotherH'),
                          zaxis = list(title = 'ChildH')))
  })
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$plot2 <- renderPlot({
    ggplot(height_data, aes(x = factor(gender), y = childHeight)) +
      geom_boxplot(na.rm = TRUE,
                   fill = "white",
                   colour = "#3366FF") +
      xlab("Gender")
  })
}
# Create Shiny app
shinyApp(ui = ui, server = server)
