# Load the shiny and ggplot2 packages
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Graph with Linear Regression"),
  
  fluidRow(
    column(
      # Two sliders to adjust the x and y axis ranges
      sliderInput("xrange", "X-axis range", min = -10, max = 10, value = c(-5, 5)),
      sliderInput("yrange", "Y-axis range", min = -10, max = 10, value = c(-5, 5)),
      # A button to reset the data
      actionButton("reset", "Reset Data"),
      width = 3
    ),
    column(
      # A plot output to display the graph
      plotOutput("plot", click = "plot_click"),
      width = 6),
    column(
      # A table output to display the statistics
      tableOutput("stats"),
      # A text output to display the model equation
      textOutput("model"), width = 3)
  )
)

# Define the server
server <- function(input, output, session) {
  
  # A reactive value to store the data frame
  values <- reactiveValues(data = NULL)
  
  empty_dataframe <- function() { data.frame(x = numeric(), y = numeric()) }
  # Initialize an empty data frame 
  observe({
    values$data <- empty_dataframe()
  })
  
  # Update the data frame when the user clicks on the plot
  observeEvent(input$plot_click, {
    # Get the coordinates of the click
    x <- input$plot_click$x
    y <- input$plot_click$y
    # Add a new row to the data frame
    values$data <- rbind(values$data, data.frame(x = x, y = y))
  })
  
  # Reset the data frame when the user clicks on the reset button
  observeEvent(input$reset, {
    values$data <- empty_dataframe()
  })
  
  # Render the plot output
  output$plot <- renderPlot({
    # Create a ggplot object with the data
    p <- ggplot(values$data, aes(x = x, y = y)) +
      geom_point(size = 3) +
      xlim(input$xrange[1], input$xrange[2]) +
      ylim(input$yrange[1], input$yrange[2]) +
      labs(x = "X", y = "Y")
    # If there is more than one point, add a linear regression line
    if (nrow(values$data) > 1) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    # Return the plot object
    p
  })
  
  # Render the table output
  output$stats <- renderTable({
    # If there is more than one point, calculate the statistics
    if (nrow(values$data) > 1) {
      # Fit a linear model
      model <- lm(y ~ x, data = values$data)
      # Get the correlation coefficient
      rho <- cor(values$data$x, values$data$y)
      # Get the sum of squared residuals
      ssr <- sum(residuals(model)^2)
      # Get the number of points
      n <- nrow(values$data)
      # Get the mean of x and y
      xbar <- mean(values$data$x)
      ybar <- mean(values$data$y)
      # Get the covariance between x and y
      covxy <- cov(values$data$x, values$data$y)
      # Create a data frame with the statistics
      stats <- data.frame(
        Statistic = c("Number of Points", "Mean of X", "Mean of Y", "Covariance between X and Y", "Correlation", "Sum of Squared Residuals"),
        Value = c(n, xbar, ybar, covxy, rho, ssr)
      )
      # Return the data frame
      stats
    } else {
      # Return an empty data frame
      data.frame()
    }
  })
  
  # Render the text output
  output$model <- renderText({
    # If there is more than one point, display the model equation
    if (nrow(values$data) > 1) {
      # Fit a linear model
      model <- lm(y ~ x, data = values$data)
      # Get the coefficients
      a <- coef(model)[1]
      b <- coef(model)[2]
      # Get the R-squared
      r2 <- summary(model)$r.squared
      # Format the equation
      equation <- paste0("y = ", round(a, 2), " + ", round(b, 2), "x")
      # Format the R-squared
      rsquared <- paste0("R^2 = ", round(r2, 2))
      # Return the text
      paste0("Model Equation:\n", equation, "\n\n", "Coefficient of Determination:\n", rsquared)
    } else {
      # Return an empty text
      ""
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
