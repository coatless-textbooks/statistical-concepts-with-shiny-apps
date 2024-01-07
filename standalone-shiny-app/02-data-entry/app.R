# Required packages ----
library(shiny)
library(DT)
library(dplyr)

# Required for allowing seamless passing of NA values on new row/column creation.
# Details: https://github.com/rstudio/DT/issues/496
options("DT.TOJSON_ARGS" = list(na = "string"))

# Define UI for app that allows data entry and summary ----
ui <- fluidPage(
  # App title ----
  titlePanel("Datatizer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Button to toggle between data entry, summary, and plot views ----
      selectInput("viewSelector", "Select View:",
                  choices = c("Data Entry", "Summary", "Plot"),
                  selected = "Data Entry"),
      br(),
      downloadButton("downloadData", "Download Data")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Conditional panel that shows either data entry, summary, or plot ----
      conditionalPanel(
        condition = "input.viewSelector == 'Data Entry'", 
        h3("Data Entry"),
        DTOutput(outputId = "dataTable"),
        fluidRow(
          align = "right",
          br(),
          column(
            6,
            actionButton(
              inputId = "addRow",
              label = "Add new row"
            )
          ),
          column(
            2,
            actionButton(
              inputId = "addColumn",
              label = "Add new column"
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.viewSelector == 'Summary'", 
        h3("Summary"),
        tableOutput(outputId = "dataSummary")
      ),
      conditionalPanel(
        condition = "input.viewSelector == 'Plot'", 
        h3("Scatterplot Matrix"),
        plotOutput(outputId = "scatterplot")
      )
    )
  )
)

# Define server logic required to handle data entry, summary, and plotting ----
server <- function(input, output, session) {
  
  # Reactive value to store the data ----
  entry_data <- reactiveVal(data.frame(x = NA_real_, y = NA_real_))
  
  # Reactive expression to save the data to disk on write ----
  saveData <- reactive({
    write.csv(entry_data(), paste0("shiny-data-entry-backup-", Sys.Date(), ".csv"), row.names = FALSE)
  })
  
  # Observe the data and save it when it changes ----
  observeEvent(entry_data(), {
    saveData()
  })
  
  # Render the data table with editable cells ----
  output$dataTable <- renderDT({
    datatable(entry_data(), editable = TRUE, rownames = FALSE)
  })
  
  # Update the data when a cell is edited ----
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    
    # Suppress immediate updates
    new_data <- isolate(entry_data())
    
    i <- info$row     # DT returns 1-based index for row
    j <- info$col + 1 # DT returns 0-based index for column
    v <- info$value
    
    # Update data entry with new value
    new_data[i, j] <- v
    
    # Add a new row when return is pressed on the last cell ----
    if (i == nrow(new_data) && j == ncol(new_data)) {
      new_data = rbind(new_data, rep(NA, ncol(new_data))) # add a new row with NA values
    }
    
    entry_data(new_data)
  })
  
  # Handle dynamic expansion of observations ----
  observeEvent(input$addRow, {
    # Suppress immediate updates
    new_data <- isolate(entry_data())
    
    # Add a new row with NA values
    new_data <- rbind(new_data, rep(NA, ncol(new_data)))
    
    # Re-build the reactive value
    entry_data(new_data)
  })
  
  # Handle dynamic expansion of variables ----
  observeEvent(input$addColumn, {
    # Suppress immediate updates
    new_data <- isolate(entry_data())
    
    # Add a new column with NA values
    new_data[paste0("col", ncol(new_data) + 1)] <- rep(NA, nrow(new_data))
    
    # Re-build the reactive value
    entry_data(new_data)
  })
  
  # Render the summary table ----
  output$dataSummary <- renderTable({
    # Pluck all numeric values and perform summary 
    do.call(cbind, lapply(Filter(is.numeric, entry_data()), summary))
  }, rownames = TRUE)
  
  # Output to display scatterplot matrix ----
  output$scatterplot <- renderPlot({
    # Pluck all numeric values and generate a scatterplot matrix 
    pairs(Filter(is.numeric, entry_data()))
  })
  
  # Download button to export data ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("shiny-entry-data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(x = entry_data(), file = file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
