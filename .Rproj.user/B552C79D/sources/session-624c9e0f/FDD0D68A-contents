library(shiny)
library(terra)
library(stringr)
library(dplyr)
library(viridis)   # for magma()

# Folder where rasters are stored
raster_dir <- "data"

# List all tif files
files <- list.files(raster_dir, pattern = "\\.tif$", full.names = FALSE)

# Extract type, theta, alpha from filenames
file_info <- str_match(
  files,
  "(functional_habitat|movement_flow)_theta([0-9\\.]+)_alpha([0-9\\.]+)\\.tif"
)

# Data frame describing each file
file_df <- data.frame(
  file = files,
  type = file_info[,2],
  theta = file_info[,3],
  alpha = file_info[,4],
  stringsAsFactors = FALSE
)

# Explicit choices (from your loop)
theta_choices <- c(0.01, 0.5, 1, 1.5, 3)
alpha_choices <- c(0.1, 0.075, 0.05, 0.01, 0.001)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Exploring alpha and theta"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Map 1"),
      selectInput("type1", "Select raster type:",
                  choices = unique(file_df$type)),
      
      h5("Lower values indicate movement near random, and higher values indicate movement near an optimized least cost path"),
      selectInput("theta1", "Select theta:",
                  choices = theta_choices),
      
      h5("Lower values indicate long distance dispersal, and higher values indicate short distance dispersal"),
      selectInput("alpha1", "Select alpha:",
                  choices = alpha_choices),
      
      hr(),
      
      h3("Map 2"),
      selectInput("type2", "Select raster type:",
                  choices = unique(file_df$type)),
      
      h5("Lower values indicate movement near random, and higher values indicate movement near an optimized least cost path"),
      selectInput("theta2", "Select theta:",
                  choices = theta_choices),
      
      h5("Lower values indicate long distance dispersal, and higher values indicate short distance dispersal"),
      selectInput("alpha2", "Select alpha:",
                  choices = alpha_choices)
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               plotOutput("rasterPlot1")
        ),
        column(6,
               plotOutput("rasterPlot2")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive for Map 1
  selected_file1 <- reactive({
    req(input$type1, input$theta1, input$alpha1)
    row <- file_df %>%
      filter(type == input$type1,
             theta == input$theta1,
             alpha == input$alpha1)
    if (nrow(row) == 1) {
      file.path(raster_dir, row$file)
    } else NULL
  })
  
  # Reactive for Map 2
  selected_file2 <- reactive({
    req(input$type2, input$theta2, input$alpha2)
    row <- file_df %>%
      filter(type == input$type2,
             theta == input$theta2,
             alpha == input$alpha2)
    if (nrow(row) == 1) {
      file.path(raster_dir, row$file)
    } else NULL
  })
  
  # Helper function to plot the raster with correct palette
  plot_raster <- function(filepath) {
    r <- rast(filepath)
    if (grepl("movement_flow", filepath)) {
      plot(r, main = filepath, col = magma(100))
    } else {
      plot(r, main = filepath)
    }
  }
  
  # Plot Map 1
  output$rasterPlot1 <- renderPlot({
    file <- selected_file1()
    validate(need(!is.null(file), "No raster found for this combination."))
    plot_raster(file)
  })
  
  # Plot Map 2
  output$rasterPlot2 <- renderPlot({
    file <- selected_file2()
    validate(need(!is.null(file), "No raster found for this combination."))
    plot_raster(file)
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
