install.packages(c("shiny", "raster", "rayshader", "plotly"))


library(shiny)
library(raster)
library(rayshader)
library(plotly)

# Define UI
ui <- fluidPage(
    titlePanel("3D Canopy Height Model Viewer"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CHM .tif File", 
                      accept = c(".tif")),
            actionButton("process", "Load CHM Data")
        ),
        
        mainPanel(
            plotlyOutput("chm3DPlot", height = 600)
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Reactive variable to store the CHM raster data
    chm_raster <- reactiveVal(NULL)
    
    observeEvent(input$process, {
        req(input$file)  # Ensure file is uploaded
        
        # Load the CHM .tif file
        chm_file <- input$file$datapath
        chm_raster_data <- raster(chm_file)
        
        # Update the reactive value
        chm_raster(chm_raster_data)
    })
    
    # Render the 3D plot output
    output$chm3DPlot <- renderPlotly({
        req(chm_raster())
        
        # Convert raster to matrix for rayshader
        chm_matrix <- matrix(
            raster::values(chm_raster()),
            nrow = ncol(chm_raster()),
            ncol = nrow(chm_raster())
        )
        chm_matrix <- t(chm_matrix)  # Transpose to match orientation
        
        # Use rayshader to create a 3D plot
        chm_matrix %>%
            height_shade() %>%
            plot_3d(chm_matrix, zscale = 1, windowsize = c(800, 800), 
                    solid = TRUE, shadowdepth = -20, soliddepth = 0, 
                    solidcolor = "grey80", shadowcolor = "grey5")
        
        # Convert the rayshader plot to a plotly widget
        p <- plot_ly(
            x = 1:nrow(chm_matrix), y = 1:ncol(chm_matrix), 
            z = ~chm_matrix, 
            type = "surface"
        ) %>%
            layout(scene = list(
                xaxis = list(title = "X"),
                yaxis = list(title = "Y"),
                zaxis = list(title = "Canopy Height (m)")
            ))
        
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
