
library(readr)
library(ggplot2)
library(DT)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

dt_time_embedding <- readr::read_csv("dt_time_embedding.csv")

ui <- fluidPage(
    fluidRow(
        column(width = 4, class = "well",
               h4("Brush and double-click to zoom, click to select time series"),
               plotOutput("plot1", height = 300,
                          dblclick = "plot1_dblclick",
                          click = "plot1_click",
                          brush = brushOpts(
                              id = "plot1_brush",
                              resetOnNew = TRUE
                          )
               )
        ),
        column(width = 8, class = "well",
               h4("Selected Time Series"),
               plotOutput("plot3", height = 300)
        )
        
    ),
    fluidRow(
        column(width = 12,
               wellPanel(width = 12,
                         h4("IDs selected by clicking:"),
                         DT::dataTableOutput("plot_clicked_points")
               )
        )
    ),
    fluidRow(
        column(width = 12,
               wellPanel(width = 12,
                         h4("IDs selected by brushing:"),
                         DT::dataTableOutput("plot_brushed_points")
               )
        )
    )
)

server <- function(input, output) {
    
    # -------------------------------------------------------------------
    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- renderPlot({
        ggplot(dt_time_embedding, aes(x, y)) +
            geom_point(aes(x = x, y = y, 
                           color = as.factor(cluster), 
                           alpha = membership_prob)) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme(legend.position = "none")
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # -------------------------------------------------------------------
    # Linked plot
    
    output$plot3 <- renderPlot({
        dt_time_filtered <- nearPoints(df = dt_time_embedding, 
                                       coordinfo = input$plot1_click, 
                                       xvar = "x", yvar = "y", 
                                       addDist = TRUE, maxpoints = NULL)
        
        if(length(unique(dt_time_filtered$id)) > 9) {
            id_limit <- sample(x = unique(dt_time_filtered$id), size = 6, 
                               prob = membership_prob)
            dt_time_filtered <- dt_time_filtered[dt_time_filtered$id %in% id_limit,]
        }
        
        if(nrow(dt_time_filtered) == 0) {
            ggplot()
        } else {
            ggplot(dt_time_filtered, aes(x = date, y = value)) +
                geom_line(color = "grey") +
                geom_point() +
                facet_wrap(~id)
        }
    })
    
    output$plot_clicked_points <- DT::renderDataTable({
        dt_time_filtered <- nearPoints(df = dt_time_embedding, coordinfo = input$plot1_click, 
                                       xvar = "x", yvar = "y", addDist = TRUE, maxpoints = NULL)
        
        dt_time_filtered$dist_ <- round(dt_time_filtered$dist_, 1)
        
        datatable(dt_time_filtered)
    })
    
    output$plot_brushed_points <- DT::renderDataTable({
        dt_time_filtered <- brushedPoints(dt_time_embedding, input$plot1_brush)
        
        datatable(dt_time_filtered)
    })
    
}

shinyApp(ui, server)