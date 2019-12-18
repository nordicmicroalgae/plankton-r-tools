library(shiny)
library(DT)
library(tidyverse)
library(leaflet)
library(readxl)
library(cerberus)

ui <- fluidPage(
    
    # App title ----
    titlePanel("Thalassa: PlanktonToolbox data processing program"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Upload text files
            fileInput("file", "Upload PlanktonToolbox Excel file", accept = ".xlsx"),
            # action button to view the file
            actionButton("view", "Viewer"),
            # action button to display map the data
            actionButton("map", "Map"),
            # action button to analyze the file
            actionButton("process", "Process file"),
            br(),
            # Map of data
            leafletOutput("mapdisplay")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            img(src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/SMHI_Logo.svg/200px-SMHI_Logo.svg.png', align = "right"),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Overview", 
                                 # to display the table of the file to be analyzed
                                 DT::dataTableOutput("file")),
                        tabPanel("Plot taxa", tableOutput("taxa")),
                        tabPanel("Plot bubbles", tableOutput("bubbles")),
                        tabPanel("Match Taxa", tableOutput("matchtaxa"))
            )
            
        )
    )
)

# server()

server <- function(input, output, session) {
    
    # View data table
    observeEvent(input$view,
                 output$datatable <- DT::renderDataTable({
                     read_excel(input$file)
                 })
                 
    )
    
    # View map 
    observeEvent(input$map,
                 output$mapdisplay <- renderLeaflet({
                     plot_map_leaflet(read_excel(input$file))                     
                 })
                 
    )
    
    # Process file
    observeEvent(input$process,
                 output$taxa <- renderTable({
                     read_excel(input$file)                     
                 })
                 
    )
    observeEvent(input$process,
                 output$bubbles <- renderTable({
                     read_excel(input$file)                     
                 })
                 
    )
    observeEvent(input$process,
                 output$matchtaxa <- renderTable({
                     match_wormstaxa(read_excel(input$file)$scientific_name)                     
                 })
                 
    )
    
}

shinyApp(ui, server)