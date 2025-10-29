library(shiny)

ui <- fluidPage(
  titlePanel("NYC LL84 Energy & Water — EDA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year", choices = c("2025","2024"), selected = "2025"),
      selectInput("ptype","Property Type", choices = c(), multiple = TRUE),
      selectInput("borough","Borough", choices = c(), multiple = TRUE),
      checkboxInput("only_pos","Only > 0", FALSE),
      checkboxInput("trim1","Trim extremes (±1%)", FALSE)
    ),
    mainPanel(
      h4("Overview"),
      plotOutput("p_overview"),
      h4("Top List (preview)"),
      tableOutput("tbl_top")
    )
  )
)

server <- function(input, output, session){
  output$p_overview <- renderPlot(plot(1,1)) 
  output$tbl_top <- renderTable(head(mtcars))  
}
shinyApp(ui, server)
