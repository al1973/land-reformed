## app.R ##
library(shiny)
library(shinydashboard)
library(rCharts)
library(reshape2)
library(rjson)
options(RCHART_WIDTH = 600)
dat <- read.csv('./data/dimple.csv')
pro <-read.csv('./data/production2014.csv')
reg <- read.csv('./data/region.csv')
top <- read.csv('./data/top5port.csv')

ui <- dashboardPage(
  dashboardHeader(title = "Port Wine Data",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new port server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2015-02-08"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Update Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Analyse Port Data Further "
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
                  
                  ),
  
  dashboardSidebar(
      sidebarMenu(
          menuItem("Port Wine Data", tabName = "portwinedata", icon = icon("dashboard"), badgeLabel = "new", badgeColor = "green"),
          menuItem("Dimple", icon = icon("bar-chart"), tabName = "dimple"),
          menuItem("High Chart", tabName = "highchart", icon = icon("bar-chart")),
          menuItem("NVD3", tabName = "NVD3", icon = icon("bar-chart")),
          menuItem("Leaflet", tabName = "leaflet",icon = icon("map-marker")),
          menuItem("Source code", icon = icon("file-code-o"), 
                   href = "https://github.com/al1973/land-reformed"),
          menuItem(sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."))
          )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "portwinedata",
              fluidRow(
                box(
                  title = "Summary 2014 Port Wine Sales", status = "primary", width=9, solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("summary")
                ),
                
                box(
                 title = "Port Wine Sales 2014",status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 tableOutput("view"),
                 numericInput("obs", "Number of observations to view:", 10),
                 submitButton("Update View")
                ),
        
                valueBox(
                  # The value comes from the server via uiOutput
                  uiOutput("orderNum"), "New Ideas", icon = icon("credit-card")
                ),
                
                valueBox(
                  # The icon can also be a uiOutput
                  uiOutput("progress"), "Progress", icon = uiOutput("progressIcon"),
                  color = "purple"
                ),
                
                # An entire box can be in a uiOutput
                uiOutput("approvalBox")
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "dimple",
              h2("Dimple Graph"),
              h3("Port Wine Sales 2013 Top 10 Countries"),
              showOutput("d1", "dimple"),
              box(
                title = "Summary 2013", status = "primary", width=9, solidHeader = TRUE,
                collapsible = TRUE,
                verbatimTextOutput("sum")
              )
      ),
      
      # Third tab content
      tabItem(tabName = "leaflet",
              h2("Leaflet Maps"),
              showOutput('cavemap', 'leaflet'),
              br(),
              showOutput('douro', 'leaflet')
      ),
      
      # Fourth tab content
      tabItem(tabName = "highchart",          
              showOutput("high", "highcharts"),
              box(title="Region & Year", width=6, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(inputId = "region", label = "Select a region:", choices = c("Lower Corgo", "Upper Corgo","Douro Superior","Outside Douro", "All Regions"),
                  selected = "Lower Corgo"),
                  sliderInput("range", label = "Years to display:", min = 2008, max = 2013, value = c(2008, 2013), sep = ""),
                  submitButton("Update View"),
                  br(),
                  helpText("The Douro is a Portuguese wine region centered on the Douro River ", 
                           "in the Trás-os-Montes e Alto Douro region. ",
                            "It is located some distance upstream from Porto, ",
                            "sheltered by mountain ranges from coastal influence. ",
                            "The region has Portugal's highest wine classification ",
                            "as a Denominação de Origem Controlada (DOC). ",
                            "While the region is associated primarily with Port wine production, ",
                            "the Douro produces just as much table wine (non-fortified wines) ",
                            "as it does fortified wine. The non-fortified wines are ",
                            "typically referred to as Douro wines. ",
                            "The Douro DOC has three sub-regions: Baixo Corgo, ",
                            "Cima Corgo, and Douro Superior, ",
                            "and the special designation Moscatel do Douro.")
              )
       ),
    
      #Fifth tab content
      tabItem(tabName = "NVD3",
              h2("NVD3 Graph"),
              showOutput('n', 'nvd3'),
                box(
                title = "Top 5 Port Buying Countries in Miliions of Euros", status = "primary", width=9, solidHeader = TRUE,
                collapsible = TRUE,
                tableOutput("top5")
                )   
      )
    )
  )
)

server <- function(input, output) {
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(pro)
  }) 
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(pro, n = input$obs)
  })
  
  # Show the first "n" observations
  output$sum <- renderPrint({
    str(dat)
  })
  
  output$d1 <- renderChart2({
    d1 <- dPlot(x = "percent", y = "country", groups = "wine", data = dat, type = "bar")
    d1$xAxis(type = "addPctAxis")
    d1$yAxis(type = "addCategoryAxis", orderRule = "country")
    d1$legend( x = 5, y = 5, width = 600, height = 25, horizontalAlign = "left", orderRule = "wine")
    return(d1)
    })
  
  output$high <- renderChart2({
    selected <- input$region
    region <- subset(reg, region == selected & year %in% seq(input$range[1], input$range[2], 1))
    high <- hPlot(x = "year", y = "litres", group = "wine", data = region, type = "line") 
    high$colors(c("red", "blue","brown","green","yellow","orange","pink"))
    high$yAxis(title = list(enabled = TRUE, text = 'litres'))
    return(high)
  })
  
  # Show the first 5 observations of NVD3 graph
  output$top5 <- renderTable({
    head(top)
  })
  
  output$n <- renderChart2({
    n <- nPlot(x = "country", y = "price", group = "year", data = top, type = "multiBarChart")
    return(n)
  })
    
  output$cavemap <- renderChart2({
    cavemap <- Leaflet$new()
    cavemap$setView(c(41.145039, -8.609508), zoom = 13)
    cavemap$marker(c(41.1381972, -8.6109222), bindPopup = "<p> Port wine cellars all around here </p>")
    cavemap$marker(c(41.161093, -8.633026), bindPopup = "<p> Hi. I am here: R Vanzeleres, 301 </p>")
    return(cavemap)
  })
  
  output$douro <- renderChart2({
    douro <- Leaflet$new()
    douro$setView(c(41.158450, -7.783655), zoom = 9)
    douro$marker(c(41.158450, -7.783655), bindPopup = "<p>Wine producing area all around here>")
    douro$marker(c(41.0798611, -7.112333), bindPopup = "<p> And here </p>")
    return(douro)
  })
  
  output$orderNum <- renderText({ 10*2 })
  
  output$progress <- renderText({ "75%" })
  
  output$progressIcon <- renderUI({ icon("list") })
  
  output$approvalBox <- renderUI({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow")
  })
}

shinyApp(ui, server)