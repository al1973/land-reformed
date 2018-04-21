## app.R ##

## load required libraries ##
library(shiny)
library(shinydashboard)
library(rCharts)
library(tidyverse)
library(rjson)
library(DT)
library(leaflet)

## read in the data files ##
special_ports <- read_csv('./data/tidy_special_ports_17.csv')
regular_ports<- read_csv('./data/tidy_regular_ports_17.csv')
sales <-read_csv('./data/sales2017.csv')
reg <- read_csv('./data/tidy-all-wines-douro.csv')
top6 <- read_csv('./data/tidy-top-six-countries.csv')
top6wide <- read_csv('./data/top-six-wide.csv')
saleschange <- read_csv('./data/port-sales-change-08-17.csv')

## UI code##
## creating a dashboard ##
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
                                 message = "The new port wine data is ready.",
                                 icon = icon("life-ring"),
                                 time = "2018-04-08"
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
          menuItem("Port Wine Data 2017", tabName = "portwinedata", icon = icon("dashboard"), badgeLabel = "new", badgeColor = "green"),
          menuItem("2017 Top 10", icon = icon("bar-chart"), tabName = "dimple"),
          menuItem("All wines", tabName = "highchart", icon = icon("bar-chart")),
          menuItem("Top 6", tabName = "NVD3", icon = icon("bar-chart")),
          menuItem("Sales over time", tabName = "DT", icon = icon("bar-chart")),
          menuItem("Source code", icon = icon("file-code-o"), 
                   href = "https://github.com/al1973/land-reformed"),
          menuItem("Data source", icon = icon("file-code-o"), href = "http://ivdp.pt/"),
          menuItem(sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."))
      
          )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content, a brief summary
      tabItem(tabName = "portwinedata",
              dataTableOutput('dtsales')
      ),
      
      # Second tab content
      tabItem(tabName = "dimple",
              h2("Port Wine Sales 2017 - Top 10 Countries"),
              p("The", strong("IVDP"),"(Wine Institute for the Douro and Porto) divides port into two categories:
                 normal (standard ruby and 3-year-old tawny, white and rosé) 
                 and special categories, which include all other ports."),
              showOutput("d1", "dimple"),
              showOutput("d2", "dimple"),
              p(strong("Ruby:"),"standard Ruby port labeled as “Ruby”, “Fine Ruby” or “Port”. 
                 The label refers to the year it was bottled. These Rubies are often aged in stainless steel vats for as little as 2 or 3 years before bottling."),
              p(strong("Tawny:"), "standard Tawny, sometimes labeled “Fine Tawny”. It is without an indication of age but must have been aged for at least 3 years before bottling."),
              p(strong("White:"), "standard port made from white grapes"),
              p(strong("Rosé:"), "technically a ruby port, but fermented in a similar manner to a rosé wine."),
              p(strong("Tawny with indication of age:"), "Tawny Ports with age indication (10, 20, 30, 40 years). The number of years does not indicate the age of the grapes, but the average age of the blends. 
                Every bottle of a designated age should have the same flavour regardless of the year it was bottled."),
              p(strong("Colheita:"), "a special Tawny port from a particularly good year. 
                It must be aged for a minimum of seven years in wood"),
              p(strong("Crusted:"), "a normally blended Port aged in the bottle; the year on the label refers to the year it was bottled."),
              p(strong("LBV:"), "Late Bottled Vintage, a high standard wine left in the barrel for longer than a vintage port. It can be filtered or unfiltered."),
              p(strong("Reserve:"), "a premium blended ruby port."),
              p(strong("Tawny Reserve:"), "a Tawny of about seven years. It comes between a standard tawny and those with an indication of age of 10 years"),
              p(strong("Vintage:"), "a port made entirely from the grapes of a declared vintage year. 
                       The decision to declare a Vintage is made in the spring of the second year following the harvest. 
                       Vintage ports are aged in barrels or stainless steel vats for a maximum of two and a half years before bottling")
),      
      
      # Third tab content, demonstrating a high chart
      tabItem(tabName = "highchart",          
              showOutput("high", "highcharts"),
              "Click on wine name to add or remove from graph",
              br(),
              br(),
              fluidRow(column(3, selectInput(inputId = "region", label = "Select a region:", choices = c("Lower Corgo", "Upper Corgo","Upper Douro","Outside Douro Region", "All Regions"),
                                                  selected = "Lower Corgo"),
                                   sliderInput("range", label = "Years to display:", min = 2008, max = 2017, value = c(2008, 2017), sep = ""),
                                   submitButton("Update View")),
                        column(9, leafletOutput("map") )),
              br(),
              br(),
              p("The Douro is a Portuguese wine region located on the banks of the Douro River
                in the Trás-os-Montes and Alto Douro regions. It is situated some distance upstream from Porto,
                sheltered by mountain ranges from coastal influence. 
                The region has Portugal's highest wine classification -", em("Denominação de Origem Controlada (DOC)"),"."),
              p("While the region is associated primarily with Port wine production, the Douro produces as much regular wine as fortified.
                The non-fortified are typically referred to as Douro wines. The region also produces Moscotel and sparkling wines"),
              p("The Douro DOC has three sub-regions: Lower Corgo, Upper Corgo, and Upper Douro:"),
              p(strong("Lower Corgo"), " – Is centered around Peso da Régua, downstream from the river Corgo.
              It is the wettest and on average coolest zone. It mainly produces the grapes for the cheaper ruby and tawny ports."),
              p(strong("Upper Corgo"), " – Is centred around Pinhão. The summertime average temperature of the region is a few degrees higher, 
              and annual rainfall is less. The grapes grown in this zone are considered of higher quality, 
              and are used for Vintage, Reserve, aged Tawny and LBV Ports."),
              p(strong("Upper Douro"), "– The easternmost zone, extending almost to the Spanish border. 
              This is the least cultivated, most arid and warmest region of the Douro.")
       ),
    
# Fourth tab content, demonstrating an NVD3 Chart with top countries over the years 2013 to 2017.
      tabItem(tabName = "NVD3",
              showOutput('n', 'nvd3'),
              dataTableOutput("top6wide")
      ),
      
#fifth tab, sales change 2008 to 2017
      tabItem(tabName = "DT",
              dataTableOutput('dt')
      )
    )
  )
)

## server code ##
server <- function(input, output) {
  ## first tab ##      
  # Data table of 2017 sales, top 25
        
     output$dtsales <- renderDataTable({
             sales <- sales %>% arrange(Rank) %>% 
                     select(1:5, 11,12)
              dtsales <- datatable(sales,colnames = c("Rank","Country","Market Share(%)","Total", "Change(%)", "Price/Litre","Change/L(%)"), rownames=FALSE,
                         extensions="Buttons","Responsive",
                         options = list(pageLength = 10, dom = 'Bfrtip', buttons = c('copy', 'print')),
                         caption = tags$caption(style = 'caption-side: bottom; text-align: center;','Table 1: Port Wine Sales 2017. Change is based on 2016')) %>% 
                         formatCurrency(4, '\U20AC',  digits = 0) %>%
                         formatCurrency(6, '\U20AC',  digits = 2) %>% 
                         formatStyle(c(5,7),  color = styleInterval(0, c('red','green')))
              return(dtsales)
     })

  
  ## code to display the "Dimple" graphs on the second tab ## 
  output$d1 <- renderChart2({
    d1 <- dPlot(x = "percent", y = "country", groups = "port", data = special_ports, type = "bar")
    d1$xAxis(type = "addPctAxis")
    d1$yAxis(type = "addCategoryAxis", orderRule = "country")
    d1$legend( x = 5, y = 5, width = 600, height = 25, horizontalAlign = "left", orderRule = "port")
    return(d1)
    })
  
  output$d2 <- renderChart2({
    d2 <- dPlot(x = "percent", y = "country", groups = "port", data = regular_ports, type = "bar")
    d2$xAxis(type = "addPctAxis")
    d2$yAxis(type = "addCategoryAxis", orderRule = "country")
    d2$legend( x = 5, y = 5, width = 600, height = 25, horizontalAlign = "left", orderRule = "port")
    return(d2)
    })
  
  
  ## code to display the "High Chart" on the third tab ##
  output$high <- renderChart2({
    selected <- input$region
    region <- subset(reg, Region == selected & year %in% seq(input$range[1], input$range[2], 1))
    high <- hPlot(x = "year", y = "Litres", group = "wine", data = region, type = "line") 
    high$colors(c("red", "blue","brown","green","yellow","orange","pink"))
    high$yAxis(title = list(enabled = TRUE, text = 'litres'))
    return(high)
  })
 # create icon for map on third tab 
  greenLeafIcon <- makeIcon(
          iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
          iconWidth = 38, iconHeight = 95,
          iconAnchorX = 22, iconAnchorY = 94,
          shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
          shadowWidth = 50, shadowHeight = 64,
          shadowAnchorX = 4, shadowAnchorY = 62
  )
  # adding 3 centres of Port wine production: Régua, São João da Pesqueira, Pinhâo to a tibble
  towns <-tibble(
          lng = c(-7.789185, -7.404620, -7.533062, -8.610706, -8.606487),
          lat = c(41.164329,  41.146975, 41.189297, 41.149990, 41.129403)
  )
  # render the map with the 3 markers displayed
  output$map <- renderLeaflet({
          map<- leaflet() %>% setView(lng = -7.404620, lat = 41.146975, zoom =9) 
          map %>% addProviderTiles(providers$CartoDB.Positron,
                                   options = providerTileOptions(noWrap = TRUE)) %>% 
        
                  addMarkers(data= towns, ~lng, ~lat, icon = greenLeafIcon)
      
  })
  
  ## code to display the "NVD3" graph on the fourth tab ##
  output$n <- renderChart2({
    n <- nPlot(x = "year", y = "total", group = "country", data = top6, type = "multiBarChart")
    return(n)
  })
  ## Show the data from the graph below #
  output$top6wide <- renderDataTable({
                  top6wide <-  datatable( top6wide, options = list(dom = 't'), 
                                          extensions= "Responsive",
                                          caption = tags$caption(style = 'caption-side: bottom; text-align: center;',
                                                                 'Top six countries for Port Wine sales 2013 to 2017, 
                                                                 graph shows sales in millions of Euro.')) %>% 
                                          formatCurrency(2:7, '\U20AC',  digits = 0)
          return(top6wide)
  })
  
  ## code to display the "Data Table" on the fifth tab ##
  output$dt <- renderDataTable({
          saleschange<-arrange(saleschange, desc(year17))
    dt <- datatable(saleschange, options = list(pageLength = 10,  dom = 'Bfrtip', buttons = c('copy', 'print')),
                    colnames = c('Country','Continent','Region','Sales 2008','Sales 2017', 'Increase/Decrease'),
                    extensions="Responsive","Buttons",
                    caption = tags$caption(
                    style = 'caption-side: bottom; text-align: center;','Table 1: Change in Port Wine Sales 2008 to 2017.')) %>% 
                    formatCurrency(4:6, '\U20AC',  digits = 0) %>% 
                    formatStyle('Increase/Decrease',  color = styleInterval(0, c('red','green')))
    return(dt)
  })
 
}

shinyApp(ui, server)