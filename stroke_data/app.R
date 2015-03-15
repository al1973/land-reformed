## app.R ##
library(shiny)
library(shinydashboard)
library(rCharts)
library(reshape2)
library(rjson)
library(DT)
library(dplyr)
options(RCHART_WIDTH = 600)

subist<- read.csv('./data/subist.csv')
table1<-read.csv('./data/table1.csv')
table2<-read.csv('./data/table2.csv')
table3<-read.csv('./data/table3.csv')
table4<-read.csv('./data/table4.csv')
table5<-read.csv('./data/table5.csv')
ui <- dashboardPage(skin="black",
        dashboardHeader(),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("IST Data", tabName = "istdata", icon = icon("dashboard"),badgeLabel = "new", badgeColor = "green"),
                        menuItem("Dimple", icon = icon("bar-chart"), tabName = "dimple"),
                        menuItem("More Tables", tabName = "NVD3", icon = icon("bar-chart")),
                        menuItem("DT", tabName = "DT", icon = icon("bar-chart")),
                        menuItem("R Markdown", tabName = "rmd", icon = icon("file-code-o")),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/al1973/land-reformed"),
                        menuItem("Data source", icon = icon("file-code-o"), href = "http://www.trialsjournal.com/content/12/1/101/")
                )
        ),
        
        dashboardBody(
                tabItems(
                        # First tab content
                        tabItem(tabName = "istdata",
                                fluidRow(
                                        box(
                                            title = "Summary International Stroke Trail", status = "primary", width=12, solidHeader = TRUE,
                                            collapsible = TRUE,
                                            tableOutput("summary")
                                        ),
                                        
                                        box(
                                            title = "IST Records",status = "primary", width=12,solidHeader = TRUE,
                                            collapsible = TRUE,
                                            tableOutput("view"),
                                            numericInput("obs", "Number of observations to view:", 10),
                                            submitButton("Update View")
                                        )
                                      
                                )
                        ),
                        
                        ## Second tab content ##
                        tabItem(tabName = "dimple",
                                h3("Sex vs 6 Month Outcome"),
                                showOutput("d1", "dimple"),
                                box(
                                    title = "Summary: sex vs 6 month outcome", status = "primary", width=9, solidHeader = TRUE,
                                    collapsible = TRUE,
                                    verbatimTextOutput("table4")
                                )
                        ),
                                              
                        ## third tab content ##
                        tabItem(tabName = "NVD3",
                                fluidRow(
                                box(
                                    title = "Aspirin vs 6 Month Outcome", status = "primary", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    tableOutput("table2")
                                ), 
                                box(
                                    title = "Heparin vs 6 Month Outcome", status = "primary", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    tableOutput("table3")
                                ),
                                box(
                                    title = "Stroke vs 6 Month Outcome", status = "primary", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    verbatimTextOutput("table1")
                                ),
                                box(
                                        title = "Country vs Cases", status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,
                                        dataTableOutput("dt1")
                                )
                                )
                                ),

                        ## fourth tab content ##
                        tabItem(tabName = "DT",
                                dataTableOutput('dt')
                                )
                )
        )
)

server <- function(input, output) {
        # Generate a summary of the dataset
        output$summary <- renderTable({
                summary(subist)
        }) 
        
        # Show the first "n" observations
        output$view <- renderTable({
                head(subist, n = input$obs)
        })
                
        # Show the first 6 observations of stroke vs outcome
        output$table1 <- renderPrint({
                str(table1)
        })
        
        # Show the first 6 observations of aspirin vs outcome
        output$table2 <- renderTable({
                head(table2, n=10)
        })
        
        # Show the first 6 observations of heparin vs outcome
        output$table3 <- renderTable({
                head(table3,n=15)
        })
        
        # Show a data table of country vs no. cases
        output$dt1 <- renderDataTable({
        dt1 <- datatable(table5, options = list(pageLength = 10),rownames = FALSE, caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;','Table 1: ', htmltools::em('Country with number of cases')))
        return(dt1)
        })
        
        # Generate a summary of sex vs 6 month outcome
        output$table4 <- renderPrint({
                str(table4)
        })
        
## nvd3 charts not used  ##        
#         output$n <- renderChart2({
#                n <- nPlot(x = "stroke", y = "n", group = "outcome", data = table1, type = "multiBarChart")
#                return(n)
#         })
#         
#         output$n1 <- renderChart2({
#                 n1 <- nPlot(x = "heparin", y = "cases", group = "outcome", data = table3, type = "multiBarChart")
#                 return(n1)
#         })
#         
#         output$n2 <- renderChart2({
#                 n2 <- nPlot(x = "aspirin", y = "cases", group = "outcome", data = table2, type = "multiBarChart")
#                 return(n2)
#         })
        
# dimple graph #
        output$d1 <- renderChart2({
              d1 <- dPlot(x = "outcome", y = "cases", groups = "sex", data = table4, type = "bar")
              d1$yAxis(type = "addPctAxis")
              d1$xAxis(type = "addCategoryAxis", orderRule = "outcome")
              d1$legend( x = 5, y = 5, width = 600, height = 25, horizontalAlign = "left", orderRule = "Porto")
              return(d1)
        })
# data table #        
        output$dt <- renderDataTable({
                ## subset of data with country = portugal ##
                table6<-filter(subist, country == "PORT")
                dt <- datatable(table6, options = list(pageLength = 10),rownames = FALSE, caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: center;','Table 2: ', htmltools::em('Subset of patient data for Portugal only')))
                return(dt)
        })
       
}

shinyApp(ui, server)