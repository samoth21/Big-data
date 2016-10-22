library(shiny)
#library(googleVis)
library(rpivotTable)
library(formattable)
library(highcharter)
#library(plotly)
# Define UI for dataset viewer application
shinyUI(fluidPage(
  #options(encoding="UTF-8")
  # Application title.
  titlePanel("CABG SRD KPI Business Intelligence Application"),
  #titlePanel("頑張!"),
   #mainPanel(
   navlistPanel(widths = c(3, 9),
                
                tabPanel(h5("Historical Utilization and Resource Mapping"), h1("Historical Utilization and Resource Mapping"),
                         helpText("The data uploaded every month will be stored in the server and 
                                  perform the historic data analysis; users can interact with the 2 plots below."),
                         #h2("Manpower Utilization Trend"),
                         fluidRow(
                           column(width = 2, class = "panel",
                                  selectInput("type", label = "Type", width = "100%",
                                              #choices = c("line", "column", "bar", "spline")), 
                                              choices = c("column", "line", "bar", "spline")), 
                                  selectInput("stacked", label = "Stacked",  width = "100%",
                                              choices = c(FALSE, "normal", "percent")),
                                  #                     selectInput("theme", label = "Theme",  width = "100%",
                                  #                                 choices = c(FALSE, "fivethirtyeight", "economist",
                                  #                                             "darkunica", "gridlight", "sandsignika",
                                  #                                             "null", "handdrwran", "chalk")),
                                  selectInput("theme", label = "Theme",  width = "100%",
                                              choices = c("fivethirtyeight",FALSE,"economist",
                                                          "darkunica", "gridlight", "sandsignika",
                                                          "null", "handdrwran", "chalk")),
                                  
                                  submitButton("Submit and view")
                           ),
                           column(width = 10,
                                  highchartOutput("hcontainer",height = "500px")
                           )),
                         br(),br(),br(),
                         h2("Project Resource Mapping Analysis"),
                         #rpivotTableOutput("historic_pivot", height = 700, width = 1200)
                         rpivotTableOutput("by_project_pivot", height = 700, width = 1200)
                         ),              
                
                tabPanel(h5("Resource Mapping for Current Month"), h1("Resource Mapping for Current Month"),
                         helpText("This function intends to generate SRD resource mapping chart automatically every month to provide director/managers to
                allocate the resources more reasonable."),
                         #              h2("Top 10 Standard Projects with Most Man Hours"),
                         #              h4("A quick glance at both run rate and JV projects."),
                         #              dataTableOutput("resource_mapping_table"),
                         #plotOutput("resource_mapping_pie", height = 400, width = 1000),br(),br(),br(),
                         fileInput('file1', 'Please upload DMS CSV File.', accept=c('text/csv',
                                                                                    'text/comma-separated-values,text/plain','.csv'), multiple = T),
                         #uiOutput("Tilemaker2"),
                         h2("Project Man Hours by Business Model and Ranking"),
                         #h4("The man hour ranking of the standard projects classfied by the business model."),
                         #h4("Then will Split by business model using bar plot:"),
                         #plotOutput("resource_mapping_orderbar", height = 600, width = 1050),
                         #dataTableOutput("order_bar"),
                         highchartOutput("resource_mapping_orderbar", height = 700, width = 1050),
                         #dataTableOutput("order_bar"),
                         h2("Project Man Hours Breakdown by Department"),
                         h4("The barplot clear shows how man hour distributed on the projects of the selected department."),
                         column(6,uiOutput("choose_Department"),submitButton("Submit and view")),br(),br(),br(),
                         #uiOutput("choose_Department"),
                         #dataTableOutput("Department_table"),br(),br(),br(),
                         #rpivotTableOutput("Department_bar")
                         plotOutput("Department_bar", height = 600, width = 1250)
                         #plotlyOutput("Department_bar", height = 250, width = 500)
                ),            
            
    tabPanel(h5("Manpower Analysis for Current Month"), h1("Manpower Analysis for Current Month"),
             helpText("This function intends to analyze the man hours in DMS and generate the plots and reports automatically every month to provide upper level managers to
                allocate the resources more reasonable."), 
             
             textInput("month", label = h4("Please input year and month."), value = NULL),submitButton("Submit to Database"),
#              dateInput("month", label = h4("Please select month."), value = NULL,format = "yyyy-mm",
#                        startview = "month" ),submitButton("Submit to Database"),
             textOutput("melt_upload"),
             #dataTableOutput("melt_upload"),
             h2("Manpower Overall Utilization"),
             h4("We used 3D pie chart in order to quickly address the overall manpower utilization of the Server RD."),
             #dataTableOutput("overall_util_table"),
             formattableOutput("overall_util_table", width = 800),
             plotOutput("overall_util_pie",height = 400, width = 900),
             h2("Reactive Dynamic Summary Tables and Plots"),
             #h4("It provides the detailed man hours for both JV and run rate projects."),
             #h2("Online Dynamic Pivot Table"),
             h4("The table is able to generate numerous summary tables and plots by dragging the columns and 
                rows by the users according to their requirements."),
             #downloadButton('downloadPlot1', 'Download the Plot'),
             #dataTableOutput("overall_util_pivot")
             rpivotTableOutput("overall_util_pivot", height = 700, width = 1200)
             #plotOutput("ggplot_boxplot")
             ),

    tabPanel(h5("Monthly Expense Analysis (Phase II)"), h1("Monthly Expense Analysis"),
             helpText("This function turns the monthly expense into a summarized pivot table and provide visualization analysis."),
             #highchartOutput("Project_expense",height = 700, width = 1200),
             fileInput('file2', 'Please upload expense CSV File.', accept=c('text/csv',
                                                                        'text/comma-separated-values,text/plain','.csv')),
             #dataTableOutput("expense_table"),
             textInput("month2", label = h4("Please input year and month."), value = NULL),submitButton("Submit to Database"),
             textOutput("melt_upload2"),
			 #dataTableOutput("melt_upload2"),
             rpivotTableOutput("expense_pivot",height = 700, width = 1200) 
            ),
    tabPanel(h5("R&D Effectiveness Metrics (Phase II)"), h1("R&D Effectiveness Metrics (Phase II)"),
             h2("We should add more KPIs as listed below...."),
             h3("1. Tangible:"),
             h4("1-1. Total R&D Headcount"),
             h4("1-2. Variance from Budget"),
             h4("1-3. Deviation from Schedule"),
             h4("1-4. Number of Projects in Active Development"),
             h4("1-5. R&D spending as Percentage of Sales"),
             h3("2. Intangible (Optional):"),
             h4("2-1. Number of Patent Applicantions"),
             h4("2-2. Number of Publications in Internatioal Journals"),
             h4("2-3. R&D Technologies Transferred to or adopted by BU Each Year")
            )
  )
))

