rm(list = ls())

library(shiny)
library(bslib)
library(rmarkdown)
library(ggplot2)
library(readxl)
library(hms)

ui <- fluidPage(
  
  #App's Title
  titlePanel("EZMSA", "EZMSA"),
  
  #App's theme
  theme = bs_theme(version = 4, bootswatch = "minty", primary = "#003299", secondary = "#003299"),
  
  #Sidebar containing the input user can change
  sidebarLayout(
    
    sidebarPanel(
      
      #File containing data and parameter selection
      fileInput("data", "Upload Data", buttonLabel = "Browse...",
                multiple = F,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv',
                         ".xlsx",
                         ".xls")
      ),
      
      #Column seperator in the input file
      radioButtons("sep", "Values separated by...", choices = list("comma" = ",",
                                                                   "semicolon" = ";",
                                                                   "tab" = "\t"), inline = T, selected = ","),
      
      #check if there is an header or not in the input file
      checkboxInput("header", label = "A header is present in the file"),
      
      #Check what kind of decimal is used in the input file
      radioButtons("dec", "Decimal seperator", choices = list("dot" = ".",
                                                              "comma" = ","), inline = T, selected = "."),
      
      hr(),
      
      #Analysis parameter for user to select
      #User select which type of weight to use
      radioButtons("weight", label = "Weight", 
                   choiceNames = list("Use best", 
                                      "1 (no weight)",
                                      "1/x",
                                      HTML(paste0("1/x ", tags$sup("2"))),
                                      "1/variance"),
                   choiceValues = list(5,1,2,3,4),inline = T),
      p(HTML(paste0("Use best : 1/x ", tags$sup("2"),  " if replicates < 3; else 1/variance"))),
      
      #User select which type of regression to use
      radioButtons("reg", label = "Regression order", 
                   choices = list("Use Best" = 1,
                                  "Linear Regression" = 2,
                                  "Quadratic Regression" = 3), inline = T, selected = 1),
      
      hr(),
      
      #User input the unit of measurment 
      textInput("unit", label = "Concentration units (e.g., ng/mL)", value = ""),
      
      #User input the p-Value to use for confidence interval and tests
      numericInput("pvalue", "Significance threshold", value = 0.05, step = 0.01),
      
      #User input uCRM 
      numericInput("ucrm", HTML(paste0("u", tags$sub("CRM"))), value = 0, step = 0.001),
      
      hr(),
      
      #Start button for the analysis
      actionButton("start", "Start Analysis")
    ),
    
    #Main panel containing the outputs of analysis
    mainPanel(
      
      tabsetPanel(
        
        #Displays the app introduction and instruction
        tabPanel("Home",
                 h1(strong("EZMSA")),
                 
                 h3("EZMSA is an application that allows the user to perform standard addition calculations from raw data, 
                    including the calculation of the unknown's concentration and measurement uncertainty. It also allows 
                    the user to manipulate the regression parameters and generate reports (HTML, PDF or Word) to fit their needs."),
                 
                 h2(strong("How to use the app")),
                 
                 h5(HTML(paste("As is the case in the EZMSA Excel tool, the", strong("spiked concentration"), "should be indicated in the",
                               strong("last column"), ", while replicate measurements appear in the first columns:"))),
                 
                 img(src = "example.png", align = "center"),
                 
                 h5(HTML(paste0(strong("Import"), " your data file (.txt or .csv) by clicking &#8220;Browse...&#8221; and navigating to the correct file."))),
                 
                 h5(HTML(paste("Select ", strong("how values are separated"), " in the file provided; whether a ", strong("header is used"), " and ",
                               strong("which decimal separator"), " is used (typically, a dot)."))),
                 
                 h5(HTML(paste("While we recommend you let the algorithm ", strong("use the best"), " weight and regression order, you can also choose to manipulate
                    the regression parameters and force a specific weight and/or regression order."))),
                 
                 h5(HTML(paste("Indicate the ", strong("concentration units"), " used, for easier to read and understand graphs and results."))),
                 
                 h5(HTML(paste("By default, a ", strong("0.05 significance threshold"), " (5%) is used for all hypothesis tests and confidence intervals 
                               calculated, but you can change this as needed."))),
                 
                 h5(HTML(paste0("Finally, enter the ", strong("uncertainty associated with the reference material (u",tags$sub("CRM"),")."),
                                "This value can be found on the Certificate of Analysis, and is used to calculate the final 
                               measurement uncertainty. If you do not know its value, or do not wish to include it in the calculation, 
                               leave its value at 0."))),
                 
                 h5(HTML(paste("Hit &#8220;", strong("Start Analysis"), "&#8221;, and explore results in the different tabs."))),
                 
                 h2(strong("Tabs")),
                 
                 h4(strong("Plots")),
                 
                 h5(HTML(paste("Displays ", strong("graphical representations"), " of the data. The top graph, the ", strong("regular"), " standard addition
                               calibration curve, uses measurements on the y axis and concentrations on the x axis, as is typically done. The bottom graph, the ",
                               strong("inverted"), " standard addition calibration curve, uses concentration on the x axis and measurements on the y axis. The 
                               inverted setup is used for the least squares regression calculating the unknown's concentration and its measurements uncertainty. 
                               Both graphs display the ", strong("calibration curve"), " used for the calculation of the unknown"))),
                 
                 
                 h4(strong("Raw Data")),
                 
                 h5("Displays a table containing the raw data (concentrations, measurement replicates) used in the analysis. 
                    Allows the user to check that the file importation worked properly."),
                 
                 h4(strong("Results")),
                 
                 h5(HTML(paste("Includes ", strong("all parameters and results"), " relevant to the least squares ", strong("regression"), " (weight and order
                               selected, equation of the final model) ", strong("measurement uncertainty"), " (standard, combined and expanded uncertainty) and ",
                               strong("unknown's concentration"), " (calculated concentration and confidence interval)."))),
                 
                 h4(strong("Report")),
                 
                 h5("Allows the user to customize the title and content of a final standard addition report. 
                    The analyst can include any comments in the “Additional Specifications” box – this text will appear at the top of the report. 
                    The report can be exported in HTML, PDF or Word formats."),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")
        ),
        
        #Displays the plot generated by the analyse
        tabPanel("Plots", plotOutput("stdPlot"), br(), plotOutput("invPlot"),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")),
        
        #Displays the raw data from the file used for the analysis
        tabPanel("Raw Data", tableOutput("dataOut"),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")),
        
        #Displays the data calculated by the execution of the analysis
        tabPanel("Results", 
                 fluidRow(
                   column(6,
                          h4(strong("Regression parameters")),
                          
                          HTML((paste0(strong("Weight used in the analysis: "), textOutput("weightRes")))),
                          
                          HTML((paste0(strong("Confidence interval on b",tags$sub(2), "(quadratic term): "), textOutput("confb2Res")))),
                          
                          HTML((paste0(strong("Order of the regression: " ), textOutput("orderRes")))),
                          
                          HTML((paste0(strong("Equation of the regression: " ), textOutput("eqRes"))))
                   ),
                   
                   column(6,
                          h4(strong("Measurement uncertainty")),
                          
                          HTML(paste0(strong("u",tags$sub("CRM")," used in the analysis: "), textOutput("ucrmRes"))),
                          
                          HTML((paste0(strong("Calculated u",tags$sub("b0"),": "), textOutput("b0Res")))),
                          
                          HTML((paste0(strong("Combined standard uncertainty: "), textOutput("stduncRes")))),
                          
                          HTML((paste0(strong("Expanded measurement uncertainty: " ), textOutput("expRes"))))
                          
                   ),
                   
                   column(6, 
                          h4(strong("Unknown's concentration")),
                          
                          HTML((paste0(strong("Calculated concentration: "), textOutput("calcRes")))),
                          
                          HTML((paste0(strong("Lower bound of the confidence interval: "), textOutput("lowRes")))),
                          
                          HTML((paste0(strong("Upper bound of the confidence interval: "), textOutput("uppRes")))),
                          
                          HTML((paste0(strong("Confidence level: "), textOutput("confRes"))))
                          
                   ),
                 ),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")),
        
        #Displays the option for generating a report of the analysis
        tabPanel("Report", 
                 
                 textInput("reportName", "Report title", value = "Report"),
                 
                 textInput("add", "Additionnal comments", width = "1000px", value = ""),
                 
                 radioButtons("raw_data_report", "Include raw data",
                              choices = list("Yes" = T, "No" = F), inline = T, selected = T),
                 
                 radioButtons("graph_report", "Graphs to be included",
                              choices = list("Both" = 1, "Standard" = 2, "Inverted" = 3), 
                              inline = T, selected = 1),
                 
                 p("Other results to be included"),
                 
                 checkboxInput("all", "Select All"),
                 
                 checkboxGroupInput("regReport", "Regression parameters",
                                    choiceNames = list("Weight",
                                                       HTML(paste0("Confidance interval on b", tags$sub(2))),
                                                       "Order of the regression",
                                                       "Equation of the regression"),
                                    choiceValues = list(1,2,3,4)),
                 
                 checkboxGroupInput("mesReport", "Measurement uncertainty",
                                    choiceNames = list(HTML(paste0("u", tags$sub("CRM"))),
                                                       HTML(paste0("u", paste0(tags$sub("b"), tags$sub(0)))),
                                                       "Combined standard uncertainty"),
                                    choiceValues = list(1,2,3)
                 ),
                 
                 checkboxGroupInput("unkReport", HTML(paste0("Calculated unknown concentration", "<br/>",
                                                             "Calculated unknown concentration with concentration units will always appear on the report.")),
                                    choiceNames = list("Expanded measurement uncertainty, as ± after the concentration",
                                                       "Confidence interval for the concentration",
                                                       "Confidence level"),
                                    choiceValues = list(1,2,3),
                                    width = "800px"),
                 
                 downloadButton("reportHTML", "Generate and save HTML report"),
                 
                 downloadButton("reportPDF", "Generate and save PDF report"),
                 
                 downloadButton("reportDOCX", "Generate and save DOCX report"),
                 
                 verbatimTextOutput("reportOut"),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")),
        
        tabPanel("Contact", 
                 h5("To report bugs or request features, please e-mail Brigitte Desharnais at brigitte.desharnais@msp.gouv.qc.ca."),
                 h5("The EZMSA R application was conceptualized by Brigitte Desharnais and coded by Étienne Lebrun, 
                    based on work from the EZMSA team Jocelyn V. Abonamah, Brigitte Desharnais, Harold E. Schueler and Szabolcs Sofalvi."),
                 #br(),
                 #h2("Citation"),
                 #h5("texte pour citation ici"),
                 
                 hr(),
                 
                 p(HTML(paste("Laboratoire de sciences judiciaires et de médecine légale",
                              "Ministère de la Sécurité Publique, Gouvernement du Québec (Canada)",
                              "Version 1.0.0, Copyright © 2023", sep="<br/>")),align = "center")
        )
      )
    )
  )
)