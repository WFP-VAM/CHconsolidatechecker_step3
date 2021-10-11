#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

#instead of finding the sheet name "Tableau 4 - Estimation Pop" - allow the users to input the excel file and then select the specific sheet
#how to make the input a reactive/observer, so that the renaming/calculation run after uploading the file


library(shiny)
library(tidyverse)
library(openxlsx)
library(DT)
library(shinyBS)

source('dataset_manip.R')

ui <- fluidPage(
   
   tabsetPanel(
      tabPanel("Import data",
               hr(),
               p(em("Will search for and load sheet name 'Tableau 4 - Estimation Pop from selected excel sheet")),
               fileInput('file', 'Input'),
               bsAlert('alert1'),
               uiOutput('select_sheet'),
               tableOutput('table')
      ),
      tabPanel("Current Results",
               hr(),
               p(em("Rows below contain errors/warnings/suggestions according to rules")),
               DTOutput('dataset_current_dt')
      ),
      #show output of the current join
      tabPanel("Projected Results",
               h1("Projected Results")
      )
   )
)


server <- function(input, output, session) {

   workbook_reactive <- reactiveVal()
   #dataset_reactive <- reactiveVal()
   
   # triggers only when event happens
   observeEvent(input$file,{
      print('New file uploadedd')
      workbook_reactive(NULL)
      closeAlert(session, 'al1')
      closeAlert(session, 'al2')
      infile <- input$file
      dataFile <- infile$datapath
      print(dataFile)
      
      excel_file <- try(openxlsx::loadWorkbook(dataFile), silent = T)
      
      if(class(excel_file) == "try-error"){
         createAlert(session, 'alert1','al1', content = 'Not an XLSX')
      }else{
         createAlert(session, 'alert1','al2', content = 'Correct File')
         workbook_reactive(excel_file)
      }

   })
   
   output$select_sheet <- renderUI({
      req(!is.null(workbook_reactive()))
      sheets_sel <- names(workbook_reactive())
      selectInput('mysheet', label = 'Select Sheet', choices = sheets_sel)
   })
   
   dataset_reactive <- reactive({
      req(!is.null(workbook_reactive()))
      req(input$mysheet)
      df <- openxlsx::read.xlsx(xlsxFile = workbook_reactive(),
                          sheet = input$mysheet,
                          startRow = 1) 
      df[-c(1:3),]
   })
   
   output$table <- renderTable({
      dataset_reactive()
   })


   # turn joined projected into datatable
   output$dataset_current_dt <- renderDT({
      
      dataset <- dataset_reactive()
      dataset_current <- dataset_clean(dataset)
      
      datatable(dataset_current, filter = "top",
                           extensions = 'Buttons',
                           options = list( # pageLength = ALL, info = FALSE,
                                               lengthMenu = list(c(15, -1), c("15", "All")),
                                               dom = 'lBfrtip',
                                               buttons = c('copy', 'csv')
                                             )
               )
      })
   
   
   
   
} 

# Run the application 
shinyApp(ui = ui, server = server)
