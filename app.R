## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

library(tidyverse)
library(qcc)


# helpers -----------------------------------------------------------------


source("tool_EWMA_helpers.R")

t_data <- c(9.45,7.99,9.29,11.66,12.16,10.18,8.04,11.46,9.2,10.34,9.03,11.47,10.51,9.4,10.08,9.37,
10.62, 10.31, 8.52, 10.84, 10.9, 9.33, 12.29, 11.5, 10.6, 11.08, 10.38, 11.62, 11.31, 10.52)

demo_df <- data.frame(test = "demo", day = 1, id_day = 1:length(t_data), result = t_data) |> 
  tibble()

st_data <- paste0(t_data, collapse = " ")

st_mean = 10
st_sd = 1

lambdas <- seq(0.1, 0.9, by = 0.1)


# ui ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "EWMA Analyser"),
  
# dashboard side -----------------------------------------------------------
  dashboardSidebar(disable = FALSE,collapsed = FALSE,minified = FALSE,
                   sidebarMenu(id = "tabs",
                               fluidRow(column(12, hr())),
                               menuItem("Instructions", tabName = "mi_instructions"),
                               menuItem("Analyzer", tabName = "mi_analyzer")) ,
    fluidRow(column(12, hr())),
    fluidRow(column(12,helpText("Test data can be directly entered or uploaded by an Excel (xlsx) file."))),
    fluidRow(column(12,helpText("Template file is also avalaible."))),
    fluidRow(column(12, hr())),
    fluidRow(column(1,helpText(" ")), column(10,downloadButton(outputId = "btn_dl_template",label =  "  Download Template") )),
    fluidRow(column(12, hr())),
    actionButton(inputId = "btn_demo",label =  "Load Demo Data"),
    fluidRow(column(12, hr())),
    fluidRow(column(12,     fileInput("file1", "Upload Data (XLSX)", accept = c(".xlsx"), width = "50%"))),
    fluidRow(column(12, htmlOutput(outputId = "txt_file_err")))
    
    
  
    
  ),

# dashboard body -----------------------------------------------------------
  dashboardBody(
        tags$head(        
      #in line styling
      tags$style(HTML(
        
        #siderbar foreground
        '.skin-blue .main-sidebar {
          foreground-color: white;}',
        
        #siderbar text color
        '.skin-blue .main-sidebar .sidebar{
          color: #white;}'
      )
      )
    ),
  tabItems(
    tabItem("mi_instructions", 
            fluidRow(h1("Instructions")),
            fluidRow(column(12,helpText("Template file is avalaible as XLSX file on the side menu"))),
            fluidRow(column(12,helpText("Demo lab data is also avalaible on the side menu."))),
            fluidRow(column(12, hr())),
            helpText(   a("Click Here for the Github page",     href="https://github.com/ditopcu/ClinLabTool")),
    ),
    tabItem("mi_analyzer", 
            fluidRow(
              column(width = 6, 
                     textAreaInput("results", "Results for calculation", value = "", width = "100%", height = "200",
                                   placeholder = "Space between each results",resize = "none"),
                     actionButton(inputId = "btn_calculate",label =  "Calculate & Plot"),
                     ),
              column(width = 3, DTOutput("tbl_results",)),
            ),
            fluidRow(    
              column(width = 2,  selectInput("slt_test", "Test", choices = "test", width = "100%",
                selected = "test", multiple = FALSE, selectize = TRUE)),
              column(width = 2,  selectInput("slt_day", "Day", choices = 1, width = "100%",
                selected = 1, multiple = FALSE, selectize = TRUE)),
              column(width = 2,  selectInput("slt_lambda", "Smoothing (Lambda)", choices = lambdas, width = "100%",
                selected = 0.1, multiple = FALSE, selectize = TRUE)),
              column(width = 2, numericInput("num_mean", label = "Mean", width = "100%", value = 0)),
              column(width = 2, numericInput("num_sd", label = "SD", width = "100%", value = 0)),
            ),
            fluidRow(  
              plotOutput("plot_EWMA", width = "100%", height = "400px")
            )
       
    ) # end tab item mi_analyzer
  ) 
  )
)  # end dashboardPage



# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  rv <- reactiveValues(ewma_df_0 = NULL, ewma_mean = NULL, ewma_SD = NULL, ewma_lambda = NULL,
                       all_results_df = NULL, all_stats_df = NULL,
                       selected_test= NULL, selected_day= NULL)
  

# values and inputs -------------------------------------------------------

  observeEvent(input$slt_lambda, {
    
    rv$ewma_lambda <- as.numeric(input$slt_lambda) 
    
  })
 
  observeEvent(input$num_mean, {
    
    
    
    t <- input$num_mean
    if (is.na(t)) {
      updateNumericInput(session, "num_mean",value = rv$ewma_mean)
    } else {
      rv$ewma_mean <- t
    }
    
  })
  
  observeEvent(input$num_sd, {
    
     t <- input$num_sd
     if (is.na(t)) {
       updateNumericInput(session, "num_sd",value = rv$ewma_SD)
     } else {
       rv$ewma_SD <- t
     }
    
  })
  
  # TODO test selection


# file ops ----------------------------------------------------------------

  # Template File download
  output$btn_dl_template <- downloadHandler(    
    filename = "EWMA Template File.xlsx",
    content =  function(file){
      file.copy(here::here("EWMA Template File.xlsx"), file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  observeEvent(input$file1, {

    #print("file input")
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    if (ext != "xlsx") {
      
      output$txt_file_err <- renderText("   Wrong file type")
      
    }
    
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    # req(file)
   

    output$txt_file_err <- renderText("")
    
    temp <- suppressMessages(read_template_file(file$datapath))
    
    if (temp[["error_text"]] != "") {
      output$txt_file_err <- renderText(temp[["error_text"]])
      return(0)
    }
    
    selected_test <- 1
    selected_day <- 1
    
    
    t_all_results_df <- temp[["results_df"]]
    t_all_stats_df <- temp[["stats_df"]]
    
    
    rv$all_results_df <- t_all_results_df
    rv$all_stats_df <- t_all_stats_df
    

    
    updated_data <- updateTT(session, t_all_results_df, t_all_stats_df, selected_test, selected_day )

    
    rv$ewma_df_0 <- updated_data[["u_final_df"]]
    rv$ewma_mean <- updated_data[["u_mean"]]
    rv$ewma_SD <- updated_data[["u_SD" ]]
    updateTabItems(session, "tabs", selected = "mi_analyzer")
    

    
  })
    


# buttons -----------------------------------------------------------------
  
  
  observeEvent(input$slt_test,{
    
    req(rv$all_results_df) # test	day	id_day	result

    test_name <- input$slt_test
    rv$selected_test <- test_name
    
    selected_test_df <- rv$all_results_df |> 
      filter(test == input$slt_test)
    
    u_day_list <- unique(selected_test_df$day)
    
    updateSelectInput(session, inputId = "slt_day", choices = u_day_list,  u_day_list[1], label = "Day"  )

  })
  
  observeEvent(input$slt_day,{
    
    req(rv$all_results_df) # test	day	id_day	result

    
    selected_day <- as.integer(input$slt_day)
    rv$selected_day <- selected_day
    
    u_final_df <-  rv$all_results_df |> 
      filter(test == rv$selected_test, day == selected_day)
    
    
    u_stats_df <- rv$all_stats_df |> 
      filter(test == rv$selected_test) |>  
      filter(row_number() == 1)
    
    
    selected_mean <- u_stats_df$mean
    selected_SD <- u_stats_df$sd
    
    updateTextAreaInput(session, "results", value = paste(u_final_df$result, collapse = " ")) 
    
    updateNumericInput(session, "num_mean",value = selected_mean)
    updateNumericInput(session, "num_sd",value = selected_SD)
    
    
    rv$ewma_df_0 <- u_final_df

    
    rv$ewma_mean <- selected_mean
    rv$ewma_SD <- selected_SD
    
    
    
    
  })
  
  observeEvent(input$btn_demo, {
     
      demo_df <- data.frame(test = "Demo", day = 1, id_day = 1:length(t_data), result = t_data) |> 
        tibble()
      
      rv$all_results_df <- demo_df
      rv$ewma_df_0 <- demo_df
      rv$ewma_mean <- st_mean
      rv$ewma_SD <- st_sd
      
      test_name <- "Demo"
      rv$selected_test <- test_name
      
      isolate({
      updateSelectInput(session, inputId = "slt_test", choices = "Demo",selected = "Demo"  ,label = "Test" )
      })
      
      updateSelectInput(session, inputId = "slt_day", choices = 1,  1, label = "Day"  )
      
      updateTextAreaInput(session, "results", value = st_data) 
      updateNumericInput(session, "num_mean",value = st_mean)
      updateNumericInput(session, "num_sd",value = st_sd)
      
      updateTabItems(session, "tabs", selected = "mi_analyzer")
  })
  

# plot --------------------------------------------------------------------

  
  observeEvent(input$btn_calculate, {
    
        req(input$results)

        tmp <-   input$results
        tt <- str_split(string = tmp, pattern = "[[:space:]]",simplify = TRUE) |> 
          as.double()
        
        new_string <- tt[!is.na(tt)]
        
        req(new_string)
        

        
        if(paste0(new_string, collapse = " ") !=  paste0(rv$ewma_df_0$result, collapse = " ")) {
          df <- data.frame(test = "Manual", day = 1, id_day = 1:length(new_string), result = new_string) |> 
          tibble()
          rv$ewma_df_0 <- df
        }

        req(rv$ewma_df_0$result)




        isolate({
          
          print((rv$ewma_SD))
          

        
          output$plot_EWMA <- renderPlot({

              ewma_plot( rv$ewma_df_0$result, rv$ewma_lambda, rv$ewma_mean, rv$ewma_SD)
        })
            
        }) 
            
    
})
  
  output$tbl_results <- renderDT({

    req(rv$ewma_df_0 )

    # print("renderDT" )
    
    rv$ewma_df_0 


  },options = list(dom = "tp", pageLength =  5  ))


}


# functions ---------------------------------------------------------------

updateTT <- function(session, all_results_df, all_stats_df, selected_test, selected_day  ) {
  
  u_test_list <- unique(all_results_df$test)

  
  u_results_df <- all_results_df |> 
    filter(test == u_test_list[selected_test])
  

  
  u_day_list <- unique(u_results_df$day)
  
  print(u_day_list)
  print(all_stats_df)
  
  
  u_stats_df <- all_stats_df |> 
    filter(test == u_test_list[selected_test]) |> 
    filter(row_number() == 1)
  
  
  selected_mean <- u_stats_df$mean
  selected_SD <- u_stats_df$sd
  

  
  
  u_final_df <-  u_results_df |> 
    filter(day == u_day_list[selected_day])
  
  updateSelectInput(session, inputId = "slt_test", choices = u_test_list,  u_test_list[selected_test], label = "Test" )
  updateSelectInput(session, inputId = "slt_day", choices = u_day_list,  u_day_list[selected_day], label = "Day" )
  updateTextAreaInput(session, "results", value = paste(u_final_df$result, collapse = " ")) 
  
  updateNumericInput(session, "num_mean",value = selected_mean)
  updateNumericInput(session, "num_sd",value = selected_SD)
  
  
  list(u_final_df = u_final_df, u_test_list = u_test_list, u_day_list = u_day_list, 
       u_mean = selected_mean, u_SD = selected_SD)
  
}







# shiny -------------------------------------------------------------------


shinyApp(ui, server)















# old ---------------------------------------------------------------------

# updateTT_old <- function(session, all_results_df, selected_test, selected_day, selected_mean, selected_SD ) {
#   
#   u_test_list <- unique(all_results_df$test)
#   
#   u_results_df <- all_results_df |> 
#     filter(test == u_test_list[selected_test])
#   
#   
#   u_day_list <- unique(u_results_df$day)
#   
#   
#   
#   u_final_df <-  u_results_df |> 
#     filter(day == u_day_list[selected_day])
#   
#   updateSelectInput(session, inputId = "slt_test", choices = u_test_list,  u_test_list[selected_test] )
#   updateSelectInput(session, inputId = "slt_day", choices = u_day_list,  u_day_list[selected_day] )
#   updateTextAreaInput(session, "results", value = paste(u_final_df$result, collapse = " ")) 
#   
#   updateNumericInput(session, "num_mean",value = selected_mean)
#   updateNumericInput(session, "num_sd",value = selected_SD)
#   
#   
#   list(u_final_df = u_final_df, u_test_list = u_test_list, u_day_list = u_day_list, 
#        u_mean = selected_mean, u_SD = selected_SD)
#   
# }

# reactive_mean <- reactive({
#   
#   print("denememeememe")
#   
#   req(input$num_mean)
#   
#   rv$ewma_mean <- input$num_mean
#   
# })

# reactive_sd <- reactive({
#   
#   req(input$num_sd)
#   
#   input$num_sd
#   
# })

# reactive_lambda <- reactive({
#   
#   req(input$slt_lambda)
#   
#   isolate({
#     as.numeric(input$slt_lambda) 
#   })

# })



# output$out_mean <- renderText({
#   
#   req(input$btn_demo)
#   
#   isolate({
#     
#     updateTextAreaInput(session, "results", value = st_data)
#     
#     req(ewma_results())
#     
#     
#     ooo <- ewma_results()
#     
#     output$plot_EWMA <- renderPlot({
#       
#       ewma_plot(ooo)
#       
#     })
#     
#     ooo
#     
#   })
#   
#   
# })




