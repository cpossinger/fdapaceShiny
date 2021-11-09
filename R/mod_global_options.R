#' global_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fileInput selectizeInput
#' 

#globalVariables(c("HTML","ID","value","variable","x","y"),add = FALSE) 
mod_global_options_ui <- function(id){
  ns <- NS(id)
  tagList(
      #tags$head(tags$style(".progress-bar{background-color:#00a65a;}")),
      #column(4,
      fileInput(ns("raw_data"), accept = c(".csv", ".rda",".RData"), 
                "Upload Functional Data"),
      #textOutput(ns("text_init"),inline = TRUE), #),
      #column(4,
      selectizeInput(ns("id_col"), "Select ID Column", c()), #),
      #column(4,
      selectizeInput(ns("time_col"), "Select Time Column", c()), #),
  )
}
    
#' global_options Server Functions
#'
#' @noRd 
#' @importFrom tools file_ext
#' @importFrom utils read.csv globalVariables
#' @import magrittr 
#' @importFrom stringr str_remove
#' @importFrom shiny reactive observeEvent req updateSelectizeInput
mod_global_options_server <- function(id){ 
    moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
   #globalVariables(c("HTML","ID","value","variable","x","y")) 
    
    # Upload Raw Data and Save to Reactive Variable
    raw_data <- reactive({
      # File must exist
      
      # File Object
      file <- input$raw_data
      # Temp File Path
      path <- file$datapath
      # File Extension
      ext <- file_ext(file$name)
      
      # Make sure extension is of an excepted type
      
      # Save RData and rda files
      # e <- new.env()
      # switch(ext,
      #       csv = read.csv(path),
      #       .RData = e[[load(path, envir = e)]],
      #       .rda = e[[load(path, envir = e)]],
      #       validate("Invalid file; Please upload a .csv or .RData/.rda")
      #        ) 
      
      
      
      if((ext %>% length) != 0){
        
      
      if(ext == "RData" | ext == "rda" | ext == "Rda"){
        e <- new.env()
        name <- load(path, envir = e)
        data <- e[[name]]
      }
      else if(ext == "csv"){
        data <- read.csv(path)
      }
        
      for(col in (data %>% colnames)){
        if((data[[col]] %>% is.vector) == FALSE){
          data[[col]] %<>% as.vector
        }else{
          next
        }
      }

     return(data)
      }else{
        
        return(NULL)
        
      }
      
       
    })
    
    raw_data_cols <- reactive({
      raw_data() %>% colnames 
    })
    
    observeEvent(raw_data(),{
      req(raw_data())
      
     req(input$raw_data)
    updateSelectizeInput(session, "time_col",selected = raw_data_cols()[2], choices = raw_data_cols())
    updateSelectizeInput(session, "id_col", choices = raw_data_cols()) 
    })
    
    observeEvent(input$id_col, {
      
      req(raw_data())
     req(input$raw_data)
    new_time_col <- raw_data_cols()[! raw_data_cols() %in% input$id_col] 
    updateSelectizeInput(session, "time_col",selected = input$time_col, choices = new_time_col)
    },ignoreInit = TRUE)
    
     observeEvent(input$time_col,{
       
      req(raw_data())
     req(input$raw_data)
      new_id_col <- raw_data_cols()[! raw_data_cols() %in% input$time_col] 
    updateSelectizeInput(session, "id_col",selected = input$id_col, choices = new_id_col ) 
    },ignoreInit = TRUE)
     
     output$text_init <- renderText({
       if(input$raw_data %>% is.null){
         "Please Enter Functional Data"
       }else{
         ""
       }
     })
    
   list(
     raw_data = reactive(raw_data()),
     time_col = reactive(input$time_col),
     id_col = reactive(input$id_col)
   )
    
  })
}
    
## To be copied in the UI
# mod_global_options_ui("global_options_ui_1")
    
## To be copied in the server
# mod_global_options_server("global_options_ui_1")
