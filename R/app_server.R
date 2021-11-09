#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @importFrom shinyWidgets show_alert
#' @importFrom graphics par
#' @importFrom stats fitted predict time 
#' @importFrom shiny observeEvent reactive observe insertUI 
#' @importFrom utils globalVariables
#' 
#' @noRd
#' 
#globalVariables(c("HTML","ID","value","variable","x","y"), add = FALSE) 

app_server <- function( input, output, session ) {
  
  # if(length(golem::get_golem_options("fpca_obj")) != 0){
  #   mod_FPCA_server("FPCA_ui_1", golem::get_golem_options("fpca_obj"))
  # }
  #  if(length(golem::get_golem_options("fpca_obj")) != 0){
  #   mod_FPCA_server("FPCA_ui_1", golem::get_golem_options("fpca_obj"))
  # } 

  #globalVariables(c("HTML","ID","value","variable","x","y"))
  
  
  init_user_select <- mod_global_options_server("global_options_ui_1")
  
  
  # FPCA ####
  id_fpca <- reactive({
    #print("id reactive")
    i <- sprintf('%04d', input$add_fpca_model)
    sprintf("FPCA_model_box_ui_1%s", i)
  })
  
  
  
  observeEvent(input$add_fpca_model, {
    if(is.null(init_user_select$raw_data()) == FALSE){
      insertUI(
        selector = '#add_fpca_model',
        where = "beforeBegin",
        ui = mod_FPCA_model_box_ui(id_fpca())
      )
    }
    
    else{
      
      
      show_alert(
      title = "Attention",
      text = "Please Enter Functional Data :)",
      type = "error",
      width = "50%"
    )
      
    }
    
  })
  
  observe({
    #req(init_user_select$raw_data())
    
    if(is.null(init_user_select$raw_data()) == FALSE){
      if(input$add_fpca_model != 0){
        mod_FPCA_model_box_server(id_fpca(), 
                                  init_user_select$raw_data, 
                                  init_user_select$id_col, 
                                  init_user_select$time_col,
                                  reactive(input$dark_mode))
      }
    }
  })
  
  
  
  # FCR #### 
  
  id_fcr <- reactive({
    i <- sprintf('%04d', input$add_fcr_model)
    sprintf("FCR_model_box_ui_1%s", i)
  })
  
  
  observeEvent(input$add_fcr_model, {
    if(is.null(init_user_select$raw_data())  == FALSE){
      insertUI(
        selector = '#add_fcr_model',
        where = "beforeBegin",
        ui = mod_FCR_model_box_ui(id_fcr())
      )
      
      removeUI(
        selector = "#add_fpca_model"
      )
      
      insertUI(
        selector = "#add_fcr_model",
        where = "beforeBegin",
        ui = actionButton("add_fpca_model", "Add FPCA Model", icon = icon("plus"))
      )
    }else{
       show_alert(
      title = "Attention",
      text = "Please Enter Functional Data :)",
      type = "error",
      width = "50%"
    )
     
      
    }
    
  })
  
  observe({
    #req(init_user_select$raw_data())
    if(is.null(init_user_select$raw_data()) == FALSE){
      if(input$add_fcr_model != 0){
        mod_FCR_model_box_server(id_fcr(), 
                                 init_user_select$raw_data, 
                                 init_user_select$id_col, 
                                 init_user_select$time_col,
                                 reactive(input$dark_mode))
      }
    }
  })
  
  
}
