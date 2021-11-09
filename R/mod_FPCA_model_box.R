#' FPCA_model_box UI Function
#'
#' @description Functional Principal Component Analysis box that contains select inputs 
#' and a tabBox with various plots  
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags fluidRow selectizeInput tabPanel plotOutput numericInput radioButtons checkboxGroupInput
#' @importFrom bs4Dash box column infoBoxOutput tabBox actionButton infoBox 
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' 

#globalVariables(c("HTML","ID","value","variable","x","y")) 
mod_FPCA_model_box_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = id,
             fluidRow(
               box(width = 12,
                   title = "Functional Principal Component Analysis",
                       fluidRow(
                   column(3,
                          selectizeInput(ns("pred_col"),
                                         "Select Predictor Function",
                                         choices = c()),
                          infoBoxOutput(ns("perc_sparse"), width = NULL),
                          tags$br(),
                          downloadButton(ns("download_model"), label = "Download Model"),
                          actionButton(ns("delete_model"), "Remove Model")),
                   column(9,
                          tabBox(id = ns("box_tabs"),
                                 width = NULL,
                                 collapsible = FALSE,
                                 maximizable = TRUE,
                                 
                                 tabPanel("Fitted vs. Observed",
                                          fluidRow(
                                            column(2,
                                          tags$br(),
                                          selectizeInput(ns("subj_select"),
                                                         "Select Subject ID",
                                                         choices = c())),
                                          column(10,
                                          plotlyOutput(ns("fpca_plot"), height = "261px")))),
                                 
                                 tabPanel("Modes of Variation",
                                          #numericInput(ns("mov_k_modes"), "Select the k-th Mode of Variation", 1,1,1,1)),
                                          
                                          
                                          fluidRow(
                                          column(2,
                                          tags$br(),
                                           radioButtons(ns("mov_k_modes"),
                                                        "Select the k-th Mode of Variation", c(1),width = "100%")),
                                          

                                          column(10,
                                          plotOutput(ns("mov_plot"), height = "261px")))
                                 ),

                                 tabPanel("Mean Curve",
                                          plotlyOutput(ns("mean_plot"), height = "261px")
                                 ),
                                 tabPanel("Eigenfunctions",
                                          #selectInput(ns("select_ef"),"Select Eigenfunction",c(), multiple = TRUE,selected = 1),
                                          fluidRow(
                                            column(2,
                                            tags$br(),
                                          checkboxGroupInput(ns("select_ef"),"Select Eigenfunction", choices = c(), selected = 1)),
                                          column(10,
                                          plotlyOutput(ns("ef_plot"), height = "261px"))),
                                 ),
                                 tabPanel("FPC",
                                          fluidRow(
                                          column(2,
                                          numericInput(ns("select_fpc_1"), "Select X-axis Eigenfunction", 1,1,1,1),
                                          numericInput(ns("select_fpc_2"), "Select Y-axis Eigenfunction", 1,1,1,1)),
                                          column(10,
                                          plotlyOutput(ns("fpc_plot"), height = "261px")))
                                 ),
                                 tabPanel("Fitted Curves",
                                          fluidRow(
                                          column(2,
                                          tags$br(),
                                                   selectizeInput(ns("subj_select_multiple"),
                                                         "Select Subject ID",
                                                         multiple = TRUE,
                                                         choices = c())),
                                          column(10,
                                    plotlyOutput(ns("fitted_curves_plot"))))
                                 ),
                                 tabPanel("FVE",
                                          DTOutput(ns("fve_table"))
                                 )
                                   
                                 )
                   )
                       )

               )
               
               
               
             )
    )
  )
}

#' FPCA_model_box Server Functions
#'
#' @noRd 
#' @importFrom shiny moduleServer observeEvent removeUI req updateSelectizeInput reactive updateRadioButtons validate need renderPlot renderUI updateCheckboxGroupInput updateNumericInput renderText updateTabsetPanel withProgress incProgress
#' @importFrom magrittr extract2 multiply_by
#' @importFrom bs4Dash renderInfoBox updateTabItems 
#' @import fdapace
#' @import ggplot2
#' @importFrom purrr map map2  
#' @importFrom stringr str_to_title
#' @importFrom plotly renderPlotly ggplotly config event_data event_register
#' @importFrom DT renderDT
#' @importFrom reshape2 melt
#' @importFrom dplyr case_when
#' @importFrom utils globalVariables
#' @importFrom tidyr drop_na
ID <- NULL
variable <- NULL
x <- NULL
y <- NULL
mod_FPCA_model_box_server <- function(id, raw_data, id_col, time_col, dark_mode){
  # stopifnot(is.reactive(data))
  # stopifnot(is.reactive(id_col))
  # stopifnot(is.reactive(time_col))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(c(dark_mode(),time_col(),id_col(),input$subj_select_multiple), {
      runjs('console.log(document.querySelectorAll("[class^=item]"))')
      
      if(dark_mode() == TRUE){
        
        delay(1,
              runjs(
                '

          for (const node of document.querySelectorAll("[class^=item]")) {
                console.log(node.nextSibling.className.includes("item") || node.nextSibling.id.includes("subj_select_multiple-selectized"))
                console.log(node.nextSibling)
                if(node.nextSibling.id.includes("subj_select_multiple-selectized") || node.nextSibling.className.includes("item")){ 
                    node.className = "item-multiple-dark";
                }else{
                continue; 
                  }
          };
          
          '
              )
        )
        
        
        
      }
      else{
        runjs(
          '
           console.log("light mode")
           console.log(document.getElementsByClassName("item-multiple-dark"))
           const dark_nodes = Array.from(document.getElementsByClassName("item-multiple-dark"))
            
           for (const node of dark_nodes) {
           console.log(node)
                  node.className = "item-multiple-light";
                  
          }'
        )

      }
    },ignoreInit = TRUE)
    
  #utils::globalVariables(c("x", "variable", "y","value"), add = FALSE)  
    
  observeEvent(input$delete_model,{
      removeUI(selector = sprintf('#%s', id))
    })   
    
    
    # Initialize prediction column only runs once 
    observeEvent(input$pred_col,{
      
      req(raw_data())
      updateSelectizeInput(session = session,
                           inputId = "pred_col", 
                           choices = new_pred_col())
      
      
      updateSelectizeInput(session = session,
                           inputId = "subj_select", 
                           choices = new_subj_select_col())
    },once = TRUE)
    
    raw_data_cols <- reactive({
      raw_data() %>% colnames 
    }) 
    
    cols_in_use <- reactive({
      c(id_col(),time_col())
    }) 
    
    new_subj_select_col <- reactive({
      raw_data() %>% extract2(id_col()) %>% unique
    })
    
    new_pred_col <- reactive({
      raw_data_cols()[! raw_data_cols() %in% cols_in_use()] 
    }) 
    # Update Prediction Select Input after initialization
    observeEvent(c(id_col(), time_col(),raw_data()),{
      req(raw_data())
      if(length(input$pred_col) != 0){
        if(input$pred_col != id_col() & input$pred_col != time_col()){
          
          updateSelectizeInput(session = session,
                               inputId = "pred_col", 
                               selected = input$pred_col,
                               choices = new_pred_col())  
        }
        else{
          updateSelectizeInput(session = session,
                               inputId = "pred_col", 
                               choices = new_pred_col())   
        }
      }
      
    })
    
    # Update Subject ID Select Input after initialization
    observeEvent(id_col(),{
      
      req(raw_data())
      updateSelectizeInput(session = session,
                           inputId = "subj_select", 
                           choices = new_subj_select_col())
    })
    
    
    
    # Transform raw data into fpca list see MakeFPCAInputs in fdapace ####
    fpca_list <- reactive({
      req(raw_data())
       req(input$pred_col) 
        #print(paste0("raw data: ",raw_data() %>% head))
        
        id_col_data <- raw_data() %>% extract2(id_col())
        time_col_data <- raw_data() %>% extract2(time_col())
        pred_col_data <- raw_data() %>% extract2(input$pred_col)
        
        
        # list with LID Ly Lt for each individual subject
        
        fpca_list <- MakeFPCAInputs(id_col_data, time_col_data, pred_col_data)
        time_sorted_index <- fpca_list$Lt %>%  
          map(~ .x %>% sort(index.return = TRUE) %>% 
                extract2("ix"))  
        
        fpca_list$Lt %<>% map(~ .x %>% sort) 
        
        fpca_list$Ly %<>% map2(time_sorted_index, ~ .x[.y]) 
        
        return(fpca_list)
    })
    
    try_fpca_obj <- reactive({
      tryCatch( FPCA(fpca_list()$Ly, fpca_list()$Lt),
                error = function(e){
                  e
                }
      )
    })
    
    
    
    error_message <- reactive({
      if(is.null(try_fpca_obj()$message)){
    error_message <- ""
  }else{
    error_message <- try_fpca_obj()$message 
  }
    }) 
    
    # Create FPCA Object ####
    fpca_obj <- reactive({
      # Extract Ly and Lt from fpca_list()
      # Should determine if the data is sparse or dense automatically
      withProgress(message = "Creating FPCA Object",{
      fpca_obj <- FPCA(fpca_list()$Ly, fpca_list()$Lt) 
      incProgress(message = "FPCA Object Completed")
      })
      return(fpca_obj) 
    })
    
    # Fitted Object ####
    fitted_obj <- reactive({
      
      # Sparse Case
      if(IsRegular(fpca_list()$Lt) == "Sparse"){
        #  A list with ciUpper, ciLower, fitted, and WorkGrid
        fitted_obj <- fpca_obj() %>% 
          fitted(ciOptns = (list(p = 0, 
                                 alpha = 0.05)))
      }
      
      # Dense Case
      else{
        # A Normal Matrix 
        fitted_obj <- fpca_obj() %>% fitted 
      }
    })
    
    # Get ID Index ####
    id_index <- reactive({
      # When Subject ID is a string 
      if(input$subj_select %>% as.integer %>% is.na){
        
        # Figure out which index integer corresponds to the ID string
        id_index <- which(fpca_list()$Lid %>% unlist %in% input$subj_select) 
        
      }
      
      # When ID is an integer
      else{
        # Convert string to integer
        id_index <- input$subj_select %>% as.integer
      }
    }) 
    
    subj_fitted_obj <- reactive({
        # Extract Fitted Curve from the matrix fitted_obj()
      
      if(IsRegular(fpca_list()$Lt) == "Sparse"){
        fitted_obj()$fitted[id_index(),] 
      }else{
        fitted_obj()[id_index(),]
      }
      
      
    })
    
    # Extract Subject Observed Data ####
    subj_obs_data <- reactive({
      # Returns Atomic Vector
      fpca_list() %>% extract2("Ly") %>% extract2(input$subj_select) 
    })
    
    # Extract Subject Time Vector ####
    subj_time_vec <- reactive({
      # Returns Atomic Vector
      fpca_list() %>% extract2("Lt") %>% extract2(input$subj_select) 
    })
    
    full_time_vec <- reactive({
      fpca_obj()$workGrid
    })
    
    
    observeEvent(c(input$pred_col,id_col()),{
     updateSelectizeInput(inputId = "subj_select_multiple", choices = raw_data() %>% extract2(id_col()) %>% unique) 
   }) 
    
     fitted_df <- reactive({   
       
     
      if(IsRegular(fpca_list()$Lt) == "Sparse"){
        fitted_matrix <- fitted_obj()$fitted 
      }else{
        fitted_matrix <- fitted_obj()
      }
        
      fitted_df <- fitted_matrix %>% t %>% as.data.frame 
      colnames(fitted_df) <- fpca_list()$Lid %>% unlist 
      fitted_df$time <- full_time_vec() 
       fitted_df %<>% melt(variable = "ID", id.vars = "time")
       
       
         color_logic <- fitted_df$ID %in% input$subj_select_multiple 
     for(val in 1:length(color_logic)){
      if(color_logic[val] == TRUE){
        color_logic[val] <- "#007bff"
      }else{
        color_logic[val] <- "gray"
      }
     }
         fitted_df$color  <- color_logic
         return(fitted_df)
     })
    
   # Fitted Curves Plot #### 
    output$fitted_curves_plot <- renderPlotly({
        validate(
        need(error_message() == "", "")
      )
     
       fitted_plot <- ggplot()+
         geom_line(data = fitted_df(),aes(x = time, y = value, group = ID ),
                                         color = fitted_df()$color)+
         xlab(time_col())+
         ylab(input$pred_col)
       
       if(dark_mode() == TRUE){
        fitted_plot <- fitted_plot + theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          plot.title = element_text(color = "white"),
                                          legend.text = element_text(color = "white"),
                                          legend.background = element_rect(fill = "#3f474e"),
                                          panel.background = element_rect(colour = "#6c757d")
                                          )
      }else{
        fitted_plot <- fitted_plot + theme_light()+theme(plot.background = element_rect(fill = "white"))
      }
       fitted_plot %<>% ggplotly%>% config(displayModeBar = FALSE)
    })
    
    
    
      # Output FPCA Plot ####
    output$fpca_plot <- renderPlotly({
      req(raw_data())
      
      validate(
        need(error_message() == "", "")
      )
      
     
      
      # Sparse Case: 
      # Iteratively add points to ggplot.
      # Then add fitted curve and CI 
      # validate(
      #   need(error_message() == "", "")
      # )
      if(IsRegular(fpca_list()$Lt) == "Sparse"){
        plot <- ggplot()
        
        # Iteratively add points to ggplot
        
        
        
        
        # Add fitted curve
        # Add confidence bands
        
        plot <- plot + geom_line(aes(full_time_vec(), 
                                     subj_fitted_obj(), 
                                     color = "Fitted"))+
          geom_ribbon(aes(x = full_time_vec(),  
                          ymax = fitted_obj()$cvgUpper[id_index(),],
                          ymin = fitted_obj()$cvgLower[id_index(),]), 
                      alpha = 0.2)+
          labs(color = "",
               title = paste0("Fitted vs. Observed for"," ",str_to_title(input$subj_select)))+
          ylab(input$pred_col)+
          xlab(time_col())+
          scale_color_manual(values = c("#007bff","black"))+
          theme(text = element_text(size=12))+
          theme(legend.key.size = unit(1, 'cm'))+
          theme(plot.title = element_text(hjust = 0.5))
        
        plot <- plot + map2(subj_time_vec(),
                            subj_obs_data(),
                            ~ geom_point(aes(.x,.y, color = "Observed"))) 
        
      }
      else{
        plot <- ggplot()+ geom_line(aes(full_time_vec(), 
                                        subj_obs_data(), 
                                        color = "Observed"))+
          geom_line(aes(full_time_vec(), 
                        subj_fitted_obj(), 
                        color = "Fitted"))+
          labs(color = "",
               title = paste0("Fitted vs. Observed for"," ",str_to_title(input$subj_select)))+
          ylab(input$pred_col)+
          xlab(time_col())+
          theme(text = element_text(size=12))+
          theme(legend.key.size = unit(1, 'cm'))+
          theme(plot.title = element_text(hjust = 0.5))+
          scale_color_manual(values = c("#007bff","black"))
        
      }
      
      if(dark_mode() == TRUE){
        plot <- plot + theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          plot.title = element_text(color = "white"),
                                          legend.text = element_text(color = "white"),
                                          legend.background = element_rect(fill = "#3f474e"),
                                          panel.background = element_rect(colour = "#6c757d")
                                          )
      }else{
        plot <- plot + theme_light()+theme(plot.background = element_rect(fill = "white"))
      }
      
      
      
      plot %>% ggplotly(tooltip = NULL) %>% config(displayModeBar = FALSE)
      
      
    })
    
    # Sparsity ValueBox #### 
    output$perc_sparse <- renderInfoBox({
      # Sparse Case
      req(raw_data())
      req(input$pred_col)
      validate(
        need(error_message() != "FPCA is aborted because 'y' members are not all of type double or integer! Try  \"lapply(y,function(x) typeof(x))\" to see the current types \n",
             "Predictor function doesn't contain numbers") %then%
          need(error_message() == "", "Something went wrong with FPCA check the columns selected")
      )
      # fraction <- subj_time_vec() %>% length / full_time_vec() %>% length 
      # percentage <- fraction %>% multiply_by(100) %>% round(digits = 2) %>% as.character 
      
      min_time <- fpca_list()$Lt %>% unlist %>% unique %>% min 
      max_time <- fpca_list()$Lt %>% unlist %>% unique %>% max 
      full_time <- seq(min_time,  max_time) %>% length 
      
      
      percentage <-fpca_list() %>% 
        extract2("Lt") %>% 
        extract2(input$subj_select) %>% 
        length %>% divide_by(full_time) %>% 
        multiply_by(100) %>% 
        round(2) 
      
      infoBox(
        title = "",
        value = paste0(percentage,"%"),
        subtitle = "Percent of Data Given",
        icon = icon("thumbs-up", lib = "glyphicon"),
        fill = TRUE,
        width = 6,
        color = "primary"
        
      )
      
    })
    
    
   
    
    # Modes of Variation #### 
    
    observe({
      validate(
        need(error_message() == "", "")
      )
      #updateNumericInput(session = session, "mov_k_modes",max = fpca_obj()$lambda %>% length)
      updateRadioButtons(session = session, "mov_k_modes",choices = 1:(fpca_obj()$lambda %>% length))
    })
    
    output$mov_plot <- renderPlot({
      validate(
        need(error_message() == "", "")
      )
      title <- paste0(input$mov_k_modes,
                      case_when(input$mov_k_modes == "1" ~ "st ", 
                                input$mov_k_modes == "2" ~ "nd ", 
                                input$mov_k_modes == "3" ~ "rd ", 
                                TRUE ~ "th "), 
                      "Mode of Variation")
      
      oldpar <- par(no.readonly = TRUE)    
      on.exit(par(oldpar))            
      
      
      par(bg = "#00000000")
      if(dark_mode() == TRUE){
      CreateModeOfVarPlot(fpca_obj(), 
                          input$mov_k_modes %>% as.integer, 
                          xlab=time_col(), 
                          ylab='', 
                          main = title,
                          col.lab = "white",
                          col.axis = "white",
                          col.main = "white")
      }else{
       CreateModeOfVarPlot(fpca_obj(), 
                          input$mov_k_modes %>% as.integer, 
                          xlab=time_col(), 
                          ylab='', 
                          main = title)
      }
    })
    
    # Mean Curve ####
    output$mean_plot <- renderPlotly({
     validate(
        need(error_message() == "", "")
      )
      mean_curve <- GetMeanCurve(fpca_list()$Ly,
                                 fpca_list()$Lt)$mu
      
      mean_plot <-  ggplot() + 
        geom_line(aes(x = full_time_vec(),
                      y = mean_curve),color = "#007bff") + 
        xlab(time_col())+ 
        ylab(paste0(input$pred_col," ", "Mean"))
      
      
      if(dark_mode() == TRUE){
        mean_plot <- mean_plot + theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          plot.title = element_text(color = "white"),
                                          panel.background = element_rect(colour = "#6c757d")
                                          )
      }else{
        mean_plot <- mean_plot + theme_light()+theme(plot.background = element_rect(fill = "white"))
      }
      
      mean_plot %>% ggplotly(tooltip = NULL) %>% config(displayModeBar = FALSE)
      
    })  
    
    # Eigenfunctions ####
    observe({
      validate(
        need(error_message() == "", "")
      )
      ef_cols <- fpca_obj()$phi %>% ncol
      #updateSelectInput(session = session, "select_ef",selected = 1,choices = 1:ef_cols)
      updateCheckboxGroupInput(session = session, "select_ef",selected = 1,choices = 1:ef_cols)
    })
    
    
    output$ef_plot <- renderPlotly({
     # Wrong eigenfunctions are being selected
      validate(
        need(error_message() == "", "")
      )

      validate(
        need(length(input$select_ef) != 0,"Please Select an Eigenfunction")
      )
      
      ef_num <- input$select_ef %>% as.integer
      
      ef_matrix <- c() 
      for(num in ef_num){
        ef_matrix %<>% 
          cbind(matrix(fpca_obj()$phi[,num], ncol = 1, dimnames = list(NULL, num))) #%>% 
        #cbind(matrix(rep(num, full_time_vec() %>% max), ncol = 1, dimnames = list(NULL, "ef")))
      }
      
      ef_matrix %<>% cbind(matrix(full_time_vec(), ncol = 1, dimnames = list(NULL, "time"))) 
      
      ef_df <- ef_matrix %>% as.data.frame 
      ef_df %<>%  melt(
        id.vars = "time"
      ) 
      
      ef_plot <- ggplot() + geom_line(data = ef_df, aes(x = time, y = value, linetype = variable),color = "#007bff")+
        xlab(time_col())+
        ylab("Eigenfunctions")+
        labs(linetype = "")
      
      if(dark_mode() == TRUE){
        ef_plot <- ef_plot + theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          plot.title = element_text(color = "white"),
                                          legend.text = element_text(color = "white"),
                                          legend.background = element_rect(fill = "#3f474e"),
                                          panel.background = element_rect(colour = "#6c757d")
                                          )
      }else{
        ef_plot <- ef_plot + theme_light()+theme(plot.background = element_rect(fill = "white"))
          
      }
      ef_plot %<>%  ggplotly(tooltip = NULL) %>% config(displayModeBar = FALSE)
      ef_plot
    })
    
    # FPC Scores #### 
    observe({
      validate(
        need(error_message() == "", "")
      )
      max_fpc <- fpca_obj()$lambda %>% length
      
      updateNumericInput(session = session, "select_fpc_1",max = max_fpc ,value = 1)
      updateNumericInput(session = session, "select_fpc_2",max = max_fpc,value = 1)
    })
    
    fpc <- reactive({
      predict(fpca_obj(), fpca_list()$Ly,fpca_list()$Lt)$scores
    })
    
    
   fpc_df <- reactive({
     
      fpc_df <- data.frame(x = fpc()[,input$select_fpc_1 %>% as.integer],
                           y = fpc()[,input$select_fpc_2 %>% as.integer],
                           ID = fpca_list()$Lid %>% unlist)
      return(fpc_df)
   }) 
    
    
    
    output$fpc_plot <- renderPlotly({
      validate(
        need(error_message() == "", "")
      ) 
    
      fpc_plot <- ggplot()+
        geom_point(data = fpc_df(), aes(x,y,customdata = ID),color = "#007bff")+
        xlab(paste0("FPC for the ", input$select_fpc_1,
                    case_when(input$select_fpc_1 == 1 ~ "st ",
                              input$select_fpc_1 == 2 ~ "nd ",
                              input$select_fpc_1 == 3 ~ "rd ",
                              TRUE ~ "th "),
                    "Eigenfunction"))+
        ylab(paste0("FPC for the ", input$select_fpc_2, 
                    case_when(input$select_fpc_2 == 1 ~ "st ",
                              input$select_fpc_2 == 2 ~ "nd ",
                              input$select_fpc_2 == 3 ~ "rd ",
                              TRUE ~ "th "),
                    "Eigenfunction"))
      if(dark_mode() == TRUE){
        
        fpc_plot <- fpc_plot+ theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          plot.title = element_text(color = "white"),
                                          panel.background = element_rect(colour = "#6c757d")
                                          )
      }else{
        fpc_plot <- fpc_plot + theme_light()+
          theme(plot.background = element_rect(fill = "white"))
      }
      
      
      
      fpc_plot %<>% ggplotly()%>% event_register("plotly_selecting")
      fpc_plot
    })
    
    observeEvent(c(dark_mode(),input$box_tabs), {
      req(input$box_tabs)
      if(dark_mode() == TRUE & input$box_tabs == "FVE"){
        
        runjs(
          '
          document.querySelectorAll("[class*=odd]")[0].className = "odd-dark";
          document.querySelectorAll("[class*=datatables]")[0].style.color = "white";
          
        '
        )
        
}else{
        
        runjs(
          '
          document.querySelectorAll("[class*=odd]")[0].className = "odd-light";
          document.querySelectorAll("[class*=datatables]")[0].style.color = "black";
          
          
          
          
          '
        )
        
      }
    })
    
    
   
    
    
    observeEvent(event_data("plotly_selected"),{
      if(length(event_data("plotly_selected")) != 0){
      updateSelectizeInput(inputId = "subj_select_multiple", 
                           selected = event_data("plotly_selected")$customdata)
      updateTabItems(inputId = "box_tabs", selected = "Fitted Curves")  
      }
    })
    
    # FVE ####
    output$fve_table <- renderDT(options = list(dom = "t",ordering = FALSE,selection = "none"),{
      validate(
        need(error_message() == "", "")
      ) 
      FVE <- sapply(1:(fpca_obj()$cumFVE %>% length),function(x){if(x != 1){fpca_obj()$cumFVE[x] - fpca_obj()$cumFVE[x-1]}else{fpca_obj()$cumFVE[x]}})
      data.frame(Eigenfunction = fpca_obj()$cumFVE %>% length %>% seq, cumFVE = fpca_obj()$cumFVE %>% round(3),FVE = FVE %>% round(3))
    })
    
    
  output$download_model <- downloadHandler(
  filename = "FPCA_model.Rds",
  content = function(file){
    saveRDS(fpca_obj(), file)
  }
  
)
    
    
  })
}

## To be copied in the UI
# mod_FPCA_model_box_ui("FPCA_model_box_ui_1")

## To be copied in the server
# mod_FPCA_model_box_server("FPCA_model_box_ui_1")
