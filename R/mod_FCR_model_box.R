#' FCR_model_box UI Function
#'
#' @description Box for Functional Concurrent Regression, 
#' includes model building select inputs and model effects plot  
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags fluidRow selectizeInput numericInput downloadButton uiOutput withProgress incProgress    
#' @importFrom bs4Dash actionButton
#' @importFrom bs4Dash box column 


mod_FCR_model_box_ui <- function(id){
HTML <- NULL
  ns <- NS(id)
  tagList(
    tags$div(id = id,
    tags$head(tags$script(type = "text/x-mathjax-config",
                          'MathJax.Hub.Config({
  "HTML-CSS": { linebreaks: { automatic: true } },
         SVG: { linebreaks: { automatic: true } }
});')),
tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
    box(width = 12,
        title = "Functional Concurrent Regression Model",
        fluidRow(
        column(3,
         selectizeInput(ns("select_resp"), 
                        "Select Response Function", 
                        choices = c()),
         selectizeInput(ns("select_pred"), 
                        "Select Predictor Function(s)", 
                        multiple = TRUE, 
                        choices = c()),
         numericInput(ns("select_bw"),
                      "Select Smoothing Bandwidth", 
                      2.5, 0, 100, 0.1),
         selectizeInput(ns("select_kern"), 
                        "Select Smoothing Kernel", 
                        selected = "gauss", 
                        choices = c("rect", "gauss", "epan", "gausvar","quar")),
        downloadButton(ns("download_model"), label = "Download Model"),
        
        actionButton(ns("delete_model"), "Remove Model"),
        
        ),
        column(9,align = "center", 
         uiOutput(ns("fcr_title")),
         tags$br(),
         plotOutput(ns("beta_plot"),height = "375px") 
        )
        )
    )
    ) 
  )
}
    
#' FCR_model_box Server Functions
#'
#'
#'
#' @importFrom shinyjs runjs delay
#' @importFrom shiny observeEvent removeUI req validate need reactive updateSelectizeInput updateSelectInput downloadHandler withMathJax
#' @importFrom tidyr pivot_longer 
#' @importFrom purrr map
#' @import ggplot2
#' @importFrom magrittr extract2 
#' @noRd 
#' 
#' 

value <- NULL
mod_FCR_model_box_server <- function(id, raw_data, id_col, time_col,dark_mode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    # Dark Mode CSS ####
    # Change multiple selectize css for dark mode switch  
    observeEvent(c(dark_mode(),time_col(),id_col(),input$select_resp,input$select_pred), {
      runjs('console.log(document.querySelectorAll("[class^=item]"))')
      
      if(dark_mode() == TRUE){
        
        delay(1,
              runjs(
                '

          for (const node of document.querySelectorAll("[class^=item]")) {
                console.log(node.nextSibling.className.includes("item") || node.nextSibling.id.includes("select_pred-selectized"))
                console.log(node)
                if(node.nextSibling.id.includes("select_pred-selectized") || node.nextSibling.className.includes("item")){ 
                    node.className = "item-multiple-dark";
                }else{
                continue; 
                  }
          }
          
          '
              )
        )
        
        
        
      }
      else{
        runjs(
          '
           for (const node of document.getElementsByClassName("item-multiple-dark")) {
                  node.className = "item-multiple-light";
          }'
        )

      }
    },ignoreInit = TRUE)
    
    # Delete Model Box ####
    observeEvent(input$delete_model,{
      removeUI(selector = sprintf('#%s', id))
    })   
    
    # Initialize Select Boxes ####
    observeEvent(input$select_resp,{
      req(raw_data())
      
      updateSelectInput(session = session,
                           inputId = "select_resp",
                           choices = new_resp_cols())


      updateSelectizeInput(session = session,
                           inputId = "select_pred",
                           choices = new_pred_cols())
                           
    },once = TRUE)
    
    # Raw Data Columns ####
    raw_data_cols <- reactive({
      raw_data() %>% colnames 
    }) 
    
    # Columns that can't be selected by response select input ####
    cols_in_use_resp <- reactive({
      c(id_col(),time_col(),input$select_pred)
    }) 
    
    # Columns that can't be selected by predictor select input ####
    cols_in_use_pred <- reactive({
      c(id_col(),time_col(),input$select_resp)
    }) 
    
    # Columns that are available to response column ####
    new_resp_cols <- reactive({
      raw_data_cols()[! raw_data_cols() %in% cols_in_use_resp()] 
    }) 
    
    # Columns that are available to predictor column ####
    new_pred_cols <- reactive({
      raw_data_cols()[! raw_data_cols() %in% cols_in_use_pred()] 
    }) 
    
    # Update Response select input after initialization
    observeEvent(c(id_col(), time_col(),raw_data(), input$select_pred),{
      req(raw_data())
      req(input$select_resp)
      not_time_col <- input$select_resp != time_col()
      not_id_col <- input$select_resp != id_col()
      not_in_pred_cols <- ! input$select_resp %in% input$select_pred
      
      if(not_time_col &  not_id_col & not_in_pred_cols){
        
        updateSelectizeInput(session = session,
                             inputId = "select_resp", 
                             selected = input$select_resp,
                             choices = new_resp_cols())  
      }
      else{
        updateSelectizeInput(session = session,
                             inputId = "select_resp", 
                             choices = new_resp_cols())   
      }
      
    })
    
     observeEvent(c(id_col(), time_col(),raw_data(), input$select_resp),{
      req(raw_data())
       if(length(input$select_pred) == 0){
         updateSelectizeInput(session = session,
                             inputId = "select_pred",
                             choices = new_pred_cols())
       }else{

      not_time_col <- ! time_col() %in% input$select_pred
      not_id_col <- ! id_col() %in% input$select_pred
      not_resp_col <- ! input$select_resp %in% input$select_pred

      if(not_time_col &  not_id_col & not_resp_col){


        updateSelectizeInput(session = session,
                             inputId = "select_pred",
                             selected = input$select_pred,
                             choices = new_pred_cols())
      }
      else{

        if(not_time_col == FALSE){
          wo_same_col <- input$select_pred[! time_col() %in% new_pred_cols()]
        }
        else if(not_id_col == FALSE){
          wo_same_col <- input$select_pred[! id_col() %in% new_pred_cols()]
        }
        else{
          wo_same_col <- input$select_pred[! input$select_pred %in% new_pred_cols()]
        }

        updateSelectizeInput(session = session,
                             inputId = "select_pred",
                             selected = wo_same_col,
                             choices = new_pred_cols())
      }
       }

    })
     
     
     
     # BW
     # Kern
     # Measurment Error
     # Outgrid
     
grouped_data <- reactive({
  raw_data() %>% split(raw_data() %>% extract2(id_col()) %>% factor) 
})

resp_list <- reactive({
  grouped_data() %>% map(~ .x %>% extract2(input$select_resp))
}) 

pred_list <- reactive({
pred_list <- list()
for(col_name in input$select_pred){
 pred_list[[col_name]] <- grouped_data() %>% map(~ .x %>% extract2(col_name)) 
} 
return(pred_list)
})


time_list <- reactive({
  grouped_data() %>% map(~.x %>% extract2(time_col()))
})

vars <- reactive({
  
vars <- list()

vars[["Y"]] <- list("Lt" = time_list(), "Ly" = resp_list()) 

for(col_num in 1:(input$select_pred %>% length)){
  col_name <- input$select_pred %>% extract2(col_num)
  vars[[paste0("beta_",col_num)]] <- list("Lt" = time_list(), 
                                          "Ly" = pred_list() %>% extract2(col_name))
}

return(vars)

})


full_time_vec <- reactive({
time_max <- raw_data() %>% extract2(time_col()) %>% unique %>% max 
time_min <- raw_data() %>% extract2(time_col()) %>% unique %>% min 
full_time_vec <- time_min:time_max
return(full_time_vec)
})


use_measure_error <- reactive({
  Lt <- grouped_data() %>% map(~.x %>% extract2(time_col()))
  if(IsRegular(Lt) == "Sparse"){
    return(FALSE)
  }else{
    return(TRUE)
  }
})

try_fcr_obj <- reactive({
      tryCatch(FCReg( vars = vars(),
  userBwMu = input$select_bw,
  userBwCov = input$select_bw,
  outGrid = full_time_vec(),
  kern = input$select_kern,
  measurementError = use_measure_error(),
) 
                
                ,
                error = function(e){
                  return(e) 
                }
      )
    })

model <- reactive({
  withProgress(message = "Creating FCR Object",{
  model <- FCReg(
  vars = vars(),
  userBwMu = input$select_bw,
  userBwCov = input$select_bw,
  outGrid = full_time_vec(),
  kern = input$select_kern,
  measurementError = use_measure_error(),
) 
  incProgress(message = "FCR Object Complete")
  }) 
  return(model)
})


beta_df <- reactive({
 beta_0 <- Lwls1D(
   bw = input$select_bw,
   kernel_type = input$select_kern,
   xin = full_time_vec(),
   yin = model() %>% extract2("beta0"),
   xout = full_time_vec(),
 ) 
 
 #beta_df_smooth <- model() %>% extract2("beta") %>% rbind(beta_0) %>% as.data.frame 
 beta_df <- model() %>% extract2("beta") %>% apply(1,function(matrix_row){
   Lwls1D(
     bw = input$select_bw,
     kernel_type = input$select_kern,
     xin = full_time_vec(),
     yin = matrix_row,
     xout = full_time_vec()
   )
 }) %>% as.data.frame %>% cbind(beta_0) 
 #beta_df_t <- beta_df %>% transpose 
 #colnames(beta_df_t) <- rownames(beta_df)
 
time_vec <- full_time_vec()
beta_df %<>% cbind(time_vec) %>% pivot_longer(!time_vec, names_to = "beta")  
return(beta_df)
})

# Beta PLot ####
output$beta_plot <- renderPlot({
  
  if(is.null(try_fcr_obj()$message)){
    error_message <- ""
  }else{
    error_message <- try_fcr_obj()$message 
  }
  
        
    validate(
        need(error_message != "attempt to select less than one element in get1index",  
             "Select at least one predictor function") %then% 
          need(error_message !=  " win, xin or yin contain only NAs!", 
             "One of the columns contains only NAs. Check response function and predictor function(s)") %then% 
          need(error_message !=  "No enough points in local window, please increase bandwidth using userBwCov.", 
             "Increase the smoothing bandwidth") %then% 
        need(error_message == "", "Something went wrong with FCReg check column values") 
      )
 
  beta_plot <- ggplot() +  
    geom_line(data = beta_df(),
              aes(x = time_vec, 
                  y = value),color = "#007bff")+
    facet_wrap(~ beta, scales = "free_y")+
    xlab(time_col())+
    ylab("Beta Functions")
    
  
  if(dark_mode() == TRUE){
        beta_plot <- beta_plot + theme(plot.background = element_rect(fill = "#3f474e"),
                                          axis.text.x = element_text(color = "white"), 
                                          axis.text.y = element_text(color = "white"),
                                          axis.title.x = element_text(color = "white"),
                                          axis.title.y = element_text(color = "white"),
                                          panel.background = element_rect(colour = "#6c757d"),
                                          strip.background = element_rect(fill = "#454d55",color = "white"),
                                          strip.text = element_text(color = 'white')
                                          )
      }else{
        beta_plot <- beta_plot + theme_light()+theme(plot.background = element_rect(fill = "white"))
      }
  
  beta_plot
  
})

output$fcr_title <- renderUI({
  validate(
        need(try_fcr_obj() != "Error", "")
      )
  beta_str <- c()
  beta_num <- 1:(input$select_pred %>% length)
  for(num in beta_num){
    beta_str %<>% append(
      paste0("$\\beta_","{",num,"}","(t)$",input$select_pred[num],"$(t)$")
    )
    
  }
  withMathJax(
      paste0(
       input$select_resp,"$(t)$"," ~ ","$\\beta_0(t)$ + ",paste0(beta_str,collapse = "+")," + $\\epsilon(t)$" 
      )
    )
})


output$download_model <- downloadHandler(
  filename = "FCReg_model.Rda", 
  content = function(file){
    saveRDS(model(), file)
  }
  
)

 
  })
}
    
## To be copied in the UI
# mod_FCR_model_box_ui("FCR_model_box_ui_1")
    
## To be copied in the server
# mod_FCR_model_box_server("FCR_model_box_ui_1")
