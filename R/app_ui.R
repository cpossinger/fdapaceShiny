#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom  shinyjs useShinyjs
#' @importFrom bs4Dash dashboardPage dashboardSidebar dashboardBody actionButton dashboardHeader dashboardBrand
#' @importFrom shiny tagList icon
#' @importFrom  htmltools div
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(fullscreen = TRUE,
                  scrollToTop = TRUE,
                  
                  
                  
      #dashboardHeader(title = golem::get_golem_options("title")),
      header = dashboardHeader(title = dashboardBrand("fdapaceShiny",
                                                    color= "primary",
                                                    href = "https://github.com/cpossinger/fdapaceShiny"),
                               skin = "light"),
      
      sidebar = dashboardSidebar(
        # sidebarMenu(
        #    if(length(golem::get_golem_options("fpca_obj")) != 0){
        #    menuItem("FPCA", tabName = "fpca")
        #    },
        #    menuItem("FPCA Modeling", tabName = "fpca_model")
        # )
        mod_global_options_ui("global_options_ui_1"),
        skin = "light",
        minified = FALSE,
      ),
      
      
      body = dashboardBody(
#         tags$style('.nav-tabs-custom .nav-tabs li.active {
#     border-top-color: ;
# }"'),
        # tabItems(
        #   tabItem(tabName = "fpca",mod_FPCA_ui("FPCA_ui_1")),
        #   tabItem(tabName = "fpca_model",
        #            fluidPage(
        #            mod_FPCA_Model_ui("FPCA_Model_ui_1")),
        #    fluidPage(
        #     #mod_FPCA_model_box_ui("FPCA_model_box_ui_10000"),
        #     actionButton("add_model", "Add Model", icon = icon("plus")))
        #    
        # )
        useShinyjs(),
        div(id = "fpca_alert", style = "position: absolute; bottom: 0; right: 0;"),
        div(style = "position: relative",
        actionButton("add_fpca_model", "Add FPCA Model", icon = icon("plus")),
        actionButton("add_fcr_model", "Add FCR Model", icon = icon("plus"))
        )
      )
    )
  )
  #)
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fdapaceShiny'
    )
  )
}

