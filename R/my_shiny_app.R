my_shiny_app <- function(thedata, ...) {
  shiny_env <- new.env()
  if(!missing(thedata)) {
    print('Setting parameters')
    assign('thedata', thedata, shiny_env)
  }
  environment(shiny_ui) <- shiny_env
  environment(shiny_server) <- shiny_env
  app <- shiny::shinyApp(
    ui = shiny_ui,
    server = shiny_server
  )
  runApp(app, ...)
}
