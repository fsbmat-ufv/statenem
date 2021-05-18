#' The Shiny App Server.
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#' @export
shiny_server <- function(input, output, session) {
  if(!exists('thedata', envir = parent.env(environment()), inherits = FALSE)) {
    message('thedata not available, using default faithful...')
    data(faithful, envir = environment())
    thedata <- faithful
  }
  output$environment <- renderPrint(
    print(ls(envir = parent.env(environment())))
  )
  output$thedata <- renderTable({
    return(thedata)
  })
}
