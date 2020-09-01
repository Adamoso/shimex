
#' Title
#'
#' @return
#' @export
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import DALEX
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom whisker whisker.render
#' @importFrom readr read_file
#' @examples
create_shimex <- function(explainer, chosen_observation, directory, selected_variables = NULL) {

  path_to_template <- system.file("templates", "default_template.txt", package="shimex")
  template <- readr::read_file(path_to_template)

  if(is.null(directory)) directory <- tempdir()
  directory <- file.path(directory, 'shimex')
  if(!dir.exists(directory)) dir.create(directory)

  data <- explainer$data

  cols1 <- paste0("'",colnames(data), "'")
  cols2 <- paste(cols1, sep="", collapse = ",")
  cols <- paste0("c(",cols2,")")

  if(is.null(selected_variables)){
    if(length(cols) < 7) selected_variables <- cols
    else selected_variables <- cols[1:7]
  }
  else{
    vars1 <- paste0("'",selected_variables, "'")
    vars2 <- paste(vars1, sep="", collapse = ",")
    selected_variables <- paste0("c(",vars2,")")
  }

  obs <- .create_observation(data)

  template_data <- list(obs = obs,
                        cols = cols,
                        selected_variables = selected_variables,
                        chosen_observation = chosen_observation)

  text_to_file <- whisker::whisker.render(template, template_data)

  file_path <- paste0(directory,"/app.R")
  file.create(file_path)
  fileConn<-file(file_path)
  writeLines(text_to_file, fileConn)
  close(fileConn)

}

.create_observation <- function(data){

  vars <- lapply(data, class)
  t_vars <- as.data.frame(cbind(names = names(vars), type = vars))
  t_vars$levels <- apply(t_vars, 1, function(x) paste0(', levels = levels(data$', x$`names`, ')'))
  t_vars$levels[t_vars$type != 'factor']  <- ''
  t_vars$as  <- ''
  t_vars$as[t_vars$type != 'factor']  <- 'as.'

  t <- apply(t_vars, 1, function(x) paste0(x$`names`, ' = ', x$`as` , x$`type` , '(input$', x$`names`, x$`levels`, ')'))

  obstr <- paste(t, collapse = ", ", '\n')
  paste0("list(", obstr,")")
}
