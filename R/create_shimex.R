
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
create_shimex <- function(..., chosen_observation = NULL, directory = NULL, selected_variables = NULL) {

  args <- list(..., version=1.0)
  options <- args[names(args) != ""]

  explainers <- args[names(args) == ""]

  path_to_template <- system.file("templates", "default_template.txt", package="shimex")
  template <- readr::read_file(path_to_template)

  if(is.null(directory)) directory <- tempdir()
  directory <- file.path(directory, 'shimex')
  if(!dir.exists(directory)) dir.create(directory)

  data <- explainers[[1]]$data

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

  buttons <- ''
  explainers_reactive <- ''
  explainers_static <- ''
  for(i in 1:length(explainers)){
    saveRDS(explainers[[i]], file = paste0(directory,"/exp",i,".rds"))
    button <- paste0('tags$li(class = "dropdown", actionBttn("exp',
                            i,'", explainer',i,'$label, style = "fill", block = TRUE))')
    buttons <- paste0(buttons, ",", button)
    explainer_reactive <- paste0('observeEvent(input$exp',i,',{exp$data <- explainer',i,'})')
    explainers_reactive <- paste0(explainers_reactive, "\n\t", explainer_reactive)
    explainer_static <- paste0('explainer',i,' <- readRDS("exp',i,'.rds")')
    explainers_static <- paste0(explainers_static, "\n", explainer_static)
  }

  template_data <- list(obs = obs,
                        cols = cols,
                        explainers_static = explainers_static,
                        selected_variables = selected_variables,
                        chosen_observation = chosen_observation,
                        explainers_reactive = explainers_reactive,
                        buttons = buttons)

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
