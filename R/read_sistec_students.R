#' @importFrom dplyr %>% sym
#' @export
read_sistec_students <- function(path = "") {
  if (path == "") stop("You need to specify the path.")

  temp <- list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp) %>% sort(decreasing = TRUE)

  classes <- "character"

  read_sistec_files(temp, classes) %>%
    select_sistec_students_variables()
}



#' @importFrom dplyr sym
select_sistec_students_variables <- function(x) {
  dplyr::transmute(x,
    S_NO_ALUNO = !!sym("NO_ALUNO"),
    S_NU_CPF = num_para_cpf(!!sym("NU_CPF")),
    S_CO_CICLO_MATRICULA = !!sym("CO_CICLO_MATRICULA"),
    S_NO_STATUS_MATRICULA = !!sym("NO_STATUS_MATRICULA")
  )
}
