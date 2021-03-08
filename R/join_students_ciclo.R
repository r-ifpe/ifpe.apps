#' @importFrom dplyr %>% sym
#' @export
join_students_ciclo <- function(students, ciclo){
  dplyr::left_join(students, ciclo, by = "S_CO_CICLO_MATRICULA") %>%
    dplyr::mutate(S_NO_STATUS_MATRICULA = dplyr::case_when(
      !!sym("S_NO_STATUS_CICLO") == "CONCLUIDO" &
        !!sym("S_NO_STATUS_MATRICULA") == "EM_CURSO" ~ "RETIDO",
      !!sym("S_NO_STATUS_MATRICULA") == "EM_CURSO" ~ "EM_FLUXO",
      TRUE ~ !!sym("S_NO_STATUS_MATRICULA")
    ))
}


