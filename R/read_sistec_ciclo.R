#' @importFrom dplyr %>% sym
#' @export
read_sistec_ciclo <- function(path = "") {
  if (path == "") stop("You need to specify the path.")

  temp <- list.files(path = path, pattern = "*.csv")
  temp <- paste0(path, "/", temp) %>% sort(decreasing = TRUE)

  classes <- "character"

  read_sistec_files(temp, classes) %>%
    select_sistec_ciclo_variables() %>%
    correct_ciclo_types() %>%
    correct_ciclo_offer() %>%
    classify_ciclo_status()
}

#' @importFrom dplyr sym
select_sistec_ciclo_variables <- function(x) {
  dplyr::transmute(x,
    S_NO_CAMPUS = !!sym("MUNICIPIO"),
    S_NO_CURSO = !!sym("NOME DO CURSO"),
    S_CO_CICLO_MATRICULA = !!sym("C\u00d3DIGO CICLO DE MATR\u00cdCULA"),
    S_NO_TIPO = !!sym("SUBTIPO CURSOS"),
    S_NO_OFERTA = !!sym("TIPO OFERTA DO CURSO"),
    S_DT_INICIO_CICLO = sistec_convert_beginning_date(!!sym("DATA INÍCIO DO CURSO")),
    S_DT_FIM_CICLO = !!sym("DATA FIM PREVISTO DO CURSO")
  )
}

#' @importFrom dplyr sym
correct_ciclo_types <- function(x) {
  valid_types <- paste0(
    "BACHARELADO|TECNOLOGIA|LICENCIATURA|TÉCNICO|",
    "ESPECIALIZA\u00c7\u00c3O|MESTRADO|",
    "FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL"
  )

  dplyr::mutate(x, S_NO_TIPO = dplyr::if_else(
    grepl(valid_types, !!sym("S_NO_TIPO")),
    !!sym("S_NO_TIPO"),
    "FORMAÇÃO CONTINUADA"
  ))
}

#' @importFrom dplyr sym
correct_ciclo_offer <- function(x) {
  dplyr::mutate(x, S_NO_OFERTA = dplyr::case_when(
    S_NO_OFERTA == "" & grepl("BACHARELADO|TECNOLOGIA|LICENCIATURA", !!sym("S_NO_TIPO"))
    ~ "SUPERIOR",
    S_NO_OFERTA == "" & grepl("ESPECIALIZA\u00c7\u00c3O|MESTRADO", !!sym("S_NO_TIPO"))
    ~ "P\u00d3S GRADUA\u00c7\u00c3O",
    S_NO_OFERTA == "" ~ "FIC",
    TRUE ~ S_NO_OFERTA
  ))
}

#' @importFrom dplyr %>% sym
classify_ciclo_status <- function(x) {
  x %>%
    dplyr::mutate(DAYS = difftime(
      as.Date(Sys.time()),
      as.Date(!!sym("S_DT_FIM_CICLO")),
      units = "days"
    )) %>%
    dplyr::mutate(S_NO_STATUS_CICLO = dplyr::case_when(
      grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", !!sym("S_NO_TIPO")) &
        DAYS > 0 ~ "CONCLUIDO",
      !grepl("FORMAÇÃO CONTINUADA|FORMAÇÃO INICIAL", !!sym("S_NO_TIPO")) &
        DAYS > 365 ~ "CONCLUIDO",
      TRUE ~ "ATIVO"
    )) %>%
    dplyr::select(
      !!sym("S_NO_CAMPUS"), !!sym("S_NO_CURSO"), !!sym("S_CO_CICLO_MATRICULA"),
      !!sym("S_NO_TIPO"), !!sym("S_NO_OFERTA"), !!sym("S_DT_INICIO_CICLO"),
      !!sym("S_NO_STATUS_CICLO")
    )
}
