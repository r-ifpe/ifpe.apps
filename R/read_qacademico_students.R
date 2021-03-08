
#' @importFrom dplyr %>% sym
#' @export
read_qacademico_students <- function(con){
  dplyr::tbl(con, "CA_LISTAGEM_GERAL_ALUNOS") %>%
    dplyr::left_join(dplyr::tbl(con, "CA_LISTAGEM_ALUNOS_POR_COTA"),
              by = c("Cpf",
                     "Ano_Letivo_Inicial" = "Ano_Ingresso",
                     "Periodo_Letivo_Inicial" = "Periodo_Ingresso")) %>%
    dplyr::filter(Ano_Letivo_Inicial == 2020) %>%
    dplyr::collect() %>%
    dplyr::transmute(
      R_NU_CPF = !!sym("Cpf"),
      R_NO_ALUNO = correct_encoding(!!sym("Nome_Aluno")),
      R_NU_MATRICULA = !!sym("Matricula"),
      R_NO_CAMPUS = correct_campus_name(!!sym("Campus.x")),
      R_NO_CURSO = correct_encoding(!!sym("Curso")),
      R_NO_STATUS = correct_encoding(!!sym("Situacao")),
      R_NO_COTA = correct_encoding(!!sym("Tipo_Cota")),
      R_DT_INICIO = correct_beginnig_date(
        !!sym("Ano_Letivo_Inicial"), !!sym("Periodo_Letivo_Inicial")
      )
    )
}

correct_encoding <- function(x){
  iconv(x, "latin1", "latin1")
}

correct_campus_name <-  function(x){
  sub("IFPE / ", "", correct_encoding(x))
}

correct_beginnig_date <- function(ano, periodo){
  paste0(ano,".", periodo)
}





