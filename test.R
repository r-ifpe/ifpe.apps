a <- read_sistec_ciclo("inst/ciclo/")
b <- read_sistec_students("inst/students/")

d <- left_join(b, a, by = "S_CO_CICLO_MATRICULA") %>%
  mutate(S_NO_STATUS_MATRICULA = dplyr::case_when(
    !!sym("S_NO_STATUS_CICLO") == "CONCLUIDO" &
      !!sym("S_NO_STATUS_MATRICULA") == "EM_CURSO" ~ "RETIDO",
    !!sym("S_NO_STATUS_MATRICULA") == "EM_CURSO" ~ "EM_FLUXO",
    TRUE ~ !!sym("S_NO_STATUS_MATRICULA")
  ))
