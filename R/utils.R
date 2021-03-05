read_sistec_files <- function(x, classes) {
  dplyr::bind_rows(
    lapply(x, utils::read.csv,
           sep = ";", stringsAsFactors = FALSE, colClasses = classes,
           encoding = "latin1", check.names = FALSE
    )
  )
}

num_para_cpf <- function(num) {
  stringr::str_replace(
    string = num,
    pattern = "([0-9]{3})([0-9]{3})([0-9]{3})",
    replacement = "\\1.\\2.\\3-"
  )
}

sistec_correct_campus_name <- function(campus) {
  stringr::str_replace(campus, "REU E LIMA", "ABREU E LIMA")
}

sistec_convert_beginning_date <- function(date) {
  first_slash <- stringr::str_locate_all(date[1], "/|-")[[1]][1]

  if (first_slash == 3) {
    year <- stringr::str_sub(date, 7, 10)
    month <- as.numeric(stringr::str_sub(date, 4, 5))
    semester <- ifelse(month > 6, 2, 1)
    paste0(year, ".", semester)
  } else {
    year <- stringr::str_sub(date, 1, 4)
    month <- as.numeric(stringr::str_sub(date, 6, 7))
    semester <- ifelse(month > 6, 2, 1)
    paste0(year, ".", semester)
  }
}

