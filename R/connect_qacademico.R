#' @export
connect_qacademico <- function(folder){
  credentials <- paste0(
    folder,
    c("driver", "server", "database", "user_id", "password", "port")
  )

  DBI::dbConnect(odbc::odbc(),
    Driver = readLines(credentials[1]),
    Server = readLines(credentials[2]),
    Database = readLines(credentials[3]),
    UID = readLines(credentials[4]),
    PWD = readLines(credentials[5]),
    Port = as.integer(readLines(credentials[6]))
  )
}
