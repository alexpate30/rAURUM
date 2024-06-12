#' Open connection to SQLite database
#'
#' @description
#' Open connection to SQLite database
#'
#' @param dbname Name of SQLite database on hard disk (including full file path relative to working directory)
#'
#' @examples
#'
#' ## Connect to a database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Check connection is open
#' inherits(aurum_extract, "DBIConnection")
#'
#' @export
connect_database <- function(dbname){
  if(!stringr::str_detect(dbname, "\\.sqlite$")) {
    dbname <- paste(dbname, "sqlite", sep = ".")
  }
  RSQLite::dbConnect(RSQLite::SQLite(), dbname)
}
