connect_database <- function(dbname){
  if(!stringr::str_detect(dbname, "\\.sqlite$")) {
    dbname <- paste(dbname, "sqlite", sep = ".")
  }
  RSQLite::dbConnect(RSQLite::SQLite(), dbname)
}
