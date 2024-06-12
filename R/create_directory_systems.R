#' Create the appropriate root directory system to be able to run functions with specifying hard filepaths
#'
#' @description
#' Create the appropriate root directory system to be able to run functions with specifying hard filepaths
#'
#' @param rootdir Directory within which to create the directory system
#'
#' @examples
#'
#' ## Print current working directory
#' getwd()
#'
#' ## Create directory system compatible with rAURUM's automatic saving of
#' ## output
#' create_directory_system()
#'
#' ## Connect
#' aurum_extract <- connect_database("data/sql/temp.sqlite")
#'
#' ## Create SQLite database using cprd_extract
#' cprd_extract(aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation", use.set = FALSE)
#'
#' ## Define cohort and add index date
#' pat<-extract_cohort(system.file("aurum_data", package = "rAURUM"))
#' pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
#'
#' ## Extract a history of type variable and save to disc automatically,
#' ## by just specifying name of database
#' extract_ho(pat,
#' codelist.vector = "187341000000114",
#' indexdt = "fup_start",
#' db = "temp",
#' tab = "observation",
#' out.save.disk = TRUE)
#'
#' ## Read file from disk into R workspace
#' readRDS("data/extraction/var_ho.rds")
#'
#' @export
create_directory_system <- function(rootdir = NULL){

  ### Start by setting and stating the root directory everything will be created in
  if (is.null(rootdir)){
    print(paste("The working directory is", getwd()))
  } else {
    setwd(rootdir)
    print(paste("The working directory is", getwd()))
  }

  ### Create the three key sub-directories
  ## code
  if (!file.exists(paste(getwd(), "/code", sep = ""))){
    dir.create(paste(getwd(), "/code", sep = ""))
  }

  ## data
  if (!file.exists(paste(getwd(), "/data", sep = ""))){
    dir.create(paste(getwd(), "/data", sep = ""))
    ## Create neccesary sub-directories in data
    dir.create(paste(getwd(), "/data/unzip", sep = ""))
    dir.create(paste(getwd(), "/data/extraction", sep = ""))
    dir.create(paste(getwd(), "/data/sql", sep = ""))
  }

  ## codelists
  if (!file.exists(paste(getwd(), "/codelists", sep = ""))){
    dir.create(paste(getwd(), "/codelists", sep = ""))
    ## Create neccesary sub-directories in codelists
    dir.create(paste(getwd(), "/codelists/analysis", sep = ""))
  }

}
