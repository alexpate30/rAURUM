#' Create the appropriate root directory system to be able to run functions with specifying hard filepaths
#'
#' @description
#' Create the appropriate root directory system to be able to run functions with specifying hard filepaths
#'
#' @param rootdir Directory within which to create the directory system
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
