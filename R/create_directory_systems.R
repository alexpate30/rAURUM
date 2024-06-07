### This program will create the appropriate root directory system to be able to run our functions

### This will be useful for users, but also allow us to run examples/vignettes
create_directory_system <- function(rootdir = NULL){

  ### Start by setting nnd stating the root directory everything will be created in
  if (is.null(rootdir)){
    print(paste("The working directory is", getwd()))
  } else {
    setwd(rootdir)
    print(paste("The working directory is", getwd()))
  }

  ### Create the three key sub-directories
  dir.create(paste(getwd(), "/code", sep = ""))
  dir.create(paste(getwd(), "/data", sep = ""))
  dir.create(paste(getwd(), "/codelists", sep = ""))

  ### Create neccesary sub-directories in data
  dir.create(paste(getwd(), "/data/unzip", sep = ""))
  dir.create(paste(getwd(), "/data/extraction", sep = ""))
  dir.create(paste(getwd(), "/data/sql", sep = ""))

  ### Create neccesary sub-directories in codelists
  dir.create(paste(getwd(), "/codelists/analysis", sep = ""))

  ### Then want to move the appropriate data provided with rCPRD (fake unzipped raw data, and codelists)
  ### into the appropriate folders
}
