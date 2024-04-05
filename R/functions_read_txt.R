### Create functions that will read in a text file with the specified number of rows, and apply appropriate variable classes where relevant

### General function for when no classes need to be applied
extract_txt <- function(filepath, ...){

  utils::read.table(filepath, sep = "\t", ..., header = TRUE)

}

### Extract txt file with all colClasses = "character"
extract_txt_char <- function(filepath, ..., select = NULL){

  out <- utils::read.table(filepath, sep = "\t", ..., colClasses = "character", header = TRUE)

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)

}

### Extract linkage eligibility file
extract_txt_linkage <- function(filepath, ...){

  utils::read.table(filepath, sep = "\t", ..., colClasses = c("character", "integer", "character", "integer", "integer", "integer",
                                                              "integer", "integer", "integer","integer", "integer", "integer",
                                                              "integer", "integer", "integer","integer"))
}

### Extract patient file
extract_txt_pat <- function(filepath, ..., set = FALSE){

  ## Extract data
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE, colClasses = c("character", "integer", "character", "integer", "integer", "integer",
                                                                                    "character", "character", "integer", "character", "integer", "character"))

  ## Convert to dates where relevant
  out$regstartdate <- as.Date(out$regstartdate, format = "%d/%m/%Y")
  out$regenddate <- as.Date(out$regenddate, format = "%d/%m/%Y")
  out$cprd_ddate <- as.Date(out$cprd_ddate, format = "%d/%m/%Y")
  out$emis_ddate <- as.Date(out$emis_ddate, format = "%d/%m/%Y")

  ### Extract the 'set' from the filename
  if (set == TRUE){
    ### Get value of set
    set.filepath <- as.numeric(stringr::str_match(filepath, "set\\s*(.*?)\\s*_")[,2])

    ### Add variable set to data
    out$set <- rep(set.filepath, nrow(out))
  }

  ### Return
  return(out)
}

### Extract Observation file
extract_txt_obs <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = c("character","character","integer","character","character","character","character","character","character",
                                          "numeric","integer","integer","numeric","numeric","character"))
  ## Convert to dates where relevant
  out$obsdate <- as.Date(out$obsdate, format = "%d/%m/%Y")
  out$enterdate <- as.Date(out$enterdate, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}

### Extract Problem file
extract_txt_prob <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = c("character","character","integer","character","character","integer","character","character","integer",
                                          "integer","integer"))
  ## Convert to dates where relevant
  out$probenddate <- as.Date(out$probenddate, format = "%d/%m/%Y")
  out$lastrevdate <- as.Date(out$lastrevdate, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}


### Extract Referral file
extract_txt_ref <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = c("character","character","integer","integer","integer","integer","integer","integer"))

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}

### Extract DrugIssue file
extract_txt_drug <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = c("character","character","integer","character","character","character","character","character","character",
                                          "character","numeric","integer","integer","numeric"))
  ## Convert to dates where relevant
  out$issuedate <- as.Date(out$issuedate, format = "%d/%m/%Y")
  out$enterdate <- as.Date(out$enterdate, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}

### Extract consultation file
extract_txt_cons <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = c("character","character","integer","character","character","character","character","integer","character"))

  ## Convert to dates where relevant
  out$consdate <- as.Date(out$consdate, format = "%d/%m/%Y")
  out$enterdate <- as.Date(out$enterdate, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)

}

### Extract hes primary diagnoses file
extract_txt_hes_primary <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = "character")

  ## Convert to dates where relevant
  out$admidate <- as.Date(out$admidate, format = "%d/%m/%Y")
  out$discharged <- as.Date(out$discharged, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}


### Extract hes primary diagnoses file
extract_txt_death <- function(filepath, ..., select = NULL){

  ## Extract and apply classes
  out <- utils::read.table(filepath, sep = "\t", ..., header = TRUE,
                           colClasses = "character")

  ## Convert to dates where relevant
  out$dor <- as.Date(out$dor, format = "%d/%m/%Y")
  out$dod <- as.Date(out$dod, format = "%d/%m/%Y")

  ## Apply selected columns
  if(!is.null(select)){
    out <- out[,select]
  }

  return(out)
}
