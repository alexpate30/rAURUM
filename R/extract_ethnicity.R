#' Extract ethnicity recorded at any time.
#'
#' @description
#' Extract ethnicity recorded at any time.
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param db.open An open SQLite database connection created using RSQLite::dbConnect, to be queried.
#' @param db Name of SQLITE database on hard disk (stored in "data/sql/"), to be queried.
#' @param db.filepath Full filepath to SQLITE database on hard disk, to be queried.
#' @param out.save.disk If `TRUE` will attempt to save outputted data frame to directory "data/extraction/".
#' @param out.subdir Sub-directory of "data/extraction/" to save outputted data frame into.
#' @param out.filepath Full filepath and filename to save outputted data frame into.
#' @param return.output If `TRUE` will return outputted data frame into R workspace.
#'
#' @details This function assumes all the codelists are already on the hard disk, stored in folder "codelists/analysis" in appropriate formats and appropriate names.
#' I will probably not export this function.
#'
#' Specifying `db` requires a specific underlying directory structure. The SQLite database must be stored in "data/sql/" relative to the working directory.
#' If the SQLite database is accessed through `db`, the connection will be opened and then closed after the query is complete. The same is true if
#' the database is accessed through `db.filepath`. A connection to the SQLite database can also be opened manually using `RSQLite::dbConnect`, and then
#' using the object as input to parameter `db.open`. After wards, the connection must be closed manually using `RSQLite::dbDisconnect`. If `db.open` is specified, this will take precedence over `db` or `db.filepath`.
#'
#' If `out.save.disk = TRUE`, the data frame will automatically be written to an .rds file in a subdirectory "data/extraction/" of the working directory.
#' This directory structure must be created in advance. `out.subdir` can be used to specify subdirectories within "data/extraction/". These options will use a default naming convention. This can be overwritten
#' using `out.filepath` to manually specify the location on the hard disk to save. Alternatively, return the data frame into the R workspace using `return.output = TRUE`
#' and then save onto the hard disk manually.
#'
#' @returns A data frame with variable ethnicity.
extract_ethnicity <- function(cohort,
                              varname = NULL,
                              db.open = NULL,
                              db = NULL,
                              db.filepath = NULL,
                              out.save.disk = FALSE,
                              out.subdir = NULL,
                              out.filepath = NULL,
                              return.output = TRUE){

  #     varname = NULL
  #     cohort = cohortZ
  #     db = "aurum_small"
  #     db.filepath = NULL
  #     save.disk = FALSE
  #     out.filepath = NULL
  #     out.subdir = NULL
  #     return.output = TRUE

  ### Preparation
  ## Reduce cohort to important variable
  cohort <- cohort[, "patid"]
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "ethnicity"
  }
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### There are a large number of codelists for this variable, extract them all
  ### Note that the user is not specifying the codelists, so they must have the appropriate codelists with names in the codelists/analysis folder
  ### to be able to run this function

  ### bangladeshi ADDED
  edh.bangladeshi <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_bangladeshi_medcodeid.csv", sep = ""),
                                       sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.bangladeshi <- edh.bangladeshi$medcodeid

  ### edh.black.african ADDED
  edh.black.african <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_black_african_medcodeid.csv", sep = ""),
                                         sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.black.african <- edh.black.african$medcodeid

  ### edh.black.caribbean ADDED
  edh.black.caribbean <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_caribbean_medcodeid.csv", sep = ""),
                                           sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.black.caribbean <- edh.black.caribbean$medcodeid

  ### edh.chinese ADDED
  edh.chinese <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_chinese_medcodeid.csv", sep = ""),
                                   sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.chinese <- edh.chinese$medcodeid

  ### edh.indian ADDED
  edh.indian <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_indian_medcodeid.csv", sep = ""),
                                  sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.indian <- edh.indian$medcodeid

  ### edh.irish ADDED
  edh.irish <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_irish_medcodeid.csv", sep = ""),
                                 sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.irish <- edh.irish$medcodeid

  ### edh.not.recorded ADDED
  edh.not.recorded <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_not_recorded_medcodeid.csv", sep = ""),
                                        sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.not.recorded <- edh.not.recorded$medcodeid

  ### edh.oth.asian ADDED
  edh.oth.asian <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_oth_asian_medcodeid.csv", sep = ""),
                                     sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.oth.asian <- edh.oth.asian$medcodeid

  ### edh.oth.black ADDED
  edh.oth.black <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_oth_black_medcodeid.csv", sep = ""),
                                     sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.oth.black <- edh.oth.black$medcodeid

  ### edh.oth.ethnic ADDED
  edh.oth.ethnic <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_oth_ethnic_medcodeid.csv", sep = ""),
                                      sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.oth.ethnic <- edh.oth.ethnic$medcodeid

  ### edh.oth.mixed ADDED
  edh.oth.mixed <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_oth_mixed_medcodeid.csv", sep = ""),
                                     sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.oth.mixed <- edh.oth.mixed$medcodeid

  ### edh.oth.white ADDED
  edh.oth.white <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_oth_white_medcodeid.csv", sep = ""),
                                     sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.oth.white <- edh.oth.white$medcodeid

  ### edh.pakistani ADDED
  edh.pakistani <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_pakistani_medcodeid.csv", sep = ""),
                                     sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.pakistani <- edh.pakistani$medcodeid

  ### edh.white.asian ADDED
  edh.white.asian <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_white_asian_medcodeid.csv", sep = ""),
                                       sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.white.asian <- edh.white.asian$medcodeid

  ### edh.white.black.african ADDED
  edh.white.black.african <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_white_black_african_medcodeid.csv", sep = ""),
                                               sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.white.black.african <- edh.white.black.african$medcodeid

  ### edh.white.black.caribbean ADDED
  edh.white.black.caribbean <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_white_black_caribbean_medcodeid.csv", sep = ""),
                                                 sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.white.black.caribbean <- edh.white.black.caribbean$medcodeid

  ### edh.white.british ADDED
  edh.white.british <- data.table::fread(file = paste(getwd(),"/codelists/analysis/edh_white_british_medcodeid.csv", sep = ""),
                                         sep = ",", header = TRUE, colClasses = rep("character", 3))
  edh.white.british <- edh.white.british$medcodeid

  ### Combine into codelist categories
  ### WANT TO ADD A CHECK THAT THESE ARE NON-OVERLAPPING GROUPS
  codes.bangladeshi <- edh.bangladeshi
  codes.black.african <- edh.black.african
  codes.black.caribbean <- edh.black.caribbean
  codes.chinese <- edh.chinese
  codes.indian <- edh.indian
  #codes.not.stated <- edh.not.recorded
  codes.oth.asian <- c(edh.oth.asian, edh.white.asian)
  codes.oth.ethnic <- c(edh.oth.ethnic, edh.oth.black, edh.oth.mixed, edh.white.black.african, edh.white.black.caribbean)
  codes.pakistani <- edh.pakistani
  codes.white <- c(edh.white.british, edh.irish, edh.oth.white)

  ### And one codelist for extracting data
  codelist <- c(codes.bangladeshi, codes.black.african, codes.black.caribbean, codes.chinese, codes.indian,
                codes.oth.asian, codes.oth.ethnic, codes.pakistani, codes.white)

  ### Query database
  db.qry <- db_query(NULL,
                     db,
                     db.filepath,
                     tab = "obs",
                     codelist.vector = codelist)

  ### Merge cohort with the database query
  cohort.qry <- merge(cohort, db.qry, by.x = "patid", by.y = "patid")

  ### Reduce to variables of interest
  cohort.qry <- cohort.qry[,c("patid", "medcodeid", "obsdate")]

  ### Create ethnicity variable
  cohort.qry <- dplyr::mutate(cohort.qry,
                              ethnicity = dplyr::case_when(medcodeid %in% codes.bangladeshi ~ "bangladeshi",
                                                           medcodeid %in% codes.black.african ~ "black.african",
                                                           medcodeid %in% codes.black.caribbean ~ "black.caribbean",
                                                           medcodeid %in% codes.chinese ~ "chinese",
                                                           medcodeid %in% codes.indian ~ "indian",
                                                           medcodeid %in% codes.oth.asian ~ "other.asian",
                                                           medcodeid %in% codes.oth.ethnic ~ "other.ethnic",
                                                           medcodeid %in% codes.pakistani ~ "pakistani",
                                                           medcodeid %in% codes.white ~ "white"))

  #   ### If an individual has "not.stated" codes, but also has other codes, remove the "not.stated" codes
  #   ### This is because we are basing ethnicity on most recent observation. If this is not stated most recently,
  #   ### but there is a historic code identifying this individual, we want to use that record.
  #   ## Identify who has not.stated
  #   patids.not.stated <- unique(cohort.qry$patid[cohort.qry$ethnicity == "not.stated"])
  #   ## Identify individuals who have actual codes
  #   patids.other <- unique(cohort.qry$patid[cohort.qry$ethnicity != "not.stated"])
  #   ## Identify thos who are in both
  #   patids.both <- patids.not.stated[patids.not.stated %in% patids.other]
  #   ## Remove observations that are equal to non.stated for these individuals
  #   cohort.qry <- subset(cohort.qry, !(patid %in% patids.both & ethnicity == "not.stated"))
  #   rm(patids.both, patids.other, patids.not.stated)

  ### For now, only keep most recent observation for each individual
  cohort.qry <- dplyr::group_by(cohort.qry, patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  ### Merge back with cohort
  variable.dat <- merge(cohort, cohort.qry, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### If ethnicity is "not.stated", set to missing

  ### Turn into factor variable
  variable.dat$ethnicity <- factor(variable.dat$ethnicity,
                                   levels = c("bangladeshi", "black.african", "black.caribbean", "chinese", "indian",
                                              "other.asian", "other.ethnic", "pakistani", "white"),
                                   labels = c("bangladeshi", "black african", "black caribbean", "chinese", "indian",
                                              "other asian", "other ethnic", "pakistani", "white"))

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "ethnicity")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "ethnicity"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}


