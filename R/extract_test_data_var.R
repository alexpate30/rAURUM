### Write a function which will extract the SBP variable
extract_sbp_var <- function(cohort,
                        varname = NULL,
                        codelist,
                        codelist.vector,
                        indexdt,
                        t = NULL,
                        t.varname = TRUE,
                        time.prev = 365.25*5,
                        time.post = 0,
                        lower.bound = -Inf,
                        upper.bound = Inf,
                        db.open = NULL,
                        db = NULL,
                        db.filepath = NULL,
                        out.save.disk = TRUE,
                        out.subdir = NULL,
                        out.filepath = NULL,
                        return.output = FALSE){

#         varname = NULL
#         codelist = "edh_sbp_medcodeid"
#         cohort = cohortZ
#         indexdt = "fup_start"
#         t = 0
#         t.varname = TRUE
#         time.prev = 365.25*5
#         time.post = 0
#         lower.bound = -Inf
#         upper.bound = Inf
#         db = "aurum_small"
#         db.filepath = NULL
#         out.save.disk = FALSE
#         out.filepath = NULL
#         out.subdir = NULL
#         return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "value_var"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query
  db.qry <- db_query(codelist,
                     db.open = db.open,
                     db = db,
                     db.filepath = db.filepath,
                     tab = "observation",
                     codelist.vector = codelist.vector)
  print(str(db.qry))

  ### Get test data for individuals in cohort, within time range and remove outliers
  variable.dat <- combine_query(cohort,
                                db.qry,
                                query.type = "test",
                                time.prev = time.prev,
                                time.post = time.post,
                                lower.bound = lower.bound,
                                upper.bound = upper.bound,
                                numobs = 200)
  print(str(variable.dat))

  ### Create a dataframe of patids for individuals who have more than one observation
  patids.multiple <- variable.dat[duplicated(variable.dat$patid)] |>
    dplyr::group_by(patid) |>
    dplyr::slice(1) |>
    dplyr::select(patid)

  print("merge")
  ### Merge with the dataset of all test data scores, keeping only individuals in 'patids.multiple'
  variable.dat <- merge(variable.dat, patids.multiple, by.x = "patid", by.y = "patid")

  ### Get sd of observations for each individual
  variable.dat <- variable.dat |>
    dplyr::group_by(patid) |>
    dplyr::summarise("sbp_var" = stats::sd(value))

  print("merge 2")
  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Change name of value to the variable name
  colnames(variable.dat)[colnames(variable.dat) == "value_var"] <- varname

  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
