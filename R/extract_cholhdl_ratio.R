### Write a function which will extract the chol/HDL ratio variable
extract_cholhdl_ratio <- function(cohort,
                                  varname = NULL,
                                  codelist.ratio,
                                  codelist.chol,
                                  codelist.hdl,
                                  codelist.ratio.vector,
                                  codelist.chol.vector,
                                  codelist.hdl.vector,
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

#           varname = NULL
#           cohort = cohortZ
#           codelist.ratio = "edh_cholhdl_ratio_medcodeid"
#           codelist.chol = "edh_chol_medcodeid"
#           codelist.hdl = "edh_hdl_medcodeid"
#           indexdt = "fup_start"
#           t = 0
#           t.varname = TRUE
#           time.prev = round(365.25*5)
#           time.post = 0
#           lower.bound = 1
#           upper.bound = 12
#           db = "aurum_nosubset_randset"
#           db.filepath = NULL
#           out.save.disk = FALSE
#           out.filepath = NULL
#           out.subdir = NULL
#           return.output = TRUE

  ### ADD TEST TO ENSURE THEY SPECIFY TIME FRAME

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "cholhdl_ratio"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Need to run three database queries, one for ratio, one for chol and one for hdl
  db.qry.ratio <- db_query(codelist.ratio,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.ratio.vector)

  db.qry.chol <- db_query(codelist.chol,
                          db.open = db.open,
                          db = db,
                          db.filepath = db.filepath,
                          tab = "observation",
                          codelist.vector = codelist.chol.vector)

  db.qry.hdl <- db_query(codelist.hdl,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.hdl.vector)

  ### Get latest test result for ratio, chol and hdl from the last five years
  variable.dat.ratio <- combine_query(cohort,
                                      db.qry.ratio,
                                      query.type = "test",
                                      time.prev = time.prev,
                                      time.post = time.post,
                                      lower.bound = lower.bound,
                                      upper.bound = upper.bound)

  variable.dat.chol <- combine_query(cohort,
                                     db.qry.chol,
                                     query.type = "test",
                                     time.prev = time.prev,
                                     time.post = time.post)

  variable.dat.hdl <- combine_query(cohort,
                                    db.qry.hdl,
                                    query.type = "test",
                                    time.prev = time.prev,
                                    time.post = time.post)

  ### Calculate ratio's estimated from chol/hdl
  ## Merge chol and hdl datasets
  variable.dat.manual <- merge(variable.dat.chol, variable.dat.hdl, by.x = "patid", by.y = "patid")
  ## Calculate ratio
  variable.dat.manual$value <- variable.dat.manual$value.x/variable.dat.manual$value.y
  ## Take furthest away date
  variable.dat.manual$obsdate <- pmin(variable.dat.manual$obsdate.x, variable.dat.manual$obsdate.y)
  ## Remove values outside of range
  ### If values are missing, < lower.bound or > upper.bound then delete
  if (!is.null(lower.bound) & !is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value > lower.bound & value < upper.bound]
  } else if (is.null(lower.bound) & !is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value < upper.bound]
  } else if (!is.null(lower.bound) & is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value > lower.bound]
  }
  variable.dat.manual <- variable.dat.manual[,c("patid", "value", "obsdate")]
  rm(variable.dat.chol, variable.dat.hdl)

  ### Merge the two
  variable.dat <- merge(variable.dat.ratio, variable.dat.manual, by.x = "patid", by.y = "patid", all.x = TRUE, all.y = TRUE)

  ### Take most recent of the two
  variable.dat <- dplyr::mutate(variable.dat, cholhdl_ratio = dplyr::case_when(is.na(value.x) & !is.na(value.y) ~ value.y,
                                                                       !is.na(value.x) & is.na(value.y) ~ value.x,
                                                                       !is.na(value.x) & !is.na(value.y) & obsdate.y > obsdate.x ~ value.y,
                                                                       !is.na(value.x) & !is.na(value.y) & obsdate.y <= obsdate.x ~ value.x))

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "cholhdl_ratio")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "cholhdl_ratio"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
