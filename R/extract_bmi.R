### Write a function which will extract the BMI variable
extract_bmi <- function(cohort,
                        varname = NULL,
                        codelist.bmi,
                        codelist.weight,
                        codelist.height,
                        codelist.bmi.vector,
                        codelist.weight.vector,
                        codelist.height.vector,
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
  #         cohort = cohortZ
  #         codelist.bmi = "edh_bmi_medcodeid"
  #         codelist.height = "height_medcodeid"
  #         codelist.weight = "weight_medcodeid"
  #         indexdt = "fup_start"
  #         t = 0
  #         t.varname = TRUE
  #         time.prev = round(365.25*5)
  #         time.post = 0
  #         lower.bound = 18
  #         upper.bound = 47
  #         db = "aurum_small"
  #         db.filepath = NULL
  #         out.save.disk = FALSE
  #         out.filepath = NULL
  #         out.subdir = NULL
  #         return.output = TRUE

  ### ADD TEST TO ENSURE THEY SPECIFY TIME FRAME

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "bmi"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Need to run three database queries, one for BMI, one for height and one for weight
  ### BMI
  db.qry.bmi <- db_query(codelist.bmi,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.bmi.vector)

  variable.dat.bmi <- combine_query(cohort,
                                    db.qry.bmi,
                                    query.type = "test",
                                    time.prev = time.prev,
                                    time.post = time.post,
                                    lower.bound = lower.bound,
                                    upper.bound = upper.bound)

  ### Height
  db.qry.height <- db_query(codelist.height,
                            db.open = db.open,
                            db = db,
                            db.filepath = db.filepath,
                            tab = "observation",
                            codelist.vector = codelist.height.vector)

  variable.dat.height <- combine_query(cohort,
                                       db.qry.height,
                                       query.type = "test",
                                       time.prev = time.prev,
                                       time.post = time.post)

  ### Weight
  db.qry.weight <- db_query(codelist.weight,
                            db.open = db.open,
                            db = db,
                            db.filepath = db.filepath,
                            tab = "observation",
                            codelist.vector = codelist.weight.vector)

  variable.dat.weight <- combine_query(cohort,
                                       db.qry.weight,
                                       query.type = "test",
                                       time.prev = time.prev,
                                       time.post = time.post)

  ### For the height query, we need to rescale those with numunitid = 173, 432 or 3202 from metres to cm
  db.qry.height <- dplyr::mutate(db.qry.height, value = dplyr::case_when(numunitid %in% c(173, 432, 3202) ~ 100*value,
                                                                         !(numunitid %in% c(173, 432, 3202)) ~ value))





  ### Calculate bmi's estimated from height/weight
  ## Merge height and weight datasets
  variable.dat.manual <- merge(variable.dat.weight, variable.dat.height, by.x = "patid", by.y = "patid")
  ## Calculate bmi
  variable.dat.manual$value <- variable.dat.manual$value.x/(variable.dat.manual$value.y/100)^2
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
  rm(variable.dat.height, variable.dat.weight)

  ### Merge the two
  variable.dat <- merge(variable.dat.bmi, variable.dat.manual, by.x = "patid", by.y = "patid", all.x = TRUE, all.y = TRUE)

  ### Take most recent of the two
  variable.dat <- dplyr::mutate(variable.dat, bmi = dplyr::case_when(is.na(value.x) & !is.na(value.y) ~ value.y,
                                                                     !is.na(value.x) & is.na(value.y) ~ value.x,
                                                                     !is.na(value.x) & !is.na(value.y) & obsdate.y > obsdate.x ~ value.y,
                                                                     !is.na(value.x) & !is.na(value.y) & obsdate.y <= obsdate.x ~ value.x))

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "bmi")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "bmi"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
