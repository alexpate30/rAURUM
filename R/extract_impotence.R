### Program to extract diabetes
### Lets write a program to extract all the "history of" variables
extract_impotence <- function(cohort,
                             varname = NULL,
                             codelist.med,
                             codelist.drug,
                             codelist.med.vector,
                             codelist.drug.vector,
                             indexdt,
                             t = NULL,
                             t.varname = TRUE,
                             drug.time.prev = Inf,
                             drug.numobs = 1,
                             db.open = NULL,
                             db = NULL,
                             db.filepath = NULL,
                             out.save.disk = TRUE,
                             out.subdir = NULL,
                             out.filepath = NULL,
                             return.output = FALSE){

#         varname = NULL
#         codelist.type1 = "edh_t1dia_medcodeid"
#         codelist.type2 = "edh_t2dia_medcodeid"
#         cohort = cohortZ
#         indexdt = "fup_start"
#         t = NULL
#         db = "aurum_small"
#         db.filepath = NULL
#         out.save.disk = TRUE
#         out.filepath = NULL
#         out.subdir = NULL
#         return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "diabetes"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query for type 1 and type 2
  db.qry.med <- db_query(codelist.med,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.med.vector)

  db.qry.drug <- db_query(codelist.drug,
                          db.open = db.open,
                          db = db,
                          db.filepath = db.filepath,
                          tab = "observation",
                          codelist.vector = codelist.drug.vector)

  ### Identify which individuals have a history of type 1 or type 2
  cohort[,"med"] <- combine_query_boolean(cohort,
                                            db.qry.med,
                                            query.type = "obs")

  cohort[,"drug"] <-   combine_query_boolean(cohort,
                                              db.query = db.qry.drug,
                                              query.type = "drug",
                                              time.prev = drug.time.prev,
                                              time.post = 0,
                                              numobs = drug.numobs)

  ### Create the varaible of interest from these
  ### if an individual has history of type 1 and type 2, they will be classed as type 1
  variable.dat <- dplyr::mutate(cohort, impotence = pmax(med, drug))

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "impotence")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "impotence"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
