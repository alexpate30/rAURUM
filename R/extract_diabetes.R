### Program to extract diabetes
### Lets write a program to extract all the "history of" variables
extract_diabetes <- function(cohort,
                             varname = NULL,
                             codelist.type1,
                             codelist.type2,
                             codelist.type1.vector,
                             codelist.type2.vector,
                             indexdt,
                             t = NULL,
                             t.varname = TRUE,
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
  db.qry.type1 <- db_query(codelist.type1,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.type1.vector)

  db.qry.type2 <- db_query(codelist.type2,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.type2.vector)

  ### Identify which individuals have a history of type 1 or type 2
  cohort[,"t1dia"] <- combine_query_boolean(cohort,
                                            db.qry.type1,
                                            query.type = "obs")

  cohort[,"t2dia"] <- combine_query_boolean(cohort,
                                            db.qry.type2,
                                            query.type = "obs")

  ### Create the varaible of interest from these
  ### if an individual has history of type 1 and type 2, they will be classed as type 1
  variable.dat <- dplyr::mutate(cohort, diabetes = dplyr::case_when(t2dia == 0 & t1dia == 0 ~ 0,
                                                                    t1dia == 1 ~ 1,
                                                                    t2dia == 1 ~ 2))

  ### Turn into factor variable
  variable.dat$diabetes <- factor(variable.dat$diabetes,
                                  levels = c(0,1,2),
                                  labels = c("Absent", "Type1", "Type2"))

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "diabetes")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "diabetes"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
