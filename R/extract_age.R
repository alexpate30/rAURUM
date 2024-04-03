### Program to extract diabetes
### Lets write a program to extract all the "history of" variables
extract_age <- function(cohort, 
                        varname = NULL,
                        indexdt, 
                        t = NULL, 
                        t.varname = TRUE,
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
  ## Add index date variable to cohort and change indexdt based on t (note the prep_cohort function also removes all other variables)
  cohort <- prep_cohort(cohort, indexdt, t, reduce = FALSE)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "age"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)
  
  ### Calculate age
  cohort <- dplyr::mutate(cohort, age = as.numeric((indexdt - dob)/365.25))
  
  ### Reduce to variables of interest
  variable.dat <- cohort[,c("patid", "age")]
  
  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "age"] <- varname
  
  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)
  
}