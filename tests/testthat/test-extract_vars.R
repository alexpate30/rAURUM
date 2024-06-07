###
### Tests for variable extraction programs
###
testthat::test_that("Test extract_ho, extract_time_until and extract_test_data, and specification of underlying directory systems", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Define codelist
  codelist <- "187341000000114"

  ###
  ### Extract a history of type variable using extract_ho
  ho <- extract_ho(pat,
                   codelist.vector = codelist,
                   indexdt = "fup_start",
                   db.open = aurum_extract,
                   tab = "observation",
                   return.output = TRUE)

  testthat::expect_equal(nrow(ho), 6)
  testthat::expect_equal(colnames(ho), c("patid", "ho"))
  testthat::expect_equal(ho$ho, c(0, 1, 0, 0, 0, 1))

  ###
  ### Extract a medication history of type variable using extract_ho
  ho.drug <- extract_ho(pat,
                   codelist.vector = 3092241000033113,
                   indexdt = "fup_start",
                   db.open = aurum_extract,
                   tab = "drugissue",
                   return.output = TRUE)

  testthat::expect_equal(nrow(ho.drug), 6)
  testthat::expect_equal(colnames(ho.drug), c("patid", "ho"))
  testthat::expect_equal(ho.drug$ho, c(1, 0, 0, 0, 0, 0))

  ###
  ### Extract a time until variable using extract_time_until
  time_until <- extract_time_until(pat,
                                   codelist.vector = codelist,
                                   indexdt = "fup_start",
                                   censdt = "fup_end",
                                   db.open = aurum_extract,
                                   tab = "observation",
                                   return.output = TRUE)

  testthat::expect_equal(nrow(time_until), 6)
  testthat::expect_equal(colnames(time_until), c("patid", "var_time", "var_indicator"))
  testthat::expect_equal(time_until$var_time, c(106, 16436,  16436,  16436,  16436,  16436))
  testthat::expect_equal(time_until$var_indicator, c(1, 0,  0,  0,  0,  0))

  ###
  ### Extract test data using extract_test_data
  test_data <- extract_test_data(pat,
                                 codelist.vector = codelist,
                                 indexdt = "fup_start",
                                 db.open = aurum_extract,
                                 time.prev = Inf,
                                 return.output = TRUE)

  testthat::expect_equal(nrow(test_data), 6)
  testthat::expect_equal(colnames(test_data), c("patid", "value"))
  testthat::expect_equal(test_data$value, c(NA, 46,  NA,  NA,  NA,  28))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ###
  ###
  ### Create a temporary directory to re-run these functions and save to disk automatically, and automatically look for SQLite database in data/sql
  ### Will recreate variables for ho and compare with ho created for previous test
  ###
  ###
  tempdir <- tempdir()
  setwd(tempdir)

  ### Create directory system
  create_directory_system()

  ### Create Aurum database in data/sql

  ### Connect
  aurum_extract <- connect_database("data/sql/temp.sqlite")

  ### Extract data using cprd_extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)


  ### Extract a history of type variable and save to disc automatically, by just specifying name of database
  extract_ho(pat,
             codelist.vector = codelist,
             indexdt = "fup_start",
             db = "temp",
             tab = "observation",
             out.save.disk = TRUE)

  ### Read from disk
  ho.disk <- readRDS("data/extraction/var_ho.rds")
  testthat::expect_equal(ho, ho.disk)

  ### Extract a history of type variable and save to disk using out.subdir
  extract_ho(pat,
             codelist.vector = codelist,
             indexdt = "fup_start",
             db = "temp",
             tab = "observation",
             out.subdir = "cohort",
             out.save.disk = TRUE)

  ### Read from disk
  ho.disk <- readRDS("data/extraction/cohort/var_ho.rds")
  testthat::expect_equal(ho, ho.disk)

  ### Extract a history of type variable and save to disk manually specifying filepath for output and db
  extract_ho(pat,
             codelist.vector = codelist,
             indexdt = "fup_start",
             db.filepath = "data/sql/temp.sqlite",
             tab = "observation",
             out.filepath = "data/extraction/eggs.rds",
             out.save.disk = TRUE)

  ### Read from disk
  ho.disk <- readRDS("data/extraction/eggs.rds")
  testthat::expect_equal(ho, ho.disk)

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ### REMAINING TEST IS TO DO THE AUTOMATIC CODELIST STUFF
})


###
### BMI
testthat::test_that("BMI", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Extract BMI
  var <- extract_bmi(cohort = pat,
                          codelist.bmi.vector = 498521000006119,
                          codelist.weight.vector = 401539014,
                          codelist.height.vector = 13483031000006114,
                          indexdt = "indexdt",
                          time.prev = Inf,
                          time.post = Inf,
                          db.open = aurum_extract,
                          return.output = TRUE)

  testthat::expect_equal(nrow(var), 6)
  testthat::expect_equal(colnames(var), c("patid", "bmi"))
  testthat::expect_equal(var$bmi, c(48, 41, NA, NA, 32, NA))

})


###
### Cholhdl ratio
testthat::test_that("Cholhdl ratio", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Extract cholhdl_ratio
  var <- extract_cholhdl_ratio(cohort = pat,
                     codelist.ratio.vector = 498521000006119,
                     codelist.chol.vector = 401539014,
                     codelist.hdl.vector = 13483031000006114,
                     indexdt = "indexdt",
                     time.prev = Inf,
                     time.post = Inf,
                     db.open = aurum_extract,
                     return.output = TRUE)

  ## NB: Value for cholhdl_ratio test are same as BMI test, because its the "ratio" medcode id that is finding the values,
  ## As opposed to finding them seperately and calculating the value from the components, which would be different
  testthat::expect_equal(nrow(var), 6)
  testthat::expect_equal(colnames(var), c("patid", "cholhdl_ratio"))
  testthat::expect_equal(var$cholhdl_ratio, c(48, 41, NA, NA, 32, NA))

})


###
### Diabetes
testthat::test_that("Diabetes", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Extract diabetes
  var <- extract_diabetes(cohort = pat,
                     codelist.type1.vector = 498521000006119,
                     codelist.type2.vector = 401539014,
                     indexdt = "indexdt",
                     db.open = aurum_extract)

  testthat::expect_equal(nrow(var), 6)
  testthat::expect_equal(colnames(var), c("patid", "diabetes"))
  testthat::expect_equal(as.character(var$diabetes), c("Type1", "Absent", "Absent", "Absent", "Type1", "Absent"))

})


###
### Smoking
testthat::test_that("Smoking", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Extract smoking
  var <- extract_smoking(cohort = pat,
                         codelist.non.vector = 498521000006119,
                         codelist.ex.vector = 401539014,
                         codelist.light.vector = 128011000000115,
                         codelist.mod.vector = 380389013,
                         codelist.heavy.vector = 13483031000006114,
                          indexdt = "indexdt",
                          db.open = aurum_extract)

  testthat::expect_equal(nrow(var), 6)
  testthat::expect_equal(colnames(var), c("patid", "smoking"))
  testthat::expect_equal(as.character(var$smoking), c("Heavy", "Non-smoker", NA, "Moderate", "Ex-smoker", "Moderate"))

})


###
### Impotence
testthat::test_that("Impotence", {

  ### Connect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", use.set = FALSE)

  ### Define pat and add index date and censoring date
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
  pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
  pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")

  ### Extract impotence
  var <- extract_impotence(cohort = pat,
                         codelist.med.vector = 498521000006119,
                         codelist.drug.vector = 3092241000033113,
                         indexdt = "indexdt",
                         db.open = aurum_extract)

  testthat::expect_equal(nrow(var), 6)
  testthat::expect_equal(colnames(var), c("patid", "impotence"))
  testthat::expect_equal(var$impotence, c(1, 0, 0, 0, 1, 0))

})
