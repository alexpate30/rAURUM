###
### Tests for variable extraction programs
###
testthat::test_that("Test low-level db query functions", {

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

  ### Extract test data using extract_test_data
  test_data <- extract_test_data(pat,
                                 codelist.vector = codelist,
                                 indexdt = "fup_start",
                                 db.open = aurum_extract,
                                 time.prev = Inf,
                                 return.output = TRUE)

  testthat::expect_equal(nrow(test_data), 6)
  testthat::expect_equal(colnames(ho), c("patid", "value"))
  testthat::expect_equal(test_data$value, c(NA, 46,  NA,  NA,  NA,  28))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ###
  ### Create a temporary directory to re-run these functions and save to disk automatically, and automatically look for SQLite database in data/sql
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

})
