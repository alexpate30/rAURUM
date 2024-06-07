###
### Tests for variable extraction programs
###
testthat::test_that("Test low-level db query functions", {

  ### Create sqlite database

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
  testthat::expect_equal(time_until$var_time, c(106, 16436,  16436,  16436,  16436,  16436))
  testthat::expect_equal(time_until$var_indicator, c(1, 0,  0,  0,  0,  0))

  ### Extract test data using extract_test_data
  test_data <- extract_test_data(pat,
                                 codelist.vector = codelist,
                                 indexdt = "fup_start",
                                 db.open = aurum_extract,
                                 time.prev = Inf,
                                 return.output = TRUE)

  testthat::expect_equal(nrow(time_until), 6)
  testthat::expect_equal(test_data$value, c(NA, 46,  NA,  NA,  NA,  28))

})
