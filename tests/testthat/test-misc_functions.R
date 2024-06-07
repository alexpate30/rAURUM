testthat::test_that("Extract multiple ways and expect equivalence. Testing add_to_database and cprd_extract", {

  ###
  ### Attempt 1

  ### Open connection
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Add observation and drugissue to database manually
  ## Obs
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_observation_001.txt", package = "rAURUM"),
                  filetype = "observation", nrows = -1, select = NULL, subset.patids = c(1,3,4,6), use.set = FALSE, db = aurum_extract, overwrite = TRUE)
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_observation_002.txt", package = "rAURUM"),
                  filetype = "observation", subset.patids = c(1,3,4,6), db = aurum_extract, append = TRUE)
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_observation_003.txt", package = "rAURUM"),
                  filetype = "observation", subset.patids = c(1,3,4,6), db = aurum_extract, append = TRUE)
  ## Drugissue
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_drugissue_001.txt", package = "rAURUM"),
                  filetype = "drugissue", nrows = -1, select = NULL, subset.patids = c(1,3,4,6), use.set = FALSE, aurum_extract, overwrite = TRUE)
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_drugissue_002.txt", package = "rAURUM"),
                  filetype = "drugissue", nrows = -1, select = NULL, subset.patids = c(1,3,4,6), use.set = FALSE, aurum_extract, append = TRUE)
  add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_drugissue_003.txt", package = "rAURUM"),
                  filetype = "drugissue", nrows = -1, select = NULL, subset.patids = c(1,3,4,6), use.set = FALSE, aurum_extract, append = TRUE)

  ### Save output
  obs1 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation')
  drugissue1 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM drugissue')
  testthat::expect_equal(RSQLite::dbListTables(aurum_extract), c("drugissue", "observation"))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ###
  ### Attempt 2

  ### Reconnect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", subset.patids = c(1,3,4,6), use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", subset.patids = c(1,3,4,6), use.set = FALSE)

  ### Save output
  obs2 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation')
  drugissue2 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM drugissue')
  testthat::expect_equal(RSQLite::dbListTables(aurum_extract), c("drugissue", "observation"))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ###
  ### Attempt 3

  ### Reconnect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Define pat
  pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"), set = TRUE)
  pat <- subset(pat, patid %in% c(1,3,4,6))

  ### Add observation files
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", nrows = -1, select = NULL, subset.patids = pat, use.set = TRUE)

  ### Add drugissue files
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", nrows = -1, select = NULL, subset.patids = pat, use.set = TRUE)

  ### Save output
  obs3 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation')
  drugissue3 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM drugissue')
  testthat::expect_equal(RSQLite::dbListTables(aurum_extract), c("drugissue", "observation"))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ###
  ### Attempt 4 (manually define str.match and extract.txt.func)

  ### Reconnect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "observation", subset.patids = c(1,3,4,6),
               extract.txt.func = extract_txt_obs, str.match = "observation", use.set = FALSE)

  cprd_extract(aurum_extract,
               filepath = system.file("aurum_data", package = "rAURUM"),
               filetype = "drugissue", subset.patids = c(1,3,4,6),
               extract.txt.func = extract_txt_drug, str.match = "drugissue", use.set = FALSE)

  ### Save output
  obs4 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation')
  drugissue4 <- RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM drugissue')
  testthat::expect_equal(RSQLite::dbListTables(aurum_extract), c("drugissue", "observation"))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

  ### Test for equivalence between extracts
  testthat::expect_equal(obs1, obs2)
  testthat::expect_equal(obs1, obs3)
  testthat::expect_equal(obs1, obs4)
  testthat::expect_equal(drugissue1, drugissue2)
  testthat::expect_equal(drugissue1, drugissue3)
  testthat::expect_equal(drugissue1, drugissue4)

  ###
  ### Attempt 6 (expect an error due to no filenames)

  ### Reconnect
  aurum_extract <- connect_database(tempfile("temp.sqlite"))

  ### Extract data using cprd_Extract
  testthat::expect_error(
    cprd_extract(aurum_extract,
                 filepath = system.file("aurum_data", package = "rAURUM"),
                 filetype = "observation", subset.patids = c(1,3,4,6),
                 str.match = "eggs", use.set = FALSE))

  ### Disconnect
  RSQLite::dbDisconnect(aurum_extract)

})

###
### Tests for db queries
###
testthat::test_that("Test low-level db query functions", {

  ### Currently empty as thinking lots of these will be covered by the extract variable functions
  ### Add here later if some functionaliy is missing

})




