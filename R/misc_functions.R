#' Adds a single .txt file to an SQLite database on the hard disk.
#'
#' @description
#' Add the raw data from one of the CPRD flatfiles to an SQLite database.
#'
#' @param filepath Path to .txt file on your system.
#' @param filetype Type of CPRD Aurum file (observation, drugissue, referral, problem, consultation, hes_primary, death)
#' @param nrows Number of rows to read in from .txt file.
#' @param select Character vector of column names to select before adding to the SQLite database.
#' @param subset.patids Patient id's to subset the .txt file on before adding to the SQLite database.
#' @param use.set Reduce subset.patids to just those with a corresponding set value to the .txt file being read in. Can greatly improve computational efficiency when subset.patids is large. See vignette XXXX for more details.
#' @param db  An open SQLite database connection created using RSQLite::dbConnect.
#' @param extract.txt.func User-defined function to read the .txt file into R.
#' @param tablename Name of table in SQLite database that the data will be added to.
#' @param ... Extract arguments passed to read.table (or extract.txt.func) when reading in .txt files.
#'
#' @returns Adds .txt file to SQLite database on hard disk.
#'
#' @details
#' Will add the file to a table named `filetype` in the SQLite database, unless `tablename` is specified.
#'
#' If `use.set = FALSE`, then `subset.patids` should be a vector of patid's that the .txt files will be subsetted on before adding to the SQLite database.
#' If `use.set = TRUE`, then `subset.patids` should be a dataframe with two columns, `patid` and `set`, where `set` corresponds to the number in the file name
#' following the word 'set'. This functionality is provided to increase computational efficiency when subsetting to a cohort of patients which is very large (millions).
#' This can be a computationally expensive process as each flatfile being read in, must be cross matched with a large vector .
#' The CPRD flatfiles are split up into groups which can be identified from their naming convention. Patients from set 1, will have their data
#' in DrugIssue, Observation, etc, all with the same "set" suffix in the flatfile name. We can utilise this to speed up the process of subsetting
#' the data from the flatfiles to only those with patids in subset.patid. Instead we subset to those with patids in subset.patids, and with the
#' corresponding value of "set", which matches the suffix "set" in the CPRD flatfile filename. For example, patients in the Patient file which had
#' suffix "set1", will have their medical data in the Observation file with suffix "set1". When subsetting the Observation file to those in
#' subset.patids (our cohort), we only need to do so for patients who were also in the patient file with suffix "set1".
#' If the cohort of patients for which you want to subset the data to is very small, the computational gains from this argument are minor and it
#' can be ignored.
#'
#' The function for reading in the .txt file will be chosen from a set of functions provided with rAURUM, based on  the fletype (`filetype`).
#' `extract.txt.func` does not need to be specified unless wanting to manually define the function for doing this. This may be beneficial if wanting to
#' change variable formats, or if the variables in the .txt files change in future releases of CPRD AURUM.
#'
#' @examples
#' ## Create connection to a temporary database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Add observation data
#' add_to_database(filepath = system.file("aurum_data",
#' "aurum_allpatid_set1_extract_observation_001.txt", package = "rAURUM"),
#' filetype = "observation", db = aurum_extract, overwrite = TRUE)
#'
#' ## Query database
#' RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation', n = 3)
#'
#' @export
add_to_database <- function(filepath,
                            filetype = c("observation", "drugissue", "referral", "problem", "consultation", "hes_primary","death"),
                            nrows = -1,
                            select = NULL,
                            subset.patids = NULL,
                            use.set = FALSE,
                            db,
                            extract.txt.func = NULL,
                            tablename = NULL,
                            ...){

  ### Check filetype
  filetype <- match.arg(filetype)

  ### Use the extract_txt function for the relevant filetype, unless otherwise stated by user
  if (is.null(extract.txt.func)){
    extract_txt_func <- list("observation" = extract_txt_obs,
                             "drugissue" = extract_txt_drug,
                             "referral" = extract_txt_ref,
                             "problem" = extract_txt_prob,
                             "consultation" = extract_txt_cons,
                             "hes_primary" = extract_txt_hes_primary,
                             "death" = extract_txt_death)[[filetype]]
  } else {
    extract_txt_func <- extract.txt.func
  }

  ### Read data into R and turn file into data.table for quicker subsetting
  ext.dat <- extract_txt_func(filepath = filepath, nrows = nrows, select = select)
  ext.dat <- data.table::as.data.table(ext.dat)

  ### Reduce to only patids from cohort of interest
  if (!is.null(subset.patids)){

    ### Reduce subset.patids input to just those from the appropriate 'set' if the filepaths are compatible
    ### This will make subsetting ext.dat to the patients %in% subset.patids a lot quicker
    if (use.set == TRUE){
      ### Turn into data.table
      subset.patids <- data.table::as.data.table(subset.patids)
      ### Extract the 'set' from the filepath
      set.filepath <- as.numeric(stringr::str_match(filepath, "set\\s*(.*?)\\s*_")[,2])
      ### Apply subsetting
      subset.patids <- subset.patids[!is.na(fastmatch::fmatch(subset.patids$set, set.filepath)), ]
      subset.patids <- subset.patids$patid
      #subset.patids <- subset.patids[set == set.filepath, patid]
    }
    ### Subset the data to those observations with patid in subset.patids
    ext.dat <- ext.dat[!is.na(fastmatch::fmatch(ext.dat$patid, subset.patids)), ]
    #ext.dat <- ext.dat[patid %in% subset.patids]
    #ext.dat <- subset(ext.dat, patid %in% subset.patids)
  }

  ### Add to sqlite database
  if (is.null(tablename)){
    RSQLite::dbWriteTable(db, filetype, ext.dat, ...)
  } else {
    RSQLite::dbWriteTable(db, tablename, ext.dat, ...)
  }

}


#' Adds all the .txt files in a directory, with certain file names, to an SQLite database on the hard disk.
#'
#' @description
#' Add the raw data from more than one of the CPRD flatfiles to an SQLite database.
#'
#' @param db An open SQLite database connection created using RSQLite::dbConnect.
#' @param filepath Path to directory containing .txt files.
#' @param filetype Type of CPRD Aurum file (observation, drugissue, referral, problem, consultation, hes_primary, death)
#' @param nrows Number of rows to read in from .txt file.
#' @param select Vector of column names to select before adding to the SQLite database.
#' @param subset.patids Patient id's to subset the .txt file on before adding to the SQLite database.
#' @param use.set Reduce subset.patids to just those with a corresponding set value to the .txt file being read in. Can greatly improve computational efficiency when subset.patids is large. See vignette XXXX for more details.
#' @param extract.txt.func User-defined function to read the .txt file into R.
#' @param str.match Character vector to match on when searching for file names to add to the database.
#' @param tablename Name of table in SQLite database that the data will be added to.
#'
#' @returns Adds .txt file to SQLite database on hard disk.
#'
#' @details
#' By default, will add files that contain `filetype` in the file name to a table named `filetype` in the SQLite database.
#' If `str.match` is specified, will add files that contain `str.match` in the file name to a table named `str.match` in the SQLite database.
#' In this case, `filetype` will still be used to choose which function reads in and formats the raw data, although this can be overwritten with
#' `extract.txt.func`. If argument `tablename` is specified, data will be added to a table called `tablename` in the SQLite database.
#'
#' Currently, rAURUM only deals with `filetype = c("observation", "drugissue", "referral", "problem", "consultation", "hes_primary", "death")` by default.
#' However, by using `str.match` and `extract.txt.func`, the user can manually search for files with any string in the file name, and read them in
#' and format using a user-defined function. This means the user is not restricted to only adding the pre-defined file types to the SQLite database.
#'
#' If `use.set = FALSE`, then `subset.patids` should be a vector of patid's that the .txt files will be subsetted on before adding to the SQLite database.
#' If `use.set = TRUE`, then `subset.patids` should be a dataframe with two columns, `patid` and `set`, where `set` corresponds to the number in the file name
#' following the word 'set'. This functionality is provided to increase computational efficiency when subsetting to a cohort of patients which is very large (millions).
#' This can be a computationally expensive process as each flatfile being read in, must be cross matched with a large vector .
#' The CPRD flatfiles are split up into groups which can be identified from their naming convention. Patients from set 1, will have their data
#' in DrugIssue, Observation, etc, all with the same "set" suffix in the flatfile name. We can utilise this to speed up the process of subsetting
#' the data from the flatfiles to only those with patids in subset.patid. Instead we subset to those with patids in subset.patids, and with the
#' corresponding value of "set", which matches the suffix "set" in the CPRD flatfile file name. For example, patients in the Patient file which had
#' suffix "set1", will have their medical data in the Observation file with suffix "set1". When subsetting the Observation file to those in
#' subset.patids (our cohort), we only need to do so for patients who were also in the patient file with suffix "set1".
#' If the cohort of patients for which you want to subset the data to is very small, the computational gains from this argument are minor and it
#' can be ignored.
#'
#' The function for reading in the .txt file will be chosen from a set of functions provided with rAURUM, based on  the filetype (`filetype`).
#' `extract.txt.func` does not need to be specified unless wanting to manually define the function for doing this. This may be beneficial if wanting to
#' change variable formats, or if the variables in the .txt files change in future releases of CPRD AURUM and rAURUM has not been updated.
#'
#' @examples
#' ## Create connection to a temporary database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Add observation data from all observation files in specified directory
#' cprd_extract(db = aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation")
#'
#' ## Query database
#' RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation', n = 3)
#'
#' @export
cprd_extract <- function(db,
                         filepath,
                         filetype = c("observation", "drugissue", "referral", "problem", "consultation", "hes_primary", "death"),
                         nrows = -1,
                         select = NULL,
                         subset.patids = NULL,
                         use.set = FALSE,
                         extract.txt.func = NULL,
                         str.match = NULL,
                         tablename = NULL){

  ### Check filetype
  filetype <- match.arg(filetype)

  # print(paste("nrows", nrows))
  # print(paste("select", select))
  # print(paste("subset.patids", subset.patids))
  # print(paste("use.set", use.set))

  ### Spit error out if subset.patids not in correct form
  if (!is.null(subset.patids)){
    if (use.set == FALSE){
      if (!is.vector(subset.patids)){
        stop("When use.set = FALSE, subset.patids should be a vector")
      }
    } else if (use.set == TRUE){
      if (!("patid" %in% colnames(subset.patids) & "set" %in% colnames(subset.patids))){
        stop("When use.set = TRUE, subset.patids should contain columns 'patid' and 'set'")
      }
    }
  }

  ### Get file names of files to be added
  filenames <- list.files(filepath, pattern = ".txt", full.names = TRUE)
  if (is.null(str.match)){
    filenames <- filenames[stringr::str_detect(filenames, filetype)]
  } else {
    filenames <- filenames[stringr::str_detect(filenames, str.match)]
    if (is.null(tablename)){
      tablename <- str.match
    }
  }

  ### Initialise progress bar
  pb <- utils::txtProgressBar(min = 0,
                              max = length(filenames),
                              style = 3,
                              char = "=")

  ### Apply the add_to_database function to each file
  if (length(filenames) >= 1){
    print(paste("Adding", filenames[1], Sys.time()))
    ### Assign counter for progress bar
    progress <- 1
    ## Overwrite for first file
    add_to_database(filenames[1],
                    filetype = filetype,
                    nrows = nrows,
                    select = select,
                    subset.patids = subset.patids,
                    use.set = use.set,
                    db = db,
                    extract.txt.func = extract.txt.func,
                    tablename = tablename,
                    overwrite = TRUE)
    ### Print progress bar
    utils::setTxtProgressBar(pb, progress)
    ### Loop through all subsequent files
    if (length(filenames) > 1){
      for (filename in filenames[-1]){
        print(paste("Adding", filename, Sys.time()))
        ## Append for all subsequent files
        add_to_database(filename,
                        filetype = filetype,
                        nrows = nrows,
                        select = select,
                        subset.patids = subset.patids,
                        use.set = use.set,
                        db = db,
                        extract.txt.func = extract.txt.func,
                        tablename = tablename,
                        append = TRUE)
        ## Add progress to progress bar
        progress <- progress + 1
        utils::setTxtProgressBar(pb, progress)
      }
    }
  } else if (length(filenames) == 0){
    stop("No files to import")
  }

}

#' Query an RSQLite database.
#'
#' @description
#' Query an RSQLite database stored on the hard disk for observations with specific codes.
#'
#' @param codelist Name of codelist to query the database with.
#' @param db.open An open SQLite database connection created using RSQLite::dbConnect, to be queried.
#' @param db Name of SQLITE database on hard disk, to be queried.
#' @param db.filepath Full filepath to SQLITE database on hard disk, to be queried.
#' @param tab Name of table in SQLite database that is to be queried.
#' @param codelist.vector Vector of codes to query the database with. This takes precedent over `codelist` if both are specified.
#'
#' @details
#' Specifying `db` requires a specific underlying directory structure. The SQLite database must be stored in "data/sql/" relative to the working directory.
#' If the SQLite database is accessed through `db`, the connection will be opened and then closed after the query is complete. The same is true if
#' the database is accessed through `db.filepath`. A connection to the SQLite database can also be opened manually using `RSQLite::dbConnect`, and then
#' using the object as input to parameter `db.open`. After wards, the connection must be closed manually using `RSQLite::dbDisconnect`. If `db.open` is specified, this will take precedence over `db` or `db.filepath`.
#'
#' Specifying `codelist` requires a specific underlying directory structure. The codelist on the hard disk must be stored in "codelists/analysis/" relative
#' to the working directory, must be a .csv file, and contain a column "medcodeid", "prodcodeid" or "ICD10" depending on the chosen `tab`. The codelist can
#' also be read in manually, and supplied as a character vector to `codelist.vector`. If `codelist.vector` is defined, this will take precedence over `codelist`.
#'
#' @returns A data.table with observations contained in the specified codelist.
#'
#' @examples
#' ## Create connection to a temporary database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Add observation data from all observation files in specified directory
#' cprd_extract(db = aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation")
#'
#' ## Query database for a specific medcode
#' db_query(db.open = aurum_extract,
#' tab ="observation",
#' codelist.vector = "187341000000114")
#'
#' @export
db_query <- function(codelist,
                     db.open = NULL,
                     db = NULL,
                     db.filepath = NULL,
                     tab = c("observation", "drugissue", "hes_primary", "death"),
                     codelist.vector = NULL){

  ### Extract codelist
  if (is.null(codelist.vector)){
    codelist <- data.table::fread(file = paste(getwd(),"/codelists/analysis/", codelist, ".csv", sep = ""),
                                  sep = ",", header = TRUE, colClasses = "character")
    if (tab == "observation"){
      codelist <- codelist$medcodeid
    } else if (tab == "drugissue"){
      codelist <- codelist$prodcodeid
    } else if (tab %in% c("hes_primary", "death")){
      codelist <- codelist$ICD10
    }
  } else {
    codelist <- codelist.vector
  }

  ### Connect to SQLite database
  if (is.null(db.open)){
    if (!is.null(db)){
      mydb <- RSQLite::dbConnect(RSQLite::SQLite(), paste("data/sql/", db, ".sqlite", sep = ""))
    } else if (!is.null(db.filepath)){
      mydb <- RSQLite::dbConnect(RSQLite::SQLite(), db.filepath)
    }
  } else {
    mydb <- db.open
  }

  ### Create the query
  if (tab == "observation"){
    where_clause <- paste0("`medcodeid` IN (", paste("'", codelist, "'", sep = "", collapse = ","), ")")
    qry <- paste("SELECT * FROM", tab, "WHERE", where_clause)
  } else if (tab == "drugissue"){
    where_clause <- paste0("`prodcodeid` IN (", paste("'", codelist, "'", sep = "", collapse = ","), ")")
    qry <- paste("SELECT * FROM", tab, "WHERE", where_clause)
  } else if (tab == "hes_primary"){
    where_clause <- paste0("`ICD_PRIMARY` IN (", paste("'", codelist, "'", sep = "", collapse = ","), ")")
    qry <- paste("SELECT * FROM", tab, "WHERE", where_clause)
  } else if (tab == "death"){
    where_clause <- paste0("`cause` IN (", paste("'", codelist, "'", sep = "", collapse = ","), ")")
    qry <- paste("SELECT * FROM", tab, "WHERE", where_clause)
  }

  ### Run the query and turn into data.table
  db.query <- RSQLite::dbGetQuery(mydb, qry)
  db.query <- data.table::as.data.table(db.query)

  ### Disconnect
  if (is.null(db.open)){
    RSQLite::dbDisconnect(mydb)
  }

  ### Return query
  return(db.query)

}


#' Combine a database query with a cohort returning a 0/1 vector depending on whether each individual has a recorded code of interest.
#'
#' @description
#' Combine a database query with a cohort returning a 0/1 vector depending on whether each individual has a recorded code of interest.
#' `cohort` must contain variables `patid` and `indexdt`. The database query will be merged with the cohort by variable \code{patid}.
#' If an individual has at least `numobs` observations between `time.prev` days prior to \code{indexdt}, and `time.post` days after
#'  \code{indexdt}, a 1 will be returned, 0 otherwise. The `type` of query must be specified for appropriate data manipulation.
#'
#' @param cohort Cohort to combine with the database query.
#' @param db.query Output from database query (ideally obtained through \code{\link{db_query}}).
#' @param query.type Type of query
#' @param time.prev Number of days prior to index date to look for codes.
#' @param time.post Number of days after index date to look for codes.
#' @param numobs Number of observations required to be observed in specified time window to return a 1.
#'
#' @returns A 0/1 vector.
#'
#' @examples
#' ## Create connection to a temporary database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Add observation data from all observation files in specified directory
#' cprd_extract(db = aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation")
#'
#' ## Query database for a specific medcode
#' db.query <- db_query(db.open = aurum_extract,
#' tab ="observation",
#' codelist.vector = "187341000000114")
#'
#' ## Define cohort
#' pat<-extract_cohort(filepath = system.file("aurum_data", package = "rAURUM"))
#'
#' ### Add an index date to pat
#' pat$indexdt <- as.Date("01/01/2020", format = "%d/%m/%Y")
#'
#' ## Combine query with cohort creating a 'history of' boolean variable
#' combine_query_boolean(cohort = pat,
#' db.query = db.query,
#' query.type = "observation")
#'
#' @export
combine_query_boolean <- function(cohort,
                                  db.query,
                                  query.type = c("observation", "drugissue", "hes_primary"),
                                  time.prev = Inf,
                                  time.post = 0,
                                  numobs = 1){

  ### Merge cohort with the database query keeping observations that are in both
  cohort.qry <- merge(cohort, db.query, by.x = "patid", by.y = "patid")
  cohort.qry <- data.table::as.data.table(cohort.qry)

  ### Reduce to variables of interest
  if (query.type == c("observation")){
    cohort.qry <- cohort.qry[,c("patid", "indexdt", "obsdate")]
  } else if (query.type == "drugissue"){
    cohort.qry <- cohort.qry[,c("patid", "indexdt", "issuedate")]
    ## rename issuedate to obsdate so we can use same code for medical or drug queries
    colnames(cohort.qry)[colnames(cohort.qry) == "issuedate"] <- "obsdate"
  } else if (query.type == "hes_primary"){
    cohort.qry <- cohort.qry[,c("patid", "indexdt", "admidate")]
    ## rename issuedate to obsdate so we can use same code for medical or drug queries
    colnames(cohort.qry)[colnames(cohort.qry) == "admidate"] <- "obsdate"
  }

  ### Remove values outside of specified time range
  cohort.qry <- cohort.qry[obsdate <= indexdt + time.post & obsdate > indexdt - time.prev]

  ### Identify which patients have had the required number of events in the specified time period
  cohort.qry <- cohort.qry |>
    dplyr::group_by(patid) |>
    dplyr::summarise(total = dplyr::n()) |>
    dplyr::filter(total >= numobs) |>
    dplyr::pull(patid)

  ### Create a 0/1 vector for whether patients are in this list and add to cohort
  cohort.qry <- as.integer(!is.na(fastmatch::fmatch(cohort$patid, cohort.qry)))

  ### Return this vector
  return(cohort.qry)

}


#' Combine a database query with a cohort.
#'
#' @description
#' Combine a database query with a cohort, only retaining observations between `time.prev` days prior to \code{indexdt}, and `time.post` days after
#'  \code{indexdt}, and for test data with values between `lower.bound` and `upper.bound`. The most recent `numobs` observations will be returned.
#'  `cohort` must contain variables `patid` and `indexdt`. The `type` of query must be specified for appropriate data manipulation. Input `type = med` if
#'  interested in medical diagnoses from the observation file, and `type = test` if interseted in test data from the observation file.
#'
#' @param cohort Cohort to combine with the database query.
#' @param db.query Output from database query (ideally obtained through \code{\link{db_query}}).
#' @param query.type Type of query
#' @param time.prev Number of days prior to index date to look for codes.
#' @param time.post Number of days after index date to look for codes.
#' @param lower.bound Lower bound for returned values when `query.type = "test"`.
#' @param upper.bound Upper bound for returned values when `query.type = "test"`.
#' @param numobs Number of observations to be returned.
#' @param value.na.rm If TRUE will remove data with NA in the \code{value} column of the queried data and remove values outside of `lower.bound` and `upper.bound` when `query.type = "test"`.
#' @param earliest.values If TRUE will return the earliest values as opposed to most recent.
#' @param reduce.output If TRUE will reduce output to just `patid`, `obsdate`, medical/product code, and test `value`.
#'
#' @details `value.na.rm = FALSE` may be of use when extracting variables like smoking status, where we want test data for number of cigarettes per day,
#' but do not want to remove all observations with NA in the \code{value} column, because the medcodeid itself may indicate smoking status.
#'
#' @returns A data.table with observations that meet specified criteria.
#'
#' @examples
#' ## Create connection to a temporary database
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Add observation data from all observation files in specified directory
#' cprd_extract(db = aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation")
#'
#' ## Query database for a specific medcode
#' db.query <- db_query(db.open = aurum_extract,
#' tab ="observation",
#' codelist.vector = "187341000000114")
#'
#' ## Define cohort
#' pat<-extract_cohort(filepath = system.file("aurum_data", package = "rAURUM"))
#'
#' ### Add an index date to pat
#' pat$indexdt <- as.Date("01/01/2020", format = "%d/%m/%Y")
#'
#' ## Combine query with cohort retaining most recent three records
#' combine_query(cohort = pat,
#' db.query = db.query,
#' query.type = "med",
#' numobs = 3)
#'
#' @export
combine_query <- function(cohort,
                          db.query,
                          query.type = c("med", "test", "drugissue", "hes_primary", "death"),
                          time.prev = Inf,
                          time.post = Inf,
                          lower.bound = -Inf,
                          upper.bound = Inf,
                          numobs = 1,
                          value.na.rm = TRUE,
                          earliest.values = FALSE,
                          reduce.output = TRUE){

  ### Merge cohort with the database query keeping observations that are in both
  cohort.qry <- merge(cohort, db.query, by.x = "patid", by.y = "patid")
  cohort.qry <- data.table::as.data.table(cohort.qry)

  ### Rename event date variables to all be "obsdate"
  if (query.type == c("med")){

  } else if (query.type == "test"){

  } else if (query.type == "drugissue"){
    ## rename issuedate to obsdate so we can use same naming for all queries
    colnames(cohort.qry)[colnames(cohort.qry) == "issuedate"] <- "obsdate"
  } else if (query.type == "hes_primary"){
    ## rename admidate to obsdate so we can use same naming for all queries
    colnames(cohort.qry)[colnames(cohort.qry) == "admidate"] <- "obsdate"
  }  else if (query.type == "death"){
    ## rename dod to obsdate so we can use same naming for all queries
    colnames(cohort.qry)[colnames(cohort.qry) == "dod"] <- "obsdate"
  }

  ### Remove values outside of specified time range
  cohort.qry <- cohort.qry[obsdate <= indexdt + time.post & obsdate > indexdt - time.prev]

  ### For test data, remove values outside of value range
  if (query.type == "test"){
    ### If values are missing, < lower.bound or > upper.bound then delete
    if (value.na.rm == TRUE){
      cohort.qry <- cohort.qry[!is.na(value) & value > lower.bound & value < upper.bound]
    }
  }

  ### Note that if value.na.rm = FALSE, we will skip this step and keep all test results including NA's.
  ### This will be of use for deriving smoking status

  ### Reduce to variables of interest
  if (reduce.output == TRUE){
    if (query.type == c("med")){
      cohort.qry <- cohort.qry[,c("patid", "medcodeid", "obsdate")]
    } else if (query.type == "test"){
      cohort.qry <- cohort.qry[,c("patid", "medcodeid", "obsdate", "value", "numunitid", "numrangelow", "numrangehigh")]
    } else if (query.type == "drugissue"){
      cohort.qry <- cohort.qry[,c("patid", "prodcodeid", "obsdate")]
    } else if (query.type == "hes_primary"){
      cohort.qry <- cohort.qry[,c("patid", "obsdate")]
    }  else if (query.type == "death"){
      cohort.qry <- cohort.qry[,c("patid", "obsdate")]
    }
  }

  ### Group by patid and obsdate and keep the most recent 'numobs' number of observations
  if (earliest.values == FALSE){
    cohort.qry <- dplyr::group_by(cohort.qry, patid) |>
      dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) <= numobs) |>
      data.table::as.data.table()
  } else if (earliest.values == TRUE){
    cohort.qry <- dplyr::group_by(cohort.qry, patid) |>
      dplyr::filter(dplyr::row_number(obsdate) <= numobs) |>
      data.table::as.data.table()
  }

  ### Return cohort.qry
  return(cohort.qry)

}

#' Internal function to prepare cohort when extracting variables.
#'
#' @description
#' Changes the name of the variable specified to be the index date to "indexdt".
#' If `t` has been specified (number of days after index date at which to extract data), `t` is added to `indexdt`.
#'
#' @param cohort Cohort for which variables are to be extracted.
#' @param indexdt Name of the variable specified to be the index date.
#' @param t Number of days after index date at which to extract data.
#' @param reduce Reduce to `patid` and `indexdt`.
#'
#' @noRd
prep_cohort <- function(cohort, indexdt, t, reduce = TRUE){

  ### Change name of indexdt variable to "indexdt" so we can easily refer to it
  colnames(cohort)[colnames(cohort) == indexdt] <- "indexdt"

  ### Reduce cohort to variables of interest
  if (reduce == TRUE){
    cohort <- cohort[,c("patid", "indexdt")]
  }

  ### If t has been specified, round it to nearest integer, add to indexdt to extract data at appropriate time
  if (!is.null(t)){
    t <- round(t)
    cohort$indexdt <- cohort$indexdt + t
  }

  return(cohort)

}

#' Internal function to prepare variable name.
#'
#' @description
#' Adds the value of `t` to variable name is `t.varname = TRUE`. May be of use if landmarking and extracting cohort at various intervals post baseline.
#'
#' @noRd
prep_varname <- function(varname, t, t.varname){
  if (!is.null(t)){
    if (t.varname == TRUE){
      varname <- paste(varname, "_t", t, sep = "")
    }
  }
  return(varname)
}

#' Internal function to prepare subdirectory for saving extracted variables to disk.
#'
#' @description
#' If out.subdir has been specified, check if it exists. If not, create directory.
#'
#' @noRd
prep_subdir <- function(out.subdir){
  if (!is.null(out.subdir)){
    if (file.exists(paste("data/extraction/", out.subdir, sep = "")) == FALSE){
      dir.create(paste("data/extraction/", out.subdir, sep = ""))
      print(paste("Directory /data/extraction/", out.subdir, " has been created", sep = ""))
    }
  }
}


#' Internal function to implement saving extracted variable to disk or returning into R workspace.
#'
#' @description
#' Will save extracted variable to disk if `out.save.disk = TRUE`. Note it relies on correct underlying structure
#' of directories. Will output extracted variable into R workspace if `return.output = TRUE`.
#'
#' @param variable.dat Dataset containing variable
#' @param varname Name of variable to use in filename
#' @param out.save.disk If TRUE will save output to disk
#' @param out.subdir Sub-directory of data/ to save output into
#' @param out.filepath Full fiilepath to save dat onto
#' @param return.output If TRUE returns output into R workspace
implement_output <- function(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output){
  ### We now have the final variable file, we just need to decide how and where to save it
  ### Save data frame to disc
  if (out.save.disk == TRUE){
    ## Use standard naming convention if out.filepath is not specified
    if (is.null(out.filepath)){
      ## Save directly into /extraction if a subdirectory is not specified
      if (is.null(out.subdir)){
        saveRDS(variable.dat, paste("data/extraction/var_", varname, ".rds", sep = ""))
        ## Save into the specified subdirectory if specified
      } else if (!is.null(out.subdir)){
        saveRDS(variable.dat, paste("data/extraction/", out.subdir, "/var_", varname, ".rds", sep = ""))
      }
      ## Use the full user specified filepath if specified
    } else if (!is.null(out.filepath)){
      saveRDS(variable.dat, out.filepath)
    }
  }

  ### Return as output of function if requested
  if (return.output == TRUE){
    return(variable.dat)
  }

}


#' Create cohort from patient files
#'
#' @description
#' Create cohort from patient files
#'
#' @param filepath Path to directory containing .txt files.
#' @param patids Patids of patients to retain in the cohort. Character vector. Numeric values should not be used.
#' @param select Character vector of column names to select.
#' @param set If `TRUE` will create a variable called `set` which will contain the number that comes after the word 'set' in the file name.
#'
#' @returns Data frame with patient information
#'
#' @examples
#'
#' ## Extract cohort data
#' pat<-extract_cohort(filepath = system.file("aurum_data", package = "rAURUM"))
#' pat
#'
#' @export
extract_cohort <- function(filepath,
                           patids = NULL,
                           select = NULL,
                           set = FALSE){

  ### Ensure patids isn't numeric
  if (!is.null(patids)){
    if (is.numeric(patids) == TRUE){
      stop("All elements of patids must be character")
    }
  }

  ### Get filenames of patient files
  filenames <- list.files(filepath, pattern = ".txt", full.names = TRUE)

  ### Reduce to those with "patient" in the filename
  filenames <- filenames[stringr::str_detect(filenames, "patient")]

  ### Read in all patient files and concatenate
  if (length(filenames) >= 1){
    pat <-  extract_txt_pat(filenames[1], set = set)
    if (length(filenames) > 1){
      ## Append for all subsequent files
      for (filename in filenames[-1]){
        pat.temp <-  extract_txt_pat(filename, set = set)
        pat <- rbind(pat, pat.temp)
      }
    }
  } else if (length(filenames) == 0){
    stop("No files to import")
  }

  ### Select variables
  if(!is.null(select)){
    ## If set == TRUE, add set to variables to select
    if(set == TRUE){
      if (!("set" %in% names(select))){
        select <- c(select, "set")
      }
    }
    ## Apply selection
    pat <- pat[,select]
  }

  ### Reduce to patients in patids vector
  if (!is.null(patids)){
    pat <- pat[!is.na(fastmatch::fmatch(pat$patid, patids)), ]
  }

  ### return pat
  return(pat)

}
