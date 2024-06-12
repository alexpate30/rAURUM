
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rAURUM

<!-- badges: start -->

[![R-CMD-check](https://github.com/alexpate30/rAURUM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alexpate30/rAURUM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/alexpate30/rAURUM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/alexpate30/rAURUM?branch=main)
<!-- badges: end -->

The goal of rAURUM is to simplify the process of extracting and
processing CPRD Aurum data into an ‘analysis-ready’ dataset which can be
used for statistical analyses. This process is somewhat difficult in R,
as the raw data is very large, provided in a large number of .txt files,
which cannot all be read into the R workspace. rAURUM utilises
[RSQLite](\url%7Bhttps://CRAN.R-project.org/package=RSQLite%7D) to
create SQLite databases which are stored on the hard disk. These are
then queried to extract the required information for a cohort of
interest. The processes follow closely that from the
[rEHR](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5323003/) package,
which was designed for extractin CPRD GOLD data, and is no longer
available on CRAN.

For a detailed guide on how to use **rAURUM** please see the [user-guide
vignette](https://alexpate30.github.io/rAURUM/articles/rAURUM.html).

## Installation

You can install the development version of rAURUM from
[GitHub](https://github.com/alexpate30/rAURUM) with:

``` r
# install.packages("devtools")
devtools::install_github("alexpate30/rAURUM")
```

The package is not yet available on CRAN.

## Example

This is a basic example which shows you how to create a dataset
containing age. All data provided with package and utilised in this
example is simulated.

Load rAURUM:

``` r
library(rAURUM)
#> Loading required package: data.table
```

Create cohort based on patient files:

``` r
pat <- extract_cohort(filepath = system.file("aurum_data", package = "rAURUM"))
str(pat)
#> 'data.frame':    12 obs. of  12 variables:
#>  $ patid         : chr  "1" "2" "3" "4" ...
#>  $ pracid        : int  49 79 98 53 62 54 49 79 98 53 ...
#>  $ usualgpstaffid: chr  "6" "11" "43" "72" ...
#>  $ gender        : int  2 1 1 2 2 1 2 1 1 2 ...
#>  $ yob           : int  1984 1932 1930 1915 1916 1914 1984 1932 1930 1915 ...
#>  $ mob           : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ emis_ddate    : Date, format: "1976-11-21" "1979-02-14" ...
#>  $ regstartdate  : Date, format: "1940-07-24" "1929-02-23" ...
#>  $ patienttypeid : int  58 21 81 10 45 85 58 21 81 10 ...
#>  $ regenddate    : Date, format: "1996-08-25" "1945-03-19" ...
#>  $ acceptable    : int  1 0 1 0 0 1 1 0 1 0 ...
#>  $ cprd_ddate    : Date, format: "1935-03-17" "1932-02-05" ...
```

Connect to an SQLite database (in this example, we create a temporary
file):

``` r
aurum_extract <- connect_database(tempfile("temp.sqlite"))
```

Read in medical data (from the observation files) and add to the SQLite
database.

``` r
cprd_extract(db = aurum_extract, 
             filepath = system.file("aurum_data", package = "rAURUM"), 
             filetype = "observation")
#> [1] "Adding C:/Program Files/R/R-4.4.0/library/rAURUM/aurum_data/aurum_allpatid_set1_extract_observation_001.txt 2024-06-12 16:01:24.008187"
#> [1] "Adding C:/Program Files/R/R-4.4.0/library/rAURUM/aurum_data/aurum_allpatid_set1_extract_observation_002.txt 2024-06-12 16:01:24.244836"
#> [1] "Adding C:/Program Files/R/R-4.4.0/library/rAURUM/aurum_data/aurum_allpatid_set1_extract_observation_003.txt 2024-06-12 16:01:24.427172"
```

Query the database for specific codes and store in an R object using the
`db_query` function:

``` r
### Create codelist
codelist <- "187341000000114"

### Query for observations with this code
db_query(db.open = aurum_extract,
         tab ="observation",
         codelist.vector = codelist)
#>     patid consid pracid  obsid obsdate enterdate staffid parentobsid
#>    <char> <char>  <int> <char>   <num>     <num>  <char>      <char>
#> 1:      1     42      1     81   -5373      4302      85          35
#> 2:      2     56      1     77   -5769    -13828      24           4
#> 3:      6     40      1     41  -14727     -6929      98          80
#>          medcodeid value numunitid obstypeid numrangelow numrangehigh probobsid
#>             <char> <num>     <int>     <int>       <num>        <num>    <char>
#> 1: 187341000000114    84        79        67          24           22         5
#> 2: 187341000000114    46        92        81          56           30        18
#> 3: 187341000000114    28        20         5          41           97        92
```

Add an index date to the patient file, which we will extract variables
relative to:

``` r
pat$fup_start <- as.Date("01/01/2020", format = "%d/%m/%Y")
```

Extract a ‘history of’ type variable, which will be equal to 1 if an
individual has a record with the specified *medcodeid* prior to the
index date, and equal 0 otherwise.

``` r
ho <- extract_ho(pat, 
                 codelist.vector = codelist, 
                 indexdt = "fup_start", 
                 db.open = aurum_extract, 
                 tab = "observation",
                 return.output = TRUE)
str(ho)
#> 'data.frame':    12 obs. of  2 variables:
#>  $ patid: chr  "1" "2" "3" "4" ...
#>  $ ho   : int  1 1 0 0 0 1 0 0 0 0 ...
```

Merge the patient file with the ‘history of’ variable to create an
analysis-ready dataset:

``` r
### Recursive merge
analysis.ready.pat <- merge(pat[,c("patid", "fup_start", "gender")], ho, by.x = "patid", by.y = "patid", all.x = TRUE) 
analysis.ready.pat
#>    patid  fup_start gender ho
#> 1      1 2020-01-01      2  1
#> 2     10 2020-01-01      2  0
#> 3     11 2020-01-01      2  0
#> 4     12 2020-01-01      1  0
#> 5      2 2020-01-01      1  1
#> 6      3 2020-01-01      1  0
#> 7      4 2020-01-01      2  0
#> 8      5 2020-01-01      2  0
#> 9      6 2020-01-01      1  1
#> 10     7 2020-01-01      2  0
#> 11     8 2020-01-01      1  0
#> 12     9 2020-01-01      1  0
```

Currently functionality exists in rAURUM to extract medical data from
the observation file (including specific functions for extracting test
data) and medication data from the drugissue file. Low level functions
exist to allow the user to query the RSQLite database and write their
own functions to define variables of interest. There are mid-level
functions which allow users to extract variables of certain types
(‘history of’, ‘time to event’, and ‘most recent test result’). There
are then very high level functions which allow users to extract specific
variables, such as body mass index, systolic blood pressure, smoking
status, diabetes status, etc. There are all functions where decisions
have been made over how to define variables. Be sure to check the code
to make sure it matches with your definition. For example,
`extract_diabetes` will return a categorical variable with three
categories, `Absent`, `type1` and `type2`. If an individual has a record
for both type 1 and type 2 diabetes (according to the users code lists),
`extract_diabetes` will assign the individual to the group `type1`.

## Package maintainence

This parts of this package which create the SQLite database are somewhat
dependent on the structure of the raw CPRD Aurum data. For example, the
functions to read in the raw text files (e.g. `extract_txt_obs`) are
hard coded to format variables with specific names in a certain way
(e.g. convert `obsdate` from a character variable to a date variable).
Over time, the structure of the CPRD Aurum data may change, which could
impact the utility of this package. We will endeavor to keep rAURUM
updated with new releases of CPRD Aurum. However, where possible, we
have tried to protect against this by giving the user flexible options
as well as the defaults. For example, `add_to_database` defaults to
using `extract_txt_obs` to read in the raw text data when
`filetype = "observation"` is specified. However, there is also an
option `extract.txt.func`, which allows the users to specify their own
function to read in the text data, and will override the use of
`extract_txt_obs`.

Despite this, there may have been breaking points we haven’t thought of,
in which case please let us know.

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/alexpate30/rAURUM).
