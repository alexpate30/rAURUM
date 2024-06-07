
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rAURUM

<!-- badges: start -->

[![R-CMD-check](https://github.com/alexpate30/rAURUM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alexpate30/rAURUM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/alexpate30/rAURUM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/alexpate30/rAURUM?branch=main)
<!-- badges: end -->

The goal of rAURUM is to simplify the process of converting raw CPRD
Aurum data, stored in large .txt files, into a ‘analysis-ready’ dataset
which can be used for statistical analyses. This process is somewhat
difficult in R, as the raw data is very large and cannot be read into
the R workspace. rAURUM utilises
[RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html) to
create SQLite databases which are stored on the hard disk. These are
then queried to extract the required information for a cohort of
interest.

For a detailed guide on how to use **rAURUM** please see the [user-guide
vignette](https://alexpate30.github.io/rAURUM/articles/user-guide.html).

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
#> Warning: package 'data.table' was built under R version 4.3.3
```

Read in patient file:

``` r
pat <- extract_txt_pat(system.file("aurum_data", "aurum_allpatid_set1_extract_patient_001.txt", package = "rAURUM"))
pat
#>   patid pracid usualgpstaffid gender  yob mob emis_ddate regstartdate
#> 1     1     49              6      2 1984  NA 1976-11-21   1940-07-24
#> 2     2     79             11      1 1932  NA 1979-02-14   1929-02-23
#> 3     3     98             43      1 1930  NA 1972-06-01   1913-07-02
#> 4     4     53             72      2 1915  NA 1989-04-24   1969-07-11
#> 5     5     62             16      2 1916  NA 1951-09-23   1919-11-07
#> 6     6     54             11      1 1914  NA 1926-09-09   1970-08-28
#>   patienttypeid regenddate acceptable cprd_ddate
#> 1            58 1996-08-25          1 1935-03-17
#> 2            21 1945-03-19          0 1932-02-05
#> 3            81 1997-04-24          1 1912-04-27
#> 4            10 1951-09-05          0 1921-02-13
#> 5            45 1998-11-25          0 1903-08-26
#> 6            85 1983-03-14          1 1963-08-27
```

Connect to an SQLite database (in this example, we create a temporary
file):

``` r
aurum_extract <- connect_database(tempfile("temp.sqlite"))
```

Add some raw medical data (from the observation file) to the SQLite
database.

``` r
add_to_database(filepath = system.file("aurum_data", "aurum_allpatid_set1_extract_observation_001.txt", package = "rAURUM"), 
                filetype = "observation", nrows = -1, select = NULL, subset.patids = c(1,3,4,6), use.set = FALSE, db = aurum_extract, overwrite = TRUE)
```

View the first three rows of the observation table in the SQLite
database with an RSQLite query: TO DO XXXX: TURN THIS INTO A GENERIC
“VIEW” OR SIMILAR

``` r
RSQLite::dbGetQuery(aurum_extract, 'SELECT * FROM observation', n = 3)
#>   patid consid pracid obsid obsdate enterdate staffid parentobsid
#> 1     1     33      1   100  -15931      -994      79          95
#> 2     1     66      1    46  -13782    -15232      34          17
#> 3     1     41      1    53  -20002      8845      35          79
#>           medcodeid value numunitid obstypeid numrangelow numrangehigh
#> 1   498521000006119    48        16        20          28           86
#> 2         401539014    22         1         2          27            8
#> 3 13483031000006114    17        78        13          87           41
#>   probobsid
#> 1        54
#> 2        35
#> 3        74
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
#>          medcodeid value numunitid obstypeid numrangelow numrangehigh probobsid
#>             <char> <num>     <int>     <int>       <num>        <num>    <char>
#> 1: 187341000000114    84        79        67          24           22         5
```

Add an index date to the patient file, which we will extract variables
relative to:

``` r
### Add an index date to pat
pat$fup_start <- as.Date("01/01/2020", format = "%d/%m/%Y")
```

Extract a ‘history of’ type variable, which will be equal to 1 if an
individual has a record with the specified *medcodeid* prior to the
index date, and equal 0 otherwise.

``` r
### Extract a history of type variable using extract_ho
ho <- extract_ho(pat, 
                 codelist.vector = codelist, 
                 indexdt = "fup_start", 
                 db.open = aurum_extract, 
                 tab = "observation",
                 return.output = TRUE)
str(ho)
#> 'data.frame':    6 obs. of  2 variables:
#>  $ patid: chr  "1" "2" "3" "4" ...
#>  $ ho   : int  1 0 0 0 0 0
```

Merge the patient file with the ‘history of’ variable:

``` r
### Recursive merge
analysis.ready.pat <- merge(pat[,c("patid", "fup_start", "gender")], ho, by.x = "patid", by.y = "patid", all.x = TRUE) 
analysis.ready.pat
#>   patid  fup_start gender ho
#> 1     1 2020-01-01      2  1
#> 2     2 2020-01-01      1  0
#> 3     3 2020-01-01      1  0
#> 4     4 2020-01-01      2  0
#> 5     5 2020-01-01      2  0
#> 6     6 2020-01-01      1  0
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
