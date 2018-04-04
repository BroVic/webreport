<!-- README.md is generated from README.Rmd. Please edit that file -->
webreport [![Travis-CI Build Status](https://travis-ci.org/BroVic/webreport.svg?branch=master)](https://travis-ci.org/BroVic/webreport)
=======================================================================================================================================

An R package for reporting on social media data

Installation
------------

From the R console:

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("NESREA/webreport")
```

Reporting
---------

To build a report in MS Word format:

``` r
build_webreport()
```

This will bring up a dialog box for selecting the appropriate data source.

Alternatively, the user can provide the path as a function argument e.g.

``` r
build_webreport("mydatabase.db")
```

By default, a filename will be generated for the report. To supply a filename, pass a second argument to the function:

``` r
build_webreport("mydatabase.db", "Report1.docx")
```

The reporting period is for the immediate past week (7 days).

Additional functionality is being developed.
