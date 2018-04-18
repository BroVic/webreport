<!-- README.md is generated from README.Rmd. Please edit that file -->
webreport [![Travis-CI Build Status](https://travis-ci.org/BroVic/webreport.svg?branch=master)](https://travis-ci.org/BroVic/webreport)
=======================================================================================================================================

An R package for reporting on social media data

Prerequisites
-------------

To use the package you need both the 32-bit and 64-bit versions of Java SE Runtime Environment (1.8 or above). *[- Downloads page -](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)*

Installation
------------

The package can be installed by copying and pasting the following lines of code and executing them in the R console

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("NESREA/webreport")
```

Data collection
---------------

To collect social media data, all you need to do is to tell R what you're looking for and where you want to store whatever data is sent. The data are stored as an [SQLite](https://www.sqlite.org/index.html) database and each category of data is stored in a separate database table.

``` r
# Example for getting data on a particular term
# The database is created if previously non-existent
library(webreport)
download_all_data("nesreanigeria", "C:/Users/Admin/Documents/nesrea.db")
```

There are functions that optionally allow downloads from specific platforms e.g. `download_fb()`, `download_tweets()` and `download_website()`.
(For now, these functions are designed for NESREA specific activity, but they do provide some support generic searches and downloads.)

Report generation
-----------------

To build a report provide the path to the data source as an argument to `build_webreport()`.

``` r
build_webreport("path/to/sqlite/file")
```

By default, a filename will be generated for the report, exclusively in MS Word. To supply a custom filename, pass a second argument to the function, for example

``` r
build_webreport("mydatabase.db", "Report1.docx")
```

To find out about other functions in the package, check the documentation via

``` r
help(package = 'webreport')
```
