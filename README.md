<!-- README.md is generated from README.Rmd. Please edit that file -->
webreport [![Travis-CI Build Status](https://travis-ci.org/BroVic/webreport.svg?branch=master)](https://travis-ci.org/BroVic/webreport)
=======================================================================================================================================

An R package for reporting on social media data

System requirements
-------------------

To use the package you need both the 32-bit and 64-bit versions of Java SE Runtime Environment (1.8 or above). *[- Downloads page -](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)*

Installation
------------

The package can be installed by copying and pasting the following lines of code and executing them in the R console

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("NESREA/webreport")
```

Creating a report
-----------------

To build a report provide the path to the data source as an argument to `build_webreport()`. The data are stored as an [SQLite](https://www.sqlite.org/index.html) database.

``` r
library(webreport)
build_webreport("path/to/sqlite/file")
```

By default, a filename will be generated for the report, exclusively in MS Word. To supply a custom filename, pass a second argument to the function, for example

``` r
build_webreport("mydatabase.db", "Report1.docx")
```

To find out about other functions in the package, such as how to **download** data, check the documentation via

``` r
help(package = 'webreport')
```
