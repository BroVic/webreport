#' Build a report from a source file
#'
#' @description Creates a report from an R Markdown template file that is
#' found in the package (precisely in \code{inst/rmarkdown}) and utilises a
#' data previously stored in a local database.
#'
#' @param data.source A database file containing web data
#' @param launch logical; whether to automatically open built document
#' @param ... Additional arguments passed to \code{rmarkdown::render()}.
#'
#' @details \emph{data.source} refers to a SQLite file (internally) wherein
#' the data for the analysis are stored. The function supports the file
#' extensions \emph{.sqlite} and \emph{.db}. The data are expected to have been
#' stored with the structure defined by the various social media platform APIs.
#'
#' \emph{outfile} currently has to be a document with extension \emph{.docx}.
#' Support for other file formats is planned in future upgrades. By default,
#' a file name is automatically supplied - this name is always unique as it
#' bears a timestamp. By default, the file is saved in the current working
#' directory.
#'
#' The document is opened, by default, when \emph{launch.file} is \code{TRUE}
#' If \code{FALSE} is passed, the document is saved and a message is sent to
#' the user stating the save location.
#'
#' Arguments can be passed to \code{rmarkdown::render()} such as
#' \code{output_dir} i.e. the directory where the built document is stored
#'
#' @return This function has no return value. The result of the operation is
#' a document built in the specified file format.
#'
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom rmarkdown render
#'
#' @export
build_webreport <- function(data.source, launch = TRUE, ...)
{
  ## Validation of argument for the path to the database
  if (missing(data.source) || !file.exists(data.source))
    stop("A valid path for 'data.source' must be provided")
  data.source <- suppressWarnings(normalizePath(data.source))

  ## Import data into R after confirming that it's an SQLite file
  file.con <- file(data.source, 'rb')
  header <- readBin(file.con, character(), endian = 'big', size = 16)
  on.exit(close(file.con))
  if (header != 'SQLite format 3')
    stop("'data.source' should be an SQLite database.")
  data <- provideInternalData(data.source)

  ## Use package's .Rmd skeleton to build the report
  outfile <-
    rmarkdown::render(
      input = system.file(
        "rmarkdown/templates/reports/skeleton/skeleton.Rmd",
        package = "webreport",
        mustWork = TRUE),
      params = list(data = data),
      ...)
  message(paste("Report has been saved to", sQuote(dirname(outfile))))
  newname <- sub('(skeleton)(\\.)',
                 paste0('report_', Sys.Date(), '\\2'),
                 outfile)
  if (!file.rename(outfile, newname))
    warning('Failed to rename the file')
  else
    if (launch)
      try(system2("open", args = newname, invisible = FALSE))
}
