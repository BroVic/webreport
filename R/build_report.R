#' Build a report from a source file
#'
#' @description Creates a report from an R Markdown template file that is
#' found in the package (precisely in \code{inst/rmarkdown}) and utilises a
#' data previously stored in a local database.
#'
#' @param data.source A database file containing web data
#' @param outfile The path of the built document.
#' @param launch.file logical; whether to automatically open built document
#' @param ... Additional arguments passed to \code{rmarkdown::render()}.
#'
#' @details \emph{data.source} refers to a SQLite file wherein the data for the
#' analysis are stored. The function supports the file extensions \emph{.sqlite}
#' and \emph{.db}. The data are expected to have been stored with the structure
#' defined by the various social media platform APIs.
#'
#' \emph{outfile} currently has to be document with extension \emph{.docx}.
#' Support for other file formats is planned for future upgrades. By default,
#' a file name is automatically supplied - this name is always unique as it
#' bears a timestamp. By default, the file is saved in the current working
#' directory.
#'
#' The document is opened by default when \emph{launch.file} is \code{TRUE}
#' (the default). If \code{FALSE} is passed, the document is saved and a
#' message is sent to the user stating the save location.
#'
#' Arguments can be passed to \code{rmarkdown::render()} such as
#' \code{output_dir} i.e. the directory where the built document is stored
#'
#' @return This function has no return value. The result of the operation is
#' a built document in the specified file format.
#'
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @import RSQLite
#' @importFrom rmarkdown render
#'
#' @export
## The function
build_webreport <- function(data.source, outfile = NULL, launch.file = TRUE, ...)
{
  if (missing(data.source))
    stop("'data.source' must be provided.")
  if (!endsWith(tolower(data.source), '.db') &
      !endsWith(tolower(data.source), '.sqlite')) {
    stop("The file 'data.source' should have '.db' or '.sqlite' extension.")
    # TODO: Use binary file identification.
  }

  con <- dbConnect(SQLite(), data.source)
  if (!dbIsValid(con))
    stop("Connection to database not established.")

  tbls <- dbListTables(con)
  dfs <- sapply(tbls, USE.NAMES = TRUE, function(table) {
    dbReadTable(con, table)
    })
  names(dfs) <- gsub('^nesreanigeria_', '', names(dfs))

  dbDisconnect(con)
  if (dbIsValid(con))
    warning("Database was not properly disconnected.")

  if (is.null(outfile)) {
    outfile <-
    tempfile(paste0("report_", format(Sys.time(), '%Y%m%d_%H%M%S')),
             tmpdir = ".", fileext = ".docx")
  }
  skl <- system.file("rmarkdown/templates/reports/skeleton/skeleton.Rmd",
                        package = "webreport")
  rmarkdown::render(input = skl,
                    output_format = "word_document",
                    output_file = outfile,
                    output_dir = ".",
                    params = list(data = dfs))
  if (launch.file) system2("open", args = outfile, invisible = FALSE)
  else message(paste("Output has been saved to the directory",
                     sQuote(dirname(normalizePath((outfile))))))
}
