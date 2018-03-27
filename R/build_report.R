#' Build a report from a source file
#'
#' @description Creates a report from an R Markdown template file that is
#' found in the package (precisely in \code{inst/rmarkdow}) and utilises a
#' data previously stored in a local database.
#'
#' @param data.source A database file containing web data
#' @param outfile The path of the built document. Defaults to the working
#' directory.
#' @param ... Additional arguments passed to \code{rmarkdown::render()}.
#'
#' @return This function has no return value. The result of the operation is
#' a built document in the specified file format.
#'
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @import RSQLite
#' @importFrom rmarkdown render
#' @importFrom utils choose.files
#'
#' @export
build_webreport <- function(data.source = NULL, outfile = NULL, ...)
{    ## TODO: Add argument for destination directory!

  ## Optionally use dialog for file selection
  if (identical(.Platform$OS.type, 'windows') & interactive()) {
    fileOpts <- matrix(c("SQLite database (*.sqlite,*.db)", "*.sqlite;*.db"),
                       ncol = 2L, dimnames = list("SQLite"))
    }

  if (is.null(data.source)) {
      data.source <- choose.files(caption = "Select a database",
                                  multi = FALSE, filters = fileOpts$SQLite)
  }
  else {
      if (!endsWith(tolower(data.source), '.db') ||
          !endsWith(tolower(data.source), '.sqlite')) {
        stop("'data.source' should be an SQLite database file.")
    }
  }
  con <- dbConnect(SQLite(), data.source)

  if (!dbIsValid(con))
    stop("Connection to database not established.")
  tbls <- dbListTables(con)
  dfs <- sapply(tbls, USE.NAMES = TRUE, function(table) {
    dbReadTable(con, table)
    })
  dbDisconnect(con)

  if (dbIsValid(con))
    stop("Database could not be disconnected.")
  names(dfs) <- gsub('^nesreanigeria_', '', names(dfs))

  if (is.null(outfile)) {
    outfile <-
    tempfile(paste0("report_", format(Sys.time(), '%Y%m%d_%H%M%S')),
             tmpdir = ".", fileext = ".docx")
  }
  rmdSkl <- system.file("rmarkdown/templates/reports/skeleton/skeleton.Rmd",
                        package = "webreport")
  rmarkdown::render(input = rmdSkl, output_format = "word_document",
                    output_file = outfile, output_dir = ".",
                    params = list(data = dfs))
}
