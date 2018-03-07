#' build_report
#'
#' @param file A path to the RMarkdown file (required).
#' @param outfile The path of the built document. Defaults to the working
#'     directory.
#' @param type The kind of output document; either a Microsoft Word (.docx)
#'     or Acrobat Portable Document Format (.pdf) document. Defaults to the
#'     former.
#' @param ... Arguments passed on to \code{rmarkdown::render()}.
#' @return This function has no return value. The result of the operation is
#'     a built document in the specified file format.
#' @import rmarkdown
#' @export
build_report <- function(file, outfile = NULL,
                         type = c("word_document", "pdf_document"), ...)
{
  stopifnot(is.character(file))
  typ <- match.arg(type)
  if (identical(match(typ, type), 1L)) {
    ext <- ".docx"
  }
  else if (identical(match(typ, type), 2L)) {
    ext <- ".pdf"
  }

  if (is.null(outfile))
    outfile <- tempfile("report_", tmpdir = ".", fileext = ext)
  rmarkdown::render(file, output_format = typ,
                    output_file = outfile, output_dir = ".", quiet = TRUE)
}
