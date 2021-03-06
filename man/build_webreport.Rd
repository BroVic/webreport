% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_report.R
\name{build_webreport}
\alias{build_webreport}
\title{Build a report from a source file}
\usage{
build_webreport(data.source, launch = TRUE, ...)
}
\arguments{
\item{data.source}{A database file containing web data}

\item{launch}{logical; whether to automatically open built document}

\item{...}{Additional arguments passed to \code{rmarkdown::render()}.}
}
\value{
This function has no return value. The result of the operation is
a document built in the specified file format.
}
\description{
Creates a report from an R Markdown template file that is
found in the package (precisely in \code{inst/rmarkdown}) and utilises a
data previously stored in a local database.
}
\details{
\emph{data.source} refers to a SQLite file (internally) wherein
the data for the analysis are stored. The function supports the file
extensions \emph{.sqlite} and \emph{.db}. The data are expected to have been
stored with the structure defined by the various social media platform APIs.

\emph{outfile} currently has to be a document with extension \emph{.docx}.
Support for other file formats is planned in future upgrades. By default,
a file name is automatically supplied - this name is always unique as it
bears a timestamp. By default, the file is saved in the current working
directory.

The document is opened, by default, when \emph{launch.file} is \code{TRUE}
If \code{FALSE} is passed, the document is saved and a message is sent to
the user stating the save location.

Arguments can be passed to \code{rmarkdown::render()} such as
\code{output_dir} i.e. the directory where the built document is stored
}
\seealso{
\code{\link[rmarkdown]{render}}
}
