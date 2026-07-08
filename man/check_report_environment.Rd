\name{check_report_environment}
\alias{check_report_environment}
\title{Check report rendering environment}
\usage{
check_report_environment(formats = c("source", "html", "pdf"), required = FALSE)
}
\arguments{
\item{formats}{Character vector of report formats to check. Supported values
are \code{"source"}, \code{"html"}, and \code{"pdf"}.}

\item{required}{Logical. If \code{TRUE}, stop when any requested format is not
available.}
}
\value{
A data frame with report-rendering availability and next-step guidance.
}
\description{
Check report rendering environment
}
