\name{connection_opener}
\alias{connection_opener}
\alias{connection_opener.default}
\alias{connection_opener.file}
\alias{connection_opener.gzfile}
\alias{connection_opener.bzfile}
\alias{connection_opener.url}
\alias{connection_opener.unz}
\alias{connection_opener.pipe}
\title{ Create a function to open connections in binary mode }
\description{
  S3 generic that returns a function to open a connection in binary read mode. Used internally by \code{fread}. Exported so packages with custom connection classes can define methods.
}
\usage{
connection_opener(con, ...)
}
\arguments{
  \item{con}{ A connection object. }
  \item{...}{ Additional arguments passed to the connection constructor. }
}
\details{
  Returns a function that accepts a description argument and opens a connection in binary read mode (\code{"rb"}). Methods are provided for \code{file}, \code{gzfile}, \code{bzfile}, \code{url}, \code{unz} and \code{pipe} connections.

  To support custom connection types with \code{fread}, define a method for your connection class that returns an opener function.
}
\value{
  A function that accepts a description argument and returns a connection object opened in binary read mode.
}
\examples{
\dontrun{
# Define a method for a custom connection class
connection_opener.my_con = function(con, ...) {
  function(description) my_con(description, mode = "rb", ...)
}
}
}
\seealso{
  \code{\link{fread}}
}
\keyword{ data }
