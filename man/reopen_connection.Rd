\name{reopen_connection}
\alias{reopen_connection}
\title{ Reopen a connection in binary mode }
\description{
  S3 generic to reopen a connection in binary read mode. Used internally by \code{fread}. Exported so packages with custom connection classes can define methods.
}
\usage{
reopen_connection(con, ...)
}
\arguments{
  \item{con}{ A connection object. }
  \item{...}{ Additional arguments for methods. }
}
\details{
  Reopens a connection in binary read mode (\code{"rb"}). Methods are provided for \code{file}, \code{gzfile}, \code{bzfile}, \code{url}, and \code{pipe} connections.

  To support custom connection types with \code{fread}, define a method for your connection class that returns a new connection opened in binary mode.
}
\value{
  A connection object opened in binary read mode.
}
\seealso{
  \code{\link{fread}}
}
\keyword{ data }
