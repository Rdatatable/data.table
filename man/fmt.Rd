\name{fmt}
\alias{fmt}
\title{ Format arbitrary objects for printing in data.table }
\description{
    S3 method to customize the printing of non-atomic items
    in \code{\link{print.data.table}}.
    
    The default method returns the first class of the object
    (as returned by \code{\link{class}}) in angle brackets
    (\dQuote{<Class>}).
}
\usage{
    fmt(x, ...)
}
\arguments{
  \item{x}{ Anything. }
  \item{...}{ Currently ignored. }
}
\value{
    A single string.
}

