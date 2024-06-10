\name{coredata}
\alias{keydata}
\alias{valuedata}
\title{ Return a data.table without keys }
\description{
  Get data.table key or value data.
}
\usage{
  keydata(x)

  valuedata(x)
}
\arguments{
  \item{x}{ A data.table object. }
}
\value{
  \code{keydata} returns a \code{data.table} containing the key columns of \code{x}.

  \code{valuedata} returns a \code{data.table} containing the value columns of \code{x}.
}
\examples{
# Create a data.table
dt = data.table(x = 1:5, y = letters[1:5], key = "x")

# Extract the key columns as a data.table
keys = keydata(dt)

# Exctract the value columns as a data.table
values = valuedata(dt)

is.data.table(keys) # TRUE
is.data.table(values) # TRUE
}
