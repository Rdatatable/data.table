\name{setNumericRounding}
\alias{setNumericRounding}
\alias{getNumericRounding}
\title{ Change or turn off numeric rounding }
\description{
Change rounding to 0, 1 or 2 bytes when joining, grouping or ordering numeric
(i.e. double, POSIXct) columns.
}
\usage{
setNumericRounding(x)
getNumericRounding()
}
\arguments{
  \item{x}{ integer or numeric vector: 0 (default), 1 or 2 byte rounding }
}
\details{
Computers cannot represent some floating point numbers (such as 0.6)
precisely, using base 2. This leads to unexpected behaviour when joining or
grouping columns of type 'numeric'; i.e. 'double', see example below. In
cases where this is undesirable, data.table allows rounding such data up to
approximately 11 s.f. which is plenty of digits for many cases. This is
achieved by rounding the last 2 bytes off the significand. Other possible
values are 1 byte rounding, or no rounding (full precision, default).

It is bytes rather than bits because it is tied in with the radix sort
algorithm for sorting numerics which sorts byte by byte. With the default
rounding of 0 bytes, at most 8 passes are needed. With rounding of 2 bytes, at
most 6 passes are needed (and therefore might be a tad faster).

For large numbers (integers > 2^31), we recommend using
\code{bit64::integer64}, even though the default is to round off 0 bytes (full
precision).
 }
\value{
\code{setNumericRounding} returns no value; the new value is applied.
\code{getNumericRounding} returns the current value: 0, 1 or 2.
}
\seealso{
\code{\link{datatable-optimize}}\cr
\url{https://en.wikipedia.org/wiki/Double-precision_floating-point_format}\cr
\url{https://en.wikipedia.org/wiki/Floating_point}\cr
\url{https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html}
}
\examples{
DT = data.table(a=seq(0,1,by=0.2),b=1:2, key="a")
DT
setNumericRounding(0)   # By default, rounding is turned off
DT[.(0.4)]   # works
DT[.(0.6)]   # no match, can be confusing since 0.6 is clearly there in DT
             # happens due to floating point representation limitations

setNumericRounding(2)   # round off last 2 bytes
DT[.(0.6)]   # works

# using type 'numeric' for integers > 2^31 (typically ids)
DT = data.table(id = c(1234567890123, 1234567890124, 1234567890125), val=1:3)
print(DT, digits=15)
DT[,.N,by=id]   # 1 row, (last 2 bytes rounded)
setNumericRounding(0)
DT[,.N,by=id]   # 3 rows, (no rounding, default)
# better to use bit64::integer64 for such ids
}
\keyword{ data }
