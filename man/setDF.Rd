\name{setDF}
\alias{setDF}
\title{Convert a data.table to data.frame by reference}
\description{
  A helper function to convert a \code{data.table} to \code{data.frame} by reference. It does not handle lists or lists of data.tables.  
}
\usage{
setDF(x)
}
\arguments{
  \item{x}{ A \code{data.table}. }
}

\details{
  This feature request came up on the data.table mailing list: \url{http://bit.ly/1xkokNQ}. All \code{data.table} attributes including any keys of the input data.table are stripped off.
}

\value{
    The input \code{data.table} is modified by reference to a \code{data.frame}.
}

\seealso{ \code{\link{setkey}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}, \code{\link{copy}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
set.seed(45L)
X = data.table(x = 1:5, y=6:10)
## convert 'X' to data.frame, without any copy.
setDF(X)
}
\keyword{ data }

