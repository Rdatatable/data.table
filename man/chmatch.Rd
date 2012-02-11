\name{chmatch}
\alias{chmatch}
\alias{%chin%}
\title{ Fast match of character vectors }
\description{
  \code{chmatch} returns a vector of the positions of (first) matches of its first argument in its second. Both arguments must be character vectors.

  \code{%chin%} is like \code{%in%} but for character vectors.
}
\usage{
chmatch(x, table)
x %chin% table
}
\arguments{
  \item{x}{ character vector: the values to be matched. }
  \item{table}{ character vector: the values to be matched against. }
}
\details{
  Fast versions of \code{match} and \code{%in%}, optimised for character vectors.
  
  Strings are already hashed internally by R (\code{CHARSXP}), and this is utilised by these functions. No hash table is calculated and cached in either vector, so the first call is the same speed as subsequent calls.
  
  Methods that do cache a hash table (such as the fastmatch package) may be even faster on subsequent calls.
  
  TO DO: cache \code{unique(y)} and use that on subsequent calls, but make sure R drops it when copying and on subassign to character vectors. Then make \code{set} and \code{:=} ul-aware.
  
  }
\value{
    As \code{match} and \code{%in%}.
}
\seealso{ \code{\link{match}}, \code{\link{%in%}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\note{ \code{base::charmatch} exists already, hence \code{chmatch}.
}
\examples{
    # Type 'example(chmatch)' to run this and see timings on your machine
    
    u = as.character(as.hexmode(1:10000))
    y = sample(u,1e7,replace=TRUE)
    x = sample(u)
    
    system.time(a <- match(x,y))    # 3.6s
    system.time(b <- chmatch(x,y))  # 0.8s
    identical(a,b)
    
    system.time(a <- x %in% y)      # 3.6s
    system.time(b <- x %chin% y)    # 0.8s
    identical(a,b)
}
\keyword{ data }


