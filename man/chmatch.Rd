\name{chmatch}
\alias{chmatch}
\alias{\%chin\%}
\title{ Faster match of character vectors }
\description{
  \code{chmatch} returns a vector of the positions of (first) matches of its first argument in its second. Both arguments must be character vectors.

  \code{\%chin\%} is like \code{\%in\%}, but for character vectors.
}
\usage{
chmatch(x, table, nomatch=NA_integer_)
x \%chin\% table
}
\arguments{
  \item{x}{ character vector: the values to be matched. }
  \item{table}{ character vector: the values to be matched against. }
  \item{nomatch}{ the value to be returned in the case when no match is found. Note that it is coerced to integer. }
}
\details{
  Fast versions of \code{match} and \code{\%in\%}, optimised for character vectors.
  
  Strings are already cached internally by R (\code{CHARSXP}) and that is utilised by these functions. No hash table is built or cached in either vector, so the first call is the same speed as subsequent calls.
  
  Methods that do build and cache a hash table (such as the \href{http://www.rforge.net/fastmatch}{fastmatch package}) are \emph{much} faster on subsequent calls (almost instant) but a little slower on the first. Therefore \code{chmatch} may be particularly suitable for ephemeral vectors (such as local variables in functions) or tasks that are only done once. Much depends on the length of \code{x} and \code{table}, how many unique strings each contains, and whether the position of the first match is all that is required.
  
  It may be possible to speed up fastmatch's hash table build time by using the technique in data.table, and we have suggested this to its author. If successful, fastmatch would then be fastest in all cases.
  }
\value{
    As \code{match} and \code{\%in\%}.
}
\seealso{ \code{\link[base]{match}}, \code{\link{\%in\%}}, \code{\link[fastmatch]{fmatch}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\note{ The name \code{charmatch} was taken by \code{\link[base]{charmatch}}, hence \code{chmatch}.
}
\examples{
    # Please type 'example(chmatch)' to run this and see timings on your machine
    
    u = as.character(as.hexmode(1:10000))
    y = sample(u,1e7,replace=TRUE)
    x = sample(u)
    
    system.time(a <- match(x,y))                 # 4.8s
    system.time(b <- chmatch(x,y))               # 0.9s   Faster than 1st fmatch
    identical(a,b)
    if (fastmatchloaded<-suppressWarnings(require(fastmatch))) {
        print(system.time(c <- fmatch(x,y)))     # 2.1s   Builds and caches hash
        print(system.time(c <- fmatch(x,y)))     # 0.00s  Uses hash
        identical(a,c)
    }
    
    system.time(a <- x \%in\% y)                   #  4.8s
    system.time(b <- x \%chin\% y)                 #  0.9s
    identical(a,b)
    if (fastmatchloaded) {
        match <- fmatch                          # fmatch is drop in replacement
        print(system.time(c <- match(x,y)))      # 0.00s
        print(system.time(c <- x \%in\% y))        # 4.8s   \%in\% still prefers base::match
        # Anyone know how to get \%in\% to use fmatch (without masking \%in\% too)?
        rm(match)
        identical(a,c)
    }
    
    # Different example with more unique strings ...
    u = as.character(as.hexmode(1:1e6))
    y = sample(u,1e7,replace=TRUE)
    x = sample(u,1e7,replace=TRUE)
    system.time(a <- match(x,y))                 # 34.0s
    system.time(b <- chmatch(x,y))               #  6.4s
    identical(a,b)
    if (fastmatchloaded) {
        print(system.time(c <- fmatch(x,y)))     #  7.9s
        print(system.time(c <- fmatch(x,y)))     #  4.0s
        identical(a,c)
    }
}
\keyword{ data }


