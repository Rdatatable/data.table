\name{substitute2}
\alias{substitute2}
\alias{substitute}
\alias{I}
\title{ Substitute expression }
\description{
  Experimental, more robust and more user-friendly version of base R \code{\link[base]{substitute}}.
}
\usage{
substitute2(expr, env)
}
\arguments{
\item{expr}{ Unevaluated expression in which substitution has to take place. }
\item{env}{ Environment or a list from which variables will be taken to inject into \code{expr}. }
}
\details{
  Function will turn any character elements of \code{env} argument into symbols. In case character is of length 2 or more, it will raise an error. Behaviour can be changed by wrapping \code{env} into \code{\link[base]{I}} call. In such case any symbols must be explicitly created, for example using \code{as.name} function. Alternatively it is possible to wrap elements of \code{env} into \code{\link[base]{I}} call, not the whole list, then only those elements will retain their original class.
}
\value{
Quoted expression having variables substituted.
}
\seealso{ \code{\link[base]{substitute}}, \code{\link[base]{I}} }
\examples{
## base R substitute vs substitute2
substitute(list(var1 = var2), list(var1 = "c1", var2 = 5L))
substitute2(list(var1 = var2), list(var1 = "c1", var2 = 5L)) ## works also on names

substitute(var1, list(var1 = "c1"))
substitute2(var1, list(var1 = I("c1"))) ## enforce character with I

substitute(var1, list(var1 = as.name("c1")))
substitute2(var1, list(var1 = "c1")) ## turn character into symbol, for convenience

## mix symbols and characters using 'I' function, both lines will yield same result
substitute2(list(var1 = var2), list(var1 = "c1", var2 = I("some_character")))
substitute2(list(var1 = var2), I(list(var1 = as.name("c1"), var2 = "some_character")))

## character to symbol coerce will not be done for character stored inside a list
substitute2(lapply(some_list, sum), list(some_list = list("V1","V2")))
## those has to be substituted before
some_list = substitute2(list(v1, v2), list(v1="V1", v2="V2"))
substitute2(lapply(some_list, sum), list(some_list = some_list))
## in case of a character list of arbitrary length
some_list = as.call(lapply(c("list", c("V1","V2")), as.name))
substitute2(lapply(some_list, sum), list(some_list = some_list))
## or from R 4.0.0 also bquote splice functionality
if (getRversion() >= "4.0.0") {
  some_list = bquote(list(..(lapply(c("V1","V2"), as.name))), splice=TRUE)
  substitute2(lapply(some_list, sum), list(some_list = some_list))
}

## using substitute2 from another function
f = function(expr, env) {
  eval(substitute(
    substitute2(.expr, env),
    list(.expr = substitute(expr))
  ))
}
f(list(var1 = var2), list(var1 = "c1", var2 = 5L))
}
\keyword{ data }
