\name{substitute2}
\alias{substitute2}
\alias{substitute}
\alias{I}
\title{ Substitute expression }
\description{
  Experimental, more robust version of base R \code{\link[base]{substitute}}.
}
\usage{
substitute2(expr, env)
}
\arguments{
\item{expr}{ Unevaluated expression in which substitution has to take place. }
\item{env}{ Environment or a list from which variables will be taken to inject into \code{expr}. }
}
\note{
  By default function will turn any character objects into symbols. In case character is of length 2 or more, it will raise an error. Behaviour can be changed by wrapping \code{env} into \code{\link[base]{I}} call. In such a case any symbols must be explicitly created, for example using \code{as.name} function. Alternatively it is possible to wrap single elements of \code{env} into \code{\link[base]{I}} call, then only those elements will retain their original class. See examples below.
}
\value{
Quoted expression having variables substituted.
}
\seealso{ \code{\link[base]{substitute}}, \code{\link[base]{I}} }
\examples{
## base R substitute vs substitute2
substitute(var1, list(var1 = "c1"))
substitute2(var1, list(var1 = I("c1")))

substitute(var1, list(var1 = as.name("c1")))
substitute(var1, list(var1 = "c1"))

substitute(list(var1 = var2), list(var1 = "c1", var2 = 5L))
substitute2(list(var1 = var2), list(var1 = "c1", var2 = 5L))

# mix symbols and characters, both lines will yield same output
substitute2(list(var1 = var2), list(var1 = "c1", var2 = I("some_character")))
substitute2(list(var1 = var2), I(list(var1 = as.name("c1"), var2 = "some_character")))

# using from inside a function
f = function(expr, env) {
  eval(substitute(
    substitute2(.expr, env),
    list(.expr = substitute(expr))
  ))
}
f(list(var1 = var2), list(var1 = "c1", var2 = 5L))
}
\keyword{ data }
