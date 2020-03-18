\name{substitute2}
\alias{substitute2}
\alias{is.AsIs}
\title{ Substitute expression }
\description{
  Experimental, more robust version of base R \code{\link[base]{substitute}} function. \code{is.AsIs} is a helper function to check if \code{x} inherits from \emph{AsIs} class.
}
\usage{
substitute2(expr, env, char.as.name=!is.AsIs(env), sub.names=TRUE)
is.AsIs(x)
}
\arguments{
\item{expr}{ Unevaluated expression in which substitution has to take place. }
\item{env}{ Environment or a list from which variables will be taken to inject into \code{expr}. }
\item{char.as.name}{ Logical, for convenience it will automatically turn \code{"my_name"} character objects into \code{`my_name`} symbols. Feature can be escaped by using \code{\link[base]{I}} function. }
\item{sub.names}{ Logical, should be names of call arguments be substituted as well, if set to \code{FALSE} it will basically fall back to base R substitute. }
\item{x}{ Any object passed to \code{is.AsIS} to be tested for inheritance of \emph{AsIs} class. }
}
\note{
  By default function will turn any character objects into symbols. In case character is of length 2 or more, it will raise an error. Behaviour can be changed by setting \code{char.as.name} to \code{FALSE}, but then any symbol must be explicitly created, for example using \code{as.name} function. Additionally using base R \code{\link[base]{I}} function make it convenient to control \code{char.as.name} behaviour globally for all elements in \code{env}, or locally for each single one, without the need to use \code{char.as.name} argument explicitly. See example below.
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
