\name{substitute2}
\alias{substitute2}
\alias{substitute}
\alias{I}
\title{ Substitute expression }
\description{
  Experimental, more robust and user-friendly version of base R \code{\link[base]{substitute}}.
}
\usage{
  substitute2(expr, env)
}
\arguments{
  \item{expr}{ Unevaluated expression in which the substitution is to take place. }
  \item{env}{ List, or an environment that will be coerced to list, from which variables will be taken to inject into \code{expr}. }
}
\details{
  For convenience, this function will turn any character elements of the \code{env} argument into symbols. If one of these character elements is length 2 or more, an error will be raised. It will also turn any list elements into list calls instead. Thos Behaviour can be changed by wrapping \code{env} in a \code{\link[base]{I}} call. In such  a case, any symbols must be explicitly created, for example, using \code{as.name} function. Alternatively, it is possible to wrap particular elements of \code{env} into a \code{\link[base]{I}} call, anf only those elements will retain their original class.

  Compared to base R's \code{\link[base]{substitute}}, the \code{substitute2} function:
\enumerate{
  \item substitutes \code{call} argument names as well
  \item by default, converts character elements of the \code{env} argument into symbols
  \item by default, converts list elements of the \code{env} argument into list calls
  \item does not accept a missing \code{env} argument
  \item evaluates the elements of the \code{env} argument
  \item Allows the use of a dot (\code{.()}) alias for \code{list} in the case where \code{env} is a list and not an environment
}
}
\note{
  Conversion of \emph{character to symbol} and \emph{list to list call} works recursively for each list element in \code{env} list. If this behaviour is not desired for your use case, we would like to hear about that via our issue tracker. In the present moment, there is an option to disable that: \code{options(datatable.enlist=FALSE)}. This option is provided only for debugging and will be removed in future. Please do not write code that depends on it, but use \code{\link[base]{I}} calls instead.
}
\value{
  Quoted expression having variables and call argument names substituted.
}
\seealso{ \code{\link[base]{substitute}}, \code{\link[base]{I}}, \code{\link[base]{call}}, \code{\link[base]{name}}, \code{\link[base]{eval}} }
\examples{
## base R substitute vs substitute2
substitute(list(var1 = var2), list(var1 = "c1", var2 = 5L))
substitute2(list(var1 = var2), list(var1 = "c1", var2 = 5L)) ## works also on names
substitute2(list(var1 = var2), .(var1 = "c1", var2 = 5L))    ## uses the dot alias to list

substitute(var1, list(var1 = "c1"))
substitute2(var1, list(var1 = I("c1"))) ## enforce character with I

substitute(var1, list(var1 = as.name("c1")))
substitute2(var1, list(var1 = "c1")) ## turn character into symbol, for convenience

## mix symbols and characters using 'I' function, both lines will yield same result
substitute2(list(var1 = var2), list(var1 = "c1", var2 = I("some_character")))
substitute2(list(var1 = var2), I(list(var1 = as.name("c1"), var2 = "some_character")))
# same, but with the dot alias
substitute2(list(var1 = var2), I(.(var1 = as.name("c1"), var2 = "some_character")))

## list elements are enlist'ed into list calls
(cl1 = substitute(f(lst), list(lst = list(1L, 2L))))
(cl2 = substitute2(f(lst), I(list(lst = list(1L, 2L)))))
(cl3 = substitute2(f(lst), list(lst = I(list(1L, 2L)))))
(cl4 = substitute2(f(lst), list(lst = quote(list(1L, 2L)))))
(cl5 = substitute2(f(lst), list(lst = list(1L, 2L))))
cl1[[2L]] ## base R substitute with list element
cl2[[2L]] ## same
cl3[[2L]] ## same
cl4[[2L]] ## desired
cl5[[2L]] ## automatically

## character to name and list into list calls works recursively
(cl1 = substitute2(f(lst), list(lst = list(1L, list(2L)))))
(cl2 = substitute2(f(lst), I(list(lst = list(1L, list(2L)))))) ## unless I() used
last(cl1[[2L]]) ## enlisted recursively
last(cl2[[2L]]) ## AsIs

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
