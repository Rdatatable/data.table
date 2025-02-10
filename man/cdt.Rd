\name{cdt}
\alias{cdatatable}
\title{ data.table exported C routines }
\description{
  Some of the internally used C routines are now exported. This interface should be considered experimental. List of exported C routines and their signatures are provided below in the usage section.
}
\usage{
# SEXP DT_subsetDT(SEXP x, SEXP rows, SEXP cols);
# p_DT_subsetDT = R_GetCCallable("data.table", "DT_subsetDT");
}
\details{
  Details on how to use these can be found in the \emph{Writing R Extensions} manual \emph{Linking to native routines in other packages} section.
  An example use with \code{Rcpp}:
\preformatted{
  dt = data.table::as.data.table(iris)
  Rcpp::cppFunction("SEXP mysub2(SEXP x, SEXP rows, SEXP cols) { return DT_subsetDT(x,rows,cols); }",
    include="#include <datatableAPI.h>",
    depends="data.table")
  mysub2(dt, 1:4, 1:4)
}
}
\note{
  Be aware C routines are likely to have less input validation than their corresponding R interface. For example one should not expect \code{DT[-5L]} will be equal to \code{.Call(DT_subsetDT, DT, -5L, seq_along(DT))} because translation of \code{i=-5L} to \code{seq_len(nrow(DT))[-5L]} might be happening on R level. Moreover checks that \code{i} argument is in range of \code{1:nrow(DT)}, missingness, etc. might be happening on R level too.
}
\references{
  \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}
}
\keyword{ data }
