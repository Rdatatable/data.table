\name{cdt}
\alias{cdatatable}
\title{ data.table exported C routines }
\description{
  Note that this interface is going to be changed in next release.
  Some of internally used C routines are now exported. This interface should be considered experimental. List of exported C routines and their signatures are provided below in the usage section.
}
\usage{
# SEXP subsetDT(SEXP x, SEXP rows, SEXP cols);
# p_dtCsubsetDT = R_GetCCallable("data.table", "CsubsetDT");
}
\details{
  For details how to use those see \emph{Writing R Extensions} manual \emph{Linking to native routines in other packages} section.
}
\note{
  Be aware C routines are likely to have less input validation than their corresponding R interface. For example one should not expect \code{DT[-5L]} will be equal to \code{.Call(CsubsetDT, DT, -5L, seq_along(DT))} because translation of \code{i=-5L} to \code{seq_len(nrow(DT))[-5L]} might be happening on R level. Moreover checks that \code{i} argument is in range of \code{1:nrow(DT)}, missingness, etc. might be happening on R level too.
}
\references{
  \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}
}
\keyword{ data }
