\name{update_dev_pkg}
\alias{update_dev_pkg}
\title{Perform update of development version of a package}
\description{
  Downloads and installs latest development version only when a new commit is available which has also passed all tests. Defaults are set to update \code{data.table}, other packages can be used as well. Their repository has to include git commit information in PACKAGES file.
}
\usage{update_dev_pkg(object="data.table",
       repo="https://Rdatatable.gitlab.io/data.table",
       field="Revision", type=getOption("pkgType"), lib=NULL, \dots)
}
\arguments{
  \item{object}{ character scalar, package name. }
  \item{repo}{ character scalar, url of package devel repository. }
  \item{field}{ character scalar, metadata field to use in PACKAGES file and
    DESCRIPTION file, default \code{"Revision"}. }
  \item{type}{ character scalar, default \code{getOption("pkgType")}, used
    to define if package has to be installed from sources, binaries or both. }
  \item{lib}{ character scalar, library location where package is meant to
    be upgraded. }
  \item{\dots}{ passed to \code{\link[utils]{install.packages}}. }
}
\details{
  In case if a devel repository does not provide binaries user will need development tools installed for package compilation, like \emph{Rtools} on Windows, and eventually set \code{type="source"}.
}
\note{
  Package namespace is unloaded before attempting to install newer version.
}
\value{
  NULL.
}
\examples{
\dontshow{ # using if(FALSE) because \dontrun could still be run by  --run-dontrun; #5421 }
  if (FALSE) data.table::update_dev_pkg()
}
\seealso{
  \code{\link{data.table}}
}
\keyword{ data }
