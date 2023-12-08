\name{update_dev_pkg}
\alias{update_dev_pkg}
\title{Perform update of development version of a package}
\description{
  Downloads and installs latest development version, only when a new commit is available. Defaults are set to update \code{data.table}, other packages can be used as well. Repository of a package has to include git commit SHA information in PACKAGES file.
}
\usage{update_dev_pkg(pkg="data.table",
       repo="https://Rdatatable.gitlab.io/data.table",
       field="Revision", type=getOption("pkgType"), lib=NULL, \dots)
}
\arguments{
  \item{pkg}{ character scalar, package name. }
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
  In case if a devel repository does not provide binaries user will need development tools installed for package compilation, like \emph{Rtools} on Windows, or alternatively eventually set \code{type="source"}.
}
\section{data.table repositories}{
  By default the function uses our GitLab-hosted R repository at \code{https://Rdatatable.gitlab.io/data.table}. This repository is updated nightly. It runs multiple test jobs (on top of GitHub tests jobs run upstream) and publish the package (sources and binaries), even if GitLab test jobs are failing. Status of GitLab test jobs can be checked at \href{https://rdatatable.gitlab.io/data.table/web/checks/check_results_data.table.html}{Package Check Results}.\cr
  We also publish bleeding edge version of the package on GitHub-hosted R repository at \code{https://Rdatatable.gitlab.io/data.table} (just minor change in url from \emph{lab} to \emph{hub}). GitHub version should be considered less stable than GitLab one. It publishes only package sources.\cr
  There are also other repositories maintained by R community, for example \code{https://rdatatable.r-universe.dev}. Those can be used as well, but as they are unlikely to provide git commit SHA, the function will install the package even if latest version is already installed.
}
\note{
  Package namespace is unloaded before attempting to install newer version.
}
\value{
  Invisibly \code{TRUE} if package was updated, otherwise \code{FALSE}.
}
\examples{
\dontshow{ # using if(FALSE) because \dontrun could still be run by  --run-dontrun; #5421 }
  if (FALSE) data.table::update_dev_pkg()
}
\seealso{
  \code{\link{data.table}}
}
\keyword{ data }
