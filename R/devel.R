# nocov start

dcf.lib = function(pkg, field, lib.loc=NULL){
  # get DESCRIPTION metadata field from local library
  stopifnot(is.character(pkg), is.character(field), length(pkg)==1L, length(field)==1L)
  dcf = system.file("DESCRIPTION", package=pkg, lib.loc=lib.loc, mustWork=TRUE)
  if (nzchar(dcf)) read.dcf(dcf, fields=field)[1L] else NA_character_
}

dcf.repo = function(pkg, repo, field, type){
  # get DESCRIPTION metadata field from remote PACKAGES file
  stopifnot(is.character(pkg), is.character(field), length(pkg)==1L, length(field)==1L, is.character(repo), length(repo)==1L, field!="Package")
  idx = file(file.path(contrib.url(repo, type=type),"PACKAGES"))
  on.exit(close(idx))
  dcf = read.dcf(idx, fields=c("Package",field))
  if (!pkg %in% dcf[,"Package"]) stop("There is no ", pkg, " package in provided repository.")
  dcf[dcf[,"Package"]==pkg, field][[1L]]
}

update.dev.pkg = function(object="data.table", repo="https://Rdatatable.gitlab.io/data.table", field="Revision", type=getOption("pkgType"), lib=NULL, ...){
  pkg = object
  # perform package upgrade when new Revision present
  stopifnot(is.character(pkg), length(pkg)==1L, !is.na(pkg),
            is.character(repo), length(repo)==1L, !is.na(repo),
            is.character(field), length(field)==1L, !is.na(field),
            is.null(lib) || (is.character(lib) && length(lib)==1L && !is.na(lib)))
  una = is.na(ups<-dcf.repo(pkg, repo, field, type))
  upg = una | !identical(ups, dcf.lib(pkg, field, lib.loc=lib))
  if (upg) utils::install.packages(pkg, repos=repo, type=type, lib=lib, ...)
  if (una) cat(sprintf("No commit information found in DESCRIPTION file for %s package. Unsure '%s' is correct field name in PACKAGES file in your devel repository '%s'.\n", pkg, field, file.path(repo, "src","contrib","PACKAGES")))
  cat(sprintf("R %s package %s %s (%s)\n",
              pkg,
              c("is up-to-date at","has been updated to")[upg+1L],
              dcf.lib(pkg, field, lib.loc=lib),
              utils::packageVersion(pkg, lib.loc=lib)))
}

# non-exported utility when using devel version #3272: data.table:::.git()
.git = function(quiet=FALSE, lib.loc=NULL) {
  ans = unname(read.dcf(system.file("DESCRIPTION", package="data.table", lib.loc=lib.loc, mustWork=TRUE), fields="Revision")[, "Revision"])
  if (!quiet && is.na(ans))
    cat("Git revision is not available. Most likely data.table was installed from CRAN or local archive.\nGit revision is available when installing from our repositories 'https://Rdatatable.gitlab.io/data.table' and 'https://Rdatatable.github.io/data.table'.\n")
  ans
}

# nocov end
