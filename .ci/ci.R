## most of functions from R tools4pkgs branch
## https://github.com/wch/r-source/tree/tools4pkgs
## https://svn.r-project.org/R/branches/tools4pkgs/src/library/tools/R/packages.R

## added ver argument to produce R version independent urls
## https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17420
contrib.url <-
function (repos, type = getOption("pkgType"), ver)
{
  type <- utils:::resolvePkgType(type)
  if (is.null(repos))
    return(NULL)
  if ("@CRAN@" %in% repos && interactive()) {
    cat(gettext("--- Please select a CRAN mirror for use in this session ---"), "\n", sep = "")
    flush.console()
    chooseCRANmirror()
    m <- match("@CRAN@", repos)
    nm <- names(repos)
    repos[m] <- getOption("repos")["CRAN"]
    if (is.null(nm))
      nm <- rep("", length(repos))
    nm[m] <- "CRAN"
    names(repos) <- nm
  }
  if ("@CRAN@" %in% repos)
    stop("trying to use CRAN without setting a mirror")
  if(missing(ver)) {
    ver <- paste(R.version$major, strsplit(R.version$minor, ".", fixed=TRUE)[[1L]][1L], sep = ".")
  } else {
    stopifnot(is.character(ver), length(ver)>0L, !is.na(ver))
  }
  mac.path <- "macosx"
  if (substr(type, 1L, 11L) == "mac.binary.") {
    mac.path <- paste(mac.path, substring(type, 12L), sep = "/")
    type <- "mac.binary"
  }
  res <- switch(
    type,
    source = paste(gsub("/$", "", repos), "src", "contrib", sep = "/"),
    mac.binary = paste(gsub("/$", "", repos), "bin", mac.path, "contrib", ver, sep = "/"),
    win.binary = paste(gsub("/$", "", repos), "bin", "windows", "contrib", ver, sep = "/")
  )
  res
}

## returns dependencies for a package based on its DESCRIPTION file
dcf.dependencies <-
function(file = "DESCRIPTION",
         which = NA,
         except.priority = "base",
         exclude = NULL) {
  if (!is.character(file) || !length(file) || !all(file.exists(file)))
    stop("file argument must be character of filepath(s) to existing DESCRIPTION file(s)")
  if (!is.character(except.priority))
    stop("except.priority should be character vector")
  if (!(all(except.priority %in% c("base","recommended")) || identical(except.priority, character(0))))
    stop("except.priority accept 'base', 'recommended', both or empty character vector")
  which_all <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  if (is.na(which))
    which = c("Depends", "Imports", "LinkingTo")
  else if (identical(which, "all"))
    which <- which_all
  else if (identical(which, "most"))
    which <- c("Depends", "Imports", "LinkingTo", "Suggests")
  if (!is.character(which) || !length(which) || !all(which %in% which_all))
    stop("which argument accept only valid dependency relation: ", paste(which_all, collapse=", "))
  x <- unlist(lapply(file, function(f, which) {
      dcf <- tryCatch(read.dcf(f, fields = which), error = identity)
      if (inherits(dcf, "error") || !length(dcf))
          warning(gettextf("error reading file '%s'", f), domain = NA, call. = FALSE)
      else dcf[!is.na(dcf)]
  }, which = which), use.names = FALSE)
  local.extract_dependency_package_names = function (x) {
    ## do not filter out R like tools:::.extract_dependency_package_names, used for web/$pkg/index.html
    if (is.na(x))
      return(character())
    x <- unlist(strsplit(x, ",[[:space:]]*"))
    x <- sub("[[:space:]]*([[:alnum:].]+).*", "\\1", x)
    x[nzchar(x)]
  }
  x <- unlist(lapply(x, local.extract_dependency_package_names))
  except <- if (length(except.priority)) c("R", unlist(tools:::.get_standard_package_names()[except.priority], use.names = FALSE))
  x = setdiff(x, except)
  if (length(exclude)) {  # to exclude knitr/rmarkdown, 5294
    if (!is.character(exclude) || anyDuplicated(exclude))
      stop("exclude may be NULL or a character vector containing no duplicates")
    x = setdiff(x, exclude)
  }
  x
}

## returns additional repositories for dependency packages based on its DESCRIPTION file
dcf.repos <-
function(file = "DESCRIPTION") {
  if (!is.character(file) || !length(file) || !all(file.exists(file)))
    stop("file argument must be character of filepath(s) to existing DESCRIPTION file(s)")
  x <- unlist(lapply(file, function(f) {
    dcf <- tryCatch(read.dcf(f, fields = "Additional_repositories"), error = identity)
    if (inherits(dcf, "error") || !length(dcf))
      warning(gettextf("error reading file '%s'", f), domain = NA, call. = FALSE)
    else dcf[!is.na(dcf)]
  }), use.names = FALSE)
  x <- trimws(unlist(strsplit(trimws(x), ",", fixed = TRUE), use.names = FALSE))
  unique(x)
}

## Mirror subset of CRAN
## download dependencies recursively for provided packages
## put all downloaded packages into local repository
mirror.packages <-
function(pkgs,
         which = c("Depends", "Imports", "LinkingTo"),
         repos = getOption("repos"),
         type = c("source", "mac.binary.big-sur-arm64", "win.binary"),
         repodir,
         except.repodir = repodir,
         except.priority = "base",
         method,
         quiet = TRUE,
         binary.ver,
         ...) {
  if (!length(pkgs)) # edge case friendly
    return(NULL)
  if (!is.character(pkgs))
    stop("pkgs argument must be character vector of packages to mirror from repository")
  if (missing(repodir) || !is.character(repodir) || length(repodir)!=1L)
    stop("repodir argument must be non-missing scalar character, local path to repo mirror")
  if (!dir.exists(repodir) && !dir.create(repodir, recursive = TRUE, showWarnings = FALSE))
    stop("Path provided in 'repodir' argument does not exists and could not be created")
  if (missing(type) && .Platform$OS.type == "windows")
    type <- "win.binary"
  type <- match.arg(type)
  if (!missing(binary.ver)) {
    if (!is.character(binary.ver) || length(binary.ver)!=1L || is.na(binary.ver))
      stop("binary.ver must be non-NA scalar character of type '3.5' so path to arbitrary binaries version can be resolved")
  } else binary.ver <- paste(R.version$major, strsplit(R.version$minor, ".", fixed=TRUE)[[1L]][1L], sep = ".")
  destdir <- contrib.url(repodir, type = type, ver = binary.ver)
  if (!dir.exists(destdir) && !dir.create(destdir, recursive = TRUE, showWarnings = FALSE))
    stop(sprintf("Your repo directory provided in 'repodir' exists, but does not have '%s' dir tree and it could not be created", destdir))
  if (length(except.repodir) && (!is.character(except.repodir) || length(except.repodir)!=1L || !dir.exists(except.repodir)))
    stop("except.repodir argument must be non-missing scalar character, local path to existing directory")
  if (!is.character(except.priority) || !length(except.priority) || !all(except.priority %in% c("base","recommended")))
    stop("except.priority accept 'base', 'recommended', both")
  if (!is.logical(quiet) || length(quiet)!=1L || is.na(quiet))
    stop("quiet argument must be TRUE or FALSE")
  which_all <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  if (identical(which, "all"))
    which <- which_all
  else if (identical(which, "most"))
    which <- c("Depends", "Imports", "LinkingTo", "Suggests")
  if (!is.character(which) || !length(which) || !all(which %in% which_all))
    stop("which argument accept only valid dependency relations: ", paste(which_all, collapse=", "))
  ## possible interactive CRAN menu
  repos.url <- contrib.url(repos, type = type, ver = binary.ver)
  db <- utils::available.packages(repos.url, type = type)
  allpkgs <- c(pkgs, unlist(tools::package_dependencies(unique(pkgs), db, which, recursive = TRUE), use.names = FALSE))
  except <- c("R", unlist(tools:::.get_standard_package_names()[except.priority], use.names = FALSE))
  ## do not re-download existing packages with the right version
  if (length(except.repodir) && file.exists(file.path(contrib.url(except.repodir, type = type, ver = binary.ver), "PACKAGES"))) {
    except.curl <- contrib.url(file.path("file:", normalizePath(except.repodir)), type = type, ver = binary.ver)
    except.db <- utils::available.packages(except.curl, type = type, fields = "Package")
    except <- c(except, merge(db, except.db, by = c("Package", "Version", "MD5sum"))[,"Package"])
  }
  newpkgs <- setdiff(allpkgs, except)
  if (!all(availpkgs<-newpkgs %in% rownames(db))) {
    ## source packages are considered mandatory due to _R_CHECK_FORCE_SUGGESTS_=true policy
    if (type=="source")
      stop(sprintf("Packages sources could not be found in provided repositories: %s", paste(newpkgs[!availpkgs], collapse = ", ")))
    warning(sprintf("Packages binaries could not be found in provided reposistories for R version %s: %s", binary.ver, paste(newpkgs[!availpkgs], collapse = ", ")))
    newpkgs <- newpkgs[availpkgs]
  }

  typeshort <- if (startsWith(type, "mac.binary.")) "mac.binary" else type
  pkgsext <- switch(typeshort,
                    "source" = "tar.gz",
                    "mac.binary" = "tgz",
                    "win.binary" = "zip")
  ## clean up stale package files for which new versions will be downloaded
  if (file.exists(file.path(destdir, "PACKAGES"))) {
    repo.db <- utils::available.packages(file.path("file:", normalizePath(destdir)), type = type)
    oldver <- repo.db[repo.db[, "Package"] %in% newpkgs, c("Package", "Version"), drop=FALSE]
    oldfiles <- file.path(destdir, sprintf("%s_%s.%s", oldver[,"Package"], oldver[,"Version"], pkgsext))
    unlink(oldfiles[file.exists(oldfiles)])
  }
  pkgsver <- db[db[, "Package"] %in% newpkgs, c("Package", "Version"), drop=FALSE]
  dlfiles <- file.path(destdir, sprintf("%s_%s.%s", pkgsver[,"Package"], pkgsver[,"Version"], pkgsext))
  unlink(dlfiles[file.exists(dlfiles)])
  ## repos argument is not used in download.packages, only as default for contriburl argument
  ## we provide contriburl to avoid interactive CRAN menu popup twice in mirror.packages
  dp <- utils::download.packages(pkgs = newpkgs, destdir = destdir,
                                 available = db, contriburl = repos.url,
                                 type = type, method = method, quiet = quiet)
  tools::write_PACKAGES(dir = destdir, type = typeshort, ...)
  dp
}

