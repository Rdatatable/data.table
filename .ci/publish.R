format.deps <- function(file, which) {
  deps.raw = read.dcf(file, fields=which)[[1L]]
  if (all(is.na(deps.raw))) return(character())
  deps.raw = gsub("\n", " ", deps.raw, fixed=TRUE)
  deps.full = trimws(strsplit(deps.raw, ", ", fixed=TRUE)[[1L]])
  deps.full.split = strsplit(deps.full, "(", fixed=TRUE)
  deps = trimws(sapply(deps.full.split, `[[`, 1L))
  vers = trimws(sapply(deps.full.split, function(x) if (length(x)>1L) paste0("(",x[[2L]]) else ""))
  vers = gsub(">=", "&ge;", vers, fixed=TRUE)
  vers = gsub("<=", "&le;", vers, fixed=TRUE)
  if (any(grepl(">", vers, fixed=TRUE), grepl("<", vers, fixed=TRUE), grepl("=", vers, fixed=TRUE)))
    stop("formatting dependencies version for CRAN-line package website failed because some dependencies have version defined using operators other than >= and <=")
  names(vers) <- deps
  base.deps = c("R", unlist(tools:::.get_standard_package_names(), use.names = FALSE))
  ans = sapply(deps, function(x) {
    if (x %in% base.deps) {
      if (nchar(vers[[x]])) paste(x, vers[[x]]) else x ## base R packages are not linked
    }
    else sprintf("<a href=\"https://cloud.r-project.org/package=%s\">%s</a>%s", x, x, if (nchar(vers[[x]])) paste0(" ",vers[[x]]) else "")
  })
  sprintf("<tr><td>%s:</td><td>%s</td></tr>", which, paste(ans, collapse=", "))
}

format.bins <- function(ver, bin_ver, cran.home, os.type, pkg, version, repodir) {
  if (os.type=="windows") {
    ext = "zip"
    plat.path = "windows"
  } else if (os.type=="macosx") {
    ext = "tgz"
    plat.path = "macosx/big-sur-arm64"
  } else stop("format.bins only valid for 'windows' or 'macosx' os.type")
  file = sprintf("bin/%s/contrib/%s/%s_%s.%s", plat.path, bin_ver, pkg, version, ext)
  fe = file.exists(file.path(repodir, file))
  ans = sprintf("%s: <a href=\"%s/%s\">%s_%s.%s</a>", ver, cran.home, file, pkg, version, ext)
  paste(ans[fe], collapse=", ")
}

format.entry <- function(field, dcf, url=FALSE) {
  if (field %in% colnames(dcf)) {
    value = gsub("\n", " ", dcf[,field], fixed=TRUE)
    if (url) {
      urls = trimws(strsplit(value, ",", fixed=TRUE)[[1L]])
      value = paste(sprintf("<a href=\"%s\">%s</a>", urls, urls), collapse=", ")
    }
    sprintf("<tr><td>%s:</td><td>%s</td></tr>", field, value)
  }
}
format.maintainer <- function(dcf) {
  if ("Maintainer" %in% colnames(dcf)) {
    text2html = function(x) {
      # https://stackoverflow.com/a/64446320/2490497
      splitted <- strsplit(x, "")[[1L]]
      intvalues <- as.hexmode(utf8ToInt(enc2utf8(x)))
      paste(paste0("&#x", intvalues, ";"), collapse = "")
    }
    tmp = gsub("@", " at ", dcf[,"Maintainer"], fixed=TRUE)
    sep = regexpr("<", tmp, fixed=TRUE)
    name = trimws(substr(tmp, 1L, sep-1L))
    mail = text2html(trimws(substr(tmp, sep, nchar(tmp))))
    sprintf("<tr><td>Maintainer:</td><td>%s  %s</td></tr>", name, mail)
  }
}
format.materials <- function() {
  return(NULL) ## TODO
  value = NA
  #NEWS
  #README
  sprintf("<tr><td>Materials:</td><td>%s</td></tr>", value)
}

package.index <- function(package, lib.loc, repodir="bus/integration/cran") {
  file = system.file("DESCRIPTION", package=package, lib.loc=lib.loc)
  dcf = read.dcf(file)
  pkg = dcf[,"Package"]
  version = dcf[,"Version"]
  if (!"Revision" %in% colnames(dcf)) dcf = cbind(dcf, Revision = NA_character_)
  tbl = c(
    sprintf("<tr><td>Version:</td><td>%s</td></tr>", version),
    sprintf("<tr><td>Revision:</td><td>%s</td></tr>", dcf[,"Revision"]), # some non data.table pkgs might have revision
    format.deps(file, "Depends"),
    format.deps(file, "Imports"),
    format.deps(file, "LinkingTo"),
    format.deps(file, "Suggests"),
    format.deps(file, "Enhances"),
    if ("Built" %in% colnames(dcf)) sprintf("<tr><td>Built:</td><td>%s</td></tr>", substr(trimws(strsplit(dcf[,"Built"], ";", fixed=TRUE)[[1L]][[3L]]), 1L, 10L)),
    if ("Author" %in% colnames(dcf)) sprintf("<tr><td>Author:</td><td>%s</td></tr>", dcf[,"Author"]),
    format.maintainer(dcf),
    format.entry("BugReports", dcf, url=TRUE),
    format.entry("License", dcf),
    format.entry("URL", dcf, url=TRUE),
    format.entry("NeedsCompilation", dcf),
    format.entry("SystemRequirements", dcf),
    format.materials(), ## TODO
    if (pkg=="data.table") sprintf("<tr><td>Checks:</td><td><a href=\"../../checks/check_results_%s.html\">%s results</a></td></tr>", pkg, pkg)
  )
  vign = tools::getVignetteInfo(pkg, lib.loc=lib.loc)
  r_rel_ver = Sys.getenv("R_REL_VERSION")
  r_dev_ver = Sys.getenv("R_DEV_VERSION")
  r_old_ver = Sys.getenv("R_OLD_VERSION")
  stopifnot(nzchar(r_rel_ver), nzchar(r_dev_ver), nzchar(r_old_ver))
  cran.home = "../../.."
  tbl.dl = c(
    sprintf("<tr><td> Reference manual: </td><td> <a href=\"%s.pdf\">%s.pdf</a>, <a href=\"%s/library/%s/html/00Index.html\">00Index.html</a> </td></tr>", pkg, pkg, cran.home, pkg),
    if (nrow(vign)) sprintf("<tr><td>Vignettes:</td><td>%s</td></tr>", paste(sprintf("<a href=\"%s/library/data.table/doc/%s\">%s</a><br/>", cran.home, vign[,"PDF"], vign[,"Title"]), collapse="\n")), # location unline cran web/pkg/vignettes to not duplicate content, documentation is in ../../../library
    sprintf("<tr><td> Package source: </td><td> <a href=\"%s/src/contrib/%s_%s.tar.gz\"> %s_%s.tar.gz </a> </td></tr>", cran.home,pkg, version, pkg, version),
    sprintf("<tr><td> Windows binaries: </td><td> %s </td></tr>", format.bins(ver=c("r-devel","r-release","r-oldrel"), bin_ver=c(r_dev_ver, r_rel_ver, r_old_ver), cran.home=cran.home, os.type="windows", pkg=pkg, version=version, repodir=repodir)),
    sprintf("<tr><td> macOS binaries: </td><td> %s </td></tr>", format.bins(ver=c("r-release","r-oldrel"), bin_ver=c(r_rel_ver, r_old_ver), cran.home=cran.home, os.type="macosx", pkg=pkg, version=version, repodir=repodir))
  )
  index.file = file.path(repodir, "web/packages", pkg, "index.html")
  if (!dir.exists(dirname(index.file))) dir.create(dirname(index.file), recursive=TRUE)
  writeLines(c(
    "<html>",
    "<head>",
    sprintf("<title>Package %s</title>", pkg),
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../CRAN_web.css\" />",
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />",
    "<style type=\"text/css\">table td { vertical-align: top; }</style>",
    "</head>",
    "<body>",
    sprintf("<h2>%s: %s</h2>", pkg, dcf[,"Title"]),
    sprintf("<p>%s</p>", dcf[,"Description"]),
    sprintf("<table summary=\"Package %s summary\">", pkg),
    tbl,
    "</table>",
    "<h4>Downloads:</h4>",
    sprintf("<table summary=\"Package %s downloads\">", pkg),
    tbl.dl,
    "</table>",
    "</body>",
    "</html>"
  ), index.file)
  file.exists(index.file)
}

lib.copy <- function(lib.from, repodir="bus/integration/cran"){
  pkgs.from<-list.dirs(lib.from, recursive=FALSE)
  pkgs.to<-list.dirs(lib.to<-file.path(repodir,"library"), recursive=FALSE)
  pkg.copy <- function(pkg.from, lib.to) {
    pkg<-basename(pkg.from);
    dir.create(file.path(lib.to, pkg), recursive=TRUE)
    lib.dirs<-intersect(c("help","html","doc"), all.lib.dirs<-list.dirs(pkg.from, full.names=FALSE))
    ans1<-setNames(file.copy(file.path(pkg.from, lib.dirs), file.path(lib.to, pkg), recursive=TRUE), lib.dirs)
    lib.files<-setdiff(list.files(pkg.from), all.lib.dirs)
    ans2<-setNames(file.copy(file.path(pkg.from, lib.files), file.path(lib.to, pkg)), lib.files)
    all(ans1, ans2)
  }
  pkgs.from.new<-pkgs.from[!basename(pkgs.from) %in% basename(pkgs.to)]
  setNames(sapply(pkgs.from.new, pkg.copy, lib.to=lib.to), basename(pkgs.from.new))
}

doc.copy <- function(repodir="bus/integration/cran"){
  cp1<-c("COPYING","AUTHORS","THANKS");
  ans1<-setNames(file.copy(file.path(R.home("doc"), cp1), file.path(repodir, "doc", cp1)), cp1); cp2<-c("html","manual");
  ans2<-setNames(file.copy(file.path(R.home("doc"), cp2), file.path(repodir,"doc"), recursive=TRUE), cp2);
  c(ans1, ans2)
}

plat <- function(x) if (grepl("^.*win", x)) "Windows" else if (grepl("^.*mac", x)) "macOS" else "Linux"

r.ver <- function(x) {
  tmp = strsplit(x, "-", fixed=TRUE)[[1L]]
  if (length(tmp) < 3L) stop("test job names must be test-[lin|win|mac]-[r.version]-...")
  v = tmp[3L]
  if (identical(v, "rel")) "r-release"
  else if (identical(v, "dev")) "r-devel"
  else if (identical(v, "old") || identical(v, "ancient")) "r-oldrel"
  else {
    if (grepl("\\D", v)) stop("third word in test job name must be rel/dev/old or numbers of R version")
    paste0("r-", paste(strsplit(v, "")[[1L]], collapse="."))
  }
}

# this for now is constant but when we move to independent pipelines (commit, daily, weekly) those values can be different
pkg.version <- function(job, pkg) {
  Rcheck = file.path("bus", job, paste(pkg, "Rcheck", sep="."))
  if (!dir.exists(Rcheck))
    return(NA_character_)
  dcf = read.dcf(file.path(Rcheck, "00_pkg_src", pkg, "DESCRIPTION"))
  dcf[,"Version"]
}
pkg.revision <- function(job, pkg) {
  Rcheck = file.path("bus", job, paste(pkg, "Rcheck", sep="."))
  if (!dir.exists(Rcheck))
    return(NA_character_)
  dcf = read.dcf(file.path(Rcheck, "00_pkg_src", pkg, "DESCRIPTION"))
  if ("Revision" %in% colnames(dcf)) {
    proj.url = Sys.getenv("CI_PROJECT_URL", "")
    if (!nzchar(proj.url)) {
      warning("pkg.revision was designed to be run on GLCI where CI_PROJECT_URL var is set, links to commits will not be produced for checks table")
      substr(dcf[,"Revision"], 1, 7)
    } else {
      sprintf("<a href=\"%s\">%s</a>", file.path(proj.url, "-", "commit", dcf[,"Revision"]), substr(dcf[,"Revision"], 1, 7))
    }
  } else ""
}
pkg.flags <- function(job, pkg) {
  Rcheck = file.path("bus", job, paste(pkg, "Rcheck", sep="."))
  if (!dir.exists(Rcheck))
    return(NA_character_)
  cc = file.path(Rcheck, pkg, "cc") ## data.table style cc file
  if (file.exists(cc)) {
    d = readLines(cc)
    w.cflags = substr(d, 1, 7)=="CFLAGS="
    if (sum(w.cflags)==1L)
      return(sub("CFLAGS=", "", d[w.cflags], fixed=TRUE))
  }
  ""
}

check.copy <- function(job, repodir="bus/integration/cran"){
  dir.create(job.checks<-file.path(repodir, "web", "checks", pkg<-"data.table", job), recursive=TRUE);
  os = plat(job)
  from = file.path("bus", sprintf("%s/%s.Rcheck", job, pkg))
  tests = list.files("tests", pattern="\\.R$")
  current.rout = c(paste0(tests, "out"), paste0(tests, "out.fail"))
  dir.create(file.path(job.checks, "tests"), showWarnings=FALSE)
  routs = file.path("tests", current.rout)
  file.copy(file.path(from, routs)[file.exists(file.path(from, routs))], file.path(job.checks, "tests"))
  inst.check.files = file.path(from, inst.check<-c("00install.out","00check.log"))
  file.copy(inst.check.files[file.exists(inst.check.files)], job.checks)
  setNames(file.exists(file.path(job.checks, c(inst.check, routs))), c(inst.check, routs))
}

check.flavors <- function(jobs, repodir="bus/integration/cran") {
  th = "<th>Flavor</th><th>R Version</th><th>OS Type</th><th>CPU Type</th><th>OS Info</th><th>CPU Info</th><th>Compilers</th>"
  tbl = sprintf(
    "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    sub("test-", "", jobs, fixed=TRUE),
    sapply(jobs, r.ver),
    sapply(jobs, plat),
    "", # "x86_64"
    "", # "Debian GNU/Linux testing"
    "", # "2x 8-core Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz"
    "" # "GCC 10.2.0 (Debian 10.2.0-13)"
  )
  file = file.path(repodir, "web/checks", "check_flavors.html")
  writeLines(c(
    "<html>",
    "<head>",
    "<title>Package Check Flavors</title>",
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"../CRAN_web.css\"/>",
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>",
    "</head>",
    "<body lang=\"en\">",
    "<h2>Package Check Flavors</h2>",
    sprintf("<p>Last updated on %s.</p>", format(Sys.time(), usetz=TRUE)),
    "<table border=\"1\" summary=\"CRAN check flavors.\">",
    "<tr>",th,"</tr>",
    tbl,
    "</table>",
    "</body>",
    "</html>"
  ), file)
  setNames(file.exists(file), file)
}

log.copy <- function(job, repodir="bus/integration/cran") {
  dir.create(job.checks<-file.path(repodir, "web", "checks", pkg<-"data.table", job), recursive=TRUE, showWarnings=FALSE)
  to = file.path(job.checks, "log")
  if (!file.exists(job_id_file <- file.path("bus", job, "id")))
    return(setNames(file.exists(to), "log"))
  job_id = readLines(job_id_file, warn=FALSE)[1L]
  from = sprintf("https://gitlab.com/Rdatatable/data.table/-/jobs/%s/raw", job_id)
  download.file(from, to, method="wget", quiet=TRUE)
  Sys.sleep(0.1) ## to not get ban from gitlab.com
  setNames(file.exists(to), "log")
}
ci.status <- function(job) {
  if (!file.exists(status_file <- file.path("bus", job, "status")))
    return(NA_character_)
  readLines(status_file, warn=FALSE)[1L]
}
ci.log <- function(jobs, repodir="bus/integration/cran") {
  pkg = "data.table"
  ans = vector("character", length(jobs))
  logs = sapply(jobs, log.copy, repodir=repodir)
  statuses = sapply(jobs, ci.status)
  statuses[statuses=="success"] = paste0("<span class=\"check_ok\">",statuses[statuses=="success"],"</span>")
  statuses[statuses=="failed"] = paste0("<span class=\"check_ko\">",statuses[statuses=="failed"],"</span>")
  ans[!logs] = statuses[!logs]
  ans[logs] = sprintf('<a href=\"%s/%s/log\">%s</a>', pkg[any(logs)], jobs[logs], statuses[logs])
  ans
}

check.index <- function(pkg, jobs, repodir="bus/integration/cran") {
  status = function(x) if (grepl("^.*ERROR", x)) "<span class=\"check_ko\">ERROR</span>" else if (grepl("^.*WARNING", x)) "<span class=\"check_ko\">WARNING</span>" else if (grepl("^.*NOTE", x)) "<span class=\"CRAN\">NOTE</span>" else if (grepl("^.*OK", x)) "<span class=\"check_ok\">OK</span>" else NA_character_
  test.files = function(job, trim=TRUE, pkg="data.table") {
    files = paste0("tests/", list.files("tests", pattern="\\.R$"), "out.fail")
    links = sapply(files, function(file) {
      if (!file.exists(file.path(repodir, "web/checks", pkg, job, file))) return(NA_character_)
      dir = if (!identical(d<-dirname(file), ".")) d
      sprintf("<a href=\"%s/%s/%s\">%s</a>", pkg, job, file,
              if (trim) sub(".Rout.fail", "", basename(file), fixed=TRUE) else file)
    })
    paste(na.omit(links), collapse=", ")
  }
  th = "<th>Flavor</th><th>Version</th><th>Revision</th><th>Install</th><th>Check</th><th>Flags</th><th>Rout.fail</th><th>Log</th>"
  tbl = sprintf(
    "<tr><td><a href=\"check_flavors.html\">%s</a></td><td>%s</td><td>%s</td><td><a href=\"%s/%s/00install.out\">out</a></td><td><a href=\"%s/%s/00check.log\">%s</a></td><td>%s</td><td>%s</td><td>%s</td></tr>",
    sub("test-", "", jobs, fixed=TRUE), ## Flavor
    sapply(jobs, pkg.version, pkg),     ## Version
    sapply(jobs, pkg.revision, pkg),    ## Revision
    pkg, jobs,                          ## Install
    pkg, jobs, sapply(sapply(jobs, check.test, pkg="data.table"), status), ## Check
    sapply(jobs, pkg.flags, pkg),                                          ## Flags
    sapply(jobs, test.files),                                              ## Rout.fail
    ci.log(jobs)                                                           ## CI job logs
  )
  file = file.path(repodir, "web/checks", sprintf("check_results_%s.html", pkg))
  writeLines(c(
    "<html>",
    "<head>",
    sprintf("<title>Package Check Results for Package %s</title>", pkg),
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"../CRAN_web.css\"/>",
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>",
    "</head>",
    "<body lang=\"en\">",
    sprintf("<h2>Package Check Results for Package <a href=\"../packages/%s/index.html\"> %s </a> </h2>", pkg, pkg),
    sprintf("<p>Last updated on %s.</p>", format(Sys.time(), usetz=TRUE)),
    sprintf("<table border=\"1\" summary=\"CRAN check results for package %s\">", pkg),
    "<tr>",th,"</tr>",
    tbl,
    "</table>",
    "</body>",
    "</html>"
  ), file)
  setNames(file.exists(file), file)
}

pdf.copy <- function(pkg, job, repodir="bus/integration/cran") {
  if (!dir.exists(pkg.to<-file.path(repodir,"web","packages",pkg))) dir.create(pkg.to, recursive=TRUE);
  file.copy(file.path("bus", job, sprintf("%s.Rcheck", pkg), sprintf("%s-manual.pdf",pkg)), to=pdf.to<-file.path(pkg.to, sprintf("%s.pdf",pkg)))
  setNames(file.exists(pdf.to), pdf.to)
}

check.test <- function(job, pkg) {
  file = file.path("bus", job, sprintf("%s.Rcheck", pkg), "00check.log")
  if (!file.exists(file)) return(NA_character_)
  check = readLines(file)
  check[length(check)]
}

