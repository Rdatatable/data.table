format.deps <- function(file, which) {
  deps.raw = read.dcf(file, fields=which)[[1L]]
  if (all(is.na(deps.raw))) return(character())
  deps.full = trimws(strsplit(deps.raw, ", ", fixed=TRUE)[[1L]])
  deps = trimws(sapply(strsplit(deps.full, "(", fixed=TRUE), `[[`, 1L))
  names(deps.full) <- deps
  base.deps = c("R", unlist(tools:::.get_standard_package_names(), use.names = FALSE))
  ans = sapply(deps, function(x) {
    if (x %in% base.deps) deps.full[[x]]
    else sprintf("<a href=\"../%s/index.html\">%s</a>", x, deps.full[[x]])
  })
  sprintf("<tr><td>%s:</td><td>%s</td></tr>", which, paste(ans, collapse=", "))
}

format.bins <- function(ver, bin_ver, cran.home, os.type, pkg, version, repodir) {
  if (os.type=="windows") {
    ext = "zip"
    plat.path = "windows"
  } else if (os.type=="macosx") {
    ext = "tgz"
    plat.path = "macosx/el-capitan"
  } else stop("format.bins only valid for 'windows' or 'macosx' os.type")
  file = sprintf("bin/%s/contrib/%s/%s_%s.%s", plat.path, bin_ver, pkg, version, ext)
  fe = file.exists(file.path(repodir, file))
  ans = sprintf("%s: <a href=\"%s/%s\">%s_%s.%s</a>", ver, cran.home, file, pkg, version, ext)
  paste(ans[fe], collapse=", ")
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
    if (pkg=="data.table") sprintf("<tr><td>Checks:</td><td><a href=\"../../checks/check_results_%s.html\">%s results</a></td></tr>", pkg, pkg)
  )
  vign = tools::getVignetteInfo(pkg, lib.loc=lib.loc)
  r_bin_ver = Sys.getenv("R_BIN_VERSION")
  r_devel_bin_ver = Sys.getenv("R_DEVEL_BIN_VERSION")
  stopifnot(nzchar(r_bin_ver), nzchar(r_devel_bin_ver))
  cran.home = "../../.."
  tbl.dl = c(
    sprintf("<tr><td> Reference manual: </td><td> <a href=\"%s.pdf\">%s.pdf</a>, <a href=\"%s/library/%s/html/00Index.html\">00Index.html</a> </td></tr>", pkg, pkg, cran.home, pkg),
    if (nrow(vign)) sprintf("<tr><td>Vignettes:</td><td>%s</td></tr>", paste(sprintf("<a href=\"%s/library/data.table/doc/%s\">%s</a><br/>", cran.home, vign[,"PDF"], vign[,"Title"]), collapse="\n")), # location unline cran web/pkg/vignettes to not duplicate content, documentation is in ../../../library
    sprintf("<tr><td> Package source: </td><td> <a href=\"%s/src/contrib/%s_%s.tar.gz\"> %s_%s.tar.gz </a> </td></tr>", cran.home,pkg, version, pkg, version),
    sprintf("<tr><td> Windows binaries: </td><td> %s </td></tr>", format.bins(ver=c("r-devel","r-release"), bin_ver=c(r_devel_bin_ver,r_bin_ver), cran.home=cran.home, os.type="windows", pkg=pkg, version=version, repodir=repodir)),
    sprintf("<tr><td> OS X binaries: </td><td> %s </td></tr>", format.bins(ver=c("r-devel","r-release"), bin_ver=c(r_devel_bin_ver, r_bin_ver), cran.home=cran.home, os.type="macosx", pkg=pkg, version=version, repodir=repodir))
  )
  if (pkg=="data.table") {
    registry = Sys.getenv("CI_REGISTRY", "registry.gitlab.com")
    namespace = Sys.getenv("CI_PROJECT_NAMESPACE", "Rdatatable")
    project = Sys.getenv("CI_PROJECT_NAME", "data.table")
    images = c("r-release","r-devel","r-release-builder")
    images.title = c("Base R release", "Base R development", "R release package builder")
    tags = rep("latest", 3)
    docker.dl = sprintf("<tr><td> %s: </td><td> <pre><code>docker pull %s/%s/%s/%s:%s</code></pre> </td></tr>", images.title, tolower(registry), tolower(namespace), tolower(project), tolower(images), tags)
  }
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
    sprintf("<h2>%s</h2>", dcf[,"Title"]),
    sprintf("<p>%s</p>", dcf[,"Description"]),
    sprintf("<table summary=\"Package %s summary\">", pkg),
    tbl,
    "</table>",
    "<h4>Downloads:</h4>",
    sprintf("<table summary=\"Package %s downloads\">", pkg),
    tbl.dl,
    "</table>",
    if (pkg=="data.table")
      c("<h4>Docker images:</h4>",
        sprintf("<table summary=\"Package %s docker images\">", pkg),
        docker.dl,
        "</table>"),
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
    lib.dirs<-intersect(c("html","doc"), all.lib.dirs<-list.dirs(pkg.from, full.names=FALSE))
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

plat <- function(x) if (grepl("^.*win", x)) "Windows" else if (grepl("^.*osx", x)) "Mac OS X" else "Linux"

check.copy <- function(job, repodir="bus/integration/cran"){
  dir.create(job.checks<-file.path(repodir, "web", "checks", pkg<-"data.table", job), recursive=TRUE);
  os = plat(job)
  from = file.path("bus", sprintf("%s/%s.Rcheck", job, pkg))
  current.rout = c("main.Rout","main.Rout.fail","knitr.Rout","knitr.Rout.fail","memtest.csv","memtest.png")
  if (os=="Windows") {
    dir.create(file.path(job.checks, "tests_i386"), showWarnings=FALSE)
    dir.create(file.path(job.checks, "tests_x64"), showWarnings=FALSE)
    rout32 = file.path("tests_i386", current.rout)
    rout64 = file.path("tests_x64", current.rout)
    file.copy(file.path(from, rout32)[file.exists(file.path(from, rout32))], file.path(job.checks, "tests_i386"))
    file.copy(file.path(from, rout64)[file.exists(file.path(from, rout64))], file.path(job.checks, "tests_x64"))
    routs = c(rout32, rout64)
  } else if (os=="Mac OS X") {
    dir.create(file.path(job.checks, "tests"), showWarnings=FALSE)
    routs = file.path("tests", current.rout)
    file.copy(file.path(from, routs)[file.exists(file.path(from, routs))], file.path(job.checks, "tests"))
  } else {
    dir.create(file.path(job.checks, "tests"), showWarnings=FALSE)
    routs = file.path("tests", current.rout)
    file.copy(file.path(from, routs)[file.exists(file.path(from, routs))], file.path(job.checks, "tests"))
  }
  inst.check.files = file.path(from, inst.check<-c("00install.out","00check.log"))
  file.copy(inst.check.files[file.exists(inst.check.files)], job.checks)
  setNames(file.exists(file.path(job.checks, c(inst.check, routs))), c(inst.check, routs))
}

check.index <- function(pkg, jobs, repodir="bus/integration/cran") {
  status = function(x) if (grepl("^.*ERROR", x)) "ERROR" else if (grepl("^.*WARNING", x)) "WARNING" else if (grepl("^.*NOTE", x)) "NOTE" else if (grepl("^.*OK", x)) "OK" else NA_character_
  test.files = function(job, files, trim.name=FALSE, trim.exts=0L, pkg="data.table") {
    stopifnot(trim.name + as.logical(trim.exts) < 2L) # cannot use both
    links = sapply(files, function(file) {
      if (!file.exists(file.path(repodir, "web/checks", pkg, job, file))) return(NA_character_)
      dir = if (!identical(d<-dirname(file), ".")) d
      sprintf("<a href=\"%s/%s/%s\">%s</a>", pkg, job, file,
              if (trim.name) paste(c(dir, tools::file_ext(file)), collapse="/") else if (trim.exts) { for (i in 1:trim.exts) { file<-tools::file_path_sans_ext(file) }; file } else file)
    })
    paste(na.omit(links), collapse=", ")
  }
  routs = lapply(jobs, function(job) {
    current.rout = c("main.Rout.fail","knitr.Rout.fail")
    os = plat(job)
    if (os=="Windows") {
      rout32 = file.path("tests_i386", current.rout)
      rout64 = file.path("tests_x64", current.rout)
      routs = c(rout32, rout64)
    } else if (os=="Mac OS X") {
      routs = file.path("tests", current.rout)
    } else {
      routs = file.path("tests", current.rout)
    }
    routs
  })
  memouts = lapply(jobs, function(job) {
    current.memout = c("memtest.csv","memtest.png")
    os = plat(job)
    if (os=="Windows") {
      mem32 = file.path("tests_i386", current.memout)
      mem64 = file.path("tests_x64", current.memout)
      memouts = c(mem32, mem64)
    } else if (os=="Mac OS X") {
      memouts = file.path("tests", current.memout)
    } else {
      memouts = file.path("tests", current.memout)
    }
    memouts
  })
  tbl = sprintf("<tr><td>%s</td><td>%s</td><td><a href=\"%s/%s/00install.out\">out</a></td><td><a href=\"%s/%s/00check.log\">%s</a></td><td>%s</td><td>%s</td></tr>",
                sub("test-", "", jobs, fixed=TRUE),
                sapply(jobs, plat),
                pkg, jobs,
                pkg, jobs, sapply(sapply(jobs, check.test, pkg="data.table"), status),
                mapply(test.files, jobs, routs, trim.exts=2L), # 1st fail, 2nd Rout, keep just: tests_x64/main
                mapply(test.files, jobs, memouts, trim.name=TRUE))
  file = file.path(repodir, "web/checks", sprintf("check_results_%s.html", pkg))
  writeLines(c("<html>",
               "<head>",
               sprintf("<title>Package Check Results for Package %s</title>", pkg),
               "<link rel=\"stylesheet\" type=\"text/css\" href=\"../CRAN_web.css\"/>",
               "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>",
               "</head>",
               "<body lang=\"en\">",
               sprintf("<h2>Package Check Results for Package <a href=\"../packages/%s/index.html\"> %s </a> </h2>", pkg, pkg),
               sprintf("<p>Last updated on %s.</p>", format(Sys.time(), usetz=TRUE)),
               sprintf("<table border=\"1\" summary=\"CRAN check results for package %s\">", pkg),
               "<tr><th>Test job</th><th>OS type</th><th>Install</th><th>Check</th><th>Rout.fail</th><th>Memtest</th></tr>",
               tbl,
               "</table>",
               "</body>",
               "</html>"),
             file)
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

move.bin <- function(job, bin.version, os.type, file="DESCRIPTION", silent=FALSE) {
  if (os.type=="unix") {
    stop("publish of linux binaries not supported")
  } else if (os.type=="windows") {
    plat.path = "windows"
    extension = "zip"
  } else if (os.type=="macosx") {
    plat.path = "macosx/el-capitan"
    extension = "tgz"
  }
  dcf = read.dcf(file)
  pkg = dcf[,"Package"][[1L]]
  version = dcf[,"Version"][[1L]]
  src.path = file.path("bus",job,"cran/bin",plat.path,"contrib",bin.version)
  if (!silent && !dir.exists(src.path)) stop(sprintf("expected directory does not exists %s", src.path))
  bin.file = sprintf("%s_%s.%s", pkg, version, extension)
  tgt.path = file.path("bus/integration/cran/bin",plat.path,"contrib",bin.version)
  if (!file.exists(file.path(src.path, bin.file))) {
    if (!silent) stop(sprintf("expected binaries does not exists %s", file.path(src.path, bin.file)))
  } else {
    if (!dir.exists(tgt.path)) dir.create(tgt.path, recursive=TRUE)
    file.rename(file.path(src.path,bin.file), file.path(tgt.path,bin.file))
  }
  setNames(file.exists(file.path(tgt.path,bin.file)), file.path(tgt.path,bin.file))
}
