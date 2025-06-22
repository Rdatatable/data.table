#!/usr/bin/Rscript
# Runner for the manual lint checks in .ci/linters
args = commandArgs(TRUE)
if (identical(args, '--help')) {
  writeLines(c(
    'Usage: Rscript .ci/lint.R .ci/linters/<KIND> <WHERE> <WHAT> [PREPROCESS]',
    'KIND must name the directory containing the *.R files defining the linter functions.',
    'WHERE must name the directory containing the files to lint, e.g. "po", or "src".',
    "WHAT must contain the regular expression matching the files to lint, e.g., '[.]po$', or '[.][ch]$'.",
  ))
  q('no')
}
stopifnot(`Invalid arguments, see .ci/lint.R --help` = length(args) == 3)

linter_env = list2env(list(.preprocess = identity))
for (f in list.files(args[[1]], full.names=TRUE)) sys.source(f, linter_env)
if (!length(ls(linter_env))) stop(
  "No linters found after sourcing files in ", dQuote(args[[1]])
)

sources = list.files(args[[2]], pattern = args[[3]], full.names = TRUE, recursive = TRUE)
if (!length(sources)) stop(
  "No files to lint found in directory ", dQuote(args[[2]]), " for mask ", dQuote(args[[3]])
)
sources = Filter(Negate(is.null), lapply(setNames(nm = sources), linter_env$.preprocess))

okay = TRUE
for (src in names(sources))
for (linter in ls(linter_env)) tryCatch(
  linter_env[[linter]](sources[[src]]),
  error = function(e) {
    message('Source file ', dQuote(src), ' failed lint check ', dQuote(linter), ': ', conditionMessage(e))
    okay <<- FALSE
  }
)
stopifnot(`Please fix the issues above.` = okay)
