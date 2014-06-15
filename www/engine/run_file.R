in_file  = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/01.introduction.md"
out_file = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/01.introduction.Rmd"
source("~/Dropbox/Public/github/Rdatatable/datatable/www/engine/scripts/parser.R")
parser(in_file, out_file)


in_file  = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/00.parser.md"
out_file = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/00.parser.Rmd"
source("~/Dropbox/Public/github/Rdatatable/datatable/www/engine/scripts/parser.R")
parser(in_file, out_file)


in_file  = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/02.quick-intro.md"
out_file = "~/Dropbox/Public/github/Rdatatable/datatable/www/engine/R/02.quick-intro.Rmd"
source("~/Dropbox/Public/github/Rdatatable/datatable/www/engine/scripts/parser.R")
parser(in_file, out_file)

# pandoc --smart --standalone -c ../css/bootstrap.css --from markdown --to html -o ../about/index.html R/01.introduction.Rmd
# pandoc --smart --standalone -c ../css/bootstrap.css --from markdown --to html -o ../internals/index.html R/00.parser.Rmd
# pandoc --smart --standalone -c ../css/bootstrap.css --from markdown --to html -o ../intro/index.html R/02.quick-intro.Rmd
