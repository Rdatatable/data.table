# NB: methods _has_ to be attached before data.table in order for methods::as() to
#   find the right dispatch when trying as(x, "IDate"). This might be an R bug, but
#   even running library(methods, pos="package:base") after attaching data.table doesn't work.
library(methods)
library(data.table)
test.data.table(script="S4.Rraw")
