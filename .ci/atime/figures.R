library(ggplot2)
pkg.info <- atime::atime_pkg_test_info("~/R/data.table")
(one.call <- pkg.info$test.call[["shallow regression fixed in #4440"]])
(one.result <- eval(one.call))

myplot <- function(name, obj, width, xmax){
  obj$unit.col.vec <- c(seconds="median")
  png(name, width=width, height=2.5, units="in", res=200)
  gg <- plot(obj)+
    facet_null()+
    scale_color_manual(values=pkg.info$version.colors)+
    scale_fill_manual(values=pkg.info$version.colors)+
    scale_x_log10(
      "N = number of rows",
      breaks=10^seq(1, 7, by=2),
      limits=c(NA,xmax))+
    scale_y_log10("Computation time (seconds)")
  print(gg)
  dev.off()
}
s.res <- eval(one.call[names(one.call) != "sha.vec"])
myplot("data.table-atime_versions.png", s.res, 3.5, 5e7)

myplot("data.table-atime_test.png", one.result, 4, 1e10)
