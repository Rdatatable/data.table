remotes::install_github("tdhock/atime@bisect")
atime::bisect("~/R/data.table", "fread N=cols regression")

library(data.table)
(results.wide <- data.table(
  csv=Sys.glob("~/R/data.table/.ci/atime/bisect/fread_N=cols_regression/csv/*")
)[
, fread(csv), by=csv
][
, rank := rank(time)
][order(rank)][, let(
  Rank = factor(rank, labels=format(time)),
  Status = factor(status)
)][])
(results.long <- melt(
  results.wide,
  measure.vars=measure(variable, version, sep=".")))
library(ggplot2)
gg <- ggplot()+
  geom_vline(aes(
    xintercept=Rank,
    linetype=Status),
    data=results.wide)+
  geom_line(aes(
    Rank, value, color=version, group=version),
    data=results.long)+
  facet_grid(variable ~ ., scales="free")+
  theme(axis.text.x=element_text(angle=30, hjust=1))
directlabels::direct.label(gg, "right.polygons")
