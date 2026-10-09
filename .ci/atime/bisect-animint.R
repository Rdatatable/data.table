remotes::install_github("tdhock/atime@bisect")
pkg.path <- "~/R/data.table"
Test <- "fread N=cols regression"
atime::bisect(pkg.path, Test)

Test.dir <- file.path(pkg.path, ".ci", "atime", "bisect", atime:::test_file_name(Test))
library(data.table)
(results.wide <- data.table(
  csv=Sys.glob(file.path(Test.dir, "csv", "*"))
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

show.versions <- unique(results.long$version)
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
for(result.RData in Sys.glob(file.path(Test.dir, "RData", "*"))){
  (objs <- load(result.RData))
  commit <- sub(".RData", "", basename(result.RData))
  details <- tres.or.status$measurements[
    show.versions,
    on="version",
    data.table(commit, expr.name, version, N, median, min, max)
  ]
  common.dt.list[[commit]] <- details[, .(
    versions=.N
  ), by=N][
    versions==max(versions)
  ][.N, data.table(commit, N)]
  evers <- unique(details[, .(expr.name, version)])
  details.dt.list[[commit]] <- details
  dot.dt.list[[commit]] <- data.table(
    commit,
    tpred$prediction[evers, on="expr.name"])
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
(common.dt <- rbindlist(common.dt.list))

limit.dt <- data.table(seconds=tpred$seconds.limit)
library(animint2)
animint(
  overview=ggplot()+
    ggtitle("Overview, select commit")+
    theme_bw()+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank,
      fill=Status),
      color=NA,
      data=results.wide)+
    geom_line(aes(
      rank, value, color=version, group=version),
      data=results.long)+
    facet_grid(variable ~ ., scales="free")+
    theme(axis.text.x=element_text(angle=30, hjust=1))+
    geom_label_aligned(aes(
      rank, value, label=version, color=version),
      data=results.long[rank==max(rank)],
      hjust=0,
      alignment="vertical")+
    scale_fill_manual(values=c(
      "0"="grey",
      "1"="white"))+
    scale_y_continuous("")+
    scale_x_continuous("commit rank over time")+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5),
      clickSelects="commit",
      alpha=1,
      alpha_off=0,
      color="black",
      color_off=NA,
      fill=NA,
      data=results.wide),
  details=ggplot()+
    ggtitle("Details for selected commit")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(width=1000)+
    geom_hline(aes(
      yintercept=seconds),
      color="grey",
      data=limit.dt)+
    geom_text(aes(
      0, seconds*1.1,
      label=sprintf(
        "time limit = %s seconds",
        paste(seconds)
      )),
      hjust=0,
      color="grey50",
      data=limit.dt)+
    geom_vline(aes(
      xintercept=N,
      key=1),
      data=common.dt,
      color="grey",
      showSelected="commit")+
    geom_text(aes(
      N, 0, label=sprintf("largest common N=%s", paste(N)),
      key=1),
      data=common.dt,
      hjust=1,
      color="grey50",
      showSelected="commit")+
    geom_text(aes(
      0, 0, label=commit,
      key=1),
      hjust=0,
      vjust=0,
      data=common.dt,
      showSelected="commit")+
    geom_ribbon(aes(
      N, ymin=min, ymax=max,
      key=version, fill=version, group=version),
      showSelected=c("version","commit"),
      alpha=0.5,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=version, color=version, group=version),
      showSelected=c("version","commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=version, color=version),
      showSelected=c("version","commit"),
      fill="white",
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=version,
      key=version, color=version),
      showSelected=c("version","commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    scale_x_log10()+
    scale_y_log10("seconds"),
  duration=list(
    commit=1000)
)

