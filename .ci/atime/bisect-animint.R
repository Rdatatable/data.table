## This script is for visualizing results computed in bisect-7907.R
library(animint2)
library(data.table)

Test.dir <- "bisect/fread_N_cols_regression/Fast-Slow"
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
  measure.vars=measure(variable, expr.name, sep=".")))

details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
for(result.RData in Sys.glob(file.path(Test.dir, "RData", "*"))){
  (objs <- load(result.RData))
  commit <- sub(".RData", "", basename(result.RData))
  details <- tres.or.status$measurements[, .(
    commit, expr.name, N, median, min, max
  )]
  common.dt.list[[commit]] <- details[, .(
    versions=.N
  ), by=N][
    versions==max(versions)
  ][.N, data.table(commit, N)]
  details.dt.list[[commit]] <- details
  dot.dt.list[[commit]] <- data.table(
    commit,
    tpred$prediction)
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
(common.dt <- rbindlist(common.dt.list))

limit.dt <- data.table(seconds=tpred$seconds.limit)
animint(
  overview=ggplot()+
    ggtitle("Overview, select commit")+
    theme_bw()+
    theme_animint(height=600)+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank,
      fill=Status),
      color=NA,
      data=results.wide)+
    geom_line(aes(
      rank, value, color=expr.name, group=expr.name),
      data=results.long)+
    facet_grid(variable ~ ., scales="free")+
    theme(axis.text.x=element_text(angle=30, hjust=1))+
    geom_label_aligned(aes(
      rank, value, label=expr.name, color=expr.name),
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
    theme_animint(width=600, height=600, last_in_row=TRUE)+
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
      key=expr.name, fill=expr.name, group=expr.name),
      showSelected=c("expr.name","commit"),
      alpha=0.5,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=expr.name, color=expr.name, group=expr.name),
      showSelected=c("expr.name","commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=expr.name, color=expr.name),
      showSelected=c("expr.name","commit"),
      fill="white",
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=expr.name,
      key=expr.name, color=expr.name),
      showSelected=c("expr.name","commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    scale_x_log10()+
    scale_y_log10("seconds"),
  duration=list(
    commit=1000)
)

