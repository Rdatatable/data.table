library(animint2)
library(data.table)
Test.dir.vec <- Sys.glob("bisect/*")
Test.dir.vec <- "~/R/data.table/.ci/atime/bisect/DT_by__fixed_in__4558_PR4164_Parent4491"
for(Test.dir in Test.dir.vec){
  print(Test.dir)
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
  version.colors <- c(
    HEAD= "blue")
  vers.dt <- nc::capture_first_vec(Test.dir, ".*_", old="[^_]+", "_", new="[^_]+")
  version.colors[vers.dt$old] <- "violet"
  version.colors[vers.dt$new] <- "red"
  viz <- animint(
    title=paste("Performance testing with git bisect in data.table", basename(Test.dir)),
    source="https://github.com/Rdatatable/data.table/pull/7912/files#diff-e7f10716f0ff9f29e3186a43cfed95d970b167dc4d6a92c00b9e0736bf4b05ae",
    out.dir=file.path("bisect-viz", basename(Test.dir)),
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
      geom_label_aligned(aes(
        rank, value, label=version, color=version),
        data=results.long[rank==max(rank)],
        hjust=0,
        alignment="vertical")+
      scale_fill_manual(values=c(
        "0"="grey",
        "1"="white"))+
      scale_color_manual(values=version.colors)+
      scale_y_log10("")+
      scale_x_continuous(
        "commit rank over time",
        breaks=results.wide$rank)+
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
      theme_animint(width=500, last_in_row=TRUE)+
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
        0, 0, label=substring(commit, 1, 10),
        href=paste0("https://github.com/Rdatatable/data.table/commit/", commit),
        key=1),
        hjust=0,
        vjust=0,
        data=common.dt,
        showSelected="commit")+
      geom_ribbon(aes(
        N, ymin=min, ymax=max,
        key=version, fill=version, group=version),
        showSelected=c("version","commit"),
        alpha=0.2,
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
      scale_color_manual(values=version.colors)+
      scale_fill_manual(values=version.colors)+
      scale_x_log10("N = number of data")+
      scale_y_log10("seconds"),
    duration=list(
      commit=1000)
  )
  print(viz)
}

if(FALSE){
  animint2::animint2pages(viz, paste0("2026-10-09-performance-bisect-", basename(Test.dir)), chromote_sleep_seconds=3)
}
