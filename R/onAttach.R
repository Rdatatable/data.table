".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type: help("data.table")')
    }
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        # See FAQ 2.23
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(data.table(...))")[[1]]
        body(tt)=ss
        unlockBinding("cbind.data.frame",baseenv())
        assignInNamespace("cbind.data.frame",tt,"base")
        lockBinding("cbind.data.frame",baseenv())
    }
    tt = base::rbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        # See FAQ 2.23
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(`.rbind.data.table`(...))")[[1]]
        body(tt)=ss
        unlockBinding("rbind.data.frame",baseenv())
        assignInNamespace("rbind.data.frame",tt,"base")
        lockBinding("rbind.data.frame",baseenv())
    }
}

