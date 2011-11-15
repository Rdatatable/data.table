".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type: help("data.table")')
    }
    "Please read data.table FAQ 2.23 (vignette('datatable-faq')) which explains (at some length) why data.table adds one line to base::cbind.data.frame. As soon as we have a better solution we will gladly change it."
    # Commented as a character string so it's retained and seen by anyone who types data.table:::.onAttach
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(data.table(...,key=key(..1)))")[[1]]
        body(tt)=ss
        unlockBinding("cbind.data.frame",baseenv())
        assign("cbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
        lockBinding("cbind.data.frame",baseenv())
    }
    tt = base::rbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(`.rbind.data.table`(...))")[[1]]
        body(tt)=ss
        unlockBinding("rbind.data.frame",baseenv())
        assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
        lockBinding("rbind.data.frame",baseenv())
    }
}

