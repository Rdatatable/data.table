".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type: help("data.table")')
    }
    assignInNamespace("cbind.data.frame",.cbind.data.frame,"base")
    tt = base::rbind.data.frame
    ss = body(tt)
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(`.rbind.data.table`(...))")[[1]]
        body(tt)=ss
        assignInNamespace("rbind.data.frame",tt,"base")
    }  
}

