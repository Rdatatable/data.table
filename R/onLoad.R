.onLoad <- function(libname, pkgname) {
    "Please read FAQ 2.23 (vignette('datatable-faq')) which explains in detail why data.table adds one line to base::cbind.data.frame and base::rbind.data.frame. If there is a better solution we will gladly change it."
    # Commented as a character string so this message is retained and seen by anyone who types data.table:::.onAttach
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text="if (inherits(..1,'data.table')) return(data.table(...,key=key(..1)))")[[1]]
        body(tt)=ss
        (unlockBinding)("cbind.data.frame",baseenv())
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
        (unlockBinding)("rbind.data.frame",baseenv())
        assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
        lockBinding("rbind.data.frame",baseenv())
    }

    # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of getOption(,default=)
    opts = c("datatable.verbose"="FALSE",            # datatable.<argument name>
             "datatable.nomatch"="NA_integer_",      # datatable.<argument name>
             "datatable.optimize"="Inf",             # datatable.<argument name>
             "datatable.print.nrows"="100L",         # datatable.<argument name>
             "datatable.print.topn"="5L",            # datatable.<argument name>
             "datatable.allow.cartesian"="FALSE",    # datatable.<argument name>
             "datatable.dfdispatchwarn"="TRUE",                   # not a function argument
             "datatable.warnredundantby"="TRUE",                  # not a function argument
             "datatable.alloccol"="quote(max(100,2*ncol(DT)))"    # argument 'n' of alloc.col
             )

    for (i in setdiff(names(opts),names(options()))) {
        eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
    }
}


