# Needs clear documentation what this script does and does not. And most likely this script is going to evolve

require(data.table)
require(tools)

gather_class <- function(dt) {
    dt[grepl("^[.]{1}.*$", elements), `:=`(attribute = "class", elements = substring(elements, 2L))]
    invisible(dt)
}

gather_role <- function(dt) {
    dt[grepl("^[_]{2}.*$", elements), `:=`(attribute = "role", elements = substring(elements, 3L))]
    invisible(dt)
}

gather_named <- function(dt) {
    idx = grepl("=", dt$elements)
    if (!any(idx)) return(invisible(dt))

    val = strsplit(dt$elements[idx], "=", fixed=TRUE)    
    dt[grepl("=", elements), `:=`(attribute = sapply(val, `[`, 1L), elements = sapply(val, `[`, 2L))]
    invisible(dt)
}

gather_id <- function(dt) {
    dt[grepl("^[#]{1}.*$", elements), `:=`(attribute = "id", elements = substring(elements, 2L))]
    invisible(dt)    
}

stitch_div <- function(dt, elem) {
    ans = dt[, list(elements=paste(elements, collapse=" ")), by=list(id, attribute)]
    ans[!is.na(attribute), elements := paste0(attribute, "='", elements, "'")]
    ans = ans[, list(elements = paste(elements, collapse=" ")), by=id]
    ans[, elements := paste0("<", elem, " ", elements, ">")]$elements
}

span_inline <- function(x) {
    ix = grep("(?<![/]{2})!![ ]*[{]", x$lines, perl=TRUE)
    if (!length(ix)) return(invisible(x))

    xi = x[ix]
    split = strsplit(xi$lines, "[ ]*[!]{2}")

    y = gsub("^.*?[{](.*)[}][ ]*$", "\\1", sapply(split, `[`, 2L))  # collect everything inside {}
    y = gsub("[ ]*=[ ]*", "=", y)                                   # take are of "val = bla" -> "val=bla"
    y = gsub("^[ ]+", "", y)                                        # remove spaces at the beginning
    y = gsub("[ ]+$", "", y)                                        # remove spaces at the end
    y = gsub("^$", " ", y)                                          # replace "" with " "
    
    ys  = strsplit(y, "[ ]+")                                       # now split on bunch of spaces
    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))

    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    ans = paste(if ("attribute" %chin% names(dt)) stitch_div(dt, "span") else "<span>", sapply(split, `[`, 1L), "</span>", sep="")
    x[(ix), lines := ans]
    invisible(x)
}

span_start <- function(x) {
    ix = grep("^[ ]*[.]span[ ]*[{]", x$lines)
    if (!length(ix)) return(invisible(x))

    xi = x[ix]

    y = gsub("^.*?[{](.*)[}][ ]*$", "\\1", xi$lines) # collect everything inside {}
    y = gsub("[ ]*=[ ]*", "=", y)                       # take are of "val = bla" -> "val=bla"
    y = gsub("^[ ]+", "", y)                            # remove spaces at the beginning
    y = gsub("[ ]+$", "", y)                            # remove spaces at the end
    y = gsub("^$", " ", y)                              # replace "" with " "
    
    ys  = strsplit(y, "[ ]+")                           # now split on bunch of spaces
    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))

    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    ans = if ("attribute" %chin% names(dt)) stitch_div(dt, "span") else "<span>"
    x[(ix), lines := ans]
    invisible(x)
}

span_end <- function(dt) {
    dt[grep("^[ ]*[.]span[ ]*$", lines), lines := "</span>"]
    return(invisible(dt))
}

div_start <- function(x) {
    ix = grep("^[ ]*[.]div[ ]*[{]", x$lines)
    if (!length(ix)) return(invisible(x))

    y = gsub("^.*?[{](.*)[}][ ]*$", "\\1", x$lines[ix]) # collect everything inside {}
    y = gsub("[ ]*=[ ]*", "=", y)                       # take are of "val = bla" -> "val=bla"
    y = gsub("^[ ]+", "", y)                            # remove spaces at the beginning
    y = gsub("[ ]+$", "", y)                            # remove spaces at the end
    y = gsub("^$", " ", y)                              # replace "" with " "

    ys  = strsplit(y, "[ ]+")                           # now split on bunch of spaces
    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))

    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    x[(ix), lines := if ("attribute" %chin% names(dt)) stitch_div(dt, "div") else "<div>"]
    invisible(x)
}

div_end <- function(dt) {
    dt[grep("^[ ]*[.]div[ ]*$", lines), lines := "</div>"]
    return(invisible(dt))
}

ul_start <- function(x) {
    ix = grep("^[ ]*[.]ul[ ]*[{]", x$lines)
    if (!length(ix)) return(invisible(x))

    y = gsub("^.*?[{](.*)[}][ ]*$", "\\1", x$lines[ix]) # collect everything inside {}
    y = gsub("[ ]*=[ ]*", "=", y)                       # take are of "val = bla" -> "val=bla"
    y = gsub("^[ ]+", "", y)                            # remove spaces at the beginning
    y = gsub("[ ]+$", "", y)                            # remove spaces at the end

    ys  = strsplit(y, "[ ]+")                           # now split on bunch of spaces
    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))

    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    x[(ix), lines := if ("attribute" %chin% names(dt)) stitch_div(dt, "ul") else "<ul>"]
    invisible(x)
}

ul_end <- function(dt) {
    dt[grep("^[ ]*[.]ul[ ]*$", lines), lines := "</ul>"]
    return(invisible(dt))
}

li_start <- function(x) {
    ix = grep("^[ ]*[.]li[ ]*[{]", x$lines)
    if (!length(ix)) return(invisible(x))

    xi = x[ix]
    split = strsplit(xi$lines, "[ ]*\\.li")

    ys = gsub("^.*?[{](.*)[}][ ]*$", "\\1", sapply(split, `[`, 2L))  # collect everything inside {}
    ys = gsub("[ ]*=[ ]*", "=", ys)                                  # take are of "val = bla" -> "val=bla"
    ys = gsub("^[ ]+", "", ys)                                       # remove spaces at the beginning
    ys = gsub("[ ]+$", "", ys)                                       # remove spaces at the end
    ys = gsub("^$", " ", ys)                                         # replace "" with " "

    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))

    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    ans = if ("attribute" %chin% names(dt)) stitch_div(dt, "li") else "<li>"
    x[(ix), lines := ans]
    invisible(x)
}

li_end <- function(dt) {
    dt[grep("^[ ]*[.]li[ ]*$", lines), lines := "</li>"]
    return(invisible(dt))
}

para <- function(x) {
    ix = grep("[^/][.]p[ ]*[{]", x$lines)
    if (!length(ix)) return(invisible(x))

    xi = x[ix]
    split = strsplit(xi$lines, "[ ]*[^/]\\.p")

    y = gsub("^.*?[{](.*)[}][ ]*$", "\\1", sapply(split, `[`, 2L))  # collect everything inside {}
    y = gsub("[ ]*=[ ]*", "=", y)                                   # take are of "val = bla" -> "val=bla"
    y = gsub("^[ ]+", "", y)                                        # remove spaces at the beginning
    y = gsub("[ ]+$", "", y)                                        # remove spaces at the end
    y = gsub("^$", " ", y)                                          # replace "" with " "
    
    ys  = strsplit(y, "[ ]+")                                       # now split on bunch of spaces
    len = vapply(ys, length, 0L)
    dt = setDT(list(id=rep(seq_along(len), len), elements=unlist(ys)))
    
    gather_class(dt)
    gather_role(dt)
    gather_named(dt)
    gather_id(dt)
    ans = paste(if ("attribute" %chin% names(dt)) stitch_div(dt, "p") else "<p>", sapply(split, `[`, 1L), "</p>", sep=" ")
    x[(ix), lines := ans]
    invisible(x)
}

parser <- function(in_file, out_file) {
    lines = readLines(in_file)
    
    x = strsplit(lines, "[^/]%>%")
    l = vapply(x, length, 0L)
    
    dt = data.table(id = rep(seq_along(l), l), lines = unlist(x))
    
    span_inline(dt)
    div_end(dt)
    ul_end(dt)
    li_end(dt)
    span_end(dt)
    ul_start(dt)
    li_start(dt)
    span_start(dt)
    div_start(dt)
    dt = dt[, list(lines = paste(lines, collapse="")), by=id]
    para(dt)
    dt[grepl("^(?<![/]{2})[#]", lines, perl=TRUE), lines := paste(lines, "\n", sep="")]
    dt[, lines := gsub("[/]{2}([ ]*[.%!#])", "\\1", lines)]
    writeLines(dt$lines, out_file, sep="\n")
}
