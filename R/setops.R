# setdiff for data.tables, internal at the moment #547
setdiff_ <- function(x, y, by.x=seq_along(x), by.y=seq_along(y)) {
    if (!is.data.table(x) || !is.data.table(y)) stop("x and y must be both data.tables")
    if (is.null(y) || ncol(y) == 0L) return(unique(x))
    if (length(by.x) != length(by.y)) stop("setdiff(x,y) requires same number of columns for both x and y. However, length(by.x) != length(by.y)") 
    if (length(by.x) == 0L) stop("by.x and by.y must be character or integer vectors of length >= 1")
    if (is.character(by.x)) by.x = chmatch(by.x, names(x))
    if (is.character(by.y)) by.y = chmatch(by.y, names(y))
    by.x = as.integer(by.x); by.y = as.integer(by.y)
    if (any(is.na(by.x))) stop("Some column(s) specified in by.x are not present in x")
    if (any(is.na(by.y))) stop("Some column(s) specified in by.y are not present in y")
    # factor in x should've factor/character in y, and viceversa
    for (a in seq_along(by.x)) {
        lc = by.y[a]
        rc = by.x[a]
        icnam = names(y)[lc]
        xcnam = names(x)[rc]
        if ( is.character(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is character, the corresponding column in y ('",icnam,"') should be factor or character, but found incompatible type '",typeof(y[[lc]]),"'.")
        } else if ( is.factor(x[[rc]]) && !(is.character(y[[lc]]) || is.factor(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is factor, the corresponding column in y ('",icnam,"') should be character or factor, but found incompatible type '",typeof(y[[lc]]),"'.")
        } else if ( (is.integer(x[[rc]]) || is.double(x[[rc]])) && (is.logical(y[[lc]]) || is.character(y[[lc]])) ) {
            stop("When x's column ('",xcnam,"') is integer or numeric, the corresponding column in y ('",icnam,"') can not be character or logical types, but found incompatible type '",typeof(y[[lc]]),"'.")
        }
    }
    ux = vector("list", length(by.x))
    uy = vector("list", length(by.y))
    point(ux, seq_along(by.x), x, by.x)
    point(uy, seq_along(by.y), y, by.y)
    setDT(ux); setDT(uy)
    setnames(ux, names(x)[by.x])
    setnames(uy, names(x)[by.x])
    # actual setdiff starts here...
    ux = unique(ux); uy = unique(uy)
    idx  = duplicated(rbind(unique(uy), unique(ux), use.names=TRUE, fill=FALSE))[-seq_len(nrow(uy))]
    .Call("CsubsetDT", ux, which(!idx), seq_along(ux))
}
