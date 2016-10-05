# is x[i] in between lower[i] and upper[i] ? 
between <- function(x,lower,upper,incbounds=TRUE) {
    is_strictly_numeric <- function(x) is.numeric(x) && !"integer64" %in% class(x)
    if (is_strictly_numeric(x) && is_strictly_numeric(lower) &&
        is_strictly_numeric(upper) && length(lower) == 1L && length(upper) == 1L) {
        # faster parallelised version for int/double for most common scenario
        .Call(Cbetween, x, lower, upper, incbounds)
    } else {
        if(incbounds) x>=lower & x<=upper
        else x>lower & x<upper
    }
}

# %between% is vectorised, #534.
"%between%" <- function(x,y) between(x,y[[1]],y[[2]],incbounds=TRUE)
# If we want non inclusive bounds with %between%, just +1 to the left, and -1 to the right (assuming integers)

# issue FR #707
# is x[i] found anywhere within [lower, upper] range?
inrange <- function(x,lower,upper,incbounds=TRUE) {
    query = setDT(list(x=x))
    subject = setDT(list(l=lower, u=upper))
    ops = if (incbounds) c(4L, 2L) else c(5L, 3L) # >=,<= and >,<
    verbose = getOption("datatable.verbose")
    if (verbose) {last.started.at=proc.time()[3];cat("forderv(query) took ... ");flush.console()}
    xo = forderv(query)
    if (verbose) {cat(round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
    ans = bmerge(shallow(subject), query, 1:2, c(1L,1L), FALSE, xo, 
            0, c(FALSE, TRUE), 0L, "all", ops, integer(0), 
            1L, verbose) # fix for #1819, turn on verbose messages
    options(datatable.verbose=FALSE)
    setDT(ans[c("starts", "lens")], key=c("starts", "lens"))
    options(datatable.verbose=verbose)
    if (verbose) {last.started.at=proc.time()[3];cat("Generating final logical vector ... ");flush.console()}
    .Call(Cinrange, idx <- vector("logical", length(x)), xo, ans[["starts"]], ans[["lens"]])
    if (verbose) {cat("done in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
    idx
}

"%inrange%" <- function(x,y) inrange(x,y[[1L]],y[[2L]],incbounds=TRUE)
