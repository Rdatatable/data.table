
# data.table defined last(x) with no arguments, just for last. If you need the last 10 then use tail(x,10).
# This last is implemented this way for compatibility with xts::last which is S3 generic taking 'n' and 'keep' arguments
# We wish for last on vectors to be fast, so that's a direct x[NROW(x)] as it was in data.table, otherwise use xts's.
# If xts is loaded higher than data.table, xts::last will work but slower.
last = function(x, ...) {
    if (nargs()==1L) {
        if (is.vector(x)) return(x[[length(x)]])  # [[ for list which is vector too
        if (is.data.table(x)) return(x[NROW(x)])
    }
    UseMethod("last")   # dispatch to xts's last
}

