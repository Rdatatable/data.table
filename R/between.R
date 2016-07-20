# is x[i] in between lower[i] and upper[i] ? 
between <- function(x,lower,upper,incbounds=TRUE) {
  if(incbounds) x>=lower & x<=upper
  else x>lower & x<upper
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
    ans = bmerge(shallow(subject), query, 1:2, c(1L,1L), FALSE, xo <- forderv(query), 
            0, c(FALSE, TRUE), 0L, "all", ops, integer(0), 
            1L, FALSE)
    setDT(ans[c("starts", "lens")], key=c("starts", "lens"))
    .Call(Cinrange, idx <- rep(FALSE, length(x)), xo, ans[["starts"]], ans[["lens"]])
    idx
}

"%inrange%" <- function(x,y) inrange(x,y[[1L]],y[[2L]],incbounds=TRUE)
