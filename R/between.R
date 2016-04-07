# is x[i] in between lower[i] and upper[i] ? 
between <- function(x,lower,upper,incbounds=TRUE) {
  if(incbounds) x>=lower & x<=upper
  else x>lower & x<upper
}

# %between% is vectorised, #534.
"%between%" <- function(x,y) between(x,y[[1]],y[[2]],incbounds=TRUE)
# If we want non inclusive bounds with %between%, just +1 to the left, and -1 to the right (assuming integers)

# is x[i] found anywhere within [lower, upper] range?
anywhere <- function(x,lower,upper,incbounds=TRUE) {
    query = setDT(list(x=x, ans=rep(FALSE, length(x))))
    subject = setDT(list(l=lower, u=upper))
    on = if (incbounds) c("x>=l", "x<=u") else c("x>l", "x<u")
    query[subject, ans := TRUE, on=on]
    query$ans
}

"%anywhere%" <- function(x,y) anywhere(x,y[[1L]],y[[2L]],incbounds=TRUE)
