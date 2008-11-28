
between = function(x,lower,upper,incbounds=TRUE)
{
  if(incbounds) x>=lower & x<=upper
  else x>lower & x<upper
}

"%between%" = function(x,y) between(x,y[1],y[2],incbounds=TRUE)
# If we want non inclusive bounds with %between%, just +1 to the left, and -1 to the right (assuming integers)
