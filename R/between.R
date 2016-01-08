between <- function(x, lower, upper, incbounds = TRUE)
{
  if (!incbounds %in% list(TRUE, FALSE, "upper", "lower")) 
    stop('Valid arguments for incbounds are TRUE, FALSE, "upper", and "lower"')
  (if ((incl <- incbounds == TRUE) | incbounds == "lower") `>=` else `>`)(x, lower) &
    (if (incl | incbounds == "upper") `<=` else `<`)(x, upper)
}

"%between%" <- function(x,y) between(x, y[1L], y[2L], incbounds = TRUE)
