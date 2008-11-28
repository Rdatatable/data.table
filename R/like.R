like = function(vector, pattern)
{
    # Intended for use with a data.table 'where' 
    # Don't use * or % like SQL's like.  Uses regexpr syntax - more powerful.
    if (is.factor(vector)) {
        which(as.integer(vector) %in% grep(pattern,levels(vector)))
    } else {
        # if (!is.character(vector)) stop("like searches character vector or factor").  Commented this out to leave it to regexpr to auto-coerce. So like is more similar to regexpr behaviour.
        which(regexpr(pattern,vector)>0)
    }    
}

"%like%" = like
