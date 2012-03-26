like = function(vector, pattern)
{
    # Intended for use with a data.table 'where' 
    # Don't use * or % like SQL's like.  Uses regexpr syntax - more powerful.
    if (is.factor(vector)) {
        as.integer(vector) %in% grep(pattern,levels(vector))
    } else {
        # most usually character, but integer and numerics will be silently coerced by grepl
        grepl(pattern,vector)
    }
    # returns 'logical' so can be combined with other where clauses.
}

"%like%" = like


