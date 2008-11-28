
trim = function(x)
{
    # Removes whitespace at the beginning and end of strings
    # Assigning to x[] to retain the original dimensions, rownames and colnames
    x[] = gsub(" +$", "", x)
    x[] = gsub("^ +", "", x)
    x
}
