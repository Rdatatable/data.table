words = function(x)
{
    # returns the words in string x
    #x = paste(" ",x," ",sep="")    # to force that xother will be first and last
    unlist(strsplit(x,"[^A-Za-z0-9._'\"]+"))
    # including ' and " here is so we leave constants as constants,  column titles for example.
}
