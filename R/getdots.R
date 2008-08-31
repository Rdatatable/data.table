getdots = function()
{
    # return a string vector of the arguments in '...'
    # My long winded way: gsub(" ","",unlist(strsplit(deparse(substitute(list(...))),"[(,)]")))[-1]
    # Peter Dalgaard's & Brian Ripley helped out and ended up with :
    as.character(match.call(sys.function(-1), call=sys.call(-1), expand.dots=FALSE)$...)
}
