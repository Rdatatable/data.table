"%+%" = function(x,y)
UseMethod("%+%")

"%+%.default" = function(x,y) paste(paste(x,collapse=","),paste(y,collapse=","),sep="")
# we often construct warning msgs with a msg followed by several items of a vector, so %+% is for convenience
