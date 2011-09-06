setattr = function(x,name,value) {
    # Wrapper for setAttrib internal R function
    # Sets attribute by reference (no copy)
    # Named setattr (rather than setattrib) at R level to more closely resemble attr<-
    .Call("setattrib", x, name, value, PACKAGE="data.table")
}

