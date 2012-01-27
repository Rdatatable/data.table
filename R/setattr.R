setattr = function(x,name,value) {
    # Wrapper for setAttrib internal R function
    # Sets attribute by reference (no copy)
    # Named setattr (rather than setattrib) at R level to more closely resemble attr<-
    # And as from 1.7.8 is made exported in NAMESPACE for use in user attributes.
    # User can also call `attr<-` function directly, but that might still copy when NAMED>0 (I think).
    # See "Confused by NAMED" thread on r-devel 24 Nov 2011.
    invisible(.Call("setattrib", x, name, value, PACKAGE="data.table"))
}

