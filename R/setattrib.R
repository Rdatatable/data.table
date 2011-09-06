setattrib = function(x,name,value) {
    # Wrapper for setAttrib internal R function
    # Sets attribute by reference (no copy)
    .Call("setattrib", x, name, value, PACKAGE="data.table")
}

