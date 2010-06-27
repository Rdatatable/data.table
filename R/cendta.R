cendta = function() {
    # Calling Environment Not Data Table Aware
    te = topenv(parent.frame(2))
    !identical(te,.GlobalEnv) &&
    (!"data.table" %in% loadedNamespaces() || !identical(te,environment(data.table::data.table))) &&
    (!exists(".required",te,inherits=FALSE) || !"data.table" %in% get(".required",te,inherits=FALSE))
    # the check if data.table is in loadedNamespaces is needed because 'data.table::data.table' itself
    # will load the namespace if not already loaded, and we don't want that.
}


