transpose <- function(l, fill=NA, ignore.empty=FALSE) {
    ans = .Call(Ctranspose, l, fill, ignore.empty)
    if (is.data.table(l)) setDT(ans)
    else if (is.data.frame(l)) { 
        if (is.null(names(ans))) 
            setattr(ans, "names", paste("V", seq_along(ans), sep = ""))
        setattr(ans, "row.names", .set_row_names(length(ans[[1L]])))
        setattr(ans, "class", "data.frame")
    }
    ans[]
}

tstrsplit <- function(x, ..., fill=NA, type.convert=FALSE, keep, names=FALSE) {
    ans = transpose(strsplit(as.character(x), ...), fill=fill, ignore.empty=FALSE)
    if (!missing(keep)) {
        keep = suppressWarnings(as.integer(keep))
        chk = min(keep) >= min(1L, length(ans)) & max(keep) <= length(ans)
        if (!isTRUE(chk)) # handles NA case too
            stop("'keep' should contain integer values between ", 
                min(1L, length(ans)), " and ", length(ans), ".")
        ans = ans[keep]
    }
    # Implementing #1094, but default FALSE
    if(type.convert) ans = lapply(ans, type.convert, as.is = TRUE)
    if (identical(names, FALSE)) return(ans)
    else if (isTRUE(names)) names = paste0("V", seq_along(ans))
    if (!is.character(names))
        stop("'names' must be TRUE/FALSE or a character vector.")
    if (length(names) != length(ans)) {
        str = if (missing(keep)) "ans" else "keep"
        stop("length(names) (= ", length(names), 
            ") is not equal to length(", str, ") (= ", length(ans), ").")
    }
    setattr(ans, 'names', names)
  ans
}
