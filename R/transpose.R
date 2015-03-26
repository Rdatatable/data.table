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

tstrsplit <- function(x, ..., fill=NA, type.convert=FALSE) {
	ans = transpose(strsplit(as.character(x), ...), fill=fill, ignore.empty = FALSE)
	# Implementing #1094, but default FALSE
  if(type.convert) ans = lapply(ans, type.convert, as.is = TRUE)
  ans
}
