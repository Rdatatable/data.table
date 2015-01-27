transpose <- function(l, fill=NA, ignore.empty=FALSE) {
	ans = .Call(Ctranspose, l, fill, ignore.empty)
	if (is.data.table(l)) setDT(ans)
	else if (is.data.frame(l)) setDF(ans)
	ans[]
}

tstrsplit <- function(..., fill=NA) {
	transpose(strsplit(...), fill=fill, ignore.empty = FALSE)
}
