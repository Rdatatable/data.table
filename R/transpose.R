transpose <- function(l, fill=NA, ignore.empty=FALSE) {
	ans = .Call(Ctranspose, l, fill, ignore.empty)
	if (is.data.frame(l)) setDT(ans)
	if (!is.data.table(l)) setDF(ans)
	ans[]
}

tstrsplit <- function(..., fill=NA) {
	transpose(strsplit(...), fill=fill, ignore.empty = FALSE)
}
