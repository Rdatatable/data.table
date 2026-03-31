tinfo <- atime::atime_pkg_test_info("../..")
names(tinfo$test.call)
tname <- "DT[by] fixed in #4558"
tcall <- tinfo$test.call[[tname]]
tres <- eval(tcall)
subject <- "void writeBool8(int8_t *col, int64_t row, char **pch)
void writeBool32(const void *col, int64_t row, char **pch)"
gsub("write(.*?)\\(.*? *col,", "write\\1(const void *col,", subject)
plot(tres)
