# make library structure and Makevars files

do_cflags = function(x) paste(paste0("-",sub("_","=",x,fixed=TRUE)), collapse=" ")
lib_path = "library"
cc = "gcc" ## const
flags = c(
  "O3","O3-mtune_native","O3-g",
  "O2","O2-mtune_native","O2-g",
  "O0","O0-g"
)
libs = file.path(lib_path, cc, flags)
vapply(libs, dir.create, showWarnings = TRUE, recursive=TRUE, NA)
mvs = file.path(libs, "Makevars")
cflags = vapply(strsplit(flags, "-", fixed=TRUE), do_cflags , "")
cc_cflags = paste(sep="\n", paste0("CC=",cc), paste0("CFLAGS=",cflags))
mapply(writeLines, cc_cflags, mvs)

# install using different compilation flags

#inst = sprintf("R_MAKEVARS_USER=%s R CMD INSTALL --library=%s %s", mvs, libs, "data.table_1.12.9.tar.gz")
#sapply(inst, system)

inst = function(lib, tarball="data.table_1.12.9.tar.gz") { ## till R_MAKEVARS_USER works
  file.copy(file.path(lib,"Makevars"), "~/.R/Makevars", overwrite=TRUE)
  cmd = sprintf("R CMD INSTALL --library=%s %s", lib, tarball)
  cat(cmd,"\n",sep="")
  system(cmd)
}
sapply(libs, inst)

# cleanup
inst("library/gcc/O3-mtune_native")

#unlink(lib_path, recursive=TRUE)

