
fread <- function(input="",file,sep="auto",sep2="auto",dec=".",quote="\"",nrows=Inf,header="auto",na.strings="NA",stringsAsFactors=FALSE,verbose=getOption("datatable.verbose"),autostart=NA,skip=0,select=NULL,drop=NULL,colClasses=NULL,integer64=getOption("datatable.integer64"), col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, showProgress=interactive(),data.table=getOption("datatable.fread.datatable"),nThread=getDTthreads(),logical01=TRUE)
{
  if (is.null(sep)) sep="\n"         # C level knows that \n means \r\n on Windows, for example
  else {
    stopifnot( length(sep)==1L, !is.na(sep), is.character(sep) )
    if (sep=="") sep="\n"            # meaning readLines behaviour. The 3 values (NULL, "" or "\n") are equivalent.
    else if (sep=="auto") sep=""     # sep=="" at C level means auto sep
    else stopifnot( nchar(sep)==1 )  # otherwise an actual character to use as sep
  }
  stopifnot( is.character(dec), length(dec)==1L, nchar(dec)==1L )
  # handle encoding, #563
  if (length(encoding) != 1L || !encoding %in% c("unknown", "UTF-8", "Latin-1")) {
    stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
  }
  isTrueFalse = function(x) isTRUE(x) || identical(FALSE, x)
  isTrueFalseNA = function(x) isTRUE(x) || identical(FALSE, x) || identical(NA, x)
  stopifnot( isTrueFalse(strip.white), isTrueFalse(blank.lines.skip), isTrueFalse(fill), isTrueFalse(showProgress),
             isTrueFalse(stringsAsFactors), isTrueFalse(verbose), isTrueFalse(check.names), isTrueFalse(logical01) )
  stopifnot( is.numeric(nrows), length(nrows)==1L )
  if (is.na(nrows) || nrows<0) nrows=Inf   # accept -1 to mean Inf, as read.table does
  if (identical(header,"auto")) header=NA
  stopifnot(isTrueFalseNA(header))
  stopifnot(length(skip)==1L)
  stopifnot(is.numeric(nThread) && length(nThread)==1L)
  nThread=as.integer(nThread)
  stopifnot(nThread>=1)
  if (!missing(file)) {
    if (!identical(input, "")) stop("You can provide 'input' or 'file', not both.")
    if (!file.exists(file)) stop(sprintf("Provided file '%s' does not exists.", file))
    input = file
  }
  if (!missing(autostart)) warning("'autostart' is now deprecated and ignored. Consider skip='string' or skip=n");
  is_url <- function(x) grepl("^(http|ftp)s?://", x)
  is_secureurl <- function(x) grepl("^(http|ftp)s://", x)
  is_file <- function(x) grepl("^file://", x)
  if (!is.character(input) || length(input)!=1L) {
    stop("'input' must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself")
  } else if (is_url(input) || is_file(input)) {
    tt = tempfile()
    on.exit(unlink(tt), add = TRUE)
    # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
    if (!is_secureurl(input)) {
      #1668 - force "auto" when is_file to
      #  ensure we don't use an invalid option, e.g. wget
      method <- if (is_file(input)) "auto" else
        getOption("download.file.method", default = "auto")
      download.file(input, tt, method = method, mode = "wb", quiet = !showProgress)
    } else {
      if (!requireNamespace("curl", quietly = TRUE))
        stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install the package using 'install.packages()'.")
      curl::curl_download(input, tt, mode = "wb", quiet = !showProgress)
    }
    input = tt
  } else if (input == "" || length(grep('\\n|\\r', input)) > 0L) {
    # text input
  } else if (isTRUE(file.info(input)$isdir)) { # fix for #989, dir.exists() requires v3.2+
    stop("'input' can not be a directory name, but must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself.")
  } else if (!file.exists(input)) {
    if (!length(grep(' ', input))) {
      stop("File '",input,"' does not exist; getwd()=='", getwd(), "'",
        ". Include correct full path, or one or more spaces to consider the input a system command.")
    }
    if (substring(input,1L,1L)==" ") {
      stop("Input argument contains no \\n and contains one or more spaces, so it looks like a system command. Please remove the leading space.")
    }
    tt = tempfile()
    on.exit(unlink(tt), add = TRUE)
    if (.Platform$OS.type == "unix") {
      if (file.exists('/dev/shm') && file.info('/dev/shm')$isdir) {
        tt = tempfile(tmpdir = '/dev/shm')
      }
      system(paste('(', input, ') > ', tt, sep=""))
    } else {
      shell(paste('(', input, ') > ', tt, sep=""))
    }
    input = tt
  }
  if (is.logical(colClasses)) {
    if (!all(is.na(colClasses))) stop("colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.")
    colClasses = NULL
  }
  if (!is.null(colClasses) && is.atomic(colClasses)) {
    if (!is.character(colClasses)) stop("colClasses is not type list or character vector")
    if (!length(colClasses)) stop("colClasses is character vector ok but has 0 length")

    if (identical(colClasses, "NULL")) {
      colClasses = NULL
      warning('colClasses="NULL" (quoted) which will be interpreted as colClasses=NULL (the default), ',
              'as opposed to dropping every column.')
    }

    if (!is.null(names(colClasses))) {   # names are column names; convert to list approach
      colClasses = tapply(names(colClasses), colClasses, c, simplify=FALSE)
    }
  }
  if (is.numeric(skip)) skip = as.integer(skip)
  warnings2errors = getOption("warn") >= 2
  ans = .Call(CfreadR,input,sep,dec,quote,header,nrows,skip,na.strings,strip.white,blank.lines.skip,
              fill,showProgress,nThread,verbose,warnings2errors,logical01,select,drop,colClasses,integer64,encoding)
  nr = length(ans[[1]])
  if ((!"bit64" %chin% loadedNamespaces()) && any(sapply(ans,inherits,"integer64"))) require_bit64()
  setattr(ans,"row.names",.set_row_names(nr))

  if (isTRUE(data.table)) {
    setattr(ans, "class", c("data.table", "data.frame"))
    alloc.col(ans)
  } else {
    setattr(ans, "class", "data.frame")
  }
  # #1027, make.unique -> make.names as spotted by @DavidArenberg
  if (check.names) {
    setattr(ans, 'names', make.names(names(ans), unique=TRUE))
  }

  # Fix Issue 1634
  set_colClasses_ante(ans,
                      select = select, drop = drop,
                      colClasses = colClasses)

  # Should be after set_colClasses_ante
  if (stringsAsFactors) {
    for (j in which(vapply(ans, is.character, logical(1L)))) {
      set(ans, j = j, value = try_with(.subset2(ans, j),
                                       as_factor(.subset2(ans, j)),
                                       paste0("Column ", j, " was type 'character', but when trying to honour `stringsAsFactors = TRUE`, fread encountered the following problem")))
    }
  }


  # 2007: is.missing is not correct since default value of select is NULL
  if (!is.null(select)) {
    # fix for #1445
    if (is.numeric(select)) {
      reorder = if (length(o <- forderv(select))) o else seq_along(select)
    } else {
      reorder = select[select %chin% names(ans)]
      # any missing columns are warning about in fread.c and skipped
    }
    setcolorder(ans, reorder)
  }
  # FR #768
  if (!missing(col.names))
    setnames(ans, col.names) # setnames checks and errors automatically
  if (!is.null(key) && data.table) {
    if (!is.character(key))
      stop("key argument of data.table() must be character")
    if (length(key) == 1L) {
      key = strsplit(key, split = ",")[[1L]]
    }
    setkeyv(ans, key)
  }
  ans
}

# for internal use only. Used in `fread` and `data.table` for 'stringsAsFactors' argument
# Not used
setfactor <- function(x, cols, verbose) {
  if (length(cols)) {
    if (verbose) cat("Converting column(s) [", paste(names(x)[cols], collapse = ", "), "] from 'char' to 'factor'\n", sep = "")
    for (j in cols) set(x, j = j, value = as_factor(.subset2(x, j)))
  }
  invisible(x)
}


set_colClasses_ante <- function(ans,
                                select,
                                drop,
                                colClasses,
                                unsupported_classes = NULL, # != "NULL"
                                already_set_classes = c("logical",
                                                        "integer", "integer64",
                                                        "numeric", "double",
                                                        "character",
                                                        NA_character_)) {

  if (length(colClasses) && any(!is.na(colClasses))) {

    switch(typeof(colClasses),
           "list" = {
             if (!all(names(colClasses) %chin% already_set_classes)) {
               # for e.g. colClasses = list(character = 1, Date = 2)
               #       to avoid arcane warnings when set is used recommending to use integers for j =.
               colClasses <- lapply(colClasses, function(x) if (is.double(x)) as.integer(x) else x)
               if (!is.null(unsupported_classes) && any(unsupported_classes %chin% names(colClasses))) {
                 names(colClasses)[which(unsupported_classes %chin% names(colClasses))] <- "character"
               }

               # If select or drop were used, colClasses is either unuseable
               # or will need modification before it can be applied safely.
               if (!is.null(select) || !is.null(drop)) {
                 if (is.character(select) || is.character(drop)) {

                   if (any(vapply(colClasses, is.integer, logical(1L)))) {
                     sel_dro <- if (is.character(select)) "select" else "drop"
                     # Difficult unless select in Cfread records the original positions in the file.
                     #
                     warning(sel_dro, " specifies columns by name, but some elements of colClasses refer to position. ",
                             "This combination is not supported. Some colClasses may not have been set.")
                     return(ans)
                   } else {
                     # both select/drop and all colClasses are character
                     if (is.null(select)) {
                       colClasses <-
                         lapply(colClasses, function(el) {
                           el[!el %chin% drop]
                         })
                     } else {
                       colClasses <-
                         lapply(colClasses, function(el) {
                           el[el %chin% drop]
                         })
                     }
                   }
                 } else {
                   # select/drop is integer
                   if (is.null(select)) {
                     # If colClasses contains a character list item here,
                     # no problem with ordering, because set() will use
                     # column names. Only need to make sure that set isn't
                     # provided with a column that doesn't exist.
                     #
                     # For integers, we need to decrement those above a dropped column
                     # by the number of dropped columns < column specified
                     colClasses <-
                       lapply(colClasses, function(el) {
                         if (is.integer(el)) {
                           # drop <- c(2, 5, 7, 10)
                           # x <- c(1, 3, 5, 9, 10, 11)
                           # expect:
                           # 1 => 1 - stays (below minimum drop)
                           # 3 => 2 - reduced by 1 = number of dropped columns below 3
                           # 5 => NULL is dropped
                           # 9 => 6 = 9 - 3 dropped columns below
                           # etc
                           out <- el[!el %in% drop]
                           if (length(out)) {
                             out - cumsum(seq_len(max(out)) %in% drop)[out]
                           } else {
                             integer(0)
                           }
                         } else {
                           # character, nothing to do
                           # except make sure we don't provide
                           # set with any alien columns
                           el[el %chin% names(ans)]
                         }
                       })
                   } else {
                     # Is this known already?
                     select <- as.integer(select)
                     # This does not conflict with #1445
                     # as this is within a function.
                     if (!is.sorted(select)) {
                       select <- select[forderv(select, by = NULL)]
                     }
                     colClasses <-
                       lapply(colClasses, function(el) {
                         if (is.integer(el)) {
                           out <- match(el, select, nomatch = 0L)
                           out[out > 0L]
                         } else {
                           el[el %chin% names(ans)]
                         }
                       })
                   }
                 }
               }

               # NULL columns should be treated differently, because
               # positions will affect columns to the right
               NULL_colClasses <- colClasses[names(colClasses) == "NULL"]

               #
               # When colClasses is a list, it looks like
               #  list(<new_class1> = <new_class1_cols>, <new_class2> = <new_class2_cols>)
               #             cCi ^
               for (cCi in seq_along(colClasses)) {
                 new_class <- names(colClasses)[cCi]
                 # Already done.
                 if (!new_class %chin% already_set_classes && length(colClasses[[cCi]])) {
                   switch(new_class,
                          "factor" = {
                            factor_cols <- colClasses[[cCi]]
                            for (factor_col in factor_cols) {
                              set(ans, j = factor_col, value = try_factor(ans[[factor_col]], factor_col))
                            }
                          },
                          "complex" = {
                            complex_cols <- colClasses[[cCi]]
                            for (complex_col in complex_cols) {
                              set(ans, j = complex_col, value = try_complex(ans[[complex_col]], complex_col))
                            }
                          },
                          "raw" = {
                            raw_cols <- colClasses[[cCi]]
                            for (raw_col in raw_cols) {
                              set(ans, j = raw_col, value = try_raw(ans[[raw_col]], raw_col))
                            }
                          },
                          "Date" = {
                            date_cols <- colClasses[[cCi]]
                            for (date_col in date_cols) {
                              set(ans, j = date_col, value = try_Date(ans[[date_col]], date_col))
                            }
                          },
                          "POSIXct" = {
                            POSIXct_cols <- colClasses[[cCi]]
                            for (POSIXct_col in POSIXct_cols) {
                              set(ans, j = POSIXct_col, value = try_POSIXct(ans[[POSIXct_col]], POSIXct_col))
                            }
                          },
                          "NULL" = {
                            # do nothing for the time being.
                          },

                          # Finally, try foreign methods
                          {
                            other_cols <- colClasses[[cCi]]
                            for (other_col in other_cols) {
                              set(ans,
                                  j = other_col,
                                  value = try_with(ans[[other_col]],
                                                   methods::as(ans[[other_col]], new_class),
                                                   paste0("Column ", other_col,
                                                          " was set by colClasses to be '", new_class,
                                                          "', but fread encountered the following")))
                            }
                          })
                 }
               }
               # Safe to use NULL_colClasses now
               if (length(NULL_colClasses)) {
                 char_NULL_colClasses <- vapply(NULL_colClasses, is.character, logical(1L))
                 if (all(char_NULL_colClasses)) {
                   null_cols <- unlist(NULL_colClasses, use.names = FALSE)
                   ans[, (null_cols) := NULL]
                 } else {

                   # consider
                   # NULL_colClasses = list(NULL = c("C", "D"), NULL = 1:2, NULL = "E", NULL = 5)
                   # Not sure which is best practice, by numbers (reversed) or by column name?
                   null_cols <- c(unlist(NULL_colClasses[char_NULL_colClasses], use.names = FALSE),
                                  names(ans)[unlist(NULL_colClasses[!char_NULL_colClasses], use.names = FALSE)])
                   ans[, (null_cols) := NULL]
                 }
               }
             }
           },

           "character" = {
             # If character, guaranteed to not have names
             # (due to tapply(names(colClasses), colClasses, c, simplify=FALSE) above)
             if (!is.null(unsupported_classes) && any(colClasses %chin% unsupported_classes)) {
               warning("colClasses contains '",
                       paste(unique(colClasses[colClasses %chin% unsupported_classes]),
                             collapse = "', '"), "', which will be ignored.")
               colClasses[colClasses %chin% unsupported_classes] <- NA_character_
             }

             if (!all(colClasses %chin% already_set_classes)) {
               which_new <- which(!colClasses %chin% already_set_classes)
               if (length(colClasses) == 1L) {
                 which_new <- seq_along(ans)
                 colClasses <- rep_len(colClasses, ncol(ans))
               }


               if (is.character(select) || is.character(drop)) {
                 sel_dro <- if (is.character(select)) "select" else "drop"
                 # Difficult unless select in Cfread records the original positions in the file.
                 #
                 warning(sel_dro, " specifies columns by name, but some elements of colClasses refer to position. ",
                         "This combination is not supported. Some colClasses may not have been set.")
                 return(ans)
               }

               if (is.numeric(select)) {
                 # Failure to include this line will result in a crash
                 select <- as.integer(select)
                 if (!is.sorted(select)) {
                   select <- select[forderv(select, by = NULL)]
                 }
                 colClasses <- colClasses[select]
                 which_new <- which(!colClasses %chin% already_set_classes)
               }
               if (is.numeric(drop)) {
                 colClasses <- colClasses[-drop]
                 which_new <- which(!colClasses %chin% already_set_classes)
               }

               for (j in which_new) {
                 v <- ans[[j]]
                 new_class <- colClasses[[j]]
                 switch(new_class,
                        "factor" = {
                          set(ans, j = j, value = try_factor(v, j))
                        },
                        "complex" = {
                          set(ans, j = j, value = try_complex(v, j))
                        },
                        "raw" = {
                          set(ans, j = j, value = try_raw(v, j))
                        },
                        "Date" = {
                          set(ans, j = j, value = try_Date(v, j))
                        },
                        "POSIXct" = {
                          set(ans, j = j, value = try_POSIXct(v, j))
                        },
                        "NULL" = {
                          # Do nothing
                        },

                        # Finally,
                        {
                          set(ans,
                              j = j,
                              value = try_with(v,
                                               methods::as(v, new_class),
                                               paste0("Column ", j,
                                                      " was set by colClasses to be '", new_class,
                                                      "', but fread encountered the following")))
                        })
               }

               if ("NULL" %chin% colClasses) {
                 null_cols <- which(colClasses == "NULL")
                 ans[, (null_cols) := NULL]
               }
             }
           })

  }
  invisible(ans)
}
# Following try_* functions designed to try coercion to a particular class,
# and aborting whenever it encounters any warning or error. Different to
# read.csv -- won't attempt coercion if NAs introduced, but also less fussy
# and won't error if a column won't work, instead reverting to previous class.
try_with <- function(v, as.v, preamble) {
  #' @param v vector of values for which class is intended to be changed
  #' @param as.v A (promise of a) function applied to v (e.g. as.numeric(v))
  #' @param preamble Length-one character, if \code{as.v} emits a warning, \code{preamble} is prepended to the warning message.
  #' @return If as.v does not emit a warning, as.v; otherwise v.
  #'

  tryCatch(as.v,
           warning = function(e) {
             warn_msg <- paste(paste(preamble, "warning:"),
                               e$message,
                               sep = "\n\t")
             warning(warn_msg,
                     "\nso the column will be left as type ", typeof(v), ".",
                     call. = FALSE)
             return(v)
           },
           # Since errors themselves raise warnings,
           # put after.
           error = function(e) {
             err_msg <- paste(paste(preamble, "error:"),
                              e$message,
                              sep = "\n\t")
             warning(err_msg,
                     "\nso the column will be left as type ", typeof(v), ".",
                     call. = FALSE)
             return(v)
           })

}

try_preamble <- function(j, cls) {
  if (is.character(j)) {
    sprintf("Column %s was set by colClasses to be '%s', but fread encountered the following", j, cls)
  } else {
    sprintf("Column %d was set by colClasses to be '%s', but fread encountered the following", j, cls)
  }
}

try_factor <- function(v, j) {
  try_with(v,
           as_factor(v),
           try_preamble(j, "factor"))
}

try_complex <- function(v, j) {
  try_with(v,
           as.complex(v),
           try_preamble(j, "complex"))
}

try_raw <- function(v, j, ...) {
  # No performance improvement with using nmax = nr
  try_with(v,
           # As in read.csv, which ultimately uses src/main/scan.c Line 193 (or thereabouts)
           # static Rbyte
           # strtoraw (const char *nptr, char **endptr)
           # {
           #   const char *p = nptr;
           #   int i, val = 0;
           #
           #   /* should have whitespace plus exactly 2 hex digits */
           #     while(Rspace(*p)) p++;
           #   for(i = 1; i <= 2; i++, p++) {
           #     val *= 16;
           #     if(*p >= '0' && *p <= '9') val += *p - '0';
           #     else if (*p >= 'A' && *p <= 'F') val += *p - 'A' + 10;
           #     else if (*p >= 'a' && *p <= 'f') val += *p - 'a' + 10;
           #     else {val = 0; break;}
           #   }
           #   *endptr = (char *) p;
           #   return (Rbyte) val;
           # }
           scan(text = v, what = raw(), ..., quiet = TRUE),
           try_preamble(j, "raw"))
}

try_Date <- function(v, j) {
  try_with(v,
           as.Date(v),
           try_preamble(j, "Date"))
}

try_POSIXct <- function(v, j) {
  try_with(v,
           as.POSIXct(v),
           try_preamble(j, "POSIXct"))
}

# simplified but faster version of `factor()` for internal use.
as_factor <- function(x) {
  lev = forderv(x, retGrp = TRUE, na.last = NA)
  # get levels, also take care of all sorted condition
  lev = if (length(lev)) x[lev[attributes(lev)$starts]] else x[attributes(lev)$starts]
  ans = chmatch(x, lev)
  setattr(ans, 'levels', lev)
  setattr(ans, 'class', 'factor')
}




