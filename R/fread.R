fread = function(
input="", file=NULL, text=NULL, cmd=NULL, sep="auto", sep2="auto", dec=".", quote="\"", nrows=Inf, header="auto",
na.strings=getOption("datatable.na.strings","NA"), stringsAsFactors=FALSE, verbose=getOption("datatable.verbose",FALSE),
skip="__auto__", select=NULL, drop=NULL, colClasses=NULL, integer64=getOption("datatable.integer64","integer64"),
col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, index=NULL,
showProgress=getOption("datatable.showProgress",interactive()), data.table=getOption("datatable.fread.datatable",TRUE),
nThread=getDTthreads(verbose), logical01=getOption("datatable.logical01",FALSE), keepLeadingZeros=getOption("datatable.keepLeadingZeros",FALSE),
yaml=FALSE, autostart=NA, tmpdir=tempdir(), tz="")
{
  if (missing(input)+is.null(file)+is.null(text)+is.null(cmd) < 3L) stop("Used more than one of the arguments input=, file=, text= and cmd=.")
  input_has_vars = length(all.vars(substitute(input)))>0L  # see news for v1.11.6
  if (is.null(sep)) sep="\n"         # C level knows that \n means \r\n on Windows, for example
  else {
    stopifnot( length(sep)==1L, !is.na(sep), is.character(sep) )
    if (sep=="") { sep="\n" }         # meaning readLines behaviour. The 3 values (NULL, "" or "\n") are equivalent.
    else if (sep=="auto") sep=""      # sep=="" at C level means auto sep
    else stopifnot( nchar(sep)==1L )  # otherwise an actual character to use as sep
  }
  stopifnot( is.character(dec), length(dec)==1L, nchar(dec)==1L )
  # handle encoding, #563
  if (length(encoding) != 1L || !encoding %chin% c("unknown", "UTF-8", "Latin-1")) {
    stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
  }
  stopifnot( isTRUEorFALSE(strip.white), isTRUEorFALSE(blank.lines.skip), isTRUEorFALSE(fill), isTRUEorFALSE(showProgress),
             isTRUEorFALSE(verbose), isTRUEorFALSE(check.names), isTRUEorFALSE(logical01), isTRUEorFALSE(keepLeadingZeros), isTRUEorFALSE(yaml) )
  stopifnot( isTRUEorFALSE(stringsAsFactors) || (is.double(stringsAsFactors) && length(stringsAsFactors)==1L && 0.0<=stringsAsFactors && stringsAsFactors<=1.0))
  stopifnot( is.numeric(nrows), length(nrows)==1L )
  if (is.na(nrows) || nrows<0L) nrows=Inf   # accept -1 to mean Inf, as read.table does
  if (identical(header,"auto")) header=NA
  stopifnot(is.logical(header) && length(header)==1L)  # TRUE, FALSE or NA
  stopifnot(is.numeric(nThread) && length(nThread)==1L)
  nThread=as.integer(nThread)
  stopifnot(nThread>=1L)
  if (!is.null(text)) {
    if (!is.character(text)) stop("'text=' is type ", typeof(text), " but must be character.")
    if (!length(text)) return(data.table())
    if (length(text) > 1L) {
      cat(text, file=(tmpFile<-tempfile(tmpdir=tmpdir)), sep="\n")  # avoid paste0() which could create a new very long single string in R's memory
      file = tmpFile
      on.exit(unlink(tmpFile), add=TRUE)
    } else {
      # avoid creating a tempfile() for single strings, which can be done a lot; e.g. in the test suite.
      input = text
    }
  }
  else if (is.null(cmd)) {
    if (!is.character(input) || length(input)!=1L) {
      stop("input= must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r")
    }
    if (input=="" || length(grep('\\n|\\r', input))) {
      # input is data itself containing at least one \n or \r
    } else {
      if (substring(input,1L,1L)==" ") {
        stop("input= contains no \\n or \\r, but starts with a space. Please remove the leading space, or use text=, file= or cmd=")
      }
      str6 = substring(input,1L,6L)   # avoid grepl() for #2531
      str7 = substring(input,1L,7L)
      str8 = substring(input,1L,8L)
      if (str7=="ftps://" || str8=="https://") {
        # nocov start
        if (!requireNamespace("curl", quietly = TRUE))
          stop("Input URL requires https:// connection for which fread() requires 'curl' package which cannot be found. Please install 'curl' using 'install.packages('curl')'.") # nocov
        tmpFile = tempfile(fileext = paste0(".",tools::file_ext(input)), tmpdir=tmpdir)  # retain .gz extension in temp filename so it knows to be decompressed further below
        curl::curl_download(input, tmpFile, mode="wb", quiet = !showProgress)
        file = tmpFile
        on.exit(unlink(tmpFile), add=TRUE)
        # nocov end
      }
      else if (str6=="ftp://" || str7== "http://" || str7=="file://") {
        # nocov start
        method = if (str7=="file://") "internal" else getOption("download.file.method", default="auto")
        # force "auto" when file:// to ensure we don't use an invalid option (e.g. wget), #1668
        tmpFile = tempfile(fileext = paste0(".",tools::file_ext(input)), tmpdir=tmpdir)
        download.file(input, tmpFile, method=method, mode="wb", quiet=!showProgress)
        # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
        file = tmpFile
        on.exit(unlink(tmpFile), add=TRUE)
        # nocov end
      }
      else if (length(grep(' ', input, fixed = TRUE)) && !file.exists(input)) {  # file name or path containing spaces is not a command
        cmd = input
        if (input_has_vars && getOption("datatable.fread.input.cmd.message", TRUE)) {
          message("Taking input= as a system command ('",cmd,"') and a variable has been used in the expression passed to `input=`. Please use fread(cmd=...). There is a security concern if you are creating an app, and the app could have a malicious user, and the app is not running in a secure environment; e.g. the app is running as root. Please read item 5 in the NEWS file for v1.11.6 for more information and for the option to suppress this message.")
        }
      }
      else {
        file = input   # filename
      }
    }
  }
  if (!is.null(cmd)) {
    (if (.Platform$OS.type == "unix") system else shell)(paste0('(', cmd, ') > ', tmpFile<-tempfile(tmpdir=tmpdir)))
    file = tmpFile
    on.exit(unlink(tmpFile), add=TRUE)
  }
  if (!is.null(file)) {
    file_info = file.info(file)
    if (is.na(file_info$size)) stop("File '",file,"' does not exist or is non-readable. getwd()=='", getwd(), "'")
    if (isTRUE(file_info$isdir)) stop("File '",file,"' is a directory. Not yet implemented.") # dir.exists() requires R v3.2+, #989
    if (!file_info$size) {
      warning("File '", file, "' has size 0. Returning a NULL ",
              if (data.table) 'data.table' else 'data.frame', ".")
      return(if (data.table) data.table(NULL) else data.frame(NULL))
    }
    ext2 = substring(file, nchar(file)-2L, nchar(file))   # last 3 characters ".gz"
    ext3 = substring(file, nchar(file)-3L, nchar(file))   # last 4 characters ".bz2"
    if (ext2==".gz" || ext3==".bz2") {
      if (!requireNamespace("R.utils", quietly = TRUE))
        stop("To read gz and bz2 files directly, fread() requires 'R.utils' package which cannot be found. Please install 'R.utils' using 'install.packages('R.utils')'.") # nocov
      FUN = if (ext2==".gz") gzfile else bzfile
      R.utils::decompressFile(file, decompFile<-tempfile(tmpdir=tmpdir), ext=NULL, FUN=FUN, remove=FALSE)   # ext is not used by decompressFile when destname is supplied, but isn't optional
      file = decompFile   # don't use 'tmpFile' symbol again, as tmpFile might be the http://domain.org/file.csv.gz download
      on.exit(unlink(decompFile), add=TRUE)
    }
    file = enc2native(file) # CfreadR cannot handle UTF-8 if that is not the native encoding, see #3078.

    input = file
  }
  if (!missing(autostart)) warning("'autostart' is now deprecated and ignored. Consider skip='string' or skip=n");
  if (is.logical(colClasses)) {
    if (!allNA(colClasses)) stop("colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.")
    colClasses = NULL
  }
  if (!is.null(colClasses) && is.atomic(colClasses)) {
    if (!is.character(colClasses)) stop("colClasses is not type list or character vector")
    if (!length(colClasses)) {
      colClasses=NULL;
    } else if (identical(colClasses, "NULL")) {
      colClasses = NULL
      warning('colClasses="NULL" (quoted) is interpreted as colClasses=NULL (the default) as opposed to dropping every column.')
    } else if (!is.null(names(colClasses))) {   # names are column names; convert to list approach
      colClasses = tapply(names(colClasses), colClasses, c, simplify=FALSE)
    }
  }
  stopifnot(length(skip)==1L, !is.na(skip), is.character(skip) || is.numeric(skip))
  if (identical(skip,"__auto__")) skip = if (yaml) 0L else -1L
  else if (is.double(skip)) skip = as.integer(skip)
  # else skip="string" so long as "string" is not "__auto__" (best conveys to user skip is automatic rather than user needing to know -1 or NA means auto)
  stopifnot(is.null(na.strings) || is.character(na.strings))
  tt = grep("^\\s+$", na.strings)
  if (length(tt)) {
    msg = paste0('na.strings[', tt[1L], ']=="',na.strings[tt[1L]],'" consists only of whitespace, ignoring. ')
    if (strip.white) {
      if (any(na.strings=="")) {
        warning(msg, 'strip.white==TRUE (default) and "" is present in na.strings, so any number of spaces in string columns will already be read as <NA>.')
      } else {
        warning(msg, 'Since strip.white=TRUE (default), use na.strings="" to specify that any number of spaces in a string column should be read as <NA>.')
      }
      na.strings = na.strings[-tt]
    } else {
      stop(msg, 'But strip.white=FALSE. Use strip.white=TRUE (default) together with na.strings="" to turn any number of spaces in string columns into <NA>')
    }
    # whitespace at the beginning or end of na.strings is checked at C level and is an error there; test 1804
  }
  if (yaml) {
    if (!requireNamespace('yaml', quietly = TRUE))
      stop("'data.table' relies on the package 'yaml' to parse the file header; please add this to your library with install.packages('yaml') and try again.") # nocov
    # for tracking which YAML elements may be overridden by being declared explicitly
    call_args = names(match.call())
    if (is.character(skip))
      warning("Combining a search string as 'skip' and reading a YAML header may not work as expected -- currently, ",
              "reading will proceed to search for 'skip' from the beginning of the file, NOT from the end of ",
              "the metadata; please file an issue on GitHub if you'd like to see more intuitive behavior supported.")
    # create connection to stream header lines from file:
    #   https://stackoverflow.com/questions/9871307
    f = base::file(input, 'r')
    first_line = readLines(f, n=1L)
    n_read = 1L
    yaml_border_re = '^#?---'
    if (!grepl(yaml_border_re, first_line)) {
      close(f)
      stop('Encountered <', substring(first_line, 1L, 50L), if (nchar(first_line) > 50L) '...', '> at the first ',
           'unskipped line (', 1L+skip, '), which does not constitute the start to a valid YAML header ',
           '(expecting something matching regex "', yaml_border_re, '"); please check your input and try again.')
    }

    yaml_comment_re = '^#'
    yaml_string = character(0L)
    while (TRUE) {
      this_line = readLines(f, n=1L)
      n_read = n_read + 1L
      if (!length(this_line)){
        close(f)
        stop('Reached the end of the file before finding a completion to the YAML header. A valid YAML header is bookended by lines matching ',
             'the regex "', yaml_border_re, '". Please double check the input file is a valid csvy.')
      }
      if (grepl(yaml_border_re, this_line)) break
      if (grepl(yaml_comment_re, this_line))
        this_line = sub(yaml_comment_re, '', this_line)
      yaml_string = paste(yaml_string, this_line, sep='\n')
    }
    close(f) # when #561 is implemented, no need to close f.

    yaml_header = yaml::yaml.load(yaml_string)
    yaml_names = names(yaml_header)
    if (verbose) cat('Processed', n_read, 'lines of YAML metadata with the following top-level fields:', brackify(yaml_names), '\n')
    # process header first since it impacts how to handle colClasses
    if ('header' %chin% yaml_names) {
      if ('header' %chin% call_args) message("User-supplied 'header' will override that found in metadata.")
      else header = as.logical(yaml_header$header)
    }
    if ('schema' %chin% yaml_names) {
      new_types = sapply(yaml_header$schema$fields, `[[`, 'type')
      if (any(null_idx <- sapply(new_types, is.null)))
        new_types = do.call(c, new_types)
      synonms = rbindlist(list(
        character = list(syn = c('character', 'string')),
        integer = list(syn = c('integer', 'int')),
        numeric = list(syn = c('numeric', 'number', 'double')),
        factor = list(syn = c('factor', 'categorical')),
        integer64 = list(syn = c('integer64', 'int64'))
      ), idcol = 'r_type')
      setkeyv(synonms, 'syn')
      new_types = synonms[list(new_types)]$r_type
      new_names = sapply(yaml_header$schema$fields[!null_idx], `[[`, 'name')

      if ('col.names' %chin% call_args) message("User-supplied column names in 'col.names' will override those found in YAML metadata.")
      # resolve any conflicts with colClasses, if supplied;
      #   colClasses (if present) is already in list form by now
      if ('colClasses' %chin% call_args) {
        if (any(idx_name <- new_names %chin% unlist(colClasses))) {
          matched_name_idx = which(idx_name)
          if (!all(idx_type <- sapply(matched_name_idx, function(ii) {
            new_names[ii] %chin% colClasses[[ new_types[ii] ]]
          }))) {
            plural = sum(idx_type) > 1L
            message('colClasses dictated by user input and those read from YAML header are in conflict (specifically, for column', if (plural) 's',
                    ' [', paste(new_names[matched_name_idx[!idx_type]], collapse = ','), ']); the proceeding assumes the user input was ',
                    'an intentional override and will ignore the types implied by the YAML header; please exclude ',
                    if (plural) 'these columns' else 'this column from colClasses if this was unintentional.')
          }
        }
        # only add unmentioned columns
        for (ii in which(!idx_name)) {
          colClasses[[ new_types[ii] ]] = c(colClasses[[ new_types[ii] ]], new_names[ii])
        }
      } else {
        # there are no names to be matched in the data, which fread expects
        #   at the C level; instead, apply these in post through col.names
        #   and send the auto-generated V1:Vn as dummies
        if (identical(header, FALSE)) {
          if (!'col.names' %chin% call_args) col.names = new_names
          new_names = paste0('V', seq_along(new_names))
        }
        colClasses = tapply(new_names, new_types, c, simplify=FALSE)
      }
    }
    sep_syn = c('sep', 'delimiter')
    if (any(sep_idx <- sep_syn %chin% yaml_names)) {
      if ('sep' %chin% call_args) message("User-supplied 'sep' will override that found in metadata.")
      else sep = yaml_header[[ sep_syn[sep_idx][1L] ]]
    }
    quote_syn = c('quote', 'quoteChar', 'quote_char')
    if (any(quote_idx <- quote_syn %chin% yaml_names)) {
      if ('quote' %chin% call_args) message("User-supplied 'quote' will override that found in metadata.")
      else quote = yaml_header[[ quote_syn[quote_idx][1L] ]]
    }
    dec_syn = c('dec', 'decimal')
    if (any(dec_idx <- dec_syn %chin% yaml_names)) {
      if ('dec' %chin% call_args) message("User-supplied 'dec' will override that found in metadata.")
      else dec = yaml_header[[ dec_syn[dec_idx][1L] ]]
    }
    if ('na.strings' %chin% yaml_names) {
      if ('na.strings' %chin% call_args) message("User-supplied 'na.strings' will override that found in metadata.")
      else na.strings = yaml_header$na.strings
    }
    if (is.integer(skip)) skip = skip + n_read
  }
  warnings2errors = getOption("warn") >= 2
  stopifnot(identical(tz,"UTC") || identical(tz,""))
  if (tz=="") {
    tt = Sys.getenv("TZ", unset=NA_character_)
    if (identical(tt,"") || is_utc(tt)) # empty TZ env variable ("") means UTC in C library, unlike R; _unset_ TZ means local
      tz="UTC"
  }
  ans = .Call(CfreadR,input,sep,dec,quote,header,nrows,skip,na.strings,strip.white,blank.lines.skip,
              fill,showProgress,nThread,verbose,warnings2errors,logical01,select,drop,colClasses,integer64,encoding,keepLeadingZeros,tz=="UTC")
  if (!length(ans)) return(null.data.table())  # test 1743.308 drops all columns
  nr = length(ans[[1L]])
  require_bit64_if_needed(ans)
  setattr(ans,"row.names",.set_row_names(nr))

  if (isTRUE(data.table)) {
    setattr(ans, "class", c("data.table", "data.frame"))
    setalloccol(ans)
  } else {
    setattr(ans, "class", "data.frame")
  }
  # #1027, make.unique -> make.names as spotted by @DavidArenberg
  if (check.names) {
    setattr(ans, 'names', make.names(names(ans), unique=TRUE))
  }

  colClassesAs = attr(ans, "colClassesAs", exact=TRUE)   # should only be present if one or more are != ""
  for (j in which(colClassesAs!="")) {       # # 1634
    v = .subset2(ans, j)
    new_class = colClassesAs[j]
    new_v = tryCatch({    # different to read.csv; i.e. won't error if a column won't coerce (fallback with warning instead)
      switch(new_class,
             "factor" = as_factor(v),
             "complex" = as.complex(v),
             "raw" = as_raw(v),  # Internal implementation
             "Date" = as.Date(v),
             "POSIXct" = as.POSIXct(v),  # test 2150.14 covers this by setting the option to restore old behaviour. Otherwise types that
             # are recognized by freadR.c (e.g. POSIXct; #4464) result in user-override-bump at C level before reading so do not reach this switch
             # see https://github.com/Rdatatable/data.table/pull/4464#discussion_r447275278.
             # Aside: as(v,"POSIXct") fails with error in R so has to be caught explicitly above
             # finally:
             methods::as(v, new_class))
      },
      warning = fun <- function(e) {
        warning("Column '", names(ans)[j], "' was requested to be '", new_class, "' but fread encountered the following ",
                if (inherits(e, "error")) "error" else "warning", ":\n\t", e$message, "\nso the column has been left as type '", typeof(v), "'", call.=FALSE)
        return(v)
      },
      error = fun)
    set(ans, j = j, value = new_v)  # aside: new_v == v if the coercion was aborted
  }
  setattr(ans, "colClassesAs", NULL)

  if (stringsAsFactors) {
    if (is.double(stringsAsFactors)) { #2025
      should_be_factor = function(v) is.character(v) && uniqueN(v) < nr * stringsAsFactors
      cols_to_factor = which(vapply_1b(ans, should_be_factor))
    } else {
      cols_to_factor = which(vapply_1b(ans, is.character))
    }
    if (verbose) cat("stringsAsFactors=", stringsAsFactors, " converted ", length(cols_to_factor), " column(s): ", brackify(names(ans)[cols_to_factor]), "\n", sep="")
    for (j in cols_to_factor) set(ans, j=j, value=as_factor(.subset2(ans, j)))
  }

  if (!missing(col.names))   # FR #768
    setnames(ans, col.names) # setnames checks and errors automatically
  if (!is.null(key) && data.table) {
    if (!is.character(key))
      stop("key argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (length(key) == 1L) {
      key = strsplit(key, split = ",", fixed = TRUE)[[1L]]
    }
    setkeyv(ans, key)
  }
  if (yaml) setattr(ans, 'yaml_metadata', yaml_header)
  if (!is.null(index) && data.table) {
    if (!all(sapply(index, is.character)))
      stop("index argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (is.list(index)) {
      to_split = sapply(index, length) == 1L
      if (any(to_split))
        index[to_split] = sapply(index[to_split], strsplit, split = ",", fixed = TRUE)
    } else {
      if (length(index) == 1L) {
        # setindexv accepts lists, so no [[1]]
        index = strsplit(index, split = ",", fixed = TRUE)
      }
    }
    setindexv(ans, index)
  }
  ans
}

# simplified but faster version of `factor()` for internal use.
as_factor = function(x) {
  lev = forderv(x, retGrp = TRUE, na.last = NA)
  # get levels, also take care of all sorted condition
  lev = if (length(lev)) x[lev[attributes(lev)$starts]] else x[attributes(lev)$starts]
  ans = chmatch(x, lev)
  setattr(ans, 'levels', lev)
  setattr(ans, 'class', 'factor')
}

as_raw = function(x) {
  scan(text=x, what=raw(), quiet=TRUE)  # as in read.csv, which ultimately uses src/main/scan.c and strtoraw
}
