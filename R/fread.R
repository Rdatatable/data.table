fread = function(
input="", file=NULL, text=NULL, cmd=NULL, sep="auto", sep2="auto", dec="auto", quote="\"", nrows=Inf, header="auto",
na.strings=getOption("datatable.na.strings","NA"), stringsAsFactors=FALSE, verbose=getOption("datatable.verbose",FALSE),
skip="__auto__", select=NULL, drop=NULL, colClasses=NULL, integer64=getOption("datatable.integer64","integer64"),
col.names, check.names=FALSE, encoding="unknown", strip.white=TRUE, fill=FALSE, blank.lines.skip=FALSE, key=NULL, index=NULL,
showProgress=getOption("datatable.showProgress",interactive()), data.table=getOption("datatable.fread.datatable",TRUE),
nThread=getDTthreads(verbose), logical01=getOption("datatable.logical01",FALSE),
logicalYN=getOption("datatable.logicalYN", FALSE),
keepLeadingZeros=getOption("datatable.keepLeadingZeros",FALSE),
yaml=FALSE, tmpdir=tempdir(), tz="UTC")
{
  if (missing(input)+is.null(file)+is.null(text)+is.null(cmd) < 3L) stopf("Used more than one of the arguments input=, file=, text= and cmd=.")
  input_has_vars = length(all.vars(substitute(input)))>0L  # see news for v1.11.6
  if (is.null(sep)) sep="\n"         # C level knows that \n means \r\n on Windows, for example
  else {
    stopifnot( length(sep)==1L, !is.na(sep), is.character(sep) )
    if (sep=="") { sep="\n" }         # meaning readLines behaviour. The 3 values (NULL, "" or "\n") are equivalent.
    else if (sep=="auto") sep=""      # sep=="" at C level means auto sep
    else stopifnot( nchar(sep)==1L )  # otherwise an actual character to use as sep
  }
  stopifnot( is.character(dec), length(dec)==1L)
  if (dec == "auto") dec = "" else stopifnot(nchar(dec) == 1L)
  # handle encoding, #563
  if (length(encoding) != 1L || !encoding %chin% c("unknown", "UTF-8", "Latin-1")) {
    stopf("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
  }
  stopifnot(
    isTRUEorFALSE(strip.white), isTRUEorFALSE(blank.lines.skip), isTRUEorFALSE(fill) || is.numeric(fill) && length(fill)==1L && fill >= 0L, isTRUEorFALSE(showProgress),
    isTRUEorFALSE(verbose), isTRUEorFALSE(check.names), isTRUEorFALSE(logical01), isTRUEorFALSE(logicalYN), isTRUEorFALSE(keepLeadingZeros), isTRUEorFALSE(yaml),
    isTRUEorFALSE(stringsAsFactors) || (is.double(stringsAsFactors) && length(stringsAsFactors)==1L && 0.0<=stringsAsFactors && stringsAsFactors<=1.0),
    is.numeric(nrows), length(nrows)==1L
  )
  fill = if(identical(fill, Inf)) .Machine$integer.max else as.integer(fill)
  nrows=as.double(nrows) #4686
  if (is.na(nrows) || nrows<0L) nrows=Inf   # accept -1 to mean Inf, as read.table does
  if (identical(header,"auto")) header=NA
  stopifnot(
    is.logical(header), length(header)==1L,  # TRUE, FALSE or NA
    is.numeric(nThread), length(nThread)==1L
  )
  nThread=as.integer(nThread)
  stopifnot(nThread>=1L)
  if (!is.null(text)) {
    if (!is.character(text)) stopf("'text=' is type %s but must be character.", typeof(text))
    if (!length(text)) return(data.table())
    if (length(text) > 1L) {
      writeLines(text, tmpFile<-tempfile(tmpdir=tmpdir))  # avoid paste0() which could create a new very long single string in R's memory
      file = tmpFile
      on.exit(unlink(tmpFile), add=TRUE)
    } else {
      # avoid creating a tempfile() for single strings, which can be done a lot; e.g. in the test suite.
      input = text
    }
  }
  else if (is.null(cmd)) {
    if (!is.character(input) || length(input)!=1L) {
      stopf("input= must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r")
    }
    if (input=="" || length(grep('\\n|\\r', input))) {
      # input is data itself containing at least one \n or \r
    } else if (startsWith(input, " ")) {
      stopf("input= contains no \\n or \\r, but starts with a space. Please remove the leading space, or use text=, file= or cmd=")
    } else if (length(grep(' ', input, fixed=TRUE)) && !file.exists(input)) {  # file name or path containing spaces is not a command
      cmd = input
      if (input_has_vars && getOption("datatable.fread.input.cmd.message", TRUE)) {
        messagef("Taking input= as a system command because it contains a space ('%s'). If it's a filename please remove the space, or use file= explicitly. A variable is being passed to input= and when this is taken as a system command there is a security concern if you are creating an app, the app could have a malicious user, and the app is not running in a secure environment; e.g. the app is running as root. Please read item 5 in the NEWS file for v1.11.6 for more information and for the option to suppress this message.", cmd)
      }
    } else {
      file = input   # filename, including URLS
    }
  }
  if (!is.null(cmd)) {
    (if (.Platform$OS.type == "unix") system else shell)(paste0('(', cmd, ') > ', tmpFile<-tempfile(tmpdir=tmpdir)))
    file = tmpFile
    on.exit(unlink(tmpFile), add=TRUE)
  }
  if (!is.null(file)) {
    if (!is.character(file) || length(file)!=1L)
      stopf("file= must be a single character string containing a filename, or URL starting 'http[s]://', 'ftp[s]://' or 'file://'")
    if (w <- startsWithAny(file, c("https://", "ftps://", "http://", "ftp://", "file://"))) {  # avoid grepl() for #2531
      # nocov start
      tmpFile = tempfile(fileext = paste0(".",tools::file_ext(file)), tmpdir=tmpdir)  # retain .gz extension in temp filename so it knows to be decompressed further below
      method = if (w==5L) "internal"  # force 'auto' when file: to ensure we don't use an invalid option (e.g. wget), #1668
               else getOption("download.file.method", default="auto")  # http: or ftp:
      # In text mode on Windows-only, R doubles up \r to make \r\r\n line endings. mode="wb" avoids that. See ?connections:"CRLF"
      download.file(file, tmpFile, method=method, mode="wb", quiet=!showProgress)
      file = tmpFile
      on.exit(unlink(tmpFile), add=TRUE)
      # nocov end
    }
    file_info = file.info(file)
    if (is.na(file_info$size)) stopf("File '%s' does not exist or is non-readable. getwd()=='%s'", file, getwd())
    if (isTRUE(file_info$isdir)) stopf("File '%s' is a directory. Not yet implemented.", file) # Could use dir.exists(), but we already ran file.info().
    if (!file_info$size) {
      warningf("File '%s' has size 0. Returning a NULL %s.", file, if (data.table) 'data.table' else 'data.frame')
      return(if (data.table) data.table(NULL) else data.frame(NULL))
    }

    # support zip and tar files #3834
    file_signature = readBin(file, raw(), 8L)

    if ((w <- endsWithAny(file, c(".zip", ".tar"))) || is_zip(file_signature)) {
      FUN = if (w==2L) untar else unzip
      fnames = FUN(file, list=TRUE)
      if (is.data.frame(fnames)) fnames = fnames[,1L]
      if (length(fnames) > 1L)
        stopf("Compressed files containing more than 1 file are currently not supported.")
      FUN(file, exdir=tmpdir)
      decompFile = file.path(tmpdir, fnames)
      file = decompFile
      on.exit(unlink(decompFile), add=TRUE)
    }

    gzsig = FALSE
    if ((w <- endsWithAny(file, c(".gz", ".bgz",".bz2"))) || (gzsig <- is_gzip(file_signature)) || is_bzip(file_signature)) {
      if (!requireNamespace("R.utils", quietly = TRUE))
        stopf("To read %s files directly, fread() requires 'R.utils' package which cannot be found. Please install 'R.utils' using 'install.packages('R.utils')'.", if (w<=2L || gzsig) "gz" else "bz2") # nocov
      FUN = if (w<=2L || gzsig) gzfile else bzfile
      R.utils::decompressFile(file, decompFile<-tempfile(tmpdir=tmpdir), ext=NULL, FUN=FUN, remove=FALSE)   # ext is not used by decompressFile when destname is supplied, but isn't optional
      file = decompFile   # don't use 'tmpFile' symbol again, as tmpFile might be the http://domain.org/file.csv.gz download
      on.exit(unlink(decompFile), add=TRUE)
    }
    file = enc2native(file) # CfreadR cannot handle UTF-8 if that is not the native encoding, see #3078.

    input = file
  }
  if (is.logical(colClasses)) {
    if (!allNA(colClasses)) stopf("colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.")
    colClasses = NULL
  }
  if (!is.null(colClasses) && is.atomic(colClasses)) { ## future R can use  if (is.atomic(.))
    if (!is.character(colClasses)) stopf("colClasses is not type list or character vector")
    if (!length(colClasses)) {
      colClasses=NULL;
    } else if (identical(colClasses, "NULL")) {
      colClasses = NULL
      warningf('colClasses="NULL" (quoted) is interpreted as colClasses=NULL (the default) as opposed to dropping every column.')
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
    msg = gettextf('na.strings[%d]=="%s" consists only of whitespace, ignoring', tt[1L], na.strings[tt[1L]])
    if (strip.white) {
      if (all(nzchar(na.strings))) {
        warningf('%s. Since strip.white=TRUE (default), use na.strings="" to specify that any number of spaces in a string column should be read as <NA>.', msg)
      } else {
        warningf('%s. strip.white==TRUE (default) and "" is present in na.strings, so any number of spaces in string columns will already be read as <NA>.', msg)
      }
      na.strings = na.strings[-tt]
    } else {
      stopf('%s. But strip.white=FALSE. Use strip.white=TRUE (default) together with na.strings="" to turn any number of spaces in string columns into <NA>', msg)
    }
    # whitespace at the beginning or end of na.strings is checked at C level and is an error there; test 1804
  }
  # nocov start. Tested in other.Rraw tests 16, not in the main suite.
  if (yaml) {
    if (!requireNamespace('yaml', quietly = TRUE))
      stopf("'data.table' relies on the package 'yaml' to parse the file header; please add this to your library with install.packages('yaml') and try again.") # nocov
    # for tracking which YAML elements may be overridden by being declared explicitly
    call_args = names(match.call())
    if (is.character(skip))
      warningf("Combining a search string as 'skip' and reading a YAML header may not work as expected -- currently, reading will proceed to search for 'skip' from the beginning of the file, NOT from the end of the metadata; please file an issue on GitHub if you'd like to see more intuitive behavior supported.")
    # create connection to stream header lines from file:
    #   https://stackoverflow.com/questions/9871307
    f = base::file(input, 'r')
    first_line = readLines(f, n=1L)
    n_read = 1L
    yaml_border_re = '^#?---'
    if (!grepl(yaml_border_re, first_line)) {
      close(f)
      stopf(
        'Encountered <%s%s> at the first unskipped line (%d), which does not constitute the start to a valid YAML header (expecting something matching regex "%s"); please check your input and try again.',
        substr(first_line, 1L, 50L), if (nchar(first_line) > 50L) '...' else '', 1L+skip, yaml_border_re
      )
    }

    yaml_comment_re = '^#'
    yaml_string = character(0L)
    repeat {
      this_line = readLines(f, n=1L)
      n_read = n_read + 1L
      if (!length(this_line)){
        close(f)
        stopf('Reached the end of the file before finding a completion to the YAML header. A valid YAML header is bookended by lines matching the regex "%s". Please double check the input file is a valid csvy.', yaml_border_re)
      }
      if (grepl(yaml_border_re, this_line)) break
      if (grepl(yaml_comment_re, this_line))
        this_line = sub(yaml_comment_re, '', this_line)
      yaml_string = paste(yaml_string, this_line, sep='\n')
    }
    close(f) # when #561 is implemented, no need to close f.

    yaml_header = yaml::yaml.load(yaml_string)
    yaml_names = names(yaml_header)
    if (verbose) catf('Processed %d lines of YAML metadata with the following top-level fields: %s\n', n_read, brackify(yaml_names))
    # process header first since it impacts how to handle colClasses
    if ('header' %chin% yaml_names) {
      if ('header' %chin% call_args) messagef("User-supplied 'header' will override that found in metadata.")
      else header = as.logical(yaml_header$header)
    }
    if ('schema' %chin% yaml_names) {
      new_types = sapply(yaml_header$schema$fields, `[[`, 'type')
      if (any(null_idx <- vapply_1b(new_types, is.null)))
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

      if ('col.names' %chin% call_args) messagef("User-supplied column names in 'col.names' will override those found in YAML metadata.")
      # resolve any conflicts with colClasses, if supplied;
      #   colClasses (if present) is already in list form by now
      if ('colClasses' %chin% call_args) {
        if (any(idx_name <- new_names %chin% unlist(colClasses))) {
          matched_name_idx = which(idx_name)
          if (!all(idx_type <- sapply(matched_name_idx, function(ii) {
            new_names[ii] %chin% colClasses[[ new_types[ii] ]]
          }))) {
            messagef('colClasses dictated by user input and those read from YAML header are in conflict (specifically, for column(s) [%s]); the proceeding assumes the user input was an intentional override and will ignore the type(s) implied by the YAML header; please exclude the column(s) from colClasses if this was unintentional.',
              brackify(new_names[matched_name_idx[!idx_type]]))
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
      if ('sep' %chin% call_args) messagef("User-supplied 'sep' will override that found in metadata.")
      else sep = yaml_header[[ sep_syn[sep_idx][1L] ]]
    }
    quote_syn = c('quote', 'quoteChar', 'quote_char')
    if (any(quote_idx <- quote_syn %chin% yaml_names)) {
      if ('quote' %chin% call_args) messagef("User-supplied 'quote' will override that found in metadata.")
      else quote = yaml_header[[ quote_syn[quote_idx][1L] ]]
    }
    dec_syn = c('dec', 'decimal')
    if (any(dec_idx <- dec_syn %chin% yaml_names)) {
      if ('dec' %chin% call_args) messagef("User-supplied 'dec' will override that found in metadata.")
      else dec = yaml_header[[ dec_syn[dec_idx][1L] ]]
    }
    if ('na.strings' %chin% yaml_names) {
      if ('na.strings' %chin% call_args) messagef("User-supplied 'na.strings' will override that found in metadata.")
      else na.strings = yaml_header$na.strings
    }
    if (is.integer(skip)) skip = skip + n_read
  }
  # nocov end
  warnings2errors = getOption("warn") >= 2L
  stopifnot(identical(tz,"UTC") || identical(tz,""))
  if (tz=="") {
    tt = Sys.getenv("TZ", unset=NA_character_)
    if (identical(tt,"") || is_utc(tt)) # empty TZ env variable ("") means UTC in C library, unlike R; _unset_ TZ means local
      tz="UTC"
  }
  ans = .Call(CfreadR,input,identical(input,file),sep,dec,quote,header,nrows,skip,na.strings,strip.white,blank.lines.skip,
              fill,showProgress,nThread,verbose,warnings2errors,logical01,logicalYN,select,drop,colClasses,integer64,encoding,keepLeadingZeros,tz=="UTC")
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
  for (j in which(nzchar(colClassesAs))) {       # # 1634
    v = .subset2(ans, j)
    new_class = colClassesAs[j]
    if (new_class %chin% c("POSIXct")) v[!nzchar(v)] = NA_character_ # as.POSIXct/as.POSIXlt cannot handle as.POSIXct("") correctly #6208
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
      warning = fun <- function(c) {
        # NB: branch here for translation purposes (e.g. if error/warning have different grammatical gender)
        if (inherits(c, "warning")) {
          msg_fmt = gettext("Column '%s' was requested to be '%s' but fread encountered the following warning:\n\t%s\nso the column has been left as type '%s'")
        } else {
          msg_fmt = gettext("Column '%s' was requested to be '%s' but fread encountered the following error:\n\t%s\nso the column has been left as type '%s'")
        }
        warningf(msg_fmt, names(ans)[j], new_class, conditionMessage(c), typeof(v), domain=NA)
        v
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
    if (verbose)
      catf(ngettext(length(cols_to_factor), "stringsAsFactors=%s converted %d column: %s\n", "stringsAsFactors=%s converted %d columns: %s\n"),
           stringsAsFactors, length(cols_to_factor), brackify(names(ans)[cols_to_factor]), domain=NA)
    for (j in cols_to_factor) set(ans, j=j, value=as_factor(.subset2(ans, j)))
  }

  if (!missing(col.names))   # FR #768
    setnames(ans, col.names) # setnames checks and errors automatically
  if (!is.null(key) && data.table) {
    if (!is.character(key))
      stopf("key argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (length(key) == 1L)
      key = cols_from_csv(key)
    setkeyv(ans, key)
  }
  if (yaml) setattr(ans, 'yaml_metadata', yaml_header) # nocov
  if (!is.null(index) && data.table) {
    if (!all(vapply_1b(index, is.character)))
      stopf("index argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)")
    if (is.list(index)) {
      to_split = lengths(index) == 1L
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

known_signatures = list(
  zip = as.raw(c(0x50, 0x4b, 0x03, 0x04)), # charToRaw("PK\x03\x04")
  gzip = as.raw(c(0x1F, 0x8B)),
  bzip = as.raw(c(0x42, 0x5A, 0x68))
)

# https://en.wikipedia.org/wiki/ZIP_(file_format)#File_headers
# not checked: what's a valid 'version' entry to check the 5th+6th bytes
is_zip = function(file_signature) {
  identical(file_signature[1:4], known_signatures$zip)
}

# https://en.wikipedia.org/wiki/Gzip#File_format
# not checked: remaining 8 bytes of header
is_gzip = function(file_signature) {
  identical(file_signature[1:2], known_signatures$gzip)
}

# https://en.wikipedia.org/wiki/Bzip2#File_format
is_bzip = function(file_signature) {
  identical(file_signature[1:3], known_signatures$bzip) &&
    isTRUE(file_signature[4L] %in% charToRaw('123456789')) # for #6304
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
