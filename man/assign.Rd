\name{:=}
\alias{:=}
\alias{set}
\title{ Assignment by reference }
\description{
    Fast add, remove and update subsets of columns, by reference. \code{:=} operator can be used in two ways: \code{LHS := RHS} form, and \code{Functional form}. See \code{Usage}.

    \code{set} is a low-overhead loop-able version of \code{:=}. It is particularly useful for repetitively updating rows of certain columns by reference (using a for-loop). See \code{Examples}. It can not perform grouping operations.

}
\usage{
# 1. LHS := RHS form
# DT[i, LHS := RHS, by = ...]
# DT[i, c("LHS1", "LHS2") := list(RHS1, RHS2), by = ...]

# 2. Functional form
# DT[i, `:=`(LHS1 = RHS1,
#            LHS2 = RHS2,
#            ...), by = ...]

set(x, i = NULL, j, value)
}
\arguments{
\item{LHS}{ A character vector of column names (or numeric positions) or a variable that evaluates as such. If the column doesn't exist, it is added, \emph{by reference}. }
\item{RHS}{ A list of replacement values. It is recycled in the usual way to fill the number of rows satisfying \code{i}, if any.  To remove a column use \code{NULL}. }
\item{x}{ A \code{data.table}. Or, \code{set()} accepts \code{data.frame}, too. }
\item{i}{ Optional. Indicates the rows on which the values must be updated with. If not provided, implies \emph{all rows}. The \code{:=} form is more powerful as it allows \emph{subsets} and \code{joins} based add/update columns by reference. See \code{Details}.

    In \code{set}, only integer type is allowed in \code{i} indicating which rows \code{value} should be assigned to. \code{NULL} represents all rows more efficiently than creating a vector such as \code{1:nrow(x)}. }
\item{j}{ Column name(s) (character) or number(s) (integer) to be assigned \code{value} when column(s) already exist, and only column name(s) if they are to be created. }
\item{value}{ A list of replacement values to assign by reference to \code{x[i, j]}. }
}
\details{
\code{:=} is defined for use in \code{j} only. It \emph{adds} or \emph{updates} or \emph{removes} column(s) by reference. It makes no copies of any part of memory at all. Please read \href{../doc/datatable-reference-semantics.html}{\code{vignette("datatable-reference-semantics")}} and follow with examples. Some typical usages are:

\preformatted{
    DT[, col := val]                              # update (or add at the end if doesn't exist) a column called "col" with value "val" (recycled if necessary).
    DT[i, col := val]                             # same as above, but only for those rows specified in i and (for new columns) NA elsewhere.
    DT[i, "col a" := val]                         # same. column is called "col a"
    DT[i, (3:6) := val]                           # update existing columns 3:6 with value. Aside: parens are not required here since : already makes LHS a call rather than a symbol.
    DT[i, colvector := val, with = FALSE]         # OLD syntax. The contents of "colvector" in calling scope determine the column(s).
    DT[i, (colvector) := val]                     # same (NOW PREFERRED) shorthand syntax. The parens are enough to stop the LHS being a symbol; same as c(colvector).
    DT[i, colC := mean(colB), by = colA]          # update (or add) column called "colC" by reference by group. A major feature of `:=`.
    DT[,`:=`(new1 = sum(colB), new2 = sum(colC))] # Functional form
}

The \code{\link{.Last.updated}} variable contains the number of rows updated by the most recent \code{:=} or \code{set} calls, which may be useful, for example, in production settings for testing assumptions about the number of rows affected by a statement; see \code{\link{.Last.updated}} for details.

Note that for efficiency no check is performed for duplicate assignments, i.e. if multiple values are passed for assignment to the same index, assignment to this index will occur repeatedly and sequentially; for a given use case, consider whether it makes sense to create your own test for duplicates, e.g. in production code.

All of the following result in a friendly error (by design) :

\preformatted{
    x := 1L
    DT[i, col] := val
    DT[i]$col := val
    DT[, {col1 := 1L; col2 := 2L}]                # Use the functional form, `:=`(), instead (see above).
}

For additional resources, please read \href{../doc/datatable-faq.html}{\code{vignette("datatable-faq")}}. Also have a look at StackOverflow's \href{http://stackoverflow.com/search?q=\%5Bdata.table\%5D+reference}{data.table tag}.

\code{:=} in \code{j} can be combined with all types of \code{i} (such as binary search), and all types of \code{by}. This a one reason why \code{:=} has been implemented in \code{j}. Please see \href{../doc/datatable-reference-semantics}{\code{vignette("datatable-reference-semantics")}} and also \code{FAQ 2.16} for analogies to SQL.

When \code{LHS} is a factor column and \code{RHS} is a character vector with items missing from the factor levels, the new level(s) are automatically added (by reference, efficiently), unlike base methods.

Unlike \code{<-} for \code{data.frame}, the (potentially large) LHS is not coerced to match the type of the (often small) RHS. Instead the RHS is coerced to match the type of the LHS, if necessary. Where this involves double precision values being coerced to an integer column, a warning is given (whether or not fractional data is truncated). The motivation for this is efficiency. It is best to get the column types correct up front and stick to them. Changing a column type is possible but deliberately harder: provide a whole column as the RHS. This RHS is then \emph{plonked} into that column slot and we call this \emph{plonk syntax}, or \emph{replace column syntax} if you prefer. By needing to construct a full length vector of a new type, you as the user are more aware of what is happening, and it is clearer to readers of your code that you really do intend to change the column type.

\code{data.table}s are \emph{not} copied-on-change by \code{:=}, \code{setkey} or any of the other \code{set*} functions. See \code{\link{copy}}.
}

\section{Advanced (internals):}{It is easy to see how \emph{sub-assigning} to existing columns is done internally. Removing columns by reference is also straightforward by modifying the vector of column pointers only (using memmove in C). However adding (new) columns is more tricky as to how the \code{data.table} can be grown \emph{by reference}: the list vector of column pointers is \emph{over-allocated}, see \code{\link{truelength}}. By defining \code{:=} in \code{j} we believe update syntax is natural, and scales, but it also bypasses \code{[<-} dispatch and allows \code{:=} to update by reference with no copies of any part of memory at all.

Since \code{[.data.table} incurs overhead to check the existence and type of arguments (for example), \code{set()} provides direct (but less flexible) assignment by reference with low overhead, appropriate for use inside a \code{for} loop. See examples. \code{:=} is more powerful and flexible than \code{set()} because \code{:=} is intended to be combined with \code{i} and \code{by} in single queries on large datasets.
}
\section{Note:}{
    \code{DT[a > 4, b := c]} is different from \code{DT[a > 4][, b := c]}. The first expression updates (or adds) column \code{b} with the value \code{c} on those rows where \code{a > 4} evaluates to \code{TRUE}. \code{X} is updated \emph{by reference}, therefore no assignment needed.

    The second expression on the other hand updates a \emph{new} \code{data.table} that's returned by the subset operation. Since the subsetted data.table is ephemeral (it is not assigned to a symbol), the result would be lost; unless the result is assigned, for example, as follows: \code{ans <- DT[a > 4][, b := c]}.
}
\value{
\code{DT} is modified by reference and returned invisibly. If you require a copy, take a \code{\link{copy}} first (using \code{DT2 = copy(DT)}).
}
\seealso{ \code{\link{data.table}}, \code{\link{copy}}, \code{\link{setalloccol}}, \code{\link{truelength}}, \code{\link{set}}, \code{\link{.Last.updated}}
}
\examples{
DT = data.table(a = LETTERS[c(3L,1:3)], b = 4:7)
DT[, c := 8]                # add a numeric column, 8 for all rows
DT[, d := 9L]               # add an integer column, 9L for all rows
DT[, c := NULL]             # remove column c
DT[2, d := -8L]             # subassign by reference to d; 2nd row is -8L now
DT                          # DT changed by reference
DT[2, d := 10L][]           # shorthand for update and print

DT[b > 4, b := d * 2L]      # subassign to b with d*2L on those rows where b > 4 is TRUE
DT[b > 4][, b := d * 2L]    # different from above. [, := ] is performed on the subset
                            # which is an new (ephemeral) data.table. Result needs to be
                            # assigned to a variable (using `<-`).

DT[, e := mean(d), by = a]  # add new column by group by reference
DT["A", b := 0L, on = "a"]  # ad-hoc update of column b for group "A" using
			    # joins-as-subsets with binary search and 'on='
# same as above but using keys
setkey(DT, a)
DT["A", b := 0L]            # binary search for group "A" and set column b using keys
DT["B", f := mean(d)]       # subassign to new column, NA initialized

# Adding multiple columns
## by name
DT[ , c('sin_d', 'log_e', 'cos_d') :=
   .(sin(d), log(e), cos(d))]
## by patterned name
DT[ , paste(c('sin', 'cos'), 'b', sep = '_') :=
   .(sin(b), cos(b))]
## using lapply & .SD
DT[ , paste0('tan_', c('b', 'd', 'e')) :=
   lapply(.SD, tan), .SDcols = c('b', 'd', 'e')]
## using forced evaluation to disambiguate a vector of names
##   and overwrite existing columns with their squares
sq_cols = c('b', 'd', 'e')
DT[ , (sq_cols) := lapply(.SD, `^`, 2L), .SDcols = sq_cols]
## by integer (NB: for robustness, it is not recommended
##   to use explicit integers to update/define columns)
DT[ , c(2L, 3L, 4L) := .(sqrt(b), sqrt(d), sqrt(e))]
## by implicit integer
DT[ , grep('a$', names(DT)) := tolower(a)]
## by implicit integer, using forced evaluation
sq_col_idx = grep('d$', names(DT))
DT[ , (sq_col_idx) := lapply(.SD, dnorm),
   .SDcols = sq_col_idx]

\dontrun{
# Speed example:

m = matrix(1, nrow = 2e6L, ncol = 100L)
DF = as.data.frame(m)
DT = as.data.table(m)

system.time(for (i in 1:1000) DF[i, 1] = i)
# 15.856 seconds
system.time(for (i in 1:1000) DT[i, V1 := i])
# 0.279 seconds  (57 times faster)
system.time(for (i in 1:1000) set(DT, i, 1L, i))
# 0.002 seconds  (7930 times faster, overhead of [.data.table is avoided)

# However, normally, we call [.data.table *once* on *large* data, not many times on small data.
# The above is to demonstrate overhead, not to recommend looping in this way. But the option
# of set() is there if you need it.
}

}
\keyword{ data }

