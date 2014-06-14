
Getting started : [data.table homepage](http://datatable.r-forge.r-project.org/)<br>
Support : [data.table tag](http://stackoverflow.com/questions/tagged/r+data.table) (please read: [how best to ask](http://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example))<br>
Notices/discussion : [Nabble](http://r.789695.n4.nabble.com/datatable-help-f2315188.html);  [Gmane](http://dir.gmane.org/gmane.comp.lang.r.datatable); 
[HTML Archive](http://lists.r-forge.r-project.org/pipermail/datatable-help); 
[RSS](http://rss.gmane.org/gmane.comp.lang.r.datatable);
[Subscribe to post](http://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/datatable-help)<br>
User reviews : [Crantastic](http://crantastic.org/packages/data-table)<br>
Current stable release (always even) : [v1.9.2 on CRAN](http://cran.r-project.org/web/packages/data.table/index.html), released 27<sup>th</sup> Feb 2014.<br>
Development version (always odd): [v1.9.3 on GitHub](https://github.com/Rdatatable/datatable/) [![Build Status](https://travis-ci.org/Rdatatable/datatable.svg?branch=master)](https://travis-ci.org/Rdatatable/datatable)

```R
# which version do you have installed?
packageVersion("data.table")

# check and update to latest version on CRAN
update.packages()

# try latest development version from GitHub
devtools::install_github("datatable", "Rdatatable")

# revert to latest version on CRAN
remove.packages("data.table")
install.packages("data.table")
```

The news below is updated as soon as new features or bug fixes are available in the latest (unstable) development version.

_Stability_ refers to features and syntax, not how buggy it is. For example, if you install the latest version from GitHub and start using a new feature, you may sometimes find it is subsequently changed and your new code breaks. When we release to CRAN we are saying that we are happy with the changes and you can rely on them being there in future.  If you hit a problem that the development version fixes, then it is usually safe to simply upgrade to it.

### Changes in v1.9.3  (in development on GitHub)

#### NEW FEATURES

  * `by=.EACHI` runs `j` for each group in `x` that each row of `i` joins to.
```S
DT[.(c("id1", "id2")), sum(val)]                # single total across both id1 and id2
DT[.(c("id1", "id2")), sum(val), by = .EACHI]   # sum(val) for each id
DT[.(c("id1", "id2")), sum(val), by = key(DT)]  # same
```
  In other words, `by-without-by` is now explicit, for clarity and consistency, #371 (git [#371](https://github.com/Rdatatable/datatable/issues/371)). NOTE : when `i` contains duplicates, `by=.EACHI` is different to `by=key(DT)`; e.g,
```S
setkey(DT, ID)
ids = c("id1", "id2, "id1")
DT[ids, sum(val), by = ID]       # 2 rows returned
DT[ids, sum(val), by = .EACHI]   # 3 rows, in the order of ids (result 1 and 3 separate)
```
  `by=.EACHI` can be useful when `i` is event data, where you don't want the events
  aggregated by common join values but wish the output to be ordered with repeats, or 
  simply just using join inherited columns as parameters; e.g.;
```S
X[Y, head(.SD, i.top), by = .EACHI]
```
  where 'top' is a non-join column in `Y`; i.e. join inherited column. Thanks to many, especially eddi, Sadao Milberg and Gabor Grothendieck for extended discussions.

  * Accordingly, `X[Y, j]` now does what `X[Y][, j]` did. A "classic" option to restore the previous default behaviour is to be dicussed and confirmed. See [this](http://r.789695.n4.nabble.com/changing-data-table-by-without-by-syntax-to-require-a-quot-by-quot-td4664770.html), [this](http://stackoverflow.com/questions/16093289/data-table-join-and-j-expression-unexpected-behavior) and [this](http://stackoverflow.com/a/16222108/403310) post for discussions. 

  * `bit64::integer64` now works in grouping and joins, **#5369** (git [#342](https://github.com/Rdatatable/datatable/issues/342)). Thanks to James Sams for highlighting UPCs and Clayton Stanley for [this SO post](http://stackoverflow.com/questions/22273321/large-integers-in-data-table-grouping-results-different-in-1-9-2-compared-to-1). **Reminder:** `fread()` has been able to detect and read `integer64` for a while. 

  * `setNumericRounding()` may be used to reduce to 1 byte or 0 byte rounding when joining to or grouping columns of type 'numeric', **#5369** (git [#342](https://github.com/Rdatatable/datatable/issues/342)). See example in `?setNumericRounding` and NEWS item below for v1.9.2. `getNumericRounding()` returns the current setting.
     
  * `X[Y]` now names non-join columns from `i` that have the same name as a column in `x`, with an `i.` prefix for consistency with the `i.` prefix that has been available in `j` for some time. This is now documented.
     
  * For a keyed table `X` where the key columns are not at the beginning in order, `X[Y]` now retains the original order of columns in X rather than moving the join columns to the beginning of the result.

  * It is no longer an error to assign to row 0 or row NA.
```S
DT[0, colA := 1L]             # now does nothing, silently (was error)
DT[NA, colA := 1L]            # now does nothing, silently (was error)
DT[c(1, NA, 0, 2), colA:=1L]  # now ignores the NA and 0 silently (was error)
DT[nrow(DT) + 1, colA := 1L]  # error (out-of-range) as before
```
  This is for convenience to avoid the need for a switch in user code that evals various i conditions in a loop passing in i as an integer vector which may containing 0 or NA.

  * A new function `setorder` is now implemented which uses data.table's internal fast order to reorder rows **by reference**. It returns the result invisibly (like `setkey`) that allows for compound statements, ex: `setorder(DT, a, -b)[, cumsum(c), by=list(a,b)]`. Check `?setorder` for more info.

  * `DT[order(x, -y)]` is now by default optimised to use data.table's internal fast order as `DT[forder(DT, x, -y)]`. It can be turned off by setting `datatable.optimize` to < 1L or just calling `base:::order` explicitly. It results in 20x speedup on data.table of 10 million rows with 2 integer columns, for example. To order character vectors in descending order it's sufficient to do `DT[order(x, -y)]` as opposed to `DT[order(x, -xtfrm(y))]` in base. This closes **#2405** (git [#603](https://github.com/Rdatatable/datatable/issues/603)).
     
  * `mult="all"` -vs- `mult="first"|"last"` now return consistent types and columns, **#5378** (git [#340](https://github.com/Rdatatable/datatable/issues/340)). Thanks to Michele Carriero for highlighting.

  * `duplicated.data.table` and `unique.data.table` gains `fromLast = TRUE/FALSE` argument, similar to base. Default value is FALSE. Closes **#5205** (git [#347](https://github.com/Rdatatable/datatable/issues/347)).

  * `anyDuplicated.data.table` is now implemented. Closes **#5172** (git [#350](https://github.com/Rdatatable/datatable/issues/350)). Thanks to M C (bluemagister) for reporting.

  * Complex j-expressions of the form `DT[, c(..., lapply(.SD, fun)), by=grp]`are now optimised as long as `.SD` is only present in the form `lapply(.SD, fun)`. This partially resolves **#2722** (git [#370](https://github.com/Rdatatable/datatable/issues/370)). Thanks to Sam Steingold for reporting.
```S
## example:
DT[, c(.I, lapply(.SD, sum), mean(x), lapply(.SD, log)), by=grp]
## is optimised to
DT[, list(.I, x=sum(x), y=sum(y), ..., mean(x), log(x), log(y), ...), by=grp]
## but
DT[, c(.SD, lapply(.SD, sum)), by=grp] 
## is not, yet.
```

  * `setDT` gains `keep.rownames = TRUE/FALSE` argument, which works only on `data.frame`s. TRUE retains the data.frame's row names as a new column named `rn`.

  * `rbindlist` gains `use.names` and `fill` arguments and is now implemented **entirely in C**. Closes **#5249** (git [#345](https://github.com/Rdatatable/datatable/issues/345)):
      * `use.names` by default is FALSE for backwards compatibility (does **not** bind by names by default)
      * `rbind(...)` now just calls `rbindlist()` internally, except that `use.names` is TRUE by default, for compatibility with base (and backwards compatibility).
      * `fill=FALSE` by default. If `fill=TRUE`, `use.names` has to be TRUE. 
      * At least one item of the input list has to have non-null column names.
      * Duplicate columns are bound in the order of occurrence, like base.
      * Attributes that might exist in individual items would be lost in the bound result.
      * Columns are coerced to the highest SEXPTYPE, if they are different, if/when possible.
      * And incredibly fast ;).
      * Documentation updated in much detail. Closes **#5158** (git [#333](https://github.com/Rdatatable/datatable/issues/333)).

  * The output of `tables()` now includes `NCOL`. Thanks to @dnlbrky for the suggestion.

  * `DT[, LHS := RHS]` (or its equivalent in `set`) now provides a warning and returns `DT` as it was, instead of an error, when `length(LHS) = 0L`, **#5357** (git [#343](https://github.com/Rdatatable/datatable/issues/343)). For example:
```S
DT[, grep("^b", names(DT)) := NULL] # where no columns start with b
# warns now and returns DT instead of error
```

  * GForce now is also optimised for j-expression with `.N`. Closes **#5760** (git [#334](https://github.com/Rdatatable/datatable/issues/334)).
```S
DT[, list(.N, mean(y), sum(y)), by=x] # 1.9.2 - doesn't know to use GForce - will be (relatively) slower
DT[, list(.N, mean(y), sum(y)), by=x] # 1.9.3+ - will use GForce.
```

  * `setDF` is now implemented. It accepts a data.table and converts it to data.frame by reference, **#5528** (git [#338](https://github.com/Rdatatable/datatable/issues/338)). Thanks to canneff for the discussion [here]() on data.table mailing list.

  * `.I` gets named as `I` (instead of `.I`) wherever possible, similar to `.N`, **#5290** (git [#344](https://github.com/Rdatatable/datatable/issues/344)).

#### BUG FIXES

  *  When joining to fewer columns than the key has, using one of the later key columns explicitly in j repeated the first value. A problem introduced by v1.9.2 and not caught bythe 1,220 tests, or tests in 37 dependent packages. Test added. Many thanks to Michele Carriero for reporting.
  ```R
  DT = data.table(a=1:2, b=letters[1:6], key="a,b")    # keyed by a and b
  DT[.(1), list(b,...)]    # correct result again (joining just to a not b but using b)
  ```
  *  `setkey` works again when a non-key column is type list (e.g. each cell can itself be a vector), # 5366. Test added. Thanks to James Sams, Michael Nelson and Musx for the reproducible examples.
  http://stackoverflow.com/questions/22186798/r-data-table-1-9-2-issue-on-setkey

  *  The warning "internal TRUE value has been modified" with recently released R 3.1 when grouping a table containing a logical column *and* where all groups are just 1 row is now fixed and tests added. Thanks to James Sams for the reproducible example. The warning is issued by R and we have asked if it can be upgraded to error (UPDATE: change now made for R 3.1.1 thanks to Luke Tierney).

  *  `data.table(list())`, `data.table(data.table())` and `data.table(data.frame())` now return a null data.table (no columns) rather than one empty column, # 5377. Test added. Thanks to Shubh Bansal for reporting.

  *  `unique(<NULL data.table>)` now returns a null data.table, # 5405. Thanks to agstudy for reporting.

  *  `data.table()` converted POSIXlt to POSIXct, consistent with `base:::data.frame()`, but now also provides a helpful warning instead of coercing silently, # 5321. Thanks to Brodie Gaslam, Patrick and Ragy Isaac for reporting.
      http://stackoverflow.com/questions/21487614/error-creating-r-data-table-with-date-time-posixlt
      http://stackoverflow.com/questions/21320215/converting-from-data-frame-to-data-table-i-get-an-error-with-head

  *  If another class inherits from data.table; e.g. `class(DT) == c("UserClass","data.table","data.frame")` then `DT[...]` now retains `UserClass` in the result. Thanks to Daniel Krizian for reporting, # 5296. Test added.

  *  An error `object '<name>' not found` could occur in some circumstances, particularly after a previous error. Reported with non-ASCII characters in a column name, a red herring we hope since non-ASCII characters are fully supported in data.table including in column names. Fix implemented and tests added.
     http://stackoverflow.com/questions/22128047/how-to-avoid-weird-umlaute-error-when-using-data-table

  *  Column order was reversed in some cases by `as.data.table.table()`, # 5408. Test added. Thanks to Benjamin Barnes for reporting.
     
  *  `DT[, !"missingcol", with=FALSE]` now returns `DT` (rather than a NULL data.table) with warning that "missingcol" is not present.

  *  `DT[,y := y * eval(parse(text="1*2"))]` resulted in error unless `eval()` was wrapped with paranthesis. That is, 
     `DT[,y := y * (eval(parse(text="1*2")))]`, # 5423. Thanks to Wet Feet for reporting and to Simon O'Hanlon for identifying the issue here on SO:
     http://stackoverflow.com/questions/22375404/unable-to-use-evalparse-in-data-table-function/22375557#22375557

  *  Using `by` columns with attributes (ex: factor, Date) in `j` did not retain the attributes, also in case of `:=`.
     This was partially a regression from an earlier fix (bug # 2531) due to recent changes for R3.1.0. Now fixed and 
     clearer tests added. Thanks to Christophe Dervieux for reporting and to Adam B for reporting here on SO:
     http://stackoverflow.com/questions/22536586/by-seems-to-not-retain-attribute-of-date-type-columns-in-data-table-possibl. 
     Closes # 5437.

  *  `.BY` special variable did not retain names of the grouping columns which resulted in not being able to access `.BY$grpcol` in `j`. Ex: `DT[, .BY$x, by=x]`. This is now fixed. Closes # 5415. Thanks to Stephane Vernede for the bug report.

  *  Fixed another issue with `eval(parse(...))` in `j` along with assignment by reference `:=`. Closes # 5527. Thanks to Michele Carriero for reporting. 

  *  `get()` in `j` did not see `i`'s columns when `i` is a data.table which lead to errors while doing operations 
     like: `DT1[DT2, list(get('c'))]`. Now, use of `get` makes *all* x's and i's columns visible (fetches all columns). 
     Still, as the verbose message states, using `.SDcols` or `eval(macro)` would be able to select just the columns 
     used, which is better for efficiency. Closes # 5443. Thanks to Eddi for reporting.

  *  Fixed an edge case with `unique` and `duplicated`, which on empty data.tables returned a 1-row data.table with all NAs. Closes # 5582. Thanks to Shubh Bansal for reporting.

  *  `dcast.data.table` resuled in error (because function `CJ()` was not visible) in packages that "import" data.table. This did not happen if the package "depends" on data.table. Closes bug # 5519. Thanks to K Davis for the excellent report. 

  *  `merge(x, y, all=TRUE)` error when `x` is empty data.table is now fixed. Closes # 5672. Thanks to Garrett See for filing the report.

  *  Implementing # 5249 closes bug # 5612, a case where rbind gave error when binding with empty data.tables. Thanks to Roger for reporting on SO :
  http://stackoverflow.com/q/23216033/559784

  *  Fixed a segfault during grouping with assignment by reference, ex: `DT[, LHS := RHS, by=.]`, where length(RHS) > group size (.N). Closes # 5647. Thanks to Zachary Long for reporting on datatable-help mailing list.

  *  Consistent subset rules on datat.tables with duplicate columns. In short, if indices are directly provided, 'j', or in .SDcols, then just those columns are either returned (or deleted if you provide -.SDcols or !j). If instead, column names are given and there are more than one occurrence of that column, then it's hard to decide which to keep and which to remove on a subset. Therefore, to remove, all occurrences of that column are removed, and to keep, always the first column is returned each time. Also closes # 5688 and # 5008.
     Note that using `by=` to aggregate on duplicate columns may not give intended result still, as it may not operate on the proper column.

  *  When DT is empty, DT[, newcol:=max(b), by=a] now properly adds the column, # 5376. Thanks to Shubh bansal for filing the report.

  *  When `j` evaluates to integer(0)/character(0), `DT[, j, with=FALSE]` resulted in error, # 5714. Thanks indirectly to Malcolm Cook for # 5372, through which this (recent) regression (from 1.9.3) was found.

  *  `print(DT)` now respects `digits` argument on list type columns, # 5435. Thanks to Frank for the discussion 
     on the mailing list and to Matthew Beckers for filing the bug report.

  *  FR # 2551 implemented leniance in warning messages when columns are coerced with `DT[, LHS := RHS]`, when `length(RHS)==1`.
     But this was very lenient. For ex: `DT[, a := "bla"]`, where `a` is a logical column should get a warning. This is 
     now fixed such that only very obvious cases coerces silently, ex: `DT[, a := 1]` where `a` is `integer`. Closes # 5442. 
     Thanks to Michele Carriero and John Laing for reporting.

#### NOTES

  *  Reminder: using `rolltolast` still works but since v1.9.2 now issues the following warning :
     `'rolltolast' has been marked 'deprecated' in ?data.table since v1.8.8 on CRAN 3 Mar 2013, see NEWS. Please
      change to the more flexible 'rollends' instead. 'rolltolast' will be removed in the next version."`

  *  Using `with=FALSE` with `:=` is now deprecated in all cases, given that wrapping the LHS of
     `:=` with parentheses has been preferred for some time.
     ```R
         colVar = "col1"
         DT[, colVar:=1, with=FALSE]                   # deprecated, still works silently as before
         DT[, (colVar):=1]                             # please change to this
         DT[, c("col1","col2"):=1]                     # no change
         DT[, 2:4 := 1]                                # no change
         DT[, c("col1","col2"):=list(sum(a),mean(b)]   # no change
         DT[, `:=`(...), by=...]                       # no change
     ```
     The next release will issue a warning when `with=FALSE` is used with `:=`.

  *  `?duplicated.data.table` explained that `by=NULL` or `by=FALSE` would use all columns, however `by=FALSE`
     resulted in error. `by=FALSE` is removed from help and `duplicated` returns an error when `by=TRUE/FALSE` now. 
     Closes # 5424.
     
  *  More info about distinguishing small numbers from 0.0 in v1.9.2+ is here :
       http://stackoverflow.com/questions/22290544/grouping-very-small-numbers-e-g-1e-28-and-0-0-in-data-table-v1-8-10-vs-v1-9-2

  *  `?dcast.data.table` now explains how the names are generated for the columns that are being casted. Closes # 5676.
  
  *  `dcast.data.table(dt, a ~ ... + b)` now generates the column names with values from `b` coming last. Closes # 5675.


