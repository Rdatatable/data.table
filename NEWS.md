
This NEWS file summarises the main changes.

# Changes in data.table version 1.9.3
```S
## installation
devtools:::install_github("datatable", "Rdatatable")
```

## New Features (subject to discussion and change)

  * `by=.EACHI` runs `j` for each group in `x` that each row of `i` joins to.
```S
DT[.(c("id1", "id2")), sum(val)]                # single total across both id1 and id2
DT[.(c("id1", "id2")), sum(val), by = .EACHI]   # sum(val) for each id
DT[.(c("id1", "id2")), sum(val), by = key(DT)]  # same
```
  In other words, `by-without-by` is now explicit, for clarity and consistency, #2696 (git [#371](https://github.com/Rdatatable/datatable/issues/371)). NOTE : when `i` contains duplicates, `by=.EACHI` is different to `by=key(DT)`; e.g,
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

  * `bit64::integer64` now works in grouping and joins, #5369 (git [#342](https://github.com/Rdatatable/datatable/issues/342)). Thanks to James Sams for highlighting UPCs and Clayton Stanley for [this SO post](http://stackoverflow.com/questions/22273321/large-integers-in-data-table-grouping-results-different-in-1-9-2-compared-to-1). **Reminder:** `fread()` has been able to detect and read `integer64` for a while. 

  * `setNumericRounding()` may be used to reduce to 1 byte or 0 byte rounding when joining to or grouping columns of type 'numeric', #5369 (git [#342](https://github.com/Rdatatable/datatable/issues/342)). See example in `?setNumericRounding` and NEWS item below for v1.9.2. `getNumericRounding()` returns the current setting.
     
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

  * `DT[order(x, -y)]` is now by default optimised to use data.table's internal fast order as `DT[forder(DT, x, -y)]`. It can be turned off by setting `datatable.optimize` to < 1L or just calling `base:::order` explicitly. It results in 20x speedup on data.table of 10 million rows with 2 integer columns, for example. To order character vectors in descending order it's sufficient to do `DT[order(x, -y)]` as opposed to `DT[order(x, -xtfrm(y))]` in base. This closes #2405 (git [#603](https://github.com/Rdatatable/datatable/issues/603)).
     
  * `mult="all"` -vs- `mult="first"|"last"` now return consistent types and columns, #5378 (git [#340](https://github.com/Rdatatable/datatable/issues/340)). Thanks to Michele Carriero for highlighting.

  * `duplicated.data.table` and `unique.data.table` gains `fromLast = TRUE/FALSE` argument, similar to base. Default value is FALSE. Closes #5205 (git [#347](https://github.com/Rdatatable/datatable/issues/347)).

  * `anyDuplicated.data.table` is now implemented. Closes #5172 (git [#350](https://github.com/Rdatatable/datatable/issues/350)). Thanks to M C (bluemagister) for reporting.

  * Complex j-expressions of the form `DT[, c(..., lapply(.SD, fun)), by=grp]`are now optimised as long as `.SD` is only present in the form `lapply(.SD, fun)`. This partially resolves #2722 (git [#370](https://github.com/Rdatatable/datatable/issues/370)). Thanks to Sam Steingold for reporting.
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

  * `rbindlist` gains `use.names` and `fill` arguments and is now implemented **entirely in C**. Closes #5249 (git [#345](https://github.com/Rdatatable/datatable/issues/345)):
      * `use.names` by default is FALSE for backwards compatibility (does **not** bind by names by default)
      * `rbind(...)` now just calls `rbindlist()` internally, except that `use.names` is TRUE by default, for compatibility with base (and backwards compatibility).
      * `fill=FALSE` by default. If `fill=TRUE`, `use.names` has to be TRUE. 
      * At least one item of the input list has to have non-null column names.
      * Duplicate columns are bound in the order of occurrence, like base.
      * Attributes that might exist in individual items would be lost in the bound result.
      * Columns are coerced to the highest SEXPTYPE, if they are different, if/when possible.
      * And incredibly fast ;).
      * Documentation updated in much detail. Closes #5158 (git [#333](https://github.com/Rdatatable/datatable/issues/333)).

  * The output of `tables()` now includes `NCOL`. Thanks to @dnlbrky for the suggestion.

  * `DT[, LHS := RHS]` (or its equivalent in `set`) now provides a warning and returns `DT` as it was, instead of an error, when `length(LHS) = 0L`, #5357 (git [#343](https://github.com/Rdatatable/datatable/issues/343)). For example:
```S
DT[, grep("^b", names(DT)) := NULL] # where no columns start with b
# warns now and returns DT instead of error
```

  * GForce now is also optimised for j-expression with `.N`. Closes #5760 (git [#334](https://github.com/Rdatatable/datatable/issues/334)).
```S
DT[, list(.N, mean(y), sum(y)), by=x] # 1.9.2 - doesn't know to use GForce - will be (relatively) slower
DT[, list(.N, mean(y), sum(y)), by=x] # 1.9.3+ - will use GForce.
```

  * `setDF` is now implemented. It accepts a data.table and converts it to data.frame by reference, #5528 (git [#338](https://github.com/Rdatatable/datatable/issues/338)). Thanks to canneff for the discussion [here]() on data.table mailing list.

  * `.I` gets named as `I` (instead of `.I`) wherever possible, similar to `.N`, #5290 (git [#344](https://github.com/Rdatatable/datatable/issues/344)).

## BUG FIXES
