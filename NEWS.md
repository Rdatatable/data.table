
This NEWS file summarises the main changes.

# Changes in data.table version 1.9.3
```S
## installation
devtools:::install_github("datatable", "Rdatatable")
```

## New Features (subject to discussion and change)

  * `by=.EACHI` runs `j` for each group in `x` that each row of `i` joins to.
```S
DT[.(c("id1","id2")), sum(val)]              # single total across both id1 and id2
DT[.(c("id1","id2")), sum(val), by=.EACHI]   # sum(val) for each id
DT[.(c("id1","id2")), sum(val), by=key(DT)]  # same
```
  In other words, `by-without-by` is now explicit, for clarity and consistency, [#2696](https://r-forge.r-project.org/tracker/index.php?func=detail&aid=2696&group_id=240&atid=978) (git #371). 

> NOTE : when `i` contains duplicates, `by=.EACHI` is different to `by=key(DT)`; e.g,

```S
setkey(DT, ID)
ids = c("id1","id2,"id1")
DT[ids, sum(val), by=ID]       #  2 rows returned
DT[ids, sum(val), by=.EACHI]   #  3 rows, in the order of ids (result 1 and 3 separate)
```
  `by=.EACHI` can be useful when `i` is event data, where you don't want the events
  aggregated by common join values but wish the output to be ordered with repeats, or 
  simply just using join inherited columns as parameters; e.g.;
```S
X[Y, head(.SD, i.top), by=.EACHI]
```
  where 'top' is a non-join column in `Y`; i.e. join inherited column. Thanks to many, especially eddi, Sadao Milberg and Gabor Grothendieck for extended discussions.

  * Accordingly, `X[Y,j]` now does what `X[Y][,j]` did. A "classic" option to restore the previous default behaviour is to be dicussed and confirmed. See [this](http://r.789695.n4.nabble.com/changing-data-table-by-without-by-syntax-to-require-a-quot-by-quot-td4664770.html), [this](http://stackoverflow.com/questions/16093289/data-table-join-and-j-expression-unexpected-behavior) and [this](http://stackoverflow.com/a/16222108/403310) post for discussions. 

  * `bit64::integer64` now works in grouping and joins, [#5369](https://r-forge.r-project.org/tracker/index.php?func=detail&aid=5369&group_id=240&atid=978) (git #342). Thanks to James Sams for highlighting UPCs and Clayton Stanley for [this SO post](http://stackoverflow.com/questions/22273321/large-integers-in-data-table-grouping-results-different-in-1-9-2-compared-to-1). **Reminder:** `fread()` has been able to detect and read `integer64` for a while. 

  * `setNumericRounding()` may be used to reduce to 1 byte or 0 byte rounding when joining to or grouping columns of type 'numeric', [#5369](https://r-forge.r-project.org/tracker/index.php?func=detail&aid=5369&group_id=240&atid=978) (git #342). See example in `?setNumericRounding` and NEWS item below for v1.9.2. `getNumericRounding()` returns the current setting.

