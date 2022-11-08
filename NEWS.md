**If you are viewing this file on CRAN, please check [latest news on GitHub](https://github.com/Rdatatable/data.table/blob/master/NEWS.md) where the formatting is also better.**

# data.table [v1.14.6](https://github.com/Rdatatable/data.table/milestone/27?closed=1)

## BUG FIXES

1. `fread()` could leak memory, [#3292](https://github.com/Rdatatable/data.table/issues/3292). Thanks to @patrickhowerter for reporting, and Jim Hester for the fix. The fix requires R 3.4.0 or later. Loading `data.table` in earlier versions now highlights this issue on startup, asks users to upgrade R, and warns that we intend to upgrade `data.table`'s dependency from 8 year old R 3.1.0 (April 2014) to 5 year old R 3.4.0 (April 2017).

## NOTES

1. Test 1962.098 has been modified to pass latest changes to `POSIXt` in R-devel.

2. `test.data.table()` no longer creates `DT` in `.GlobalEnv`, a CRAN policy violation, [#5514](https://github.com/Rdatatable/data.table/issues/5514). No other writes occurred to `.GlobalEnv` and release procedures have been improved to prevent this happening again.

3. The memory usage of the test suite has been halved, [#5507](https://github.com/Rdatatable/data.table/issues/5507).


# data.table [v1.14.4](https://github.com/Rdatatable/data.table/milestone/26?closed=1)  (17 Oct 2022)

## NOTES

1. gcc 12.1 (May 2022) now detects and warns about an always-false condition (`-Waddress`) in `fread` which caused a small efficiency saving never to be invoked, [#5476](https://github.com/Rdatatable/data.table/pull/5476). Thanks to CRAN for testing latest versions of compilers.

2. `update.dev.pkg()` has been renamed `update_dev_pkg()` to get out of the way of the `stats::update` generic function, [#5421](https://github.com/Rdatatable/data.table/pull/5421). This is a utility function which upgrades the version of `data.table` to the latest commit in development which has passed all tests. As such we don't expect any backwards compatibility concerns. Its manual page was causing an intermittent hang/crash from `R CMD check` on Windows-only on CRAN which we hope will be worked around by changing its name.

3. Internal C code now passes `-Wstrict-prototypes` to satisfy the warnings now displayed on CRAN, [#5477](https://github.com/Rdatatable/data.table/pull/5477).

4. `write.csv` in R-devel no longer responds to `getOption("digits.secs")` for `POSIXct`, [#5478](https://github.com/Rdatatable/data.table/issues/5478). This caused our tests of `fwrite(, dateTimeAs="write.csv")` to fail on CRAN's daily checks using latest daily R-devel. While R-devel discussion continues, and currently it seems like the change is intended with further changes possible, this `data.table` release massages our tests to pass on latest R-devel. The idea is to try to get out of the way of R-devel changes in this regard until the new behavior of `write.csv` is released and confirmed. Package updates are not accepted on CRAN if they do not pass the latest daily version of R-devel, even if R-devel changes after the package update is submitted. If the change to `write.csv()` stands, then a future release of `data.table` will be needed to make `fwrite(, dateTimeAs="write.csv")` match `write.csv()` output again in that future version of R onwards. If you use an older version of `data.table` than said future one in the said future version of R, then `fwrite(, dateTimeAs="write.csv")` may not match `write.csv()` if you are using `getOption("digits.secs")` too. However, you can always check that your installation of `data.table` works in your version of R on your platform by simply running `test.data.table()` yourself. Doing so would detect such a situation for you: test 1741 would fail in this case. `test.data.table()` runs the entire suite of tests and is always available to you locally. This way you do not need to rely on our statements about which combinations of versions of R and `data.table` on which platforms we have tested and support; just run `test.data.table()` yourself. Having said that, because test 1741 has been relaxed in this release in order to be accepted on CRAN to pass latest R-devel, this won't be true for this particular release in regard to this particular test.

    ```R
    $ R --vanilla
    R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
    > DF = data.frame(A=as.POSIXct("2022-10-01 01:23:45.012"))
    > options(digits.secs=0)
    > write.csv(DF)
    "","A"
    "1",2022-10-01 01:23:45
    > options(digits.secs=3)
    > write.csv(DF)
    "","A"
    "1",2022-10-01 01:23:45.012
    
    $ Rdevel --vanilla
    R Under development (unstable) (2022-10-06 r83040) -- "Unsuffered Consequences"
    > DF = data.frame(A=as.POSIXct("2022-10-01 01:23:45.012"))
    > options(digits.secs=0)
    > write.csv(DF)
    "","A"
    "1",2022-10-01 01:23:45.012
    ```

5. Many thanks to Kurt Hornik for investigating potential impact of a possible future change to `base::intersect()` on empty input, providing a patch so that `data.table` won't break if the change is made to R, and giving us plenty of notice, [#5183](https://github.com/Rdatatable/data.table/pull/5183).

6. `datatable.[dll|so]` has changed name to `data_table.[dll|so]`, [#4442](https://github.com/Rdatatable/data.table/pull/4442). Thanks to Jan Gorecki for the PR. We had previously removed the `.` since `.` is not allowed by the following paragraph in the Writing-R-Extensions manual. Replacing `.` with `_` instead now seems more consistent with the last sentence.

    > ... the basename of the DLL needs to be both a valid file name and valid as part of a C entry point (e.g. it cannot contain ‘.’): for portable code it is best to confine DLL names to be ASCII alphanumeric plus underscore. If entry point R_init_lib is not found it is also looked for with ‘.’ replaced by ‘_’.


# data.table [v1.14.2](https://github.com/Rdatatable/data.table/milestone/24?closed=1)  (27 Sep 2021)

## NOTES

1. clang 13.0.0 (Sep 2021) requires the system header `omp.h` to be included before R's headers, [#5122](https://github.com/Rdatatable/data.table/issues/5122). Many thanks to Prof Ripley for testing and providing a patch file.


# data.table [v1.14.0](https://github.com/Rdatatable/data.table/milestone/23?closed=1)  (21 Feb 2021)

## POTENTIALLY BREAKING CHANGES

1. In v1.13.0 (July 2020) native parsing of datetime was added to `fread` by Michael Chirico which dramatically improved performance. Before then datetime was read as type character by default which was slow. Since v1.13.0, UTC-marked datetime (e.g. `2020-07-24T10:11:12.134Z` where the final `Z` is present) has been read automatically as POSIXct and quickly. We provided the migration option `datatable.old.fread.datetime.character` to revert to the previous slow character behavior. We also added the `tz=` argument to control unmarked datetime; i.e. where the `Z` (or equivalent UTC postfix) is missing in the data. The default `tz=""` reads unmarked datetime as character as before, slowly. We gave you the ability to set `tz="UTC"` to turn on the new behavior and read unmarked datetime as UTC, quickly. R sessions that are running in UTC by setting the TZ environment variable, as is good practice and common in production, have also been reading unmarked datetime as UTC since v1.13.0, much faster. Note 1 of v1.13.0 (below in this file) ended `In addition to convenience, fread is now significantly faster in the presence of dates, UTC-marked datetimes, and unmarked datetime when tz="UTC" is provided.`.

    At `rstudio::global(2021)`, Neal Richardson, Director of Engineering at Ursa Labs, compared Arrow CSV performance to `data.table` CSV performance, [Bigger Data With Ease Using Apache Arrow](https://www.rstudio.com/resources/rstudioglobal-2021/bigger-data-with-ease-using-apache-arrow/). He opened by comparing to `data.table` as his main point. Arrow was presented as 3 times faster than `data.table`. He talked at length about this result. However, no reproducible code was provided and we were not contacted in advance in case we had any comments. He mentioned New York Taxi data in his talk which is a dataset known to us as containing unmarked datetime. [Rebuttal](https://twitter.com/MattDowle/status/1360073970498875394).

    `tz=`'s default is now changed from `""` to `"UTC"`. If you have been using `tz=` explicitly then there should be no change. The change to read UTC-marked datetime as POSIXct rather than character already happened in v1.13.0. The change now is that unmarked datetimes are now read as UTC too by default without needing to set `tz="UTC"`. None of the 1,017 CRAN packages directly using `data.table` are affected. As before, the migration option `datatable.old.fread.datetime.character` can still be set to TRUE to revert to the old character behavior. This migration option is temporary and will be removed in the near future.

    The community was consulted in [this tweet](https://twitter.com/MattDowle/status/1358011599336931328) before release.

## BUG FIXES

1. If `fread()` discards a single line footer, the warning message which includes the discarded text now displays any non-ASCII characters correctly on Windows, [#4747](https://github.com/Rdatatable/data.table/issues/4747). Thanks to @shrektan for reporting and the PR.

2. `fintersect()` now retains the order of the first argument as reasonably expected, rather than retaining the order of the second argument, [#4716](https://github.com/Rdatatable/data.table/issues/4716). Thanks to Michel Lang for reporting, and Ben Schwen for the PR.

## NOTES

1. Compiling from source no longer requires `zlib` header files to be available, [#4844](https://github.com/Rdatatable/data.table/pull/4844). The output suggests installing `zlib` headers, and how (e.g. `zlib1g-dev` on Ubuntu) as before, but now proceeds with `gzip` compression disabled in `fwrite`. Upon calling `fwrite(DT, "file.csv.gz")` at runtime, an error message suggests to reinstall `data.table` with `zlib` headers available. This does not apply to users on Windows or Mac who install the pre-compiled binary package from CRAN.

2. `r-datatable.com` continues to be the short, canonical and long-standing URL which forwards to the current homepage. The homepage domain has changed a few times over the years but those using `r-datatable.com` did not need to change their links. For example, we use `r-datatable.com` in messages (and translated messages) in preference to the word 'homepage' to save users time in searching for the current homepage. The web forwarding was provided by Domain Monster but they do not support `https://r-datatable.com`, only `http://r-datatable.com`, despite the homepage being forwarded to being `https:` for many years. Meanwhile, CRAN submission checks now require all URLs to be `https:`, rejecting `http:`. Therefore we have moved to [gandi.net](https://www.gandi.net) who do support `https:` web forwarding and so [https://r-datatable.com](https://r-datatable.com) now forwards correctly. Thanks to Dirk Eddelbuettel for suggesting Gandi. Further, Gandi allows the web-forward to be marked 301 (permanent) or 302 (temporary). Since the very point of `https://r-datatable.com` is to be a forward, 302 is appropriate in this case. This enables us to link to it in DESCRIPTION, README, and this NEWS item. Otherwise, CRAN submission checks would require the 301 forward to be followed; i.e. the forward replaced with where it points to and the package resubmitted. Thanks to Uwe Ligges for explaining this distinction.


# data.table [v1.13.6](https://github.com/Rdatatable/data.table/milestone/22?closed=1)  (30 Dec 2020)

## BUG FIXES

1. Grouping could throw an error `Failed to allocate counts or TMP` with more than 1e9 rows even with sufficient RAM due to an integer overflow, [#4295](https://github.com/Rdatatable/data.table/issues/4295) [#4818](https://github.com/Rdatatable/data.table/issues/4818). Thanks to @renkun-ken and @jangorecki for reporting, and @shrektan for fixing.

2. `fwrite()`'s mutithreaded `gzip` compression failed on Solaris with Z_STREAM_ERROR, [#4099](https://github.com/Rdatatable/data.table/issues/4099). Since this feature was released in Oct 2019 (see item 3 in v1.12.4 below in this news file) there have been no known problems with it on Linux, Windows or Mac. For Solaris, we have been successively adding more and more detailed tracing to the output in each release, culminating in tracing `zlib` internals at byte level by reading `zlib`'s source. The problem did not manifest itself on [R-hub](https://builder.r-hub.io/)'s Solaris instances, so we had to work via CRAN output. If `zlib`'s `z_stream` structure is declared inside a parallel region but before a parallel for, it appears that the particular OpenMP implementation used by CRAN's Solaris moves the structure to a new address on entering the parallel for. Ordinarily this memory move would not matter, however, `zlib` internals have a self reference pointer to the parent, and check that the pointers match. This mismatch caused the -2 (Z_STREAM_ERROR). Allocating an array of structures, one for each thread, before the parallel region avoids the memory move with no cost.

    It should be carefully noted that we cannot be sure it really is a problem unique to CRAN's Solaris. Even if it seems that way after one year of observations. For example, it could be compiler flags, or particular memory circumstances, either of which could occur on other operating systems too. However, we are unaware of why it would make sense for the OpenMP implementation to move the structure at that point. Any optimizations such as aligning the set of structures to cache line boundaries could be performed at the start of the parallel region, not after the parallel for. If anyone reading this knows more, please let us know. 

## NOTES

1. The last release took place at the same time as several breaking changes were made to R-devel. The CRAN submissions process runs against latest daily R-devel so we had to keep up with those latest changes by making several resubmissions. Then each resubmission reruns against the new latest R-devel again. Overall it took 7 days. For example, we added the new `environments=FALSE` to our `all.equal` call. Then about 4 hours after 1.13.4 was accepted, the `s` was dropped and we now need to resubmit with `environment=FALSE`. In any case, we have suggested that the default should be FALSE first to give packages some notice, as opposed to generating errors in the CRAN submissions process within hours. Then the default for `environment=` could be TRUE in 6 months time after packages have had some time to update in advance of the default change. Readers of this NEWS file will be familiar with `data.table`'s approach to change control and know that we do this ourselves.


# data.table [v1.13.4](https://github.com/Rdatatable/data.table/milestone/21?closed=1)  (08 Dec 2020)

## BUG FIXES

1. `as.matrix(<empty DT>)` now retains the column type for the empty matrix result, [#4762](https://github.com/Rdatatable/data.table/issues/4762). Thus, for example, `min(DT[0])` where DT's columns are numeric, is now consistent with non-empty all-NA input and returns `Inf` with R's warning `no non-missing arguments to min; returning Inf` rather than R's error `only defined on a data frame with all numeric[-alike] variables`. Thanks to @mb706 for reporting.

2. `fsort()` could crash when compiled using `clang-11` (Oct 2020), [#4786](https://github.com/Rdatatable/data.table/issues/4786). Multithreaded debugging revealed that threads are no longer assigned iterations monotonically by the dynamic schedule. Although never guaranteed by the OpenMP standard, in practice monotonicity could be relied on as far as we knew, until now. We rely on monotonicity in the `fsort` implementation. Happily, a schedule modifier `monotonic:dynamic` was added in OpenMP 4.5 (Nov 2015) which we now use if available (e.g. gcc 6+, clang 3.9+). If you have an old compiler which does not support OpenMP 4.5, it's probably the case that the unmodified dynamic schedule is monotonic anyway, so `fsort` now checks that threads are receiving iterations monotonically and emits a graceful error if not. It may be that `clang` prior to version 11, and `gcc` too, exhibit the same crash. It was just that `clang-11` was the first report. To know which version of OpenMP `data.table` is using, `getDTthreads(verbose=TRUE)` now reports the `YYYYMM` value `_OPENMP`; e.g. 201511 corresponds to v4.5, and 201811 corresponds to v5.0. Oddly, the `x.y` version number is not provided by the OpenMP API. OpenMP 4.5 may be enabled in some compilers using `-fopenmp-version=45`. Otherwise, if you need to upgrade compiler, https://www.openmp.org/resources/openmp-compilers-tools/ may be helpful.

3. Columns containing functions that don't inherit the class `'function'` would fail to group, [#4814](https://github.com/Rdatatable/data.table/issues/4814). Thanks @mb706 for reporting, @ecoRoland2 for helping investigate, and @Coorsaa for a follow-up example involving environments.

## NOTES

1. Continuous daily testing by CRAN using latest daily R-devel revealed, within one day of the change to R-devel, that a future version of R would break one of our tests, [#4769](https://github.com/Rdatatable/data.table/issues/4769). The characters "-alike" were added into one of R's error messages, so our too-strict test which expected the error `only defined on a data frame with all numeric variables` will fail when it sees the new error message `only defined on a data frame with all numeric-alike variables`. We have relaxed the pattern the test looks for to `data.*frame.*numeric` well in advance of the future version of R being released. Readers are reminded that CRAN is not just a host for packages. It is also a giant test suite for R-devel. For more information, [behind the scenes of cran, 2016](https://h2o.ai/blog/behind-the-scenes-of-cran/).

2. `as.Date.IDate` is no longer exported as a function to solve a new error in R-devel `S3 method lookup found 'as.Date.IDate' on search path`, [#4777](https://github.com/Rdatatable/data.table/issues/4777). The S3 method is still exported; i.e. `as.Date(x)` will still invoke the `as.Date.IDate` method when `x` is class `IDate`. The function had been exported, in addition to exporting the method, to solve a compatibility issue with `zoo` (and `xts` which uses `zoo`) because `zoo` exports `as.Date` which masks `base::as.Date`. Happily, since zoo 1.8-1 (Jan 2018) made a change to its `as.IDate`, the workaround is no longer needed.

3. Thanks to @fredguinog for testing `fcase` in development before 1.13.0 was released and finding a segfault, [#4378](https://github.com/Rdatatable/data.table/issues/4378). It was found separately by the `rchk` tool (which uses static code analysis) in release procedures and fixed before `fcase` was released, but the reproducible example has now been added to the test suite for completeness. Thanks also to @shrektan for investigating, proposing a very similar fix at C level, and a different reproducible example which has also been added to the test suite.


# data.table [v1.13.2](https://github.com/Rdatatable/data.table/milestone/19?closed=1)  (19 Oct 2020)

## BUG FIXES

1. `test.data.table()` could fail the 2nd time it is run by a user in the same R session on Windows due to not resetting locale properly after testing Chinese translation, [#4630](https://github.com/Rdatatable/data.table/pull/4630). Thanks to Cole Miller for investigating and fixing.

2. A regression in v1.13.0 resulted in installation on Mac often failing with `shared object 'datatable.so' not found`, and FreeBSD always failing with `expr: illegal option -- l`, [#4652](https://github.com/Rdatatable/data.table/issues/4652) [#4640](https://github.com/Rdatatable/data.table/issues/4640) [#4650](https://github.com/Rdatatable/data.table/issues/4650). Thanks to many for assistance including Simon Urbanek, Brian Ripley, Wes Morgan, and @ale07alvarez. There were no installation problems on Windows or Linux.

3. Operating on columns of type `list`, e.g. `dt[, listCol[[1]], by=id]`, suffered a performance regression in v1.13.0, [#4646](https://github.com/Rdatatable/data.table/issues/4646) [#4658](https://github.com/Rdatatable/data.table/issues/4658). Thanks to @fabiocs8 and @sandoronodi for the detailed reports, and to Cole Miller for substantial debugging, investigation and proposals at C level which enabled the root cause to be fixed. Related, and also fixed, was a segfault revealed by package POUMM, [#4746](https://github.com/Rdatatable/data.table/issues/4746), when grouping a list column where each item has an attribute; e.g., `coda::mcmc.list`. Detected thanks to CRAN's ASAN checks, and thanks to Venelin Mitov for assistance in tracing the memory fault. Thanks also to Hongyuan Jia and @ben-schwen for assistance in debugging the fix in dev to pass reverse dependency testing which highlighted, before release, that package `eplusr` would fail. Its good usage has been added to `data.table`'s test suite.

4. `fread("1.2\n", colClasses='integer')` (note no columns names in the data) would segfault when creating a warning message, [#4644](https://github.com/Rdatatable/data.table/issues/4644). It now warns with `Attempt to override column 1 of inherent type 'float64' down to 'int32' ignored.` When column names are present however, the warning message includes the name as before; i.e., `fread("A\n1.2\n", colClasses='integer')` produces `Attempt to override column 1 <<A>> of inherent type 'float64' down to 'int32' ignored.`. Thanks to Kun Ren for reporting.

5. `dplyr::mutate(setDT(as.list(1:64)), V1=11)` threw error `can't set ALTREP truelength`, [#4734](https://github.com/Rdatatable/data.table/issues/4734). Thanks to @etryn for the reproducible example, and to Cole Miller for refinements.

## NOTES

1. `bit64` v4.0.2 and `bit` v4.0.3, both released on 30th July, correctly broke `data.table`'s tests. Like other packages on our `Suggest` list, we check `data.table` works with `bit64` in our tests. The first break was because `all.equal` always returned `TRUE` in previous versions of `bit64`. Now that `all.equal` works for `integer64`, the incorrect test comparison was revealed. If you use `bit64`, or `nanotime` which uses `bit64`, it is highly recommended to upgrade to the latest `bit64` version. Thanks to Cole Miller for the PR to accommodate `bit64`'s update.

    The second break caused by `bit` was the addition of a `copy` function. We did not ask, but the `bit` package kindly offered to change to a different name since `data.table::copy` is long standing. `bit` v4.0.4 released 4th August renamed `copy` to `copy_vector`. Otherwise, users of `data.table` would have needed to prefix every occurrence of `copy` with `data.table::copy` if they use `bit64` too, since `bit64` depends on (rather than importing) `bit`. Again, this impacted `data.table`'s tests which mimic a user's environment; not `data.table` itself per se.
    
    We have requested that CRAN policy be modified to require that reverse dependency testing include packages which `Suggest` the package. Had this been the case, reverse dependency testing of `bit64` would have caught the impact on `data.table` before release.

2. `?.NGRP` now displays the help page as intended, [#4946](https://github.com/Rdatatable/data.table/issues/4649). Thanks to @KyleHaynes for posting the issue, and Cole Miller for the fix. `.NGRP` is a symbol new in v1.13.0; see below in this file.

3. `test.data.table()` failed in non-English locales such as `LC_TIME=fr_FR.UTF-8` due to `Jan` vs `janv.` in tests 168 and 2042, [#3450](https://github.com/Rdatatable/data.table/issues/3450). Thanks to @shrektan for reporting, and @tdhock for making the tests locale-aware.

4. User-supplied `PKG_LIBS` and `PKG_CFLAGS` are now retained and the suggestion in https://mac.r-project.org/openmp/; i.e.,
    `PKG_CPPFLAGS='-Xclang -fopenmp' PKG_LIBS=-lomp R CMD INSTALL data.table_<ver>.tar.gz`
has a better chance of working on Mac.


# data.table [v1.13.0](https://github.com/Rdatatable/data.table/milestone/17?closed=1)  (24 Jul 2020)

## POTENTIALLY BREAKING CHANGES

1. `fread` now supports native parsing of `%Y-%m-%d`, and [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) `%Y-%m-%dT%H:%M:%OS%z`, [#4464](https://github.com/Rdatatable/data.table/pull/4464). Dates are returned as `data.table`'s `integer`-backed `IDate` class (see `?IDate`), and datetimes are returned as `POSIXct` provided either `Z` or the offset from `UTC` is present; e.g. `fwrite()` outputs UTC by default including the final `Z`. Reminder that `IDate` inherits from R's `Date` and is identical other than it uses the `integer` type where (oddly) R uses the `double` type for dates (8 bytes instead of 4). `fread()` gains a `tz` argument to control datetime values that are missing a Z or UTC-offset (now referred to as *unmarked* datetimes); e.g. as written by `write.csv`. By default `tz=""` means, as in R, read the unmarked datetime in local time. Unless the timezone of the R session is UTC (e.g. the TZ environment variable is set to `"UTC"`, or `""` on non-Windows), unmarked datetime will then by read by `fread` as character, as before. If you have been using `colClasses="POSIXct"` that will still work using R's `as.POSIXct()` which will interpret the unmarked datetime in local time, as before, and still slowly. You can tell `fread` to read unmarked datetime as UTC, and quickly, by passing `tz="UTC"` which may be appropriate in many circumstances. Note that the default behaviour of R to read and write csv using unmarked datetime can lead to different research results when the csv file has been saved in one timezone and read in another due to observations being shifted to a different date. If you have been using `colClasses="POSIXct"` for UTC-marked datetime (e.g. as written by `fwrite` including the final `Z`) then it will automatically speed up with no changes needed.

    Since this is a potentially breaking change, i.e. existing code may depend on dates and datetimes being read as type character as before, a temporary option is provided to restore the old behaviour: `options(datatable.old.fread.datetime.character=TRUE)`. However, in most cases, we expect existing code to still work with no changes.
    
    The minor version number is bumped from 12 to 13, i.e. `v1.13.0`, where the `.0` conveys 'be-aware' as is common practice. As with any new feature, there may be bugs to fix and changes to defaults required in future. In addition to convenience, `fread` is now significantly faster in the presence of dates, UTC-marked datetimes, and unmarked datetime when tz="UTC" is provided.

## NEW FEATURES

1. `%chin%` and `chmatch(x, table)` are faster when `x` is length 1, `table` is long, and `x` occurs near the start of `table`. Thanks to Michael Chirico for the suggestion, [#4117](https://github.com/Rdatatable/data.table/pull/4117#discussion_r358378409).

2. `CsubsetDT` C function is now exported for use by other packages, [#3751](https://github.com/Rdatatable/data.table/issues/3751). Thanks to Leonardo Silvestri for the request and the PR. This uses R's `R_RegisterCCallable` and `R_GetCCallable` mechanism, [R-exts§5.4.3](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Linking-to-native-routines-in-other-packages) and [`?cdt`](https://rdatatable.gitlab.io/data.table/reference/cdt.html). Note that organization of our C interface will be changed in future.

3. `print` method for `data.table` gains `trunc.cols` argument (and corresponding option `datatable.print.trunc.cols`, default `FALSE`), [#1497](https://github.com/Rdatatable/data.table/issues/1497), part of [#1523](https://github.com/Rdatatable/data.table/issues/1523). This prints only as many columns as fit in the console without wrapping to new lines (e.g., the first 5 of 80 columns) and a message that states the count and names of the variables not shown. When `class=TRUE` the message also contains the classes of the variables. `data.table` has always automatically truncated _rows_ of a table for efficiency (e.g. printing 10 rows instead of 10 million); in the future, we may do the same for _columns_ (e.g., 10 columns instead of 20,000) by changing the default for this argument. Thanks to @nverno for the initial suggestion and to @TysonStanley for the PR.

4. `setnames(DT, new=new_names)` (i.e. explicitly named `new=` argument) now works as expected rather than an error message requesting that `old=` be supplied too, [#4041](https://github.com/Rdatatable/data.table/issues/4041). Thanks @Kodiologist for the suggestion.

5. `nafill` and `setnafill` gain `nan` argument to say whether `NaN` should be considered the same as `NA` for filling purposes, [#4020](https://github.com/Rdatatable/data.table/issues/4020). Prior versions had an implicit value of `nan=NaN`; the default is now `nan=NA`, i.e., `NaN` is treated as if it's missing. Thanks @AnonymousBoba for the suggestion. Also, while `nafill` still respects `getOption('datatable.verbose')`, the `verbose` argument has been removed.

6. New function `fcase(...,default)` implemented in C by Morgan Jacob, [#3823](https://github.com/Rdatatable/data.table/issues/3823), is inspired by SQL `CASE WHEN` which is a common tool in SQL for e.g. building labels or cutting age groups based on conditions. `fcase` is comparable to R function `dplyr::case_when` however it evaluates its arguments in a lazy way (i.e. only when needed) as shown below. Please see `?fcase` for more details.

    ```R
    # Lazy evaluation
    x = 1:10
    data.table::fcase(
	    x < 5L, 1L,
	    x >= 5L, 3L,
	    x == 5L, stop("provided value is an unexpected one!")
    )
    # [1] 1 1 1 1 3 3 3 3 3 3

    dplyr::case_when(
	    x < 5L ~ 1L,
	    x >= 5L ~ 3L,
	    x == 5L ~ stop("provided value is an unexpected one!")
    )
    # Error in eval_tidy(pair$rhs, env = default_env) :
    #  provided value is an unexpected one!

    # Benchmark
    x = sample(1:100, 3e7, replace = TRUE) # 114 MB
    microbenchmark::microbenchmark(
    dplyr::case_when(
      x < 10L ~ 0L,
      x < 20L ~ 10L,
      x < 30L ~ 20L,
      x < 40L ~ 30L,
      x < 50L ~ 40L,
      x < 60L ~ 50L,
      x > 60L ~ 60L
    ),
    data.table::fcase(
      x < 10L, 0L,
      x < 20L, 10L,
      x < 30L, 20L,
      x < 40L, 30L,
      x < 50L, 40L,
      x < 60L, 50L,
      x > 60L, 60L
    ),
    times = 5L,
    unit = "s")
    # Unit: seconds
    #               expr   min    lq  mean   median    uq    max neval
    # dplyr::case_when   11.57 11.71 12.22    11.82 12.00  14.02     5
    # data.table::fcase   1.49  1.55  1.67     1.71  1.73   1.86     5
    ```

7. `.SDcols=is.numeric` now works; i.e., `SDcols=` accepts a function which is used to select the columns of `.SD`, [#3950](https://github.com/Rdatatable/data.table/issues/3950). Any function (even _ad hoc_) that returns scalar `TRUE`/`FALSE` for each column will do; e.g., `.SDcols=!is.character` will return _non_-character columns (_a la_ `Negate()`). Note that `.SDcols=patterns(...)` can still be used for filtering based on the column names.

8. Compiler support for OpenMP is now detected during installation, which allows `data.table` to compile from source (in single threaded mode) on macOS which, frustratingly, does not include OpenMP support by default, [#2161](https://github.com/Rdatatable/data.table/issues/2161), unlike Windows and Linux. A helpful message is emitted during installation from source, and on package startup as before. Many thanks to @jimhester for the PR.

9. `rbindlist` now supports columns of type `expression`, [#546](https://github.com/Rdatatable/data.table/issues/546). Thanks @jangorecki for the report.

10. The dimensions of objects in a `list` column are now displayed, [#3671](https://github.com/Rdatatable/data.table/issues/3671). Thanks to @randomgambit for the request, and Tyson Barrett for the PR.

11. `frank` gains `ties.method='last'`, paralleling the same in `base::order` which has been available since R 3.3.0 (April 2016), [#1689](https://github.com/Rdatatable/data.table/issues/1689). Thanks @abudis for the encouragement to accommodate this.

12. The `keep.rownames` argument in `as.data.table.xts` now accepts a string, which can be used for specifying the column name of the index of the xts input, [#4232](https://github.com/Rdatatable/data.table/issues/4232). Thanks to @shrektan for the request and the PR.

13. New symbol `.NGRP` available in `j`, [#1206](https://github.com/Rdatatable/data.table/issues/1206). `.GRP` (the group number) was already available taking values from `1` to `.NGRP`. The number of groups, `.NGRP`, might be useful in `j` to calculate a percentage of groups processed so far, or to do something different for the last or penultimate group, for example.

14. Added support for `round()` and `trunc()` to extend functionality of `ITime`. `round()` and `trunc()` can be used with argument units: "hours" or "minutes". Thanks to @JensPederM for the suggestion and PR.

15. A new throttle feature has been introduced to speed up small data tasks that are repeated in a loop, [#3175](https://github.com/Rdatatable/data.table/issues/3175) [#3438](https://github.com/Rdatatable/data.table/issues/3438) [#3205](https://github.com/Rdatatable/data.table/issues/3205) [#3735](https://github.com/Rdatatable/data.table/issues/3735) [#3739](https://github.com/Rdatatable/data.table/issues/3739) [#4284](https://github.com/Rdatatable/data.table/issues/4284) [#4527](https://github.com/Rdatatable/data.table/issues/4527) [#4294](https://github.com/Rdatatable/data.table/issues/4294) [#1120](https://github.com/Rdatatable/data.table/issues/1120). The default throttle of 1024 means that a single thread will be used when nrow<=1024, two threads when nrow<=2048, etc. To change the default, use `setDTthreads(throttle=)`. Or use the new environment variable `R_DATATABLE_THROTTLE`. If you use `Sys.setenv()` in a running R session to change this environment variable, be sure to run an empty `setDTthreads()` call afterwards for the change to take effect; see `?setDTthreads`. The word *throttle* is used to convey that the number of threads is restricted (throttled) for small data tasks. Reducing throttle to 1 will turn off throttling and should revert behaviour to past versions (i.e. using many threads even for small data). Increasing throttle to, say, 65536 will utilize multi-threading only for larger datasets. The value 1024 is a guess. We welcome feedback and test results indicating what the best default should be.

## BUG FIXES

1. A NULL timezone on POSIXct was interpreted by `as.IDate` and `as.ITime` as UTC rather than the session's default timezone (`tz=""`) , [#4085](https://github.com/Rdatatable/data.table/issues/4085).

2. `DT[i]` could segfault when `i` is a zero-column `data.table`, [#4060](https://github.com/Rdatatable/data.table/issues/4060). Thanks @shrektan for reporting and fixing.

3. Dispatch of `first` and `last` functions now properly works again for `xts` objects, [#4053](https://github.com/Rdatatable/data.table/issues/4053). Thanks to @ethanbsmith for reporting.

4. If `.SD` is returned as-is during grouping, it is now unlocked for downstream usage, part of [#4159](https://github.com/Rdatatable/data.table/issues/4159). Thanks also to @mllg for detecting a problem with the initial fix here during the dev release [#4173](https://github.com/Rdatatable/data.table/issues/4173).

5. `GForce` is deactivated for `[[` on non-atomic input, part of [#4159](https://github.com/Rdatatable/data.table/issues/4159). Thanks @hongyuanjia and @ColeMiller1 for helping debug an issue in dev with the original fix before release, [#4612](https://github.com/Rdatatable/data.table/issues/4612).

6. `all.equal(DT, y)` no longer errors when `y` is not a data.table, [#4042](https://github.com/Rdatatable/data.table/issues/4042). Thanks to @d-sci for reporting and the PR.

7. A length 1 `colClasses=NA_character_` would cause `fread` to incorrectly coerce all columns to character, [#4237](https://github.com/Rdatatable/data.table/issues/4237).

8. An `fwrite` error message could include a garbled number and cause test 1737.5 to fail, [#3492](https://github.com/Rdatatable/data.table/issues/3492). Thanks to @QuLogic for debugging the issue on ARMv7hl, and the PR fixing it.

9. `fread` improves handling of very small (<1e-300) or very large (>1e+300) floating point numbers on non-x86 architectures (specifically ppc64le and armv7hl). Thanks to @QuLogic for reporting and fixing, [PR#4165](https://github.com/Rdatatable/data.table/pull/4165).

10. When updating by reference, the use of `get` could result in columns being re-ordered silently, [#4089](https://github.com/Rdatatable/data.table/issues/4089). Thanks to @dmongin for reporting and Cole Miller for the fix.

11. `copy()` now overallocates deeply nested lists of `data.table`s, [#4205](https://github.com/Rdatatable/data.table/issues/4205). Thanks to @d-sci for reporting and the PR.

12. `rbindlist` no longer errors when coercing complex vectors to character vectors, [#4202](https://github.com/Rdatatable/data.table/issues/4202). Thanks to @sritchie73 for reporting and the PR.

13. A relatively rare case of segfault when combining non-equi joins with `by=.EACHI` is now fixed, closes [#4388](https://github.com/Rdatatable/data.table/issues/4388).

14. Selecting key columns could incur a large speed penalty, [#4498](https://github.com/Rdatatable/data.table/issues/4498). Thanks to @Jesper on Stack Overflow for the report.

15. `all.equal(DT1, DT2, ignore.row.order=TRUE)` could return TRUE incorrectly in the presence of NAs, [#4422](https://github.com/Rdatatable/data.table/issues/4422).

16. Non-equi joins now automatically set `allow.cartesian=TRUE`, [4489](https://github.com/Rdatatable/data.table/issues/4489). Thanks to @Henrik-P for reporting.

17. `X[Y, on=character(0)]` and `merge(X, Y, by.x=character(0), by.y=character(0))` no longer crash, [#4272](https://github.com/Rdatatable/data.table/pull/4272). Thanks to @tlapak for the PR.

18. `by=col1:col4` gave an incorrect result if `key(DT)==c("col1","col4")`, [#4285](https://github.com/Rdatatable/data.table/issues/4285). Thanks to @cbilot for reporting, and Cole Miller for the PR.

19. Matrices resulting from logical operators or comparisons on `data.table`s, e.g. in `dta == dtb`, can no longer have their colnames changed by reference later, [#4323](https://github.com/Rdatatable/data.table/issues/4323). Thanks to @eyherabh for reporting and @tlapak for the PR.

20. The environment variable `R_DATATABLE_NUM_THREADS` was being limited by `R_DATATABLE_NUM_PROCS_PERCENT` (by default 50%), [#4514](https://github.com/Rdatatable/data.table/issues/4514). It is now consistent with `setDTthreads()` and only limited by the full number of logical CPUs. For example, on a machine with 8 logical CPUs, `R_DATATABLE_NUM_THREADS=6` now results in 6 threads rather than 4 (50% of 8).

## NOTES

0. Retrospective license change permission was sought from and granted by 4 contributors who were missed in [PR#2456](https://github.com/Rdatatable/data.table/pull/2456), [#4140](https://github.com/Rdatatable/data.table/pull/4140). We had used [GitHub's contributor page](https://github.com/Rdatatable/data.table/graphs/contributors) which omits 3 of these due to invalid email addresses, unlike GitLab's contributor page which includes the ids. The 4th omission was a PR to a script which should not have been excluded; a script is code too. We are sorry these contributors were not properly credited before. They have now been added to the contributors list as displayed on CRAN. All the contributors of code to data.table hold its copyright jointly; your contributions belong to you. You contributed to data.table when it had a particular license at that time, and you contributed on that basis. This is why in the last license change, all contributors of code were consulted and each had a veto.

1. `as.IDate`, `as.ITime`, `second`, `minute`, and `hour` now recognize UTC equivalents for speed: GMT, GMT-0, GMT+0, GMT0, Etc/GMT, and Etc/UTC, [#4116](https://github.com/Rdatatable/data.table/issues/4116).

2. `set2key`, `set2keyv`, and `key2` have been removed, as they have been warning since v1.9.8 (Nov 2016) and halting with helpful message since v1.11.0 (May 2018). When they were introduced in version 1.9.4 (Oct 2014) they were marked as 'experimental' and quickly superseded by `setindex` and `indices`.

3. `data.table` now supports messaging in simplified Chinese (locale `zh_CN`). This was the result of a monumental collaboration to translate `data.table`'s roughly 1400 warnings, errors, and verbose messages (about 16,000 words/100,000 characters) over the course of two months from volunteer translators in at least 4 time zones, most of whom are first-time `data.table` contributors and many of whom are first-time OSS contributors!

    A big thanks goes out to @fengqifang, @hongyuanjia, @biobai, @zhiiiyang, @Leo-Lee15, @soappp9527, @amy17519, @Zachary-Wu, @caiquanyou, @dracodoc, @JulianYlli12, @renkun-ken, @Xueliang24, @koohoko, @KingdaShi, @gaospecial, @shrektan, @sunshine1126, @shawnchen1996, @yc0802, @HesperusArcher, and @Emberwhirl, all of whom took time from their busy schedules to translate and review others' translations. Especial thanks goes to @zhiiiyang and @hongyuanjia who went above and beyond in helping to push the project over the finish line, and to @GuangchuangYu who helped to organize the volunteer pool.

    `data.table` joins `lubridate` and `nlme` as the only of the top 200 most-downloaded community packages on CRAN to offer non-English messaging, and is the only of the top 50 packages to offer complete support of all messaging. We hope this is a first step in broadening the reach and accessibility of the R ecosystem to more users globally and look forward to working with other maintainers looking to bolster the portability of their packages by offering advice on learnings from this undertaking.

    We would be remiss not to mention the laudable lengths to which the R core team goes to maintain the _much_ larger repository (about 6,000 messages in more than 10 languages) of translations for R itself.

    We will evaluate the feasibility (in terms of maintenance difficulty and CRAN package size limits) of offering support for other languages in later releases.

4. `fifelse` and `fcase` now notify users that S4 objects (except `nanotime`) are not supported [#4135](https://github.com/Rdatatable/data.table/issues/4135). Thanks to @torema-ed for bringing it to our attention and Morgan Jacob for the PR.

5. `frank(..., ties.method="random", na.last=NA)` now returns the same random ordering that `base::rank` does, [#4243](https://github.com/Rdatatable/data.table/pull/4243).

6. The error message when mistakenly using `:=` in `i` instead of `j` has been much improved, [#4227](https://github.com/Rdatatable/data.table/issues/4227). Thanks to Hugh Parsonage for the detailed suggestion.

    ```R
    > DT = data.table(A=1:2)
    > DT[B:=3]
    Error: Operator := detected in i, the first argument inside DT[...], but is only valid in
           the second argument, j. Most often, this happens when forgetting the first comma
           (e.g. DT[newvar:=5] instead of DT[, new_var:=5]). Please double-check the
           syntax. Run traceback(), and debugger() to get a line number.
    > DT[, B:=3]
    > DT
           A     B
       <int> <num>
    1:     1     3
    2:     2     3
    ```

7. Added more explanation/examples to `?data.table` for how to use `.BY`, [#1363](https://github.com/Rdatatable/data.table/issues/1363).

8. Changes upstream in R have been accomodated; e.g. `c.POSIXct` now raises `'origin' must be supplied` which impacted `foverlaps`, [#4428](https://github.com/Rdatatable/data.table/pull/4428).

9. `data.table::update.dev.pkg()` now unloads the `data.table` namespace to alleviate a DLL lock issue on Windows, [#4403](https://github.com/Rdatatable/data.table/issues/4403). Thanks to @drag5 for reporting.

10. `data.table` packages binaries built by R version 3 (R3) should only be installed in R3, and similarly `data.table` package binaries built by R4 should only be installed in R4. Otherwise, `package ‘data.table’ was built under R version...` warning will occur which should not be ignored. This is due to a very welcome change to `rbind` and `cbind` in R 4.0.0 which enabled us to remove workarounds, see news item in v1.12.6 below in this file. To continue to support both R3 and R4, `data.table`'s NAMESPACE file contains a condition on the R major version (3 or 4) and this is what gives rise to the requirement that the major version used to build `data.table` must match the major version used to install it. Thanks to @vinhdizzo for reporting, [#4528](https://github.com/Rdatatable/data.table/issues/4528).

11. Internal function `shallow()` no longer makes a deep copy of secondary indices. This eliminates a relatively small time and memory overhead when indices are present that added up significantly when performing many operations, such as joins, in a loop or when joining in `j` by group, [#4311](https://github.com/Rdatatable/data.table/issues/4311). Many thanks to @renkun-ken for the report, and @tlapak for the investigation and PR.

12. The `datatable.old.unique.by.key` option has been removed as per the 4 year schedule detailed in note 10 of v1.12.4 (Oct 2019), note 10 of v1.11.0 (May 2018), and note 1 of v1.9.8 (Nov 2016). It has been generating a helpful warning for 2 years, and helpful error for 1 year.


# data.table [v1.12.8](https://github.com/Rdatatable/data.table/milestone/15?closed=1)  (09 Dec 2019)

## NEW FEATURES

1. `DT[, {...; .(A,B)}]` (i.e. when `.()` is the final item of a multi-statement `{...}`) now auto-names the columns `A` and `B` (just like `DT[, .(A,B)]`) rather than `V1` and `V2`, [#2478](https://github.com/Rdatatable/data.table/issues/2478) [#609](https://github.com/Rdatatable/data.table/issues/609). Similarly, `DT[, if (.N>1) .(B), by=A]` now auto-names the column `B` rather than `V1`. Explicit names are unaffected; e.g. `DT[, {... y= ...; .(A=C+y)}, by=...]` named the column `A` before, and still does. Thanks also to @renkun-ken for his go-first strong testing which caught an issue not caught by the test suite or by revdep testing, related to NULL being the last item, [#4061](https://github.com/Rdatatable/data.table/issues/4061).

## BUG FIXES

1. `frollapply` could segfault and exceed R's C protect limits, [#3993](https://github.com/Rdatatable/data.table/issues/3993). Thanks to @DavisVaughan for reporting and fixing.

2. `DT[, sum(grp), by=grp]` (i.e. aggregating the same column being grouped) could error with `object 'grp' not found`, [#3103](https://github.com/Rdatatable/data.table/issues/3103). Thanks to @cbailiss for reporting.

## NOTES

1. Links in the manual were creating warnings when installing HTML, [#4000](https://github.com/Rdatatable/data.table/issues/4000). Thanks to Morgan Jacob.

2. Adjustments for R-devel (R 4.0.0) which now has reference counting turned on, [#4058](https://github.com/Rdatatable/data.table/issues/4058) [#4093](https://github.com/Rdatatable/data.table/issues/4093). This motivated early release to CRAN because every day CRAN tests every package using the previous day's changes in R-devel; a much valued feature of the R ecosystem. It helps R-core if packages can pass changes in R-devel as soon as possible. Thanks to Luke Tierney for the notice, and for implementing reference counting which we look forward to very much.

3. C internals have been standardized to use `PRI[u|d]64` to print `[u]int64_t`. This solves new warnings from `gcc-8` on Windows with `%lld`, [#4062](https://github.com/Rdatatable/data.table/issues/4062), in many cases already working around `snprintf` on Windows not supporting `%zu`. Release procedures have been augmented to prevent any internal use of `llu`, `lld`, `zu` or `zd`.

4. `test.data.table()` gains `showProgress=interactive()` to suppress the thousands of `Running test id <num> ...` lines displayed by CRAN checks when there are warnings or errors.


# data.table [v1.12.6](https://github.com/Rdatatable/data.table/milestone/18?closed=1)  (18 Oct 2019)

## BUG FIXES

1. `shift()` on a `nanotime` with the default `fill=NA` now fills a `nanotime` missing value correctly, [#3945](https://github.com/Rdatatable/data.table/issues/3945). Thanks to @mschubmehl for reporting and fixing in PR [#3942](https://github.com/Rdatatable/data.table/pull/3942).

2. Compilation failed on CRAN's MacOS due to an older version of `zlib.h/zconf.h` which did not have `z_const` defined, [#3939](https://github.com/Rdatatable/data.table/issues/3939). Other open-source projects unrelated to R have experienced this problem on MacOS too. We have followed the common practice of removing `z_const` to support the older `zlib` versions, and data.table's release procedures have gained a `grep` to ensure `z_const` isn't used again by accident in future. The library `zlib` is used for `fwrite`'s new feature of multithreaded compression on-the-fly; see item 3 of 1.12.4 below.

3. A runtime error in `fwrite`'s compression, but only observed so far on Solaris 10 32bit with zlib 1.2.8 (Apr 2013), [#3931](https://github.com/Rdatatable/data.table/issues/3931): `Error -2: one or more threads failed to allocate buffers or there was a compression error.` In case it happens again, this area has been made more robust and the error more detailed. As is often the case, investigating the Solaris problem revealed secondary issues in the same area of the code. In this case, some `%d` in verbose output should have been `%lld`. This obliquity that CRAN's Solaris provides is greatly appreciated.

4. A leak could occur in the event of an unsupported column type error, or if working memory could only partially be allocated; [#3940](https://github.com/Rdatatable/data.table/issues/3940). Found thanks to `clang`'s Leak Sanitizer (prompted by CRAN's diligent use of latest tools), and two tests in the test suite which tested the unsupported-type error.

## NOTES

1. Many thanks to Kurt Hornik for fixing R's S3 dispatch of `rbind` and `cbind` methods, [#3948](https://github.com/Rdatatable/data.table/issues/3948). With `R>=4.0.0` (current R-devel), `data.table` now registers the S3 methods `cbind.data.table` and `rbind.data.table`, and no longer applies the workaround documented in FAQ 2.24.


# data.table [v1.12.4](https://github.com/Rdatatable/data.table/milestone/16?closed=1)  (03 Oct 2019)

## NEW FEATURES

1. `rleid()` functions now support long vectors (length > 2 billion).

2. `fread()`:
    * now skips embedded `NUL` (`\0`), [#3400](https://github.com/Rdatatable/data.table/issues/3400). Thanks to Marcus Davy for reporting with examples, Roy Storey for the initial PR, and Bingjie Qian for testing this feature on a very complicated real-world file.
    * `colClasses` now supports `'complex'`, `'raw'`, `'Date'`, `'POSIXct'`, and user-defined classes (so long as an `as.` method exists), [#491](https://github.com/Rdatatable/data.table/issues/491) [#1634](https://github.com/Rdatatable/data.table/issues/1634) [#2610](https://github.com/Rdatatable/data.table/issues/2610). Any error during coercion results in a warning and the column is left as the default type (probably `"character"`). Thanks to @hughparsonage for the PR.
    * `stringsAsFactors=0.10` will factorize any character column containing under `0.10*nrow` unique strings, [#2025](https://github.com/Rdatatable/data.table/issues/2025). Thanks to @hughparsonage for the PR.
    * `colClasses=list(numeric=20:30, numeric="ID")` will apply the `numeric` type to column numbers `20:30` as before and now also column name `"ID"`; i.e. all duplicate class names are now respected rather than only the first. This need may arise when specifying some columns by name and others by number, as in this example. Thanks to @hughparsonage for the PR.
    * gains `yaml` (default `FALSE`) and the ability to parse CSVY-formatted input files; i.e., csv files with metadata in a header formatted as YAML (https://csvy.org/), [#1701](https://github.com/Rdatatable/data.table/issues/1701). See `?fread` and files in `/inst/tests/csvy/` for sample formats. Please provide feedback if you find this feature useful and would like extended capabilities. For now, consider it experimental, meaning the API/arguments may change. Thanks to @leeper at [`rio`](https://github.com/leeper/rio) for the inspiration and @MichaelChirico for implementing.
    * `select` can now be used to specify types for just the columns selected, [#1426](https://github.com/Rdatatable/data.table/issues/1426). Just like `colClasses` it can be a named vector of `colname=type` pairs, or a named `list` of `type=col(s)` pairs. For example:

    ```R
    fread(file, select=c(colD="character",     # returns 2 columns: colD,colA
                         colA="integer64"))
    fread(file, select=list(character="colD",  # returns 5 columns: colD,8,9,10,colA
                            integer=  8:10,
                            character="colA"))
    ```
    * gains `tmpdir=` argument which is passed to `tempfile()` whenever a temporary file is needed. Thanks to @mschubmehl for the PR. As before, setting `TMPDIR` (to `/dev/shm` for example) before starting the R session still works too; see `?base::tempdir`.

3. `fwrite()`:
    * now writes compressed `.gz` files directly, [#2016](https://github.com/Rdatatable/data.table/issues/2016). Compression, like `fwrite()`, is multithreaded and compresses each chunk on-the-fly (a full size intermediate file is not created). Use a ".gz" extension, or the new `compress=` option. Many thanks to Philippe Chataignon for the significant PR. For example:

    ```R
    DT = data.table(A=rep(1:2, 100e6), B=rep(1:4, 50e6))
    fwrite(DT, "data.csv")      # 763MB; 1.3s
    fwrite(DT, "data.csv.gz")   #   2MB; 1.6s
    identical(fread("data.csv.gz"), DT)
    ```

    Note that compression is handled using `zlib` library. In the unlikely event of missing `zlib.h`, on a machine that is compiling `data.table` from sources, one may get `fwrite.c` compilation error `zlib.h: No such file or directory`. As of now, the easiest solution is to install missing library using `sudo apt install zlib1g-dev` (Debian/Ubuntu). Installing R (`r-base-dev`) depends on `zlib1g-dev` so this should be rather uncommon. If it happens to you please upvote related issue [#3872](https://github.com/Rdatatable/data.table/issues/3872).

    * Gains `yaml` argument matching that of `fread`, [#3534](https://github.com/Rdatatable/data.table/issues/3534). See the item in `fread` for a bit more detail; here, we'd like to reiterate that feedback is appreciated in the initial phase of rollout for this feature.

    * Gains `bom` argument to add a *byte order mark* (BOM) at the beginning of the file to signal that the file is encoded in UTF-8, [#3488](https://github.com/Rdatatable/data.table/issues/3488). Thanks to Stefan Fleck for requesting and Philippe Chataignon for implementing.

    * Now supports type `complex`, [#3690](https://github.com/Rdatatable/data.table/issues/3690).

    * Gains `scipen` [#2020](https://github.com/Rdatatable/data.table/issues/2020), the number 1 most-requested feature [#3189](https://github.com/Rdatatable/data.table/issues/3189). The default is `getOption("scipen")` so that `fwrite` will now respect R's option in the same way as `base::write.csv` and `base::format`, as expected. The parameter and option name have been kept the same as base R's `scipen` for consistency and to aid online search. It stands for 'scientific penalty'; i.e., the number of characters to add to the width within which non-scientific number format is used if it will fit. A high penalty essentially turns off scientific format. We believe that common practice is to use a value of 999, however, if you do use 999, because your data _might_ include very long numbers such as `10^300`, `fwrite` needs to account for the worst case field width in its buffer allocation per thread. This may impact space or time. If you experience slowdowns or unacceptable memory usage, please pass `verbose=TRUE` to `fwrite`, inspect the output, and report the issue. A workaround, until we can determine the best strategy, may be to pass a smaller value to `scipen`, such as 50. We have observed that `fwrite(DT, scipen=50)` appears to write `10^50` accurately, unlike base R. However, this may be a happy accident and not apply generally. Further work may be needed in this area.

    ```R
    DT = data.table(a=0.0001, b=1000000)
    fwrite(DT)
    # a,b
    # 1e-04,1e+06
    fwrite(DT,scipen=1)
    # a,b
    # 0.0001,1e+06
    fwrite(DT,scipen=2)
    # a,b
    # 0.0001,1000000

    10^50
    # [1] 1e+50
    options(scipen=50)
    10^50
    # [1] 100000000000000007629769841091887003294964970946560
    fwrite(data.table(A=10^50))
    # A
    # 100000000000000000000000000000000000000000000000000
    ```

4. Assigning to one item of a list column no longer requires the RHS to be wrapped with `list` or `.()`, [#950](https://github.com/Rdatatable/data.table/issues/950).

    ```R
    > DT = data.table(A=1:3, B=list(1:2,"foo",3:5))
    > DT
           A      B
       <int> <list>
    1:     1    1,2
    2:     2    foo
    3:     3  3,4,5
    >
    # The following all accomplish the same assignment:
    > DT[2, B:=letters[9:13]]           # was error, now works
    > DT[2, B:=.(letters[9:13])]        # was error, now works
    > DT[2, B:=.(list(letters[9:13]))]  # .(list()) was needed, still works
    > DT
           A         B
       <int>    <list>
    1:     1       1,2
    2:     2 i,j,k,l,m
    3:     3     3,4,5
    ```

5. `print.data.table()` gains an option to display the timezone of `POSIXct` columns when available, [#2842](https://github.com/Rdatatable/data.table/issues/2842). Thanks to Michael Chirico for reporting and Felipe Parages for the PR.

6. New functions `nafill` and `setnafill`, [#854](https://github.com/Rdatatable/data.table/issues/854). Thanks to Matthieu Gomez for the request and Jan Gorecki for implementing.

    ```R
    DT = setDT(lapply(1:100, function(i) sample(c(rnorm(9e6), rep(NA_real_, 1e6)))))
    format(object.size(DT), units="GB")  ## 7.5 Gb
    zoo::na.locf(DT, na.rm=FALSE)  ## zoo           53.518s
    setDTthreads(1L)
    nafill(DT, "locf")             ## DT 1 thread    7.562s
    setDTthreads(0L)
    nafill(DT, "locf")             ## DT 40 threads  0.605s
    setnafill(DT, "locf")          ## DT in-place    0.367s
    ```

7. New variable `.Last.updated` (similar to R's `.Last.value`) contains the number of rows affected by the most recent `:=` or `set()`, [#1885](https://github.com/Rdatatable/data.table/issues/1885). For details see `?.Last.updated`.

8. `between()` and `%between%` are faster for `POSIXct`, [#3519](https://github.com/Rdatatable/data.table/issues/3519), and now support the `.()` alias, [#2315](https://github.com/Rdatatable/data.table/issues/2315). Thanks to @Henrik-P for the reports. There is now also support for `bit64`'s `integer64` class and more robust coercion of types, [#3517](https://github.com/Rdatatable/data.table/issues/3517). `between()` gains `check=` which checks `any(lower>upper)`; off by default for speed in particular for type character.

9. New convenience functions `%ilike%` and `%flike%` which map to new `like()` arguments `ignore.case` and `fixed` respectively, [#3333](https://github.com/Rdatatable/data.table/issues/3333). `%ilike%` is for case-insensitive pattern matching. `%flike%` is for more efficient matching of fixed strings. Thanks to @andreasLD for providing most of the core code.

10. `on=.NATURAL` (or alternatively `X[on=Y]` [#3621](https://github.com/Rdatatable/data.table/issues/3621)) joins two tables on their common column names, so called _natural join_, [#629](https://github.com/Rdatatable/data.table/issues/629). Thanks to David Kulp for request. As before, when `on=` is not provided, `X` must have a key and the key columns are used to join (like rownames, but multi-column and multi-type).

11. `as.data.table` gains `key` argument mirroring its use in `setDT` and `data.table`, [#890](https://github.com/Rdatatable/data.table/issues/890). As a byproduct, the arguments of `as.data.table.array` have changed order, which could affect code relying on positional arguments to this method. Thanks @cooldome for the suggestion and @MichaelChirico for implementation.

12. `merge.data.table` is now exported, [#2618](https://github.com/Rdatatable/data.table/pull/2618). We realize that S3 methods should not ordinarily be exported. Rather, the method should be invoked via S3 dispatch. But users continue to request its export, perhaps because of intricacies relating to the fact that data.table inherits from data.frame, there are two arguments to `merge()` but S3 dispatch applies just to the first, and a desire to explicitly call `data.table::merge.data.table` from package code. Thanks to @AndreMikulec for the most recent request.

13. New rolling function to calculate rolling sum has been implemented and exported, see `?frollsum`, [#2778](https://github.com/Rdatatable/data.table/issues/2778).

14. `setkey` to an existing index now uses the index, [#2889](https://github.com/Rdatatable/data.table/issues/2889). Thanks to @MichaelChirico for suggesting and @saraswatmks for the PR.

15. `DT[order(col)[1:5], ...]` (i.e. where `i` is a compound expression involving `order()`) is now optimized to use `data.table`'s multithreaded `forder`, [#1921](https://github.com/Rdatatable/data.table/issues/1921). This example is not a fully optimal top-N query since the full ordering is still computed. The improvement is that the call to `order()` is computed faster for any `i` expression using `order`.

16. `as.data.table` now unpacks columns in a `data.frame` which are themselves a `data.frame` or `matrix`. This need arises when parsing JSON, a corollary in [#3369](https://github.com/Rdatatable/data.table/issues/3369#issuecomment-462662752). Bug fix 19 in v1.12.2 (see below) added a helpful error (rather than segfault) to detect such invalid `data.table`, and promised that `as.data.table()` would unpack these columns in the next release (i.e. this release) so that the invalid `data.table` is not created in the first place. Further, `setDT` now warns if it observes such columns and suggests using `as.data.table` instead, [#3760](https://github.com/Rdatatable/data.table/issues/3760).

17. `CJ` has been ported to C and parallelized, thanks to a PR by Michael Chirico, [#3596](https://github.com/Rdatatable/data.table/pull/3596). All types benefit, but, as in many `data.table` operations, factors benefit more than character.

    ```R
    # default 4 threads on a laptop with 16GB RAM and 8 logical CPU

    ids = as.vector(outer(LETTERS, LETTERS, paste0))
    system.time( CJ(ids, 1:500000) )  # 3.9GB; 340m rows
    #   user  system elapsed (seconds)
    #  3.000   0.817   3.798  # was
    #  1.800   0.832   2.190  # now

    # ids = as.factor(ids)
    system.time( CJ(ids, 1:500000) )  # 2.6GB; 340m rows
    #   user  system elapsed (seconds)
    #  1.779   0.534   2.293  # was
    #  0.357   0.763   0.292  # now
    ```

18. New function `fcoalesce(...)` has been written in C, and is multithreaded for `numeric` and `factor`. It replaces missing values according to a prioritized list of candidates (as per SQL COALESCE, `dplyr::coalesce`, and `hutils::coalesce`), [#3424](https://github.com/Rdatatable/data.table/issues/3424). It accepts any number of vectors in several forms. For example, given three vectors `x`, `y`, and `z`, where each `NA` in `x` is to be replaced by the corresponding value in `y` if that is non-NA, else the corresponding value in `z`, the following equivalent forms are all accepted: `fcoalesce(x,y,z)`, `fcoalesce(x,list(y,z))`, and `fcoalesce(list(x,y,z))`. Being a new function, its behaviour is subject to change particularly for type `list`, [#3712](https://github.com/Rdatatable/data.table/issues/3712).

    ```R
    # default 4 threads on a laptop with 16GB RAM and 8 logical CPU
    N = 100e6
    x = replicate(5, {x=sample(N); x[sample(N, N/2)]=NA; x}, simplify=FALSE)  # 2GB
    y1 = do.call(dplyr::coalesce, x))
    y2 = do.call(hutils::coalesce, x))
    y3 = do.call(data.table::fcoalesce, x))
    #   user  system elapsed (seconds)
    #  4.935   1.876   6.810  # dplyr::coalesce
    #  3.122   0.831   3.956  # hutils::coalesce
    #  0.915   0.099   0.379  # data.table::fcoalesce
    identical(y1,y2) && identical(y1,y3)
    # TRUE
    ```

19. Type `complex` is now supported by `setkey`, `setorder`, `:=`, `by=`, `keyby=`, `shift`, `dcast`, `frank`, `rowid`, `rleid`, `CJ`, `fcoalesce`, `unique`, and `uniqueN`, [#3690](https://github.com/Rdatatable/data.table/issues/3690). Thanks to Gareth Ward and Elio Campitelli for their reports and input. Sorting `complex` is achieved the same way as base R; i.e., first by the real part then by the imaginary part (as if the `complex` column were two separate columns of `double`). There is no plan to support joining/merging on `complex` columns until a user demonstrates a need for that.

20. `setkey`, `[key]by=` and `on=` in verbose mode (`options(datatable.verbose=TRUE)`) now detect any columns inheriting from `Date` which are stored as 8 byte double, test if any fractions are present, and if not suggest using a 4 byte integer instead (such as `data.table::IDate`) to save space and time, [#1738](https://github.com/Rdatatable/data.table/issues/1738). In future this could be upgraded to `message` or `warning` depending on feedback.

21. New function `fifelse(test, yes, no, na)` has been implemented in C by Morgan Jacob, [#3657](https://github.com/Rdatatable/data.table/issues/3657) and [#3753](https://github.com/Rdatatable/data.table/issues/3753). It is comparable to `base::ifelse`, `dplyr::if_else`, `hutils::if_else`, and (forthcoming) [`vctrs::if_else()`](https://vctrs.r-lib.org/articles/stability.html#ifelse). It returns a vector of the same length as `test` but unlike `base::ifelse` the output type is consistent with those of `yes` and `no`. Please see `?data.table::fifelse` for more details.

    ```R
    # default 4 threads on a laptop with 16GB RAM and 8 logical CPU
    x = sample(c(TRUE,FALSE), 3e8, replace=TRUE)  # 1GB
    microbenchmark::microbenchmark(
      base::ifelse(x, 7L, 11L),
      dplyr::if_else(x, 7L, 11L),
      hutils::if_else(x, 7L, 11L),
      data.table::fifelse(x, 7L, 11L),
      times = 5L, unit="s"
    )
    # Unit: seconds
    #                            expr  min  med  max neval
    #        base::ifelse(x, 7L, 11L)  8.5  8.6  8.8     5
    #      dplyr::if_else(x, 7L, 11L)  9.4  9.5  9.7     5
    #     hutils::if_else(x, 7L, 11L)  2.6  2.6  2.7     5
    # data.table::fifelse(x, 7L, 11L)  1.5  1.5  1.6     5  # setDTthreads(1)
    # data.table::fifelse(x, 7L, 11L)  0.8  0.8  0.9     5  # setDTthreads(2)
    # data.table::fifelse(x, 7L, 11L)  0.4  0.4  0.5     5  # setDTthreads(4)
    ```

22. `transpose` gains `keep.names=` and `make.names=` arguments, [#1886](https://github.com/Rdatatable/data.table/issues/1886). Previously, column names were dropped and there was no way to keep them. `keep.names="rn"` keeps the column names and puts them in the `"rn"` column of the result. Similarly, `make.names="rn"` uses column `"rn"` as the column names of the result. Both arguments are `NULL` by default for backwards compatibility. As these new arguments are new, they are subject to change in future according to community feedback. Thanks to @ghost for the request.

23. Added a `data.table` method for `utils::edit` to ensure a `data.table` is returned, for convenience, [#593](https://github.com/Rdatatable/data.table/issues/593).

24. More efficient optimization of many columns in `j` (e.g. from `.SD`), [#1470](https://github.com/Rdatatable/data.table/issues/1470). Thanks @Jorges1000 for the report.

25. `setnames(DT, old, new)` now omits any `old==new` to save redundant key and index name updates, [#3783](https://github.com/Rdatatable/data.table/issues/3783). `setnames(DT, new)` (i.e. not providing `old`) already omitted any column name updates where `names(DT)==new`; e.g. `setnames(DT, gsub('^_', '', names(DT)))` exits early if no columns start with `_`.

26. `[[` by group is now optimized for regular vectors (not type list), [#3209](https://github.com/Rdatatable/data.table/issues/3209). Thanks @renkun-ken for the suggestion. `[` by group was already optimized. Please file a feature request if you would like this optimization for list columns.

27. New function `frollapply` for rolling computation of arbitrary R functions (caveat: input `x` is coerced to numeric beforehand, and the function must return a scalar numeric value). The API is consistent to extant rolling functions `frollmean` and `frollsum`; note that it will generally be slower than those functions because (1) the known functions use our optimized internal C implementation and (2) there is no thread-safe API to R's C `eval`. Nevertheless `frollapply` is faster than corresponding `base`-only and `zoo` versions:

    ```R
    set.seed(108)
    x = rnorm(1e6); n = 1e3
    base_rollapply = function(x, n, FUN) {
      nx = length(x)
      ans = rep(NA_real_, nx)
      for (i in n:nx) ans[i] = FUN(x[(i-n+1):i])
      ans
    }
    system.time(base_rollapply(x, n, mean))
    system.time(zoo::rollapplyr(x, n, function(x) mean(x), fill=NA))
    system.time(zoo::rollmeanr(x, n, fill=NA))
    system.time(frollapply(x, n, mean))
    system.time(frollmean(x, n))

    ### fun             mean     sum  median
    # base_rollapply   8.815   5.151  60.175
    # zoo::rollapply  34.373  27.837  88.552
    # zoo::roll[fun]   0.215   0.185      NA   ## median not fully supported
    # frollapply       5.404   1.419  56.475
    # froll[fun]       0.003   0.002      NA   ## median not yet supported
    ```

28. `setnames()` now accepts functions in `old=` and `new=`, [#3703](https://github.com/Rdatatable/data.table/issues/3703). Thanks @smingerson for the feature request and @shrektan for the PR.

    ```R
    DT = data.table(a=1:3, b=4:6, c=7:9)
    setnames(DT, toupper)
    names(DT)
    # [1] "A" "B" "C"
    setnames(DT, c(1,3), tolower)
    names(DT)
    # [1] "a" "B" "c"
    ```

29. `:=` and `set()` now use zero-copy type coercion. Accordingly, `DT[..., integerColumn:=0]` and `set(DT,i,j,0)` no longer warn about the `0` ('numeric') needing to be `0L` ('integer') because there is no longer any time or space used for this coercion. The old long warning was off-putting to new users ("what and why L?"), whereas advanced users appreciated the old warning so they could avoid the coercion. Although the time and space for one coercion in a single call is unmeasurably small, when placed in a loop the small overhead of any allocation on R's heap could start to become noticeable (more so for `set()` whose purpose is low-overhead looping). Further, when assigning a value across columns of varying types, it could be inconvenient to supply the correct type for every column. Hence, zero-copy coercion was introduced to satisfy all these requirements. A warning is still issued, as before, when fractional data is discarded; e.g. when 3.14 is assigned to an integer column. Zero-copy coercion applies to length>1 vectors as well as length-1 vectors.

## BUG FIXES

1. `first`, `last`, `head` and `tail` by group no longer error in some cases, [#2030](https://github.com/Rdatatable/data.table/issues/2030) [#3462](https://github.com/Rdatatable/data.table/issues/3462). Thanks to @franknarf1 for reporting.

2. `keyby=colName` could use the wrong index and return incorrect results if both `colName` and `colNameExtra` (where `colName` is a leading subset of characters of `colNameExtra`) are column names and an index exists on `colNameExtra`, [#3498](https://github.com/Rdatatable/data.table/issues/3498). Thanks to Xianying Tan for the detailed report and pinpointing the source line at fault.

3. A missing item in `j` such as `j=.(colA, )` now gives a helpful error (`Item 2 of the .() or list() passed to j is missing`) rather than the unhelpful error `argument "this_jsub" is missing, with no default` (v1.12.2) or `argument 2 is empty` (v1.12.0 and before), [#3507](https://github.com/Rdatatable/data.table/issues/3507). Thanks to @eddelbuettel for the report.

4. `fwrite()` could crash when writing very long strings such as 30 million characters, [#2974](https://github.com/Rdatatable/data.table/issues/2974), and could be unstable in memory constrained environments, [#2612](https://github.com/Rdatatable/data.table/issues/2612). Thanks to @logworthy and @zachokeeffe for reporting and Philippe Chataignon for fixing in PR [#3288](https://github.com/Rdatatable/data.table/pull/3288).

5. `fread()` could crash if `quote=""` (i.e. ignore quotes), the last line is too short, and `fill=TRUE`, [#3524](https://github.com/Rdatatable/data.table/pull/3524). Thanks to Jiucang Hao for the report and reproducible example.

6. Printing could occur unexpectedly when code is run with `source`, [#2369](https://github.com/Rdatatable/data.table/issues/2369). Thanks to @jan-glx for the report and reproducible example.

7. Grouping by `NULL` on zero rows `data.table` now behaves consistently to non-zero rows `data.table`, [#3530](https://github.com/Rdatatable/data.table/issues/3530). Thanks to @SymbolixAU for the report and reproducible example.

8. GForce optimization of `median` did not retain the class; e.g. `median` of `Date` or `POSIXct` would return a raw number rather than retain the date class, [#3079](https://github.com/Rdatatable/data.table/issues/3079). Thanks to @Henrik-P for reporting.

9. `DT[, format(mean(date,""%b-%Y")), by=group]` could fail with `invalid 'trim' argument`, [#1876](https://github.com/Rdatatable/data.table/issues/1876). Thanks to Ross Holmberg for reporting.

10. `externalVar=1:5; DT[, mean(externalVar), by=group]` could return incorrect results rather than a constant (`3` in this example) for each group, [#875](https://github.com/Rdatatable/data.table/issues/875). GForce optimization was being applied incorrectly to the `mean` without realizing `externalVar` was not a column.

11. `test.data.table()` now passes in non-English R sessions, [#630](https://github.com/Rdatatable/data.table/issues/630) [#3039](https://github.com/Rdatatable/data.table/issues/3039). Each test still checks that the number of warnings and/or errors produced is correct. However, a message is displayed suggesting to restart R with `LANGUAGE=en` in order to test that the text of the warning and/or error messages are as expected, too.

12. Joining a double column in `i` containing say 1.3, with an integer column in `x` containing say 1, would result in the 1.3 matching to 1, [#2592](https://github.com/Rdatatable/data.table/issues/2592), and joining a factor column to an integer column would match the factor's integers rather than error. The type coercion logic has been revised and strengthened. Many thanks to @MarkusBonsch for reporting and fixing. Joining a character column in `i` to a factor column in `x` is now faster and retains the character column in the result rather than coercing it to factor. Joining an integer column in `i` to a double column in `x` now retains the integer type in the result rather than coercing the integers into the double type. Logical columns may now only be joined to logical columns, other than all-NA columns which are coerced to the matching column's type. All coercions are reported in verbose mode: `options(datatable.verbose=TRUE)`.

13. Attempting to recycle 2 or more items into an existing `list` column now gives the intended helpful error rather than `Internal error: recycle length error not caught earlier.`, [#3543](https://github.com/Rdatatable/data.table/issues/3543). Thanks to @MichaelChirico for finding and reporting.

14. Subassigning using `$<-` to a `data.table` embedded in a list column of a single-row `data.table` could fail, [#3474](https://github.com/Rdatatable/data.table/issues/3474). Note that `$<-` is not recommended; please use `:=` instead which already worked in this case. Thanks to Jakob Richter for reporting.

15. `rbind` and `rbindlist` of zero-row items now retain (again) the unused levels of any (zero-length) factor columns, [#3508](https://github.com/Rdatatable/data.table/issues/3508). This was a regression in v1.12.2 just for zero-row items. Unused factor levels were already retained for items having `nrow>=1`. Thanks to Gregory Demin for reporting.

16. `rbind` and `rbindlist` of an item containing an ordered factor with levels containing an `NA` (as opposed to an NA integer) could segfault, [#3601](https://github.com/Rdatatable/data.table/issues/3601). This was a a regression in v1.12.2. Thanks to Damian Betebenner for reporting. Also a related segfault when recycling a length-1 factor column, [#3662](https://github.com/Rdatatable/data.table/issues/3662).

17. `example(":=", local=TRUE)` now works rather than error, [#2972](https://github.com/Rdatatable/data.table/issues/2972). Thanks @vlulla for the report.

18. `rbind.data.frame` on `IDate` columns changed the column from `integer` to `double`, [#2008](https://github.com/Rdatatable/data.table/issues/2008). Thanks to @rmcgehee for reporting.

19. `merge.data.table` now retains any custom classes of the first argument, [#1378](https://github.com/Rdatatable/data.table/issues/1378). Thanks to @michaelquinn32 for reopening.

20. `c`, `seq` and `mean` of `ITime` objects now retain the `ITime` class via new `ITime` methods, [#3628](https://github.com/Rdatatable/data.table/issues/3628). Thanks @UweBlock for reporting. The `cut` and `split` methods for `ITime` have been removed since the default methods work, [#3630](https://github.com/Rdatatable/data.table/pull/3630).

21. `as.data.table.array` now handles the case when some of the array's dimension names are `NULL`, [#3636](https://github.com/Rdatatable/data.table/issues/3636).

22. Adding a `list` column using `cbind`, `as.data.table`, or `data.table` now works rather than treating the `list` as if it were a set of columns and introducing an invalid NA column name, [#3471](https://github.com/Rdatatable/data.table/pull/3471). However, please note that using `:=` to add columns is preferred.

    ```R
    cbind( data.table(1:2), list(c("a","b"),"a") )
    #       V1     V2     NA   # v1.12.2 and before
    #    <int> <char> <char>
    # 1:     1      a      a
    # 2:     2      b      a
    #
    #       V1     V2          # v1.12.4+
    #    <int> <list>
    # 1:     1    a,b
    # 2:     2      a
    ```

23. Incorrect sorting/grouping results due to a bug in Intel's `icc` compiler 2019 (Version 19.0.4.243 Build 20190416) has been worked around thanks to a report and fix by Sebastian Freundt, [#3647](https://github.com/Rdatatable/data.table/issues/3647). Please run `data.table::test.data.table()`. If that passes, your installation does not have the problem.

24. `column not found` could incorrectly occur in rare non-equi-join cases, [#3635](https://github.com/Rdatatable/data.table/issues/3635). Thanks to @UweBlock for the report.

25. Slight fix to the logic for auto-naming the `by` clause for using a custom function like `evaluate` to now be named `evaluate` instead of the name of the first symbolic argument, [#3758](https://github.com/Rdatatable/data.table/issues/3758).

26. Column binding of zero column `data.table` will now work as expected, [#3334](https://github.com/Rdatatable/data.table/issues/3334). Thanks to @kzenstratus for the report.

27. `integer64` sum-by-group is now properly optimized, [#1647](https://github.com/Rdatatable/data.table/issues/1647), [#3464](https://github.com/Rdatatable/data.table/issues/3464). Thanks to @mlandry22-h2o for the report.

28. From v1.12.0 `between()` and `%between%` interpret missing values in `lower=` or `upper=` as unlimited bounds. A new parameter `NAbounds` has been added to achieve the old behaviour of returning `NA`, [#3522](https://github.com/Rdatatable/data.table/issues/3522). Thanks @cguill95 for reporting. This is now consistent for character input, [#3667](https://github.com/Rdatatable/data.table/issues/3667) (thanks @AnonymousBoba), and class `nanotime` is now supported too.

29. `integer64` defined on a subset of a new column would leave "gibberish" on the remaining rows, [#3723](https://github.com/Rdatatable/data.table/issues/3723). A bug in `rbindlist` with the same root cause was also fixed, [#1459](https://github.com/Rdatatable/data.table/issues/1459). Thanks @shrektan and @jangorecki for the reports.

30. `groupingsets` functions now properly handle alone special symbols when using an empty set to group by, [#3653](https://github.com/Rdatatable/data.table/issues/3653). Thanks to @Henrik-P for the report.

31. A `data.table` created using `setDT()` on a `data.frame` containing identical columns referencing each other would cause `setkey()` to return incorrect results, [#3496](https://github.com/Rdatatable/data.table/issues/3496) and [#3766](https://github.com/Rdatatable/data.table/issues/3766). Thanks @kirillmayantsev and @alex46015 for reporting, and @jaapwalhout and @Atrebas for helping to debug and isolate the issue.

32. `x[, round(.SD, 1)]` and similar operations on the whole of `.SD` could return a locked result, incorrectly preventing `:=` on the result, [#2245](https://github.com/Rdatatable/data.table/issues/2245). Thanks @grayskripko for raising.

33. Using `get`/`mget` in `j` could cause `.SDcols` to be ignored or reordered, [#1744](https://github.com/Rdatatable/data.table/issues/1744), [#1965](https://github.com/Rdatatable/data.table/issues/1965), and [#2036](https://github.com/Rdatatable/data.table/issues/2036). Thanks @franknarf1, @MichaelChirico, and @TonyBonen, for the reports.

34. `DT[, i-1L, with=FALSE]` would misinterpret the minus sign and return an incorrect result, [#2019](https://github.com/Rdatatable/data.table/issues/2109). Thanks @cguill95 for the report.

35. `DT[id==1, DT2[.SD, on="id"]]` (i.e. joining from `.SD` in `j`) could incorrectly fail in some cases due to `.SD` being locked, [#1926](https://github.com/Rdatatable/data.table/issues/1926), and when updating-on-join with factors [#3559](https://github.com/Rdatatable/data.table/issues/3559) [#2099](https://github.com/Rdatatable/data.table/issues/2099). Thanks @franknarf1 and @Henrik-P for the reports and for diligently tracking use cases for almost 3 years!

36. `as.IDate.POSIXct` returned `NA` for UTC times before Dec 1901 and after Jan 2038, [#3780](https://github.com/Rdatatable/data.table/issues/3780). Thanks @gschett for the report.

37. `rbindlist` now returns correct idcols for lists with different length vectors, [#3785](https://github.com/Rdatatable/data.table/issues/3785), [#3786](https://github.com/Rdatatable/data.table/pull/3786). Thanks to @shrektan for the report and fix.

38. `DT[ , !rep(FALSE, ncol(DT)), with=FALSE]` correctly returns the full table, [#3013](https://github.com/Rdatatable/data.table/issues/3013) and [#2917](https://github.com/Rdatatable/data.table/issues/2917). Thanks @alexnss and @DavidArenburg for the reports.

39. `shift(x, 0:1, type='lead', give.names=TRUE)` uses `lead` in all returned column names, [#3832](https://github.com/Rdatatable/data.table/issues/3832). Thanks @daynefiler for the report.

40. Subtracting two `POSIXt` objects by group could lead to incorrect results because the `base` method internally calls `difftime` with `units='auto'`; `data.table` does not notice if the chosen units differ by group and only the last group's `units` attribute was retained, [#3694](https://github.com/Rdatatable/data.table/issues/3694) and [#761](https://github.com/Rdatatable/data.table/issues/761). To surmount this, we now internally force `units='secs'` on all `POSIXt-POSIXt` calls (reported when `verbose=TRUE`); generally we recommend calling `difftime` directly instead. Thanks @oliver-oliver and @boethian for the reports.

41. Using `get`/`mget` in `j` could cause `.SDcols` to be ignored or reordered, [#1744](https://github.com/Rdatatable/data.table/issues/1744), [#1965](https://github.com/Rdatatable/data.table/issues/1965), [#2036](https://github.com/Rdatatable/data.table/issues/2036), and [#2946](https://github.com/Rdatatable/data.table/issues/2946). Thanks @franknarf1, @MichaelChirico, @TonyBonen, and Steffen J. (StackOverflow) for the reports.

42. `DT[...,by={...}]` now handles expressions in `{`, [#3156](https://github.com/Rdatatable/data.table/issues/3156). Thanks to @tdhock for the report.

43. `:=` could change a `data.table` creation statement in the body of the function calling it, or a variable in calling scope, [#3890](https://github.com/Rdatatable/data.table/issues/3890). Many thanks to @kirillmayantsev for the detailed reports.

44. Grouping could create a `malformed factor` and/or segfault when the factors returned by each group did not have identical levels, [#2199](https://github.com/Rdatatable/data.table/issues/2199) and [#2522](https://github.com/Rdatatable/data.table/issues/2522). Thanks to Václav Hausenblas, @franknarf1, @ben519, and @Henrik-P for reporting.

45. `rbindlist` (and printing a `data.table` with over 100 rows because that uses `rbindlist(head, tail)`) could error with `malformed factor` for unordered factor columns containing a used `NA_character_` level, [#3915](https://github.com/Rdatatable/data.table/issues/3915). This is an unusual input for unordered factors because NA_integer_ is recommended by default in R. Thanks to @sindribaldur for reporting.

46. Adding a `list` column containing an item of type `list` to a one row `data.table` could fail, [#3626](https://github.com/Rdatatable/data.table/issues/3626). Thanks to Jakob Richter for reporting.

## NOTES

1. `rbindlist`'s `use.names="check"` now emits its message for automatic column names (`"V[0-9]+"`) too, [#3484](https://github.com/Rdatatable/data.table/pull/3484). See news item 5 of v1.12.2 below.

2. Adding a new column by reference using `set()` on a `data.table` loaded from binary file now give a more helpful error message, [#2996](https://github.com/Rdatatable/data.table/issues/2996). Thanks to Joseph Burling for reporting.

    ```
    This data.table has either been loaded from disk (e.g. using readRDS()/load()) or constructed
    manually (e.g. using structure()). Please run setDT() or alloc.col() on it first (to pre-allocate
    space for new columns) before adding new columns by reference to it.
    ```

3. `setorder` on a superset of a keyed `data.table`'s key now retains its key, [#3456](https://github.com/Rdatatable/data.table/issues/3456). For example, if `a` is the key of `DT`, `setorder(DT, a, -v)` will leave `DT` keyed by `a`.

4. New option `options(datatable.quiet = TRUE)` turns off the package startup message, [#3489](https://github.com/Rdatatable/data.table/issues/3489). `suppressPackageStartupMessages()` continues to work too. Thanks to @leobarlach for the suggestion inspired by `options(tidyverse.quiet = TRUE)`. We don't know of a way to make a package respect the `quietly=` option of `library()` and `require()` because the `quietly=` isn't passed through for use by the package's own `.onAttach`. If you can see how to do that, please submit a patch to R.

5. When loading a `data.table` from disk (e.g. with `readRDS`), best practice is to run `setDT()` on the new object to assure it is correctly allocated memory for new column pointers. Barring this, unexpected behavior can follow; for example, if you assign a new column to `DT` from a function `f`, the new columns will only be assigned within `f` and `DT` will be unchanged. The `verbose` messaging in this situation is now more helpful, [#1729](https://github.com/Rdatatable/data.table/issues/1729). Thanks @vspinu for sharing his experience to spur this.

6. New vignette _Using `.SD` for Data Analysis_, a deep dive into use cases for the `.SD` variable to help illuminate this topic which we've found to be a sticking point for beginning and intermediate `data.table` users, [#3412](https://github.com/Rdatatable/data.table/issues/3412).

7. Added a note to `?frank` clarifying that ranking is being done according to C sorting (i.e., like `forder`), [#2328](https://github.com/Rdatatable/data.table/issues/2328). Thanks to @cguill95 for the request.

8. Historically, `dcast` and `melt` were built as enhancements to `reshape2`'s own `dcast`/`melt`. We removed dependency on `reshape2` in v1.9.6 but maintained some backward compatibility. As that package has been deprecated since December 2017, we will begin to formally complete the split from `reshape2` by removing some last vestiges. In particular we now warn when redirecting to `reshape2` methods and will later error before ultimately completing the split; see [#3549](https://github.com/Rdatatable/data.table/issues/3549) and [#3633](https://github.com/Rdatatable/data.table/issues/3633). We thank the `reshape2` authors for their original inspiration for these functions, and @ProfFancyPants for testing and reporting regressions in dev which have been fixed before release.

9. `DT[col]` where `col` is a column containing row numbers of itself to select, now suggests the correct syntax (`DT[(col)]` or `DT[DT$col]`), [#697](https://github.com/Rdatatable/data.table/issues/697). This expands the message introduced in [#1884](https://github.com/Rdatatable/data.table/issues/1884) for the case where `col` is type `logical` and `DT[col==TRUE]` is suggested.

10. The `datatable.old.unique.by.key` option has been warning for 1 year that it is deprecated: `... Please stop using it and pass by=key(DT) instead for clarity ...`. This warning is now upgraded to error as per the schedule in note 10 of v1.11.0 (May 2018), and note 1 of v1.9.8 (Nov 2016). In June 2020 the option will be removed.

11. We intend to deprecate the `datatable.nomatch` option, [more info](https://github.com/Rdatatable/data.table/pull/3578/files). A message is now printed upon use of the option (once per session) as a first step. It asks you to please stop using the option and to pass `nomatch=NULL` explicitly if you require inner join. Outer join (`nomatch=NA`) has always been the default because it is safer; it does not drop missing data silently. The problem is that the option is global; i.e., if a user changes the default using this option for their own use, that can change the behavior of joins inside packages that use `data.table` too. This is the only `data.table` option with this concern.

12. The test suite of 9k tests now runs with three R options on: `warnPartialMatchArgs`, `warnPartialMatchAttr`, and `warnPartialMatchDollar`. This ensures that we don't rely on partial argument matching in internal code, for robustness and efficiency, and so that users can turn these options on for their code in production, [#3664](https://github.com/Rdatatable/data.table/issues/3664). Thanks to Vijay Lulla for the suggestion, and Michael Chirico for fixing 48 internal calls to `attr()` which were missing `exact=TRUE`, for example. Thanks to R-core for adding these options to R 2.6.0 (Oct 2007).

13. `test.data.table()` could fail if the `datatable.integer64` user option was set, [#3683](https://github.com/Rdatatable/data.table/issues/3683). Thanks @xiaguoxin for reporting.

14. The warning message when using `keyby=` together with `:=` is clearer, [#2763](https://github.com/Rdatatable/data.table/issues/2763). Thanks to @eliocamp.

15. `first` and `last` gain an explicit `n=1L` argument so that it's clear the default is 1, and their almost identical manual pages have been merged into one.

16. Rolling functions (`?froll`) coerce `logical` input to `numeric` (instead of failing) to mimic the behavior of `integer` input.

17. The warning message when using `strptime` in `j` has been improved, [#2068](https://github.com/Rdatatable/data.table/issues/2068). Thanks to @tdhock for the report.

18. Added a note to `?setkey` clarifying that `setkey` always uses C-locale sorting (as has been noted in `?setorder`). Thanks @JBreidaks for the report in [#2114](https://github.com/Rdatatable/data.table/issues/2114).

19. `hour()`/`minute()`/`second()` are much faster for `ITime` input, [#3518](https://github.com/Rdatatable/data.table/issues/3158).

20. New alias `setalloccol` for `alloc.col`, [#3475](https://github.com/Rdatatable/data.table/issues/3475). For consistency with `set*` prefixes for functions that operate in-place (like `setkey`, `setorder`, etc.). `alloc.col` is not going to be deprecated but we recommend using `setalloccol`.

21. `dcast` no longer emits a message when `value.var` is missing but `fun.aggregate` is explicitly set to `length` (since `value.var` is arbitrary in this case), [#2980](https://github.com/Rdatatable/data.table/issues/2980).

22. Optimized `mean` of `integer` columns no longer warns about a coercion to numeric, [#986](https://github.com/Rdatatable/data.table/issues/986). Thanks @dgrtwo for his [YouTube tutorial at 3:01](https://youtu.be/AmE4LXPQErM?t=175) where the warning occurs.

23. Using `first` and `last` function on `POSIXct` object no longer loads `xts` namespace, [#3857](https://github.com/Rdatatable/data.table/issues/3857). `first` on empty `data.table` returns empty `data.table` now [#3858](https://github.com/Rdatatable/data.table/issues/3858).

24. Added some clarifying details about what happens when a shell command is used in `fread`, [#3877](https://github.com/Rdatatable/data.table/issues/3877). Thanks Brian for the StackOverflow question which highlighted the lack of explanation here.

25. We continue to encourage packages to `Import` rather than `Depend` on `data.table`, [#3076](https://github.com/Rdatatable/data.table/issues/3076). To prevent the growth rate in new packages using `Depend`, we have requested that CRAN apply a small patch we provided to prevent new submissions using `Depend`. If this is accepted, the error under `--as-cran` will be as follows. The existing 73 packages using `Depend` will continue to pass OK until they next update, at which point they will be required to change from `Depend` to `Import`.

    ```
    R CMD check <pkg> --as-cran
    ...
    * checking package dependencies ... ERROR

    data.table should be in Imports not Depends. Please contact its
    maintainer for more information.
    ```


# data.table [v1.12.2](https://github.com/Rdatatable/data.table/milestone/14?closed=1)  (07 Apr 2019)

## NEW FEATURES

1. `:=` no longer recycles length>1 RHS vectors. There was a warning when recycling left a remainder but no warning when the LHS length was an exact multiple of the RHS length (the same behaviour as base R). Consistent feedback for several years has been that recycling is more often a bug. In rare cases where you need to recycle a length>1 vector, please use `rep()` explicitly. Single values are still recycled silently as before. Early warning was given in [this tweet](https://twitter.com/MattDowle/status/1088544083499311104). The 774 CRAN and Bioconductor packages using `data.table` were tested and the maintainers of the 16 packages affected (2%) were consulted before going ahead, [#3310](https://github.com/Rdatatable/data.table/pull/3310). Upon agreement we went ahead. Many thanks to all those maintainers for already updating on CRAN, [#3347](https://github.com/Rdatatable/data.table/pull/3347).

2. `foverlaps` now supports `type="equal"`, [#3416](https://github.com/Rdatatable/data.table/issues/3416) and part of [#3002](https://github.com/Rdatatable/data.table/issues/3002).

3. The number of logical CPUs used by default has been reduced from 100% to 50%. The previous 100% default was reported to cause significant slow downs when other non-trivial processes were also running, [#3395](https://github.com/Rdatatable/data.table/issues/3395) [#3298](https://github.com/Rdatatable/data.table/issues/3298). Two new optional environment variables (`R_DATATABLE_NUM_PROCS_PERCENT` & `R_DATATABLE_NUM_THREADS`) control this default. `setDTthreads()` gains `percent=` and `?setDTthreads` has been significantly revised. The output of `getDTthreads(verbose=TRUE)` has been expanded. The environment variable `OMP_THREAD_LIMIT` is now respected ([#3300](https://github.com/Rdatatable/data.table/issues/3300)) in addition to `OMP_NUM_THREADS` as before.

4. `rbind` and `rbindlist` now retain the position of duplicate column names rather than grouping them together [#3373](https://github.com/Rdatatable/data.table/issues/3373), fill length 0 columns (including NULL) with NA with warning [#1871](https://github.com/Rdatatable/data.table/issues/1871), and recycle length-1 columns [#524](https://github.com/Rdatatable/data.table/issues/524). Thanks to Kun Ren for the requests which arose when parsing JSON.

5. `rbindlist`'s `use.names=` default has changed from `FALSE` to `"check"`. This emits a message if the column names of each item are not identical and then proceeds as if `use.names=FALSE` for backwards compatibility; i.e., bind by column position not by column name. The `rbind` method for `data.table` already sets `use.names=TRUE` so this change affects `rbindlist` only and not `rbind.data.table`. To stack differently named columns together silently (the previous default behavior of `rbindlist`), it is now necessary to specify `use.names=FALSE` for clarity to readers of your code. Thanks to Clayton Stanley who first raised the issue [here](https://lists.r-forge.r-project.org/pipermail/datatable-help/2014-April/002480.html). To aid pinpointing the calls to `rbindlist` that need attention, the message can be turned to error using `options(datatable.rbindlist.check="error")`. This option also accepts `"warning"`, `"message"` and `"none"`. In this release the message is suppressed for default column names (`"V[0-9]+"`); the next release will emit the message for those too. In 6 months the default will be upgraded from message to warning. There are two slightly different messages. They are helpful, include context and point to this news item :

    ```
    Column %d ['%s'] of item %d is missing in item %d. Use fill=TRUE to fill with
      NA (NULL for list columns), or use.names=FALSE to ignore column names.
      See news item 5 in v1.12.2 for options to control this message.

    Column %d ['%s'] of item %d appears in position %d in item %d. Set use.names=TRUE
      to match by column name, or use.names=FALSE to ignore column names.
      See news item 5 in v1.12.2 for options to control this message.
    ```

6. `fread` gains `keepLeadingZeros`, [#2999](https://github.com/Rdatatable/data.table/issues/2999). By default `FALSE` so that, as before, a field containing `001` is interpreted as the integer 1, otherwise the character string `"001"`. The default may be changed using `options(datatable.keepLeadingZeros=TRUE)`. Many thanks to @marc-outins for the PR.

## BUG FIXES

1. `rbindlist()` of a malformed factor which is missing a levels attribute is now a helpful error rather than a cryptic error about `STRING_ELT`, [#3315](https://github.com/Rdatatable/data.table/issues/3315). Thanks to Michael Chirico for reporting.

2. Forgetting `type=` in `shift(val, "lead")` would segfault, [#3354](https://github.com/Rdatatable/data.table/issues/3354). A helpful error is now produced to indicate `"lead"` is being passed to `n=` rather than the intended `type=` argument. Thanks to @SymbolixAU for reporting.

3. The default print output (top 5 and bottom 5 rows) when ncol>255 could display the columns in the wrong order, [#3306](https://github.com/Rdatatable/data.table/issues/3306). Thanks to Kun Ren for reporting.

4. Grouping by unusual column names such as `by='string_with_\\'` and `keyby="x y"` could fail, [#3319](https://github.com/Rdatatable/data.table/issues/3319) [#3378](https://github.com/Rdatatable/data.table/issues/3378). Thanks to @HughParsonage for reporting and @MichaelChirico for the fixes.

5. `foverlaps()` could return incorrect results for `POSIXct <= 1970-01-01`, [#3349](https://github.com/Rdatatable/data.table/issues/3349). Thanks to @lux5 for reporting.

6. `dcast.data.table` now handles functions passed to `fun.aggregate=` via a variable; e.g., `funs <- list(sum, mean); dcast(..., fun.aggregate=funs`, [#1974](https://github.com/Rdatatable/data.table/issues/1974) [#1369](https://github.com/Rdatatable/data.table/issues/1369) [#2064](https://github.com/Rdatatable/data.table/issues/2064) [#2949](https://github.com/Rdatatable/data.table/issues/2949). Thanks to @sunbee, @Ping2016, @smidelius and @d0rg0ld for reporting.

7. Some non-equijoin cases could segfault, [#3401](https://github.com/Rdatatable/data.table/issues/3401). Thanks to @Gayyam for reporting.

8. `dcast.data.table` could sort rows containing `NA` incorrectly, [#2202](https://github.com/Rdatatable/data.table/issues/2202). Thanks to @Galileo-Galilei for the report.

9. Sorting, grouping and finding unique values of a numeric column containing at most one finite value (such as `c(Inf,0,-Inf)`) could return incorrect results, [#3372](https://github.com/Rdatatable/data.table/issues/3372) [#3381](https://github.com/Rdatatable/data.table/issues/3381); e.g., `data.table(A=c(Inf,0,-Inf), V=1:3)[,sum(V),by=A]` would treat the 3 rows as one group. This was a regression in 1.12.0. Thanks to Nicolas Ampuero for reporting.

10. `:=` with quoted expression and dot alias now works as expected, [#3425](https://github.com/Rdatatable/data.table/pull/3425). Thanks to @franknarf1 for raising and @jangorecki for the PR.

11. A join's result could be incorrectly keyed when a single nomatch occurred at the very beginning while all other values matched, [#3441](https://github.com/Rdatatable/data.table/issues/3441). The incorrect key would cause incorrect results in subsequent queries. Thanks to @symbalex for reporting and @franknarf1 for pinpointing the root cause.

12. `rbind` and `rbindlist(..., use.names=TRUE)` with over 255 columns could return the columns in a random order, [#3373](https://github.com/Rdatatable/data.table/issues/3373). The contents and name of each column was correct but the order that the columns appeared in the result might not have matched the original input.

13. `rbind` and `rbindlist` now combine `integer64` columns together with non-`integer64` columns correctly [#1349](https://github.com/Rdatatable/data.table/issues/1349), and support `raw` columns [#2819](https://github.com/Rdatatable/data.table/issues/2819).

14. `NULL` columns are caught and error appropriately rather than segfault in some cases, [#2303](https://github.com/Rdatatable/data.table/issues/2303) [#2305](https://github.com/Rdatatable/data.table/issues/2305). Thanks to Hugh Parsonage and @franknarf1 for reporting.

15. `melt` would error with 'factor malformed' or segfault in the presence of duplicate column names, [#1754](https://github.com/Rdatatable/data.table/issues/1754). Many thanks to @franknarf1, William Marble, wligtenberg and Toby Dylan Hocking for reproducible examples. All examples have been added to the test suite.

16. Removing a column from a null (0-column) data.table is now a (standard and simpler) warning rather than error, [#2335](https://github.com/Rdatatable/data.table/issues/2335). It is no longer an error to add a column to a null (0-column) data.table.

17. Non-UTF8 strings were not always sorted correctly on Windows (a regression in v1.12.0), [#3397](https://github.com/Rdatatable/data.table/issues/3397) [#3451](https://github.com/Rdatatable/data.table/issues/3451). Many thanks to @shrektan for reporting and fixing.

18. `cbind` with a null (0-column) `data.table` now works as expected, [#3445](https://github.com/Rdatatable/data.table/issues/3445). Thanks to @mb706 for reporting.

19. Subsetting does a better job of catching a malformed `data.table` with error rather than segfault. A column may not be NULL, nor may a column be an object which has columns (such as a `data.frame` or `matrix`). Thanks to a comment and reproducible example in [#3369](https://github.com/Rdatatable/data.table/issues/3369) from Drew Abbot which demonstrated the issue which arose from parsing JSON. The next release will enable `as.data.table` to unpack columns which are `data.frame` to support this use case.

## NOTES

1. When upgrading to 1.12.0 some Windows users might have seen `CdllVersion not found` in some circumstances. We found a way to catch that so the [helpful message](https://twitter.com/MattDowle/status/1084528873549705217) now occurs for those upgrading from versions prior to 1.12.0 too, as well as those upgrading from 1.12.0 to a later version. See item 1 in notes section of 1.12.0 below for more background.

2. v1.12.0 checked itself on loading using `tools::checkMD5sums("data.table")` but this check failed under the `packrat` package manager on Windows because `packrat` appears to modify the DESCRIPTION file of packages it has snapshot, [#3329](https://github.com/Rdatatable/data.table/issues/3329). This check is now removed. The `CdllVersion` check was introduced after the `checkMD5sums()` attempt and is better; e.g., reliable on all platforms.

3. As promised in new feature 6 of v1.11.6 Sep 2018 (see below in this news file), the `datatable.CJ.names` option's default is now `TRUE`. In v1.13.0 it will be removed.

4. Travis CI gains OSX using homebrew llvm for OpenMP support, [#3326](https://github.com/Rdatatable/data.table/issues/3326). Thanks @marcusklik for the PR.

5. Calling `data.table:::print.data.table()` directly (i.e. bypassing method dispatch by using 3 colons) and passing it a 0-column `data.frame` (not `data.table`) now works, [#3363](https://github.com/Rdatatable/data.table/pull/3363). Thanks @heavywatal for the PR.

6. v1.12.0 did not compile on Solaris 10 using Oracle Developer Studio 12.6, [#3285](https://github.com/Rdatatable/data.table/issues/3285). Many thanks to Prof Ripley for providing and testing a patch. For future reference and other package developers, a `const` variable should not be passed to OpenMP's `num_threads()` directive otherwise `left operand must be modifiable lvalue` occurs. This appears to be a compiler bug which is why the specific versions are mentioned in this note.

7. `foverlaps` provides clearer error messages w.r.t. factor and POSIXct interval columns, [#2645](https://github.com/Rdatatable/data.table/issues/2645) [#3007](https://github.com/Rdatatable/data.table/issues/3007) [#1143](https://github.com/Rdatatable/data.table/issues/1143). Thanks to @sritchie73, @msummersgill and @DavidArenburg for the reports.

8. `unique(DT)` checks up-front the types of all the columns and will fail if any column is type `list` even though those `list` columns may not be needed to establish uniqueness. Use `unique(DT, by=...)` to specify columns that are not type `list`. v1.11.8 and before would also correctly fail with the same error, but not when uniqueness had been established in prior columns: it would stop early, not look at the `list` column and return the correct result. Checking up-front was necessary for some internal optimizations and it's probably best to be explicit anyway. Thanks to James Lamb for reporting, [#3332](https://github.com/Rdatatable/data.table/issues/3332). The error message has been embellished :

    ```
    Column 2 of by= (2) is type 'list', not yet supported. Please use the by= argument to specify
    columns with types that are supported.
    ```

9. Reminder that note 11 in v1.11.0 (May 2018) warned that `set2key()` and `key2()` will be removed in May 2019. They have been warning since v1.9.8 (Nov 2016) and their warnings were upgraded to errors in v1.11.0 (May 2018). When they were introduced in version 1.9.4 (Oct 2014) they were marked as 'experimental'.

10. The `key(DT)<-` form of `setkey()` has been warning since at least 2012 to use `setkey()`. The warning is now stronger: `key(x)<-value is deprecated and not supported. Please change to use setkey().`. This warning will be upgraded to error in one year.


# data.table v1.12.0  (13 Jan 2019)

## NEW FEATURES

1. `setDTthreads()` gains `restore_after_fork=`, [#2885](https://github.com/Rdatatable/data.table/issues/2885). The default `NULL` leaves the internal option unchanged which by default is `TRUE`. `data.table` has always switched to single-threaded mode on fork. It used to restore multithreading after a fork too but problems were reported on Mac and Intel OpenMP library (see 1.10.4 notes below). We are now trying again thanks to suggestions and success reported by Kun Ren and Mark Klik in package `fst`. If you experience problems with multithreading after a fork, please restart R and call `setDTthreads(restore_after_fork=FALSE)`.

2. Subsetting, ordering and grouping now use more parallelism. See benchmarks [here](https://h2oai.github.io/db-benchmark/) and Matt Dowle's presentation in October 2018 on YouTube [here](https://youtu.be/Ddr8N9STSuI). These internal changes gave rise to 4 regressions which were found before release thanks to Kun Ren, [#3211](https://github.com/Rdatatable/data.table/issues/3211). He kindly volunteers to 'go-first' and runs data.table through his production systems before release. We are looking for a 'go-second' volunteer please. A request to test before release was tweeted on 17 Dec [here](https://twitter.com/MattDowle/status/1074746218645938176). As usual, all CRAN and Bioconductor packages using data.table (currently 750) have been tested against this release, [#3233](https://github.com/Rdatatable/data.table/issues/3233). There are now 8,000 tests in 13,000 lines of test code; more lines of test code than there is code. Overall coverage has increased to 94% thanks to Michael Chirico.

3. New `frollmean` has been added by Jan Gorecki to calculate _rolling mean_, see `?froll` for documentation. Function name and arguments are experimental. Related to [#2778](https://github.com/Rdatatable/data.table/issues/2778) (and [#624](https://github.com/Rdatatable/data.table/issues/624), [#626](https://github.com/Rdatatable/data.table/issues/626), [#1855](https://github.com/Rdatatable/data.table/issues/1855)). Other rolling statistics will follow.

4. `fread()` can now read a remote compressed file in one step; `fread("https://domain.org/file.csv.bz2")`. The `file=` argument now supports `.gz` and `.bz2` too; i.e. `fread(file="file.csv.gz")` works now where only `fread("file.csv.gz")` worked in 1.11.8.

5. `nomatch=NULL` now does the same as `nomatch=0L` in both `DT[...]` and `foverlaps()`; i.e. discards missing values silently (inner join). The default is still `nomatch=NA` (outer join) for statistical safety so that missing values are retained by default. After several years have elapsed, we will start to deprecate `0L`; please start using `NULL`. In future `nomatch=.(0)` (note that `.()` creates a `list` type and is different to `nomatch=0`) will fill with `0` to save replacing `NA` with `0` afterwards, [#857](https://github.com/Rdatatable/data.table/issues/857).

6. `setnames()` gains `skip_absent` to skip names in `old` that aren't present, [#3030](https://github.com/Rdatatable/data.table/issues/3030). By default `FALSE` so that it is still an error, as before, to attempt to change a column name that is not present. Thanks to @MusTheDataGuy for the suggestion and the PR.

7. `NA` in `between()` and `%between%`'s `lower` and `upper` are now taken as missing bounds and return `TRUE` rather than `NA`. This is now documented.

8. `shift()` now interprets negative values of `n` to mean the opposite `type=`, [#1708](https://github.com/Rdatatable/data.table/issues/1708). When `give.names=TRUE` the result is named using a positive `n` with the appropriate `type=`. Alternatively, a new `type="shift"` names the result using a signed `n` and constant type.

    ```R
    shift(x, n=-5:5, give.names=TRUE)                =>  "_lead_5" ... "_lag_5"
    shift(x, n=-5:5, type="shift", give.names=TRUE)  =>  "_shift_-5" ... "_shift_5"
    ```

9. `fwrite()` now accepts `matrix`, [#2613](https://github.com/Rdatatable/data.table/issues/2613). Thanks to Michael Chirico for the suggestion and Felipe Parages for implementing. For now matrix input is converted to data.table (which can be costly) before writing.

10. `fread()` and `fwrite()` can now handle file names in native and UTF-8 encoding, [#3078](https://github.com/Rdatatable/data.table/issues/3078). Thanks to Daniel Possenriede (@dpprdan) for reporting and fixing.

11. `DT[i]` and `DT[i,cols]` now call internal parallel subsetting code, [#2951](https://github.com/Rdatatable/data.table/issues/2951). Subsetting is significantly faster (as are many other operations) with factor columns rather than character.

    ```R
    N = 2e8                           # 4GB data on 4-core CPU with 16GB RAM
    DT = data.table(ID = sample(LETTERS,N,TRUE),
                    V1 = sample(5,N,TRUE),
                    V2 = runif(N))
    w = which(DT$V1 > 3)              #  select 40% of rows
                                      #  v1.12.0   v1.11.8
    system.time(DT[w])                #     0.8s      2.6s
    DT[, ID := as.factor(ID)]
    system.time(DT[w])                #     0.4s      2.3s
    system.time(DT[w, c("ID","V2")])  #     0.3s      1.9s
    ```

12. `DT[..., .SDcols=]` now accepts `patterns()`; e.g. `DT[..., .SDcols=patterns("^V")]`, for filtering columns according to a pattern (as in `melt.data.table`), [#1878](https://github.com/Rdatatable/data.table/issues/1878). Thanks to many people for pushing for this and @MichaelChirico for ultimately filing the PR. See `?data.table` for full details and examples.

13. `split` data.table method will now preserve attributes, closes [#2047](https://github.com/Rdatatable/data.table/issues/2047). Thanks to @caneff for reporting.

14. `DT[i,j]` now retains user-defined and inherited attributes, [#995](https://github.com/Rdatatable/data.table/issues/995); e.g.

    ```R
    attr(datasets::BOD,"reference")                     # "A1.4, p. 270"
    attr(as.data.table(datasets::BOD)[2],"reference")   # was NULL now "A1.4, p. 270"
    ```

    If a superclass defines attributes that may not be valid after a `[` subset then the superclass should implement its own `[` method to manage those after calling `NextMethod()`.

## BUG FIXES

1. Providing an `i` subset expression when attempting to delete a column correctly failed with helpful error, but when the column was missing too created a new column full of `NULL` values, [#3089](https://github.com/Rdatatable/data.table/issues/3089). Thanks to Michael Chirico for reporting.

2. Column names that look like expressions (e.g. `"a<=colB"`) caused an error when used in `on=` even when wrapped with backticks, [#3092](https://github.com/Rdatatable/data.table/issues/3092). Additionally, `on=` now supports white spaces around operators; e.g. `on = "colA == colB"`. Thanks to @mt1022 for reporting and to @MarkusBonsch for fixing.

3. Unmatched `patterns` in `measure.vars` fail early and with feedback, [#3106](https://github.com/Rdatatable/data.table/issues/3106).

4. `fread(..., skip=)` now skips non-standard `\r` and `\n\r` line endings properly again, [#3006](https://github.com/Rdatatable/data.table/issues/3006). Standard line endings (`\n` Linux/Mac and `\r\n` Windows) were skipped ok. Thanks to @brattono and @tbrycekelly for providing reproducible examples, and @st-pasha for fixing.

5. `fread(..., colClasses=)` could return a corrupted result when a lower type was requested for one or more columns (e.g. reading "3.14" as integer), [#2922](https://github.com/Rdatatable/data.table/issues/2922) [#2863](https://github.com/Rdatatable/data.table/issues/2863) [#3143](https://github.com/Rdatatable/data.table/issues/3143). It now ignores the request as documented and the helpful message in verbose mode is upgraded to warning. In future, coercing to a lower type might be supported (with warning if any accuracy is lost). `"NULL"` is recognized again in both vector and list mode; e.g. `colClasses=c("integer","NULL","integer")` and `colClasses=list(NULL=2, integer=10:40)`. Thanks to Arun Srinivasan, Kun Ren, Henri Ståhl and @kszela24 for reporting.

6. `cube()` will now produce expected order of results, [#3179](https://github.com/Rdatatable/data.table/issues/3179). Thanks to @Henrik-P for reporting.

7. `groupingsets()` groups by empty column set and constant value in `j`, [#3173](https://github.com/Rdatatable/data.table/issues/3173).

8. `split.data.table()` failed if `DT` had a factor column named `"x"`, [#3151](https://github.com/Rdatatable/data.table/issues/3151). Thanks to @tdeenes for reporting and fixing.

9. `fsetequal` now handles properly datasets having last column a character, closes [#2318](https://github.com/Rdatatable/data.table/issues/2318). Thanks to @pschil and @franknarf1 for reporting.

10. `DT[..., .SDcols=integer(0L)]` could fail, [#3185](https://github.com/Rdatatable/data.table/issues/3185). An empty `data.table` is now returned correctly.

11. `as.data.table.default` method will now always copy its input, closes [#3230](https://github.com/Rdatatable/data.table/issues/3230). Thanks to @NikdAK for reporting.

12. `DT[..., .SDcols=integer()]` failed with `.SDcols is numeric but has both +ve and -ve indices`, [#1789](https://github.com/Rdatatable/data.table/issues/1789) and [#3185](https://github.com/Rdatatable/data.table/issues/3185). It now functions as `.SDcols=character()` has done and creates an empty `.SD`. Thanks to Gabor Grothendieck and Hugh Parsonage for reporting. A related issue with empty `.SDcols` was fixed in development before release thanks to Kun Ren's testing, [#3211](https://github.com/Rdatatable/data.table/issues/3211).

13. Multithreaded stability should be much improved with R 3.5+. Many thanks to Luke Tierney for pinpointing a memory issue with package `constellation` caused by `data.table` and his advice, [#3165](https://github.com/Rdatatable/data.table/issues/3165). Luke also added an extra check to R-devel when compiled with `--enable-strict-barrier`. The test suite is run through latest daily R-devel after every commit as usual, but now with `--enable-strict-barrier` on too via GitLab CI ("Extra" badge on the `data.table` homepage) thanks to Jan Gorecki.

14. Fixed an edge-case bug of platform-dependent output of `strtoi("", base = 2L)` on which `groupingsets` had relied, [#3267](https://github.com/Rdatatable/data.table/issues/3267).

## NOTES

1. When data.table loads it now checks its DLL version against the version of its R level code. This is to detect installation issues on Windows when i) the DLL is in use by another R session and ii) the CRAN source version > CRAN binary binary which happens just after a new release (R prompts users to install from source until the CRAN binary is available). This situation can lead to a state where the package's new R code calls old C code in the old DLL; [R#17478](https://bugs.r-project.org/show_bug.cgi?id=17478), [#3056](https://github.com/Rdatatable/data.table/issues/3056). This broken state can persist until, hopefully, you experience a strange error caused by the mismatch. Otherwise, wrong results may occur silently. This situation applies to any R package with compiled code not just data.table, is Windows-only, and is long-standing. It has only recently been understood as it typically only occurs during the few days after each new release until binaries are available on CRAN.

2. When `on=` is provided but not `i=`, a helpful error is now produced rather than silently ignoring `on=`. Thanks to Dirk Eddelbuettel for the idea.

3. `.SDcols=` is more helpful when passed non-existent columns, [#3116](https://github.com/Rdatatable/data.table/issues/3116) and [#3118](https://github.com/Rdatatable/data.table/issues/3118). Thanks to Michael Chirico for the investigation and PR.

4. `update.dev.pkg()` gains `type=` to specify if update should be made from binaries, sources or both. [#3148](https://github.com/Rdatatable/data.table/issues/3148). Thanks to Reino Bruner for the detailed suggestions.

5. `setDT()` improves feedback when passed a ragged list (i.e. where all columns in the list are not the same length), [#3121](https://github.com/Rdatatable/data.table/issues/3121). Thanks @chuk-yong for highlighting.

6. The one and only usage of `UNPROTECT_PTR()` has been removed, [#3232](https://github.com/Rdatatable/data.table/issues/3232). Thanks to Tomas Kalibera's investigation and advice here: https://developer.r-project.org/Blog/public/2018/12/10/unprotecting-by-value/index.html


# data.table v1.11.8  (30 Sep 2018)

## NEW FEATURES

1. `fread()` can now read `.gz` and `.bz2` files directly: `fread("file.csv.gz")`, [#717](https://github.com/Rdatatable/data.table/issues/717) [#3058](https://github.com/Rdatatable/data.table/issues/3058). It uses `R.utils::decompressFile` to decompress to a `tempfile()` which is then read by `fread()` in the usual way. For greater speed on large-RAM servers, it is recommended to use ramdisk for temporary files by setting `TMPDIR` to `/dev/shm` before starting R; see `?tempdir`. The decompressed temporary file is removed as soon as `fread` completes even if there is an error reading the file. Reading a remote compressed file in one step will be supported in the next version; e.g. `fread("https://domain.org/file.csv.bz2")`.

## BUG FIXES

1. Joining two keyed tables using `on=` to columns not forming a leading subset of `key(i)` could result in an invalidly keyed result, [#3061](https://github.com/Rdatatable/data.table/issues/3061). Subsequent queries on the result could then return incorrect results. A warning `longer object length is not a multiple of shorter object length` could also occur. Thanks to @renkun-ken for reporting and the PR.

2. `keyby=` on columns for which an index exists now uses the index (new feature 7 in v1.11.6 below) but if an `i` subset is present in the same query then it could segfault, [#3062](https://github.com/Rdatatable/data.table/issues/3062). Again thanks to @renkun-ken for reporting.

3. Assigning an out-of-range integer to an item in a factor column (a rare operation) correctly created an `NA` in that spot with warning, but now no longer also corrupts the variable being assigned, [#2984](https://github.com/Rdatatable/data.table/issues/2984). Thanks to @radfordneal for reporting and @MarkusBonsch for fixing. Assigning a string which is missing from the factor levels continues to automatically append the string to the factor levels.

4. Assigning a sequence to a column using base R methods (e.g. `DT[["foo"]] = 1:10`) could cause subsetting to fail with `Internal error in subset.c: column <n> is an ALTREP vector`, [#3051](https://github.com/Rdatatable/data.table/issues/3051). Thanks to Michel Lang for reporting.

5. `as.data.table` `matrix` method now properly handles rownames for 0 column data.table output. Thanks @mllg for reporting. Closes [#3149](https://github.com/Rdatatable/data.table/issues/3149).

## NOTES

1. The test suite now turns on R's new _R_CHECK_LENGTH_1_LOGIC2_ to catch when internal use of `&&` or `||` encounter arguments of length more than one. Thanks to Hugh Parsonage for implementing and fixing the problems caught by this.

2. Some namespace changes have been made with respect to melt, dcast and xts. No change is expected but if you do have any trouble, please file an issue.

3. `split.data.table` was exported in v1.11.6 in addition to being registered using `S3method(split, data.table)`. The export has been removed again. It had been added because a user said they found it difficult to find, [#2920](https://github.com/Rdatatable/data.table/issues/2920). But S3 methods are not normally exported explicitly by packages. The proper way to access the `split.data.table` method is to call `split(DT)` where `DT` is a `data.table`. The generic (`base::split` in this case) then dispatches to the `split.data.table` method. v1.11.6 was not on CRAN very long (1 week) so we think it's better to revert this change quickly. To know what methods exist, R provides the `methods()` function.

    ```R
    methods(split)               # all the methods for the split generic
    methods(class="data.table")  # all the generics that data.table has a method for (47 currently)
    ```


# data.table v1.11.6  (19 Sep 2018)

## NEW FEATURES

1. For convenience when some of the files in `fnams` are empty in `rbindlist(lapply(fnams,fread))`, `fread` now reads empty input as a null-data.table with warning rather than error, [#2898](https://github.com/Rdatatable/data.table/issues/2898). For consistency, `fwrite(data.table(NULL))` now creates an empty file and warns instead of error, too.

2. `setcolorder(DT)` without further arguments now defaults to moving the key columns to be first, [#2895](https://github.com/Rdatatable/data.table/issues/2895). Thanks to @jsams for the PR.

3. Attempting to subset on `col` when the column is actually called `Col` will still error, but the error message will helpfully suggest similarly-spelled columns, [#2887](https://github.com/Rdatatable/data.table/issues/2887). This is experimental, applies just to `i` currently, and we look forward to feedback. Thanks to Michael Chirico for the suggestion and PR.

4. `fread()` has always accepted literal data; e.g. `fread("A,B\n1,2\n3,4")`. It now gains explicit `text=`; e.g. `fread(text="A,B\n1,2\n3,4")`. Unlike the first general purpose `input=` argument, the `text=` argument accepts multi-line input; e.g. `fread(text=c("A,B","1,2","3,4"))`, [#1423](https://github.com/Rdatatable/data.table/issues/1423). Thanks to Douglas Clark for the request and Hugh Parsonage for the PR.

5. `fread()` has always accepted system commands; e.g. `fread("grep blah file.txt")`. It now gains explicit `cmd=`; e.g. `fread(cmd="grep blah file.txt")`. Further, if and only if `input=` is a system command and a variable was used to hold that command (`fread(someCommand)` not `fread("grep blah file.txt")`) or a variable is used to construct it (`fread(paste("grep",variable,"file.txt"))`), a message is now printed suggesting `cmd=`. This is to inform all users that there is a potential security concern if you are i) creating apps, and ii) your app takes input from a public user who could be malicious, and iii) input from the malicious user (such as a filename) is passed by your app to `fread()`, and iv) your app in not running in a protected environment. If all 4 conditions hold then the malicious user could provide a system command instead of a filename which `fread()` would run, and that would be a problem too. If the app is not running in a protected environment (e.g. app is running as root) then this could do damage or obtain data you did not intend. Public facing apps should be running with limited operating system permission so that any breach from any source is contained. We agree with [Linus Torvald's advice](https://lkml.org/lkml/2017/11/21/356) on this which boils down to: "when addressing security concerns the first step is do no harm, just inform". If you aren't creating apps or apis that could have a malicious user then there is no risk but we can't distinguish you so we have to inform everyone. Please change to `fread(cmd=...)` at your leisure. The new message can be suppressed with `options(datatable.fread.input.cmd.message=FALSE)`. Passing system commands to `fread()` continues to be recommended and encouraged and is widely used; e.g. via the techniques gathered together in the book [Data Science at the Command Line](https://datascienceatthecommandline.com/). A `warning()` is too strong because best-practice for production systems is to set `options(warn=2)` to tolerate no warnings. Such production systems have no user input and so there is no security risk; we don't want to do harm by breaking production systems via a `warning()` which gets turned into an error by `options(warn=2)`. Now that we have informed all users, we request feedback. There are 3 options for future releases: i) remove the message, ii) leave the message in place, iii) upgrade the message to warning and then eventually error. The default choice is the middle one: leave the message in place.

6. New `options(datatable.CJ.names=TRUE)` changes `CJ()` to auto-name its inputs exactly as `data.table()` does, [#1596](https://github.com/Rdatatable/data.table/issues/1596). Thanks @franknarf1 for the suggestion. Current default is `FALSE`; i.e. no change. The option's default will be changed to `TRUE` in v1.12.0 and then eventually the option will be removed. Any code that depends on `CJ(x,y)$V1` will need to be changed to `CJ(x,y)$x` and is more akin to a bug fix due to the inconsistency with `data.table()`.

7. If an appropriate index exists, `keyby=` will now use it. For example, given `setindex(DT,colA,colB)`, both `DT[,j,keyby=colA]` (a leading subset of the index columns) and `DT[,j,keyby=.(colA,colB)]` will use the index, but not `DT[,j,keyby=.(colB,colA)]`. The option `options(datatable.use.index=FALSE)` will turn this feature off. Please always use `keyby=` unless you wish to retain the order of groups by first-appearance order (in which case use `by=`). Also, both `keyby=` and `by=` already used the key where possible but are now faster when using just the first column of the key. As usual, setting `verbose=TRUE` either per-query or globally using `options(datatable.verbose=TRUE)` will report what's being done internally.

## BUG FIXES

1. `fread` now respects the order of columns passed to `select=` when column numbers are used, [#2986](https://github.com/Rdatatable/data.table/issues/2986). It already respected the order when column names are used. Thanks @privefl for raising the issue.

2. `gmin` and `gmax` no longer fail on _ordered_ factors, [#1947](https://github.com/Rdatatable/data.table/issues/1947). Thanks to @mcieslik-mctp for identifying and @mbacou for the nudge.

3. `as.ITime.character` now properly handles NA when attempting to detect the format of non-NA values in vector. Thanks @polyjian for reporting, closes [#2940](https://github.com/Rdatatable/data.table/issues/2940).

4. `as.matrix(DT, rownames="id")` now works when `DT` has a single row, [#2930](https://github.com/Rdatatable/data.table/issues/2930). Thanks to @malcook for reporting and @sritchie73 for fixing. The root cause was the dual meaning of the `rownames=` argument: i) a single column name/number (most common), or ii) rowname values length 1 for the single row. For clarity and safety, `rownames.value=` has been added. Old usage (i.e. `length(rownames)>1`) continues to work for now but will issue a warning in a future release, and then error in a release after that.

5. Fixed regression in v1.11.0 (May 2018) caused by PR [#2389](https://github.com/Rdatatable/data.table/pull/2389) which introduced partial key retainment on `:=` assigns. This broke the joining logic that assumed implicitly that assigning always drops keys completely. Consequently, join and subset results could be wrong when matching character to factor columns with existing keys, [#2881](https://github.com/Rdatatable/data.table/issues/2881). Thanks to @ddong63 for reporting and to @MarkusBonsch for fixing. Missing test added to ensure this doesn't arise again.

6. `as.IDate.numeric` no longer ignores "origin", [#2880](https://github.com/Rdatatable/data.table/issues/2880). Thanks to David Arenburg for reporting and fixing.

7. `as.ITime.times` was rounding fractional seconds while other methods were truncating, [#2870](https://github.com/Rdatatable/data.table/issues/2870). The `as.ITime` method gains `ms=` taking `"truncate"` (default), `"nearest"` and `"ceil"`. Thanks to @rossholmberg for reporting and Michael Chirico for fixing.

8. `fwrite()` now writes POSIXct dates after 2038 correctly, [#2995](https://github.com/Rdatatable/data.table/issues/2995). Thanks to Manfred Zorn for reporting and Philippe Chataignon for the PR fixing it.

9. `fsetequal` gains the `all` argument to make it consistent with the other set operator functions `funion`, `fsetdiff` and `fintersect` [#2968](https://github.com/Rdatatable/data.table/issues/2968). When `all = FALSE` `fsetequal` will treat rows as elements in a set when checking whether two `data.tables` are equal (i.e. duplicate rows will be ignored). For now the default value is `all = TRUE` for backwards compatibility, but this will be changed to `all = FALSE` in a future release to make it consistent with the other set operation functions. Thanks to @franknarf1 for reporting and @sritchie73 for fixing.

10. `fintersect` failed on tables with a column called `y`, [#3034](https://github.com/Rdatatable/data.table/issues/3034). Thanks to Maxim Nazarov for reporting.

11. Compilation fails in AIX because NAN and INFINITY macros definition in AIX make them not constant literals, [#3043](https://github.com/Rdatatable/data.table/pull/3043). Thanks to Ayappan for reporting and fixing.

12. The introduction of altrep in R 3.5.0 caused some performance regressions of about 20% in some cases, [#2962](https://github.com/Rdatatable/data.table/issues/2962). Investigating this led to some improvements to grouping which are faster than before R 3.5.0 in some cases. Thanks to Nikolay S. for reporting. The work to accomodate altrep is not complete but it is better and it is highly recommended to upgrade to this update.

13. Fixed 7 memory faults thanks to CRAN's [`rchk`](https://github.com/kalibera/rchk) tool by Tomas Kalibera, [#3033](https://github.com/Rdatatable/data.table/pull/3033).

## NOTES

1. The type coercion warning message has been improved, [#2989](https://github.com/Rdatatable/data.table/pull/2989). Thanks to @sarahbeeysian on Twitter for highlighting. For example, given the follow statements:

    ```R
    DT = data.table(id=1:3)
    DT[2, id:="foo"]
    ```

    the warning message has changed from :

    ```
    Coerced character RHS to integer to match the column's type. Either change the target column
    ['id'] to character first (by creating a new character vector length 3 (nrows of entire table) and
    assign that; i.e. 'replace' column), or coerce RHS to integer (e.g. 1L, NA_[real|integer]_, as.*,
    etc) to make your intent clear and for speed. Or, set the column type correctly up front when you
    create the table and stick to it, please.
    ```

    to :

    ```
    Coerced character RHS to integer to match the type of the target column (column 1 named 'id'). If
    the target column's type integer is correct, it's best for efficiency to avoid the coercion and
    create the RHS as type integer. To achieve that consider the L postfix: typeof(0L) vs typeof(0),
    and typeof(NA) vs typeof(NA_integer_) vs typeof(NA_real_). Wrapping the RHS with as.integer() will
    avoid this warning but still perform the coercion. If the target column's type is not correct, it
    is best to revisit where the DT was created and fix the column type there; e.g., by using
    colClasses= in fread(). Otherwise, you can change the column type now by plonking a new column (of
    the desired type) over the top of it; e.g. DT[, `id`:=as.character(`id`)]. If the RHS of := has
    nrow(DT) elements then the assignment is called a column plonk and is the way to change a column's
    type. Column types can be observed with sapply(DT,typeof).
    ```

    Further, if a coercion from double to integer is performed, fractional data such as 3.14 is now detected and the truncation to 3 is warned about if and only if truncation has occurred.

    ```R
    DT = data.table(v=1:3)
    DT[2, v:=3.14]
    Warning message:
      Coerced double RHS to integer to match the type of the target column (column 1 named 'v'). One
      or more RHS values contain fractions which have been lost; e.g. item 1 with value 3.140000 has
      been truncated to 3.
    ```

2. `split.data.table` method is now properly exported, [#2920](https://github.com/Rdatatable/data.table/issues/2920). But we don't recommend it because `split` copies all the pieces into new memory.

3. Setting indices on columns which are part of the key will now create those indices.

4. `hour`, `minute`, and `second` utility functions use integer arithmetic when the input is already (explicitly) UTC-based `POSIXct` for 4-10x speedup vs. using `as.POSIXlt`.

5. Error added for incorrect usage of `%between%`, with some helpful diagnostic hints, [#3014](https://github.com/Rdatatable/data.table/issues/3014). Thanks @peterlittlejohn for offering his user experience and providing the impetus.


# data.table v1.11.4  (27 May 2018)

1. Empty RHS of `:=` is no longer an error when the `i` clause returns no rows to assign to anyway, [#2829](https://github.com/Rdatatable/data.table/issues/2829). Thanks to @cguill95 for reporting and to @MarkusBonsch for fixing.

2. Fixed runaway memory usage with R-devel (R > 3.5.0), [#2882](https://github.com/Rdatatable/data.table/pull/2882). Thanks to many people but in particular to Trang Nguyen for making the breakthrough reproducible example, Paul Bailey for liaising, and Luke Tierney for then pinpointing the issue. It was caused by an interaction of two or more data.table threads operating on new compact vectors in the ALTREP framework, such as the sequence `1:n`. This interaction could result in R's garbage collector turning off, and hence the memory explosion. Problems may occur in R 3.5.0 too but we were only able to reproduce in R > 3.5.0. The R code in data.table's implementation benefits from ALTREP (`for` loops in R no longer allocate their range vector input, for example) but are not so appropriate as data.table columns. Sequences such as `1:n` are common in test data but not very common in real-world datasets. Therefore, there is no need for data.table to support columns which are ALTREP compact sequences. The `data.table()` function already expanded compact vectors (by happy accident) but `setDT()` did not (it now does). If, somehow, a compact vector still reaches the internal parallel regions, a helpful error will now be generated. If this happens, please report it as a bug.

3. Tests 1590.3 & 1590.4 now pass when users run `test.data.table()` on Windows, [#2856](https://github.com/Rdatatable/data.table/pull/2856). Thanks to Avraham Adler for reporting. Those tests were passing on AppVeyor, win-builder and CRAN's Windows because `R CMD check` sets `LC_COLLATE=C` as documented in R-exts$1.3.1, whereas by default on Windows `LC_COLLATE` is usually a regional Windows-1252 dialect such as `English_United States.1252`.

4. Around 1 billion very small groups (of size 1 or 2 rows) could result in `"Failed to realloc working memory"` even when plenty of memory is available, [#2777](https://github.com/Rdatatable/data.table/issues/2777). Thanks once again to @jsams for the detailed report as a follow up to bug fix 40 in v1.11.0.


# data.table v1.11.2  (08 May 2018)

1. `test.data.table()` created/overwrote variable `x` in `.GlobalEnv`, [#2828](https://github.com/Rdatatable/data.table/issues/2828); i.e. a modification of user's workspace which is not allowed. Thanks to @etienne-s for reporting.

2. `as.chron` methods for `IDate` and `ITime` have been removed, [#2825](https://github.com/Rdatatable/data.table/issues/2825). `as.chron` still works since `IDate` inherits from `Date`. We are not sure why we had specific methods in the first place. It may have been from a time when `IDate` did not inherit from `Date`, perhaps. Note that we don't use `chron` ourselves in our own work.

3. Fixed `SETLENGTH() cannot be applied to an ALTVEC object` starting in R-devel (R 3.6.0) on 1 May 2018, a few hours after 1.11.0 was accepted on CRAN, [#2820](https://github.com/Rdatatable/data.table/issues/2820). Many thanks to Luke Tierney for pinpointing the problem.

4. Fixed some rare memory faults in `fread()` and `rbindlist()` found with `gctorture2()` and [`rchk`](https://github.com/kalibera/rchk), [#2841](https://github.com/Rdatatable/data.table/issues/2841).


# data.table v1.11.0  (01 May 2018)

## NOTICE OF INTENDED FUTURE POTENTIAL BREAKING CHANGES

1. `fread()`'s `na.strings=` argument :

    ```R
    "NA"                                      # old default
    getOption("datatable.na.strings", "NA")   # this release; i.e. the same; no change yet
    getOption("datatable.na.strings", "")     # future release
    ```

    This option controls how `,,` is read in character columns. It does not affect numeric columns which read `,,` as `NA` regardless. We would like `,,`=>`NA` for consistency with numeric types, and `,"",`=>empty string to be the standard default for `fwrite/fread` character columns so that `fread(fwrite(DT))==DT` without needing any change to any parameters. `fwrite` has never written `NA` as `"NA"` in case `"NA"` is a valid string in the data; e.g., 2 character id columns sometimes do. Instead, `fwrite` has always written `,,` by default for an `<NA>` in a character columns. The use of R's `getOption()` allows users to move forward now, using `options(datatable.fread.na.strings="")`, or restore old behaviour when the default's default is changed in future, using `options(datatable.fread.na.strings="NA")`.

2. `fread()` and `fwrite()`'s `logical01=` argument :

    ```R
    logical01 = FALSE                         # old default
    getOption("datatable.logical01", FALSE)   # this release; i.e. the same; no change yet
    getOption("datatable.logical01", TRUE)    # future release
    ```

    This option controls whether a column of all 0's and 1's is read as `integer`, or `logical` directly to avoid needing to change the type afterwards to `logical` or use `colClasses`. `0/1` is smaller and faster than `"TRUE"/"FALSE"`, which can make a significant difference to space and time the more `logical` columns there are. When the default's default changes to `TRUE` for `fread` we do not expect much impact since all arithmetic operators that are currently receiving 0's and 1's as type `integer` (think `sum()`) but instead could receive `logical`, would return exactly the same result on the 0's and 1's as `logical` type. However, code that is manipulating column types using `is.integer` or `is.logical` on `fread`'s result, could require change. It could be painful if `DT[(logical_column)]` (i.e. `DT[logical_column==TRUE]`) changed behaviour due to `logical_column` no longer being type `logical` but `integer`. But that is not the change proposed. The change is the other way around; i.e., a previously `integer` column holding only 0's and 1's would now be type `logical`. Since it's that way around, we believe the scope for breakage is limited. We think a lot of code is converting 0/1 integer columns to logical anyway, either using `colClasses=` or afterwards with an assign. For `fwrite`, the level of breakage depends on the consumer of the output file. We believe `0/1` is a better more standard default choice to move to. See notes below about improvements to `fread`'s sampling for type guessing, and automatic rereading in the rare cases of out-of-sample type surprises.


These options are meant for temporary use to aid your migration, [#2652](https://github.com/Rdatatable/data.table/pull/2652). You are not meant to set them to the old default and then not migrate your code that is dependent on the default. Either set the argument explicitly so your code is not dependent on the default, or change the code to cope with the new default. Over the next few years we will slowly start to remove these options, warning you if you are using them, and return to a simple default. See the history of NEWS and NEWS.0 for past migrations that have, generally speaking, been successfully managed in this way. For example, at the end of NOTES for this version (below in this file) is a note about the usage of `datatable.old.unique.by.key` now warning, as you were warned it would do over a year ago. When that change was introduced, the default was changed and that option provided an option to restore the old behaviour. These `fread`/`fwrite` changes are even more cautious and not even changing the default's default yet. Giving you extra warning by way of this notice to move forward. And giving you a chance to object.

## NEW FEATURES

1. `fread()`:
    * Efficiency savings at C level including **parallelization** announced [here](https://github.com/Rdatatable/data.table/wiki/talks/BARUG_201704_ParallelFread.pdf); e.g. a 9GB 2 column integer csv input is **50s down to 12s** to cold load on a 4 core laptop with 16GB RAM and SSD. Run `echo 3 >/proc/sys/vm/drop_caches` first to measure cold load time. Subsequent load time (after file has been cached by OS on the first run) **40s down to 6s**.
    * The [fread for small data](https://github.com/Rdatatable/data.table/wiki/Convenience-features-of-fread) page has been revised.
    * Memory maps lazily; e.g. reading just the first 10 rows with `nrow=10` is **12s down to 0.01s** from cold for the 9GB file. Large files close to your RAM limit may work more reliably too. The progress meter will commence sooner and more consistently.
    * `fread` has always jumped to the middle and to the end of the file for a much improved column type guess. The sample size is increased from 100 rows at 10 jump jump points (1,000 rows) to 100 rows at 100 jumps points (10,000 row sample). In the rare case of there still being out-of-sample type exceptions, those columns are now *automatically reread* so you don't have to use `colClasses` yourself.
    * Large number of columns support; e.g. **12,000 columns** tested.
    * **Quoting rules** are more robust and flexible. See point 10 on the wiki page [here](https://github.com/Rdatatable/data.table/wiki/Convenience-features-of-fread#10-automatic-quote-escape-method-detection-including-no-escape).
    * Numeric data that has been quoted is now detected and read as numeric.
    * The ability to position `autostart` anywhere inside one of multiple tables in a single file is removed with warning. It used to search upwards from that line to find the start of the table based on a consistent number of columns. People appear to be using `skip="string"` or `skip=nrow` to find the header row exactly, which is retained and simpler. It was too difficult to retain search-upwards-autostart together with skipping/filling blank lines, filling incomplete rows and parallelization too. If there is any header info above the column names, it is still auto detected and auto skipped (particularly useful when loading a set of files where the column names start on different lines due to a varying height messy header).
    * `dec=','` is now implemented directly so there is no dependency on locale. The options `datatable.fread.dec.experiment` and `datatable.fread.dec.locale` have been removed.
    * `\\r\\r\\n` line endings are now handled such as produced by `base::download.file()` when it doubles up `\\r`. Other rare line endings (`\\r` and `\\n\\r`) are now more robust.
    * Mixed line endings are now handled; e.g. a file formed by concatenating a Unix file and a Windows file so that some lines end with `\\n` while others end with `\\r\\n`.
    * Improved automatic detection of whether the first row is column names by comparing the types of the fields on the first row against the column types ascertained by the 10,000 rows sample (or `colClasses` if provided). If a numeric column has a string value at the top, then column names are deemed present.
    * Detects GB-18030 and UTF-16 encodings and in verbose mode prints a message about BOM detection.
    * Detects and ignores trailing ^Z end-of-file control character sometimes created on MS DOS/Windows, [#1612](https://github.com/Rdatatable/data.table/issues/1612). Thanks to Gergely Daróczi for reporting and providing a file.
    * Added ability to recognize and parse hexadecimal floating point numbers, as used for example in Java. Thanks for @scottstanfield [#2316](https://github.com/Rdatatable/data.table/issues/2316) for the report.
    * Now handles floating-point NaN values in a wide variety of formats, including `NaN`, `sNaN`, `1.#QNAN`, `NaN1234`, `#NUM!` and others, [#1800](https://github.com/Rdatatable/data.table/issues/1800). Thanks to Jori Liesenborgs for highlighting and the PR.
    * If negative numbers are passed to `select=` the out-of-range error now suggests `drop=` instead, [#2423](https://github.com/Rdatatable/data.table/issues/2423). Thanks to Michael Chirico for the suggestion.
    * `sep=NULL` or `sep=""` (i.e., no column separator) can now be used to specify single column input reliably like `base::readLines`, [#1616](https://github.com/Rdatatable/data.table/issues/1616). `sep='\\n'` still works (even on Windows where line ending is actually `\\r\\n`) but `NULL` or `""` are now documented and recommended. Thanks to Dmitriy Selivanov for the pull request and many others for comments. As before, `sep=NA` is not valid; use the default `"auto"` for automatic separator detection. `sep='\\n'` is now deprecated and in future will start to warn when used.
    * Single-column input with blank lines is now valid and the blank lines are significant (representing `NA`). The blank lines are significant even at the very end, which may be surprising on first glance. The change is so that `fread(fwrite(DT))==DT` for single-column inputs containing `NA` which are written as blank. There is no change when `ncol>1`; i.e., input stops with detailed warning at the first blank line, because a blank line when `ncol>1` is invalid input due to no separators being present. Thanks to @skanskan, Michael Chirico, @franknarf1 and Pasha for the testing and discussions, [#2106](https://github.com/Rdatatable/data.table/issues/2106).
    * Too few column names are now auto filled with default column names, with warning, [#1625](https://github.com/Rdatatable/data.table/issues/1625). If there is just one missing column name it is guessed to be for the first column (row names or an index), otherwise the column names are filled at the end. Similarly, too many column names now automatically sets `fill=TRUE`, with warning.
    * `skip=` and `nrow=` are more reliable and are no longer affected by invalid lines outside the range specified. Thanks to Ziyad Saeed and Kyle Chung for reporting, [#1267](https://github.com/Rdatatable/data.table/issues/1267).
    * Ram disk (`/dev/shm`) is no longer used for the output of system command input. Although faster when it worked, it was causing too many device full errors; e.g., [#1139](https://github.com/Rdatatable/data.table/issues/1139) and [zUMIs/19](https://github.com/sdparekh/zUMIs/issues/19). Thanks to Kyle Chung for reporting. Standard `tempdir()` is now used. If you wish to use ram disk, set TEMPDIR to `/dev/shm`; see `?tempdir`.
    * Detecting whether a very long input string is a file name or data is now much faster, [#2531](https://github.com/Rdatatable/data.table/issues/2531). Many thanks to @javrucebo for the detailed report, benchmarks and suggestions.
    * A column of `TRUE/FALSE`s is ok, as well as `True/False`s and `true/false`s, but mixing styles (e.g. `TRUE/false`) is not and will be read as type `character`.
    * New argument `index` to compliment the existing `key` argument for applying secondary orderings out of the box for convenience, [#2633](https://github.com/Rdatatable/data.table/issues/2633).
    * A warning is now issued whenever incorrectly quoted fields have been detected and fixed using a non-standard quote rule. `fread` has always used these advanced rules but now it warns that it is using them. Most file writers correctly quote fields if the field contains the field separator, but a common error is not to also quote fields that contain a quote and then escape those quotes, particularly if that quote occurs at the start of the field. The ability to detect and fix such files is referred to as self-healing. Ambiguities are resolved using the knowledge that the number of columns is constant, and therefore this ability is not available when `fill=TRUE`. This feature can be improved in future by using column type consistency as well as the number of fields. For example:

    ```R
    txt = 'A,B\n1,hello\n2,"howdy" said Joe\n3,bonjour\n'
    cat(txt)
    # A,B
    # 1,hello
    # 2,"howdy" said Joe
    # 3,bonjour
    fread(txt)
           A                B
       <int>           <char>
    1:     1            hello
    2:     2 "howdy" said Joe
    3:     3          bonjour
    Warning message:
    In fread(txt) : Found and resolved improper quoting
    ```

    * Many thanks to @yaakovfeldman, Guillermo Ponce, Arun Srinivasan, Hugh Parsonage, Mark Klik, Pasha Stetsenko, Mahyar K, Tom Crockett, @cnoelke, @qinjs, @etienne-s, Mark Danese, Avraham Adler, @franknarf1, @MichaelChirico, @tdhock, Luke Tierney, Ananda Mahto, @memoryfull, @brandenkmurray for testing dev and reporting these regressions before release to CRAN: #1464, #1671, #1888, #1895, #2070, #2073, #2087, #2091, #2092, #2107, #2118, #2123, #2167, #2194, #2196, #2201, #2222, #2228, #2238, #2246, #2251, #2265, #2267, #2285, #2287, #2299, #2322, #2347, #2352, #2370, #2371, #2395, #2404, #2446, #2453, #2457, #2464, #2481, #2499, #2512, #2515, #2516, #2518, #2520, #2523, #2526, #2535, #2542, #2548, #2561, #2600, #2625, #2666, #2697, #2735, #2744.

2. `fwrite()`:
    * empty strings are now always quoted (`,"",`) to distinguish them from `NA` which by default is still empty (`,,`) but can be changed using `na=` as before. If `na=` is provided and `quote=` is the default `'auto'` then `quote=` is set to `TRUE` so that if the `na=` value occurs in the data, it can be distinguished from `NA`. Thanks to Ethan Welty for the request [#2214](https://github.com/Rdatatable/data.table/issues/2214) and Pasha for the code change and tests, [#2215](https://github.com/Rdatatable/data.table/issues/2215).
    * `logical01` has been added and the old name `logicalAsInt` retained. Pease move to the new name when convenient for you. The old argument name (`logicalAsInt`) will slowly be deprecated over the next few years. The default is unchanged: `FALSE`, so `logical` is still written as `"TRUE"`/`"FALSE"` in full by default. We intend to change the default's default in future to `TRUE`; see the notice at the top of these release notes.

3. Added helpful message when subsetting by a logical column without wrapping it in parentheses, [#1844](https://github.com/Rdatatable/data.table/issues/1844). Thanks @dracodoc for the suggestion and @MichaelChirico for the PR.

4. `tables` gains `index` argument for supplementary metadata about `data.table`s in memory (or any optionally specified environment), part of [#1648](https://github.com/Rdatatable/data.table/issues/1648). Thanks due variously to @jangorecki, @rsaporta, @MichaelChirico for ideas and work towards PR.

5. Improved auto-detection of `character` inputs' formats to `as.ITime` to mirror the logic in `as.POSIXlt.character`, [#1383](https://github.com/Rdatatable/data.table/issues/1383) Thanks @franknarf1 for identifying a discrepancy and @MichaelChirico for investigating.

6. `setcolorder()` now accepts less than `ncol(DT)` columns to be moved to the front, [#592](https://github.com/Rdatatable/data.table/issues/592). Thanks @MichaelChirico for the PR. This also incidentally fixed [#2007](https://github.com/Rdatatable/data.table/issues/2007) whereby explicitly setting `select = NULL` in `fread` errored; thanks to @rcapell for reporting that and @dselivanov and @MichaelChirico for investigating and providing a new test.

7. Three new *Grouping Sets* functions: `rollup`, `cube` and `groupingsets`, [#1377](https://github.com/Rdatatable/data.table/issues/1377). Allows to aggregation on various grouping levels at once producing sub-totals and grand total.

8. `as.data.table()` gains new method for `array`s to return a useful data.table, [#1418](https://github.com/Rdatatable/data.table/issues/1418).

9. `print.data.table()` (all via master issue  [#1523](https://github.com/Rdatatable/data.table/issues/1523)):

    * gains `print.keys` argument, `FALSE` by default, which displays the keys and/or indices (secondary keys) of a `data.table`. Thanks @MichaelChirico for the PR, Yike Lu for the suggestion and Arun for honing that idea to its present form.

    * gains `col.names` argument, `"auto"` by default, which toggles which registers of column names to include in printed output. `"top"` forces `data.frame`-like behavior where column names are only ever included at the top of the output, as opposed to the default behavior which appends the column names below the output as well for longer (>20 rows) tables. `"none"` shuts down column name printing altogether. Thanks @MichaelChirico for the PR, Oleg Bondar for the suggestion, and Arun for guiding commentary.

    * list columns would print the first 6 items in each cell followed by a comma if there are more than 6 in that cell. Now it ends ",..." to make it clearer, part of [#1523](https://github.com/Rdatatable/data.table/issues/1523). Thanks to @franknarf1 for drawing attention to an issue raised on Stack Overflow by @TMOTTM [here](https://stackoverflow.com/q/47679701).

10. `setkeyv` accelerated if key already exists [#2331](https://github.com/Rdatatable/data.table/issues/2331). Thanks to @MarkusBonsch for the PR.

11. Keys and indexes are now partially retained up to the key column assigned to with ':=' [#2372](https://github.com/Rdatatable/data.table/issues/2372). They used to be dropped completely if any one of the columns was affected by `:=`. Tanks to @MarkusBonsch for the PR.

12. Faster `as.IDate` and `as.ITime` methods for `POSIXct` and `numeric`, [#1392](https://github.com/Rdatatable/data.table/issues/1392). Thanks to Jan Gorecki for the PR.

13. `unique(DT)` now returns `DT` early when there are no duplicates to save RAM, [#2013](https://github.com/Rdatatable/data.table/issues/2013). Thanks to Michael Chirico for the PR, and thanks to @mgahan for pointing out a reversion in `na.omit.data.table` before release, [#2660](https://github.com/Rdatatable/data.table/issues/2660#issuecomment-371027948).

14. `uniqueN()` is now faster on logical vectors. Thanks to Hugh Parsonage for [PR#2648](https://github.com/Rdatatable/data.table/pull/2648).

    ```R
    N = 1e9
                                       #   was      now
    x = c(TRUE,FALSE,NA,rep(TRUE,N))   #
    uniqueN(x) == 3                    #  5.4s    0.00s
    x = c(TRUE,rep(FALSE,N), NA)       #
    uniqueN(x,na.rm=TRUE) == 2         #  5.4s    0.00s
    x = c(rep(TRUE,N),FALSE,NA)        #
    uniqueN(x) == 3                    #  6.7s    0.38s
    ```

15. Subsetting optimization with keys and indices is now possible for compound queries like `DT[a==1 & b==2]`, [#2472](https://github.com/Rdatatable/data.table/issues/2472).
Thanks to @MichaelChirico for reporting and to @MarkusBonsch for the implementation.

16. `melt.data.table` now offers friendlier functionality for providing `value.name` for `list` input to `measure.vars`, [#1547](https://github.com/Rdatatable/data.table/issues/1547). Thanks @MichaelChirico and @franknarf1 for the suggestion and use cases, @jangorecki and @mrdwab for implementation feedback, and @MichaelChirico for ultimate implementation.

17. `update.dev.pkg` is new function to update package from development repository, it will download package sources only when newer commit is available in repository. `data.table::update.dev.pkg()` defaults updates `data.table`, but any package can be used.

18. Item 1 in NEWS for [v1.10.2](https://github.com/Rdatatable/data.table/blob/master/NEWS.md#changes-in-v1102--on-cran-31-jan-2017) on CRAN in Jan 2017 included :

    > When j is a symbol prefixed with `..` it will be looked up in calling scope and its value taken to be column names or numbers.
    > When you see the `..` prefix think one-level-up, like the directory `..` in all operating systems means the parent directory.
    > In future the `..` prefix could be made to work on all symbols apearing anywhere inside `DT[...]`.

    The response has been positive ([this tweet](https://twitter.com/MattDowle/status/967290562725359617) and [FR#2655](https://github.com/Rdatatable/data.table/issues/2655)) and so this prefix is now expanded to all symbols appearing in `j=` as a first step; e.g.

    ```R
    cols = "colB"
    DT[, c(..cols, "colC")]   # same as DT[, .(colB,colC)]
    DT[, -..cols]             # all columns other than colB
    ```

    Thus, `with=` should no longer be needed in any cases. Please change to using the `..` prefix and over the next few years we will start to formally deprecate and remove the `with=` parameter.  If this is well received, the `..` prefix could be expanded to symbols appearing in `i=` and `by=`, too. Note that column names should not now start with `..`. If a symbol `..var` is used in `j=` but `..var` exists as a column name, the column still takes precedence, for backwards compatibility. Over the next few years, data.table will start issuing warnings/errors when it sees column names starting with `..`. This affects one CRAN package out of 475 using data.table, so we do not believe this restriction to be unreasonable. Our main focus here which we believe `..` achieves is to resolve the more common ambiguity when `var` is in calling scope and `var` is a column name too. Further, we have not forgotten that in the past we recommended prefixing the variable in calling scope with `..` yourself. If you did that and `..var` exists in calling scope, that still works, provided neither `var` exists in calling scope nor `..var` exists as a column name. Please now remove the `..` prefix on `..var` in calling scope to tidy this up. In future data.table will start to warn/error on such usage.

19. `setindexv` can now assign multiple (separate) indices by accepting a `list` in the `cols` argument.

20. `as.matrix.data.table` method now has an additional `rownames` argument allowing for a single column to be used as the `rownames` after conversion to a `matrix`. Thanks to @sritchie73 for the suggestion, use cases, [#2692](https://github.com/Rdatatable/data.table/issues/2692) and implementation [PR#2702](https://github.com/Rdatatable/data.table/pull/2702) and @MichaelChirico for additional use cases.

## BUG FIXES

1. The new quote rules handles this single field `"Our Stock Screen Delivers an Israeli Software Company (MNDO, CTCH)<\/a> SmallCapInvestor.com - Thu, May 19, 2011 10:02 AM EDT<\/cite><\/div>Yesterday in \""Google, But for Finding
 Great Stocks\"", I discussed the value of stock screeners as a powerful tool"`, [#2051](https://github.com/Rdatatable/data.table/issues/2051). Thanks to @scarrascoso for reporting. Example file added to test suite.

2. `fwrite()` creates a file with permissions that now play correctly with `Sys.umask()`, [#2049](https://github.com/Rdatatable/data.table/issues/2049). Thanks to @gnguy for reporting.

3. `fread()` no longer holds an open lock on the file when a line outside the large sample has too many fields and generates an error, [#2044](https://github.com/Rdatatable/data.table/issues/2044). Thanks to Hugh Parsonage for reporting.

4. Setting `j = {}` no longer results in an error, [#2142](https://github.com/Rdatatable/data.table/issues/2142). Thanks Michael Chirico for the pull request.

5. Segfault in `rbindlist()` when one or more items are empty, [#2019](https://github.com/Rdatatable/data.table/issues/2019). Thanks Michael Lang for the pull request. Another segfault if the result would be more than 2bn rows, thanks to @jsams's comment in [#2340](https://github.com/Rdatatable/data.table/issues/2340#issuecomment-331505494).

6. Error printing 0-length `ITime` and `NA` objects, [#2032](https://github.com/Rdatatable/data.table/issues/2032) and [#2171](https://github.com/Rdatatable/data.table/issues/2171). Thanks Michael Chirico for the pull requests and @franknarf1 for pointing out a shortcoming of the initial fix.

7. `as.IDate.POSIXct` error with `NULL` timezone, [#1973](https://github.com/Rdatatable/data.table/issues/1973). Thanks @lbilli for reporting and Michael Chirico for the pull request.

8. Printing a null `data.table` with `print` no longer visibly outputs `NULL`, [#1852](https://github.com/Rdatatable/data.table/issues/1852). Thanks @aaronmcdaid for spotting and @MichaelChirico for the PR.

9. `data.table` now works with Shiny Reactivity / Flexdashboard. The error was typically something like `col not found` in `DT[col==val]`. Thanks to Dirk Eddelbuettel leading Matt through reproducible steps and @sergeganakou and Richard White for reporting. Closes [#2001](https://github.com/Rdatatable/data.table/issues/2001) and [shiny/#1696](https://github.com/rstudio/shiny/issues/1696).

10. The `as.IDate.POSIXct` method passed `tzone` along but was not exported. So `tzone` is now taken into account by `as.IDate` too as well as `IDateTime`, [#977](https://github.com/Rdatatable/data.table/issues/977) and [#1498](https://github.com/Rdatatable/data.table/issues/1498). Tests added.

11. Named logical vector now select rows as expected from single row data.table. Thanks to @skranz for reporting. Closes [#2152](https://github.com/Rdatatable/data.table/issues/2152).

12. `fread()`'s rare `Internal error: Sampling jump point 10 is before the last jump ended` has been fixed, [#2157](https://github.com/Rdatatable/data.table/issues/2157). Thanks to Frank Erickson and Artem Klevtsov for reporting with example files which are now added to the test suite.

13. `CJ()` no longer loses attribute information, [#2029](https://github.com/Rdatatable/data.table/issues/2029). Thanks to @MarkusBonsch and @royalts for the pull request.

14. `split.data.table` respects `factor` ordering in `by` argument, [#2082](https://github.com/Rdatatable/data.table/issues/2082). Thanks to @MichaelChirico for identifying and fixing the issue.

15. `.SD` would incorrectly include symbol on lhs of `:=` when `.SDcols` is specified and `get()` appears in `j`. Thanks @renkun-ken for reporting and the PR, and @ProfFancyPants for reporing a regression introduced in the PR. Closes [#2326](https://github.com/Rdatatable/data.table/issues/2326) and [#2338](https://github.com/Rdatatable/data.table/issues/2338).

16. Integer values that are too large to fit in `int64` will now be read as strings [#2250](https://github.com/Rdatatable/data.table/issues/2250).

17. Internal-only `.shallow` now retains keys correctly, [#2336](https://github.com/Rdatatable/data.table/issues/2336). Thanks to @MarkusBonsch for reporting, fixing ([PR #2337](https://github.com/Rdatatable/data.table/pull/2337)) and adding 37 tests. This much advances the journey towards exporting `shallow()`, [#2323](https://github.com/Rdatatable/data.table/issues/2323).

18. `isoweek` calculation is correct regardless of local timezone setting (`Sys.timezone()`), [#2407](https://github.com/Rdatatable/data.table/issues/2407). Thanks to @MoebiusAV and @SimonCoulombe for reporting and @MichaelChirico for fixing.

19. Fixed `as.xts.data.table` to support all xts supported time based index clasess [#2408](https://github.com/Rdatatable/data.table/issues/2408). Thanks to @ebs238 for reporting and for the PR.

20. A memory leak when a very small number such as `0.58E-2141` is bumped to type `character` is resolved, [#918](https://github.com/Rdatatable/data.table/issues/918).

21. The edge case `setnames(data.table(), character(0))` now works rather than error, [#2452](https://github.com/Rdatatable/data.table/issues/2452).

22. Order of rows returned in non-equi joins were incorrect in certain scenarios as reported under [#1991](https://github.com/Rdatatable/data.table/issues/1991). This is now fixed. Thanks to @Henrik-P for reporting.

23. Non-equi joins work as expected when `x` in `x[i, on=...]` is a 0-row data.table. Closes [#1986](https://github.com/Rdatatable/data.table/issues/1986).

24. Non-equi joins along with `by=.EACHI` returned incorrect result in some rare cases as reported under [#2360](https://github.com/Rdatatable/data.table/issues/2360). This is fixed now. This fix also takes care of [#2275](https://github.com/Rdatatable/data.table/issues/2275). Thanks to @ebs238 for the nice minimal reproducible report, @Mihael for asking on SO and to @Frank for following up on SO and filing an issue.

25. `by=.EACHI` works now when `list` columns are being returned and some join values are missing, [#2300](https://github.com/Rdatatable/data.table/issues/2300). Thanks to @jangorecki and @franknarf1 for the reproducible examples which have been added to the test suite.

26. Indices are now retrieved by exact name, [#2465](https://github.com/Rdatatable/data.table/issues/2465). This prevents usage of wrong indices as well as unexpected row reordering in join results. Thanks to @pannnda for reporting and providing a reproducible example and to @MarkusBonsch for fixing.

27. `setnames` of whole table when original table had `NA` names skipped replacing those, [#2475](https://github.com/Rdatatable/data.table/issues/2475). Thanks to @franknarf1 and [BenoitLondon on StackOverflow](https://stackoverflow.com/questions/47228836/) for the report and @MichaelChirico for fixing.

28. `CJ()` works with multiple empty vectors now [#2511](https://github.com/Rdatatable/data.table/issues/2511). Thanks to @MarkusBonsch for fixing.

29. `:=` assignment of one vector to two or more columns, e.g. `DT[, c("x", "y") := 1:10]`, failed to copy the `1:10` data causing errors later if and when those columns were updated by reference, [#2540](https://github.com/Rdatatable/data.table/issues/2540). This is an old issue ([#185](https://github.com/Rdatatable/data.table/issues/185)) that had been fixed but reappeared when code was refactored. Thanks to @patrickhowerter for the detailed report with reproducible example and to @MarkusBonsch for fixing and strengthening tests so it doesn't reappear again.

30. "Negative length vectors not allowed" error when grouping `median` and `var` fixed, [#2046](https://github.com/Rdatatable/data.table/issues/2046) and [#2111](https://github.com/Rdatatable/data.table/issues/2111). Thanks to @caneff and @osofr for reporting and to @kmillar for debugging and explaining the cause.

31. Fixed a bug on Windows where `data.table`s containing non-UTF8 strings in `key`s were not properly sorted, [#2462](https://github.com/Rdatatable/data.table/issues/2462), [#1826](https://github.com/Rdatatable/data.table/issues/1826)  and [StackOverflow](https://stackoverflow.com/questions/47599934/why-doesnt-r-data-table-support-well-for-non-ascii-keys-on-windows). Thanks to @shrektan for reporting and fixing.

32. `x.` prefixes during joins sometimes resulted in a "column not found" error. This is now fixed. Closes [#2313](https://github.com/Rdatatable/data.table/issues/2313). Thanks to @franknarf1 for the MRE.

33. `setattr()` no longer segfaults when setting 'class' to empty character vector, [#2386](https://github.com/Rdatatable/data.table/issues/2386). Thanks to @hatal175 for reporting and to @MarkusBonsch for fixing.

34. Fixed cases where the result of `merge.data.table()` would contain duplicate column names if `by.x` was also in `names(y)`.
`merge.data.table()` gains the `no.dups` argument (default TRUE) to match the correpsonding patched behaviour in `base:::merge.data.frame()`. Now, when `by.x` is also in `names(y)` the column name from `y` has the corresponding `suffixes` added to it. `by.x` remains unchanged for backwards compatibility reasons.
In addition, where duplicate column names arise anyway (i.e. `suffixes = c("", "")`) `merge.data.table()` will now throw a warning to match the behaviour of `base:::merge.data.frame()`.
Thanks to @sritchie73 for reporting and fixing [PR#2631](https://github.com/Rdatatable/data.table/pull/2631) and [PR#2653](https://github.com/Rdatatable/data.table/pull/2653)

35. `CJ()` now fails with proper error message when results would exceed max integer, [#2636](https://github.com/Rdatatable/data.table/issues/2636).

36. `NA` in character columns now display as `<NA>` just like base R to distinguish from `""` and `"NA"`.

37. `getDTthreads()` could return INT_MAX (2 billion) after an explicit call to `setDTthreads(0)`, [PR#2708](https://github.com/Rdatatable/data.table/pull/2708).

38. Fixed a bug on Windows that `data.table` may break if the garbage collecting was triggered when sorting a large number of non-ASCII characters. Thanks to @shrektan for reporting and fixing [PR#2678](https://github.com/Rdatatable/data.table/pull/2678),  [#2674](https://github.com/Rdatatable/data.table/issues/2674).

39. Internal aliasing of `.` to `list` was over-aggressive in applying `list` even when `.` was intended within `bquote`, [#1912](https://github.com/Rdatatable/data.table/issues/1912). Thanks @MichaelChirico for reporting/filing and @ecoRoland for suggesting and testing a fix.

40. Attempt to allocate a wildly large amount of RAM (16EB) when grouping by key and there are close to 2 billion 1-row groups, [#2777](https://github.com/Rdatatable/data.table/issues/2777). Thanks to @jsams for the detailed report.

41. Fix a bug that `print(dt, class=TRUE)` shows only `topn - 1` rows. Thanks to @heavywatal for reporting [#2803](https://github.com/Rdatatable/data.table/issues/2803) and filing [PR#2804](https://github.com/Rdatatable/data.table/pull/2804).

## NOTES

0. The license has been changed from GPL to MPL (Mozilla Public License). All contributors were consulted and approved. [PR#2456](https://github.com/Rdatatable/data.table/pull/2456) details the reasons for the change.

1. `?data.table` makes explicit the option of using a `logical` vector in `j` to select columns, [#1978](https://github.com/Rdatatable/data.table/issues/1978). Thanks @Henrik-P for the note and @MichaelChirico for filing.

2. Test 1675.1 updated to cope with a change in R-devel in June 2017 related to `factor()` and `NA` levels.

3. Package `ezknitr` has been added to the whitelist of packages that run user code and should be consider data.table-aware, [#2266](https://github.com/Rdatatable/data.table/issues/2266). Thanks to Matt Mills for testing and reporting.

4. Printing with `quote = TRUE` now quotes column names as well, [#1319](https://github.com/Rdatatable/data.table/issues/1319). Thanks @jan-glx for the suggestion and @MichaelChirico for the PR.

5. Added a blurb to `?melt.data.table` explicating the subtle difference in behavior of the `id.vars` argument vis-a-vis its analog in `reshape2::melt`, [#1699](https://github.com/Rdatatable/data.table/issues/1699). Thanks @MichaelChirico for uncovering and filing.

6. Added some clarification about the usage of `on` to `?data.table`, [#2383](https://github.com/Rdatatable/data.table/issues/2383). Thanks to @peterlittlejohn for volunteering his confusion and @MichaelChirico for brushing things up.

7. Clarified that "data.table always sorts in `C-locale`" means that upper-case letters are sorted before lower-case letters by ordering in data.table (e.g. `setorder`, `setkey`, `DT[order(...)]`). Thanks to @hughparsonage for the pull request editing the documentation. Note this makes no difference in most cases of data; e.g. ids where only uppercase or lowercase letters are used (`"AB123"<"AC234"` is always true, regardless), or country names and words which are consistently capitalized. For example, `"America" < "Brazil"` is not affected (it's always true), and neither is `"america" < "brazil"` (always true too); since the first letter is consistently capitalized. But, whether `"america" < "Brazil"` (the words are not consistently capitalized) is true or false in base R depends on the locale of your R session. In America it is true by default and false if you i) type `Sys.setlocale(locale="C")`, ii) the R session has been started in a C locale for you which can happen on servers/services (the locale comes from the environment the R session is started in). However, `"america" < "Brazil"` is always, consistently false in data.table which can be a surprise because it differs to base R by default in most regions. It is false because `"B"<"a"` is true because all upper-case letters come first, followed by all lower case letters (the ascii number of each letter determines the order, which is what is meant by `C-locale`).

8. `data.table`'s dependency has been moved forward from R 3.0.0 (Apr 2013) to R 3.1.0 (Apr 2014; i.e. 3.5 years old). We keep this dependency as old as possible for as long as possible as requested by users in managed environments. Thanks to Jan Gorecki, the test suite from latest dev now runs on R 3.1.0 continously, as well as R-release (currently 3.4.2) and latest R-devel snapshot. The primary motivation for the bump to R 3.1.0 was allowing one new test which relies on better non-copying behaviour in that version, [#2484](https://github.com/Rdatatable/data.table/issues/2484). It also allows further internal simplifications. Thanks to @MichaelChirico for fixing another test that failed on R 3.1.0 due to slightly different behaviour of `base::read.csv` in R 3.1.0-only which the test was comparing to, [#2489](https://github.com/Rdatatable/data.table/pull/2489).

9. New vignette added: _Importing data.table_ - focused on using data.table as a dependency in R packages. Answers most commonly asked questions and promote good practices.

10. As warned in v1.9.8 release notes below in this file (25 Nov 2016) it has been 1 year since then and so use of `options(datatable.old.unique.by.key=TRUE)` to restore the old default is now deprecated with warning. The new warning states that this option still works and repeats the request to pass `by=key(DT)` explicitly to `unique()`, `duplicated()`, `uniqueN()` and `anyDuplicated()` and to stop using this option. In another year, this warning will become error. Another year after that the option will be removed.

11. As `set2key()` and `key2()` have been warning since v1.9.8 (Nov 2016), their warnings have now been upgraded to errors. Note that when they were introduced in version 1.9.4 (Oct 2014) they were marked as 'experimental' in NEWS item 4. They will be removed in one year.

    ```
    Was warning: set2key() will be deprecated in the next relase. Please use setindex() instead.
    Now error: set2key() is now deprecated. Please use setindex() instead.
    ```

12. The option `datatable.showProgress` is no longer set to a default value when the package is loaded. Instead, the `default=` argument of `getOption` is used by both `fwrite` and `fread`. The default is the result of `interactive()` at the time of the call. Using `getOption` in this way is intended to be more helpful to users looking at `args(fread)` and `?fread`.

13. `print.data.table()` invisibly returns its first argument instead of `NULL`. This behavior is compatible with the standard `print.data.frame()` and tibble's `print.tbl_df()`. Thanks to @heavywatal for [PR#2807](https://github.com/Rdatatable/data.table/pull/2807)


# data.table v1.10.4-3  (20 Oct 2017)

1. Fixed crash/hang on MacOS when `parallel::mclapply` is used and data.table is merely loaded, [#2418](https://github.com/Rdatatable/data.table/issues/2418). Oddly, all tests including test 1705 (which tests `mclapply` with data.table) passed fine on CRAN. It appears to be some versions of MacOS or some versions of libraries on MacOS, perhaps. Many thanks to Martin Morgan for reporting and confirming this fix works. Thanks also to @asenabouth, Joe Thorley and Danton Noriega for testing, debugging and confirming that automatic parallelism inside data.table (such as `fwrite`) works well even on these MacOS installations. See also news items below for 1.10.4-1 and 1.10.4-2.


# data.table v1.10.4-2  (12 Oct 2017)

1. OpenMP on MacOS is now supported by CRAN and included in CRAN's package binaries for Mac. But installing v1.10.4-1 from source on MacOS failed when OpenMP was not enabled at compile time, [#2409](https://github.com/Rdatatable/data.table/issues/2409). Thanks to Liz Macfie and @fupangpangpang for reporting. The startup message when OpenMP is not enabled has been updated.

2. Two rare potential memory faults fixed, thanks to CRAN's automated use of latest compiler tools; e.g. clang-5 and gcc-7


# data.table v1.10.4-1  (09 Oct 2017)

1. The `nanotime` v0.2.0 update (June 2017) changed from `integer64` to `S4` and broke `fwrite` of `nanotime` columns. Fixed to work with `nanotime` both before and after v0.2.0.

2. Pass R-devel changes related to `deparse(,backtick=)` and `factor()`.

3. Internal `NAMED()==2` now `MAYBE_SHARED()`, [#2330](https://github.com/Rdatatable/data.table/issues/2330). Back-ported to pass under the stated dependency, R 3.0.0.

4. Attempted improvement on Mac-only when the `parallel` package is used too (which forks), [#2137](https://github.com/Rdatatable/data.table/issues/2137). Intel's OpenMP implementation appears to leave threads running after the OpenMP parallel region (inside data.table) has finished unlike GNU libgomp. So, if and when `parallel`'s `fork` is invoked by the user after data.table has run in parallel already, instability occurs. The problem only occurs with Mac package binaries from CRAN because they are built by CRAN with Intel's OpenMP library. No known problems on Windows or Linux and no known problems on any platform when `parallel` is not used. If this Mac-only fix still doesn't work, call `setDTthreads(1)` immediately after `library(data.table)` which has been reported to fix the problem by putting `data.table` into single threaded mode earlier.

5. When `fread()` and `print()` see `integer64` columns are present but package `bit64` is not installed, the warning is now displayed as intended. Thanks to a question by Santosh on r-help and forwarded by Bill Dunlap.


# data.table v1.10.4  (01 Feb 2017)

## BUG FIXES

1. The new specialized `nanotime` writer in `fwrite()` type punned using `*(long long *)&REAL(column)[i]` which, strictly, is undefined behavour under C standards. It passed a plethora of tests on linux (gcc 5.4 and clang 3.8), win-builder and 6 out 10 CRAN flavours using gcc. But failed (wrong data written) with the newest version of clang (3.9.1) as used by CRAN on the failing flavors, and solaris-sparc. Replaced with the union method and added a grep to CRAN_Release.cmd.


# data.table v1.10.2  (31 Jan 2017)

## NEW FEATURES

1. When `j` is a symbol prefixed with `..` it will be looked up in calling scope and its value taken to be column names or numbers.

    ```R
    myCols = c("colA","colB")
    DT[, myCols, with=FALSE]
    DT[, ..myCols]              # same
    ```

   When you see the `..` prefix think _one-level-up_ like the directory `..` in all operating systems meaning the parent directory. In future the `..` prefix could be made to work on all symbols apearing anywhere inside `DT[...]`. It is intended to be a convenient way to protect your code from accidentally picking up a column name. Similar to how `x.` and `i.` prefixes (analogous to SQL table aliases) can already be used to disambiguate the same column name present in both `x` and `i`. A symbol prefix rather than a `..()` _function_ will be easier for us to optimize internally and more convenient if you have many variables in calling scope that you wish to use in your expressions safely. This feature was first raised in 2012 and long wished for, [#633](https://github.com/Rdatatable/data.table/issues/633). It is experimental.

2. When `fread()` or `print()` see `integer64` columns are present, `bit64`'s namespace is now automatically loaded for convenience.

3. `fwrite()` now supports the new [`nanotime`](https://cran.r-project.org/package=nanotime) type by Dirk Eddelbuettel, [#1982](https://github.com/Rdatatable/data.table/issues/1982). Aside: `data.table` already automatically supported `nanotime` in grouping and joining operations via longstanding support of its underlying `integer64` type.

4. `indices()` gains a new argument `vectors`, default `FALSE`. This strsplits the index names by `__` for you, [#1589](https://github.com/Rdatatable/data.table/issues/1589).

    ```R
    DT = data.table(A=1:3, B=6:4)
    setindex(DT, B)
    setindex(DT, B, A)
    indices(DT)
    [1] "B"    "B__A"
    indices(DT, vectors=TRUE)
    [[1]]
    [1] "B"
    [[2]]
    [1] "B" "A"
    ```

## BUG FIXES

1. Some long-standing potential instability has been discovered and resolved many thanks to a detailed report from Bill Dunlap and Michael Sannella. At C level any call of the form `setAttrib(x, install(), allocVector())` can be unstable in any R package. Despite `setAttrib()` PROTECTing its inputs, the 3rd argument (`allocVector`) can be executed first only for its result to to be released by `install()`'s potential GC before reaching `setAttrib`'s PROTECTion of its inputs. Fixed by either PROTECTing or pre-`install()`ing. Added to CRAN_Release.cmd procedures: i) `grep`s to prevent usage of this idiom in future and ii) running data.table's test suite with `gctorture(TRUE)`.

2. A new potential instability introduced in the last release (v1.10.0) in GForce optimized grouping has been fixed by reverting one change from malloc to R_alloc. Thanks again to Michael Sannella for the detailed report.

3. `fwrite()` could write floating point values incorrectly, [#1968](https://github.com/Rdatatable/data.table/issues/1968). A thread-local variable was incorrectly thread-global. This variable's usage lifetime is only a few clock cycles so it needed large data and many threads for several threads to overlap their usage of it and cause the problem. Many thanks to @mgahan and @jmosser for finding and reporting.

## NOTES

1. `fwrite()`'s `..turbo` option has been removed as the warning message warned. If you've found a problem, please [report it](https://github.com/Rdatatable/data.table/issues).

2. No known issues have arisen due to `DT[,1]` and `DT[,c("colA","colB")]` now returning columns as introduced in v1.9.8. However, as we've moved forward by setting `options('datatable.WhenJisSymbolThenCallingScope'=TRUE)` introduced then too, it has become clear a better solution is needed. All 340 CRAN and Bioconductor packages that use data.table have been checked with this option on. 331 lines would need to be changed in 59 packages. Their usage is elegant, correct and recommended, though. Examples are `DT[1, encoding]` in quanteda and `DT[winner=="first", freq]` in xgboost. These are looking up the columns `encoding` and `freq` respectively and returning them as vectors. But if, for some reason, those columns are removed from `DT` and `encoding` or `freq` are still variables in calling scope, their values in calling scope would be returned. Which cannot be what was intended and could lead to silent bugs. That was the risk we were trying to avoid. <br>
`options('datatable.WhenJisSymbolThenCallingScope')` is now removed. A migration timeline is no longer needed. The new strategy needs no code changes and has no breakage. It was proposed and discussed in point 2 [here](https://github.com/Rdatatable/data.table/issues/1188#issuecomment-127824969), as follows.<br>
When `j` is a symbol (as in the quanteda and xgboost examples above) it will continue to be looked up as a column name and returned as a vector, as has always been the case.  If it's not a column name however, it is now a helpful error explaining that data.table is different to data.frame and what to do instead (use `..` prefix or `with=FALSE`).  The old behaviour of returning the symbol's value in calling scope can never have been useful to anybody and therefore not depended on. Just as the `DT[,1]` change could be made in v1.9.8, this change can be made now. This change increases robustness with no downside. Rerunning all 340 CRAN and Bioconductor package checks reveal 2 packages throwing the new error: partools and simcausal. Their maintainers have been informed that there is a likely bug on those lines due to data.table's (now remedied) weakness. This is exactly what we wanted to reveal and improve.

3. As before, and as we can see is in common use in CRAN and Bioconductor packages using data.table, `DT[,myCols,with=FALSE]` continues to lookup `myCols` in calling scope and take its value as column names or numbers.  You can move to the new experimental convenience feature `DT[, ..myCols]` if you wish at leisure.


# data.table v1.10.0  (03 Dec 2016)

## BUG FIXES

1. `fwrite(..., quote='auto')` already quoted a field if it contained a `sep` or `\n`, or `sep2[2]` when `list` columns are present. Now it also quotes a field if it contains a double quote (`"`) as documented, [#1925](https://github.com/Rdatatable/data.table/issues/1925). Thanks to Aki Matsuo for reporting. Tests added. The `qmethod` tests did test escaping embedded double quotes, but only when `sep` or `\n` was present in the field as well to trigger the quoting of the field.

2. Fixed 3 test failures on Solaris only, [#1934](https://github.com/Rdatatable/data.table/issues/1934). Two were on both sparc and x86 and related to a `tzone` attribute difference between `as.POSIXct` and `as.POSIXlt` even when passed the default `tz=""`. The third was on sparc only: a minor rounding issue in `fwrite()` of 1e-305.

3. Regression crash fixed when 0's occur at the end of a non-empty subset of an empty table, [#1937](https://github.com/Rdatatable/data.table/issues/1937). Thanks Arun for tracking down. Tests added. For example, subsetting the empty `DT=data.table(a=character())` with `DT[c(1,0)]` should return a 1 row result with one `NA` since 1 is past the end of `nrow(DT)==0`, the same result as `DT[1]`.

4. Fixed newly reported crash that also occurred in old v1.9.6 when `by=.EACHI`, `nomatch=0`, the first item in `i` has no match AND `j` has a function call that is passed a key column, [#1933](https://github.com/Rdatatable/data.table/issues/1933). Many thanks to Reino Bruner for finding and reporting with a reproducible example. Tests added.

5. Fixed `fread()` error occurring for a subset of Windows users: `showProgress is not type integer but type 'logical'.`, [#1944](https://github.com/Rdatatable/data.table/issues/1944) and [#1111](https://github.com/Rdatatable/data.table/issues/1111). Our tests cover this usage (it is just default usage), pass on AppVeyor (Windows), win-builder (Windows) and CRAN's Windows so perhaps it only occurs on a specific and different version of Windows to all those. Thanks to @demydd for reporting. Fixed by using strictly `logical` type at R level and `Rboolean` at C level, consistently throughout.

6. Combining `on=` (new in v1.9.6) with `by=` or `keyby=` gave incorrect results, [#1943](https://github.com/Rdatatable/data.table/issues/1943). Many thanks to Henrik-P for the detailed and reproducible report. Tests added.

7. New function `rleidv` was ignoring its `cols` argument, [#1942](https://github.com/Rdatatable/data.table/issues/1942). Thanks Josh O'Brien for reporting. Tests added.

## NOTES

1. It seems OpenMP is not available on CRAN's Mac platform; NOTEs appeared in [CRAN checks](https://cran.r-project.org/web/checks/check_results_data.table.html) for v1.9.8. Moved `Rprintf` from `init.c` to `packageStartupMessage` to avoid the NOTE as requested urgently by Professor Ripley. Also fixed the bad grammar of the message: 'single threaded' now 'single-threaded'. If you have a Mac and run macOS or OS X on it (I run Ubuntu on mine) please contact CRAN maintainers and/or Apple if you'd like CRAN's Mac binary to support OpenMP. Otherwise, please follow [these instructions for OpenMP on Mac](https://github.com/Rdatatable/data.table/wiki/Installation) which people have reported success with.

2. Just to state explicitly: data.table does not now depend on or require OpenMP. If you don't have it (as on CRAN's Mac it appears but not in general on Mac) then data.table should build, run and pass all tests just fine.

3. There are now 5,910 raw tests as reported by `test.data.table()`. Tests cover 91% of the 4k lines of R and 89% of the 7k lines of C. These stats are now known thanks to Jim Hester's [Covr](https://CRAN.R-project.org/package=covr) package and [Codecov.io](https://about.codecov.io/). If anyone is looking for something to help with, creating tests to hit the missed lines shown by clicking the `R` and `src` folders at the bottom [here](https://codecov.io/github/Rdatatable/data.table?branch=master) would be very much appreciated.

4. The FAQ vignette has been revised given the changes in v1.9.8. In particular, the very first FAQ.

5. With hindsight, the last release v1.9.8 should have been named v1.10.0 to convey it wasn't just a patch release from .6 to .8 owing to the 'potentially breaking changes' items. Thanks to @neomantic for correctly pointing out. The best we can do now is now bump to 1.10.0.


# data.table v1.9.8 (Nov 2016) back to v1.2 (Aug 2008) has been moved to [NEWS.0.md](https://github.com/Rdatatable/data.table/blob/master/NEWS.0.md)

