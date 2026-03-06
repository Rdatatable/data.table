---
name: Revdep check failure
about: Contact a reverse dependency maintainer about a new check failure caused by data.table changes
title: "[revdep] YOUR_PACKAGE: new check failure with data.table master"
labels: []
assignees: []
---
Hi @MAINTAINER,
After installing **data.table** from GitHub master
```r
data.table::update_dev_pkg()
```
and then running `R CMD check` on **YOUR_PACKAGE**, I get the following new failure, which is **not present** when using data.table from CRAN.
## Package and environment
- Package: YOUR_PACKAGE
- Package version: YOUR_PACKAGE_VERSION
- R version(s): R_RELEASE_AND_OR_R_DEVEL
- data.table versions:
  - CRAN: DT_CRAN_VERSION
  - GitHub master: DT_MASTER_SHA_OR_VERSION
- Platform: OS / architecture (for example, linux-x86_64)
## Related data.table issue and commit
- Data.table issue: https://github.com/Rdatatable/data.table/issues/REVDEP_ISSUE_NUMBER
- First bad commit (from revdep checks): https://github.com/Rdatatable/data.table/commit/FIRST_BAD_COMMIT_SHA
## Check output
Below is the relevant part of the failing check log:
```text
OUTPUT_FROM_FAILING_CHECK
```
## Suggested fix
Before uploading new versions to CRAN, **data.table** needs to ensure that updates do not break CRAN checks in dependent packages like **YOUR_PACKAGE**. So can you please submit an updated version of **YOUR_PACKAGE** to CRAN that fixes this check issue?
In particular, I would suggest to avoid
> DOING_WHAT_YOU_ARE_DOING_CURRENTLY
...
> DOING_SOMETHING_MORE_ROBUST.
## Additional context (optional)
- Minimal reproducible example, if available.
- Links to previous revdep check runs or CRAN check results for YOUR_PACKAGE.
- Any notes that might help debug.






