---

name: Revdep check failure

about: Contact a reverse dependency maintainer about a new check failure caused by data.table changes

title: "\[revdep] YOUR\_PACKAGE: new check failure with data.table master"

labels: \[]

assignees: \[]

---



Hi @MAINTAINER,



After installing \*\*data.table\*\* from GitHub master



```r
data.table::update\_dev\_pkg()
```



and then running `R CMD check` on \*\*YOUR\_PACKAGE\*\*, I get the following new failure, which is \*\*not present\*\* when using data.table from CRAN.





\## Package and environment



\- Package: YOUR\_PACKAGE

\- Package version: YOUR\_PACKAGE\_VERSION

\- R version(s): R\_RELEASE\_AND\_OR\_R\_DEVEL

\- data.table versions:

  - CRAN: DT\_CRAN\_VERSION

  - GitHub master: DT\_MASTER\_SHA\_OR\_VERSION

\- Platform: OS / architecture (for example, linux-x86\_64)



\## Related data.table issue and commit



We track this revdep failure in the data.table issue tracker here:



\- Data.table issue: https://github.com/Rdatatable/data.table/issues/REVDEP\_ISSUE\_NUMBER

\- First bad commit (from revdep checks): https://github.com/Rdatatable/data.table/commit/FIRST\_BAD\_COMMIT\_SHA





\## Check output



Below is the relevant part of the failing check log:



```text
OUTPUT\_FROM\_FAILING\_CHECK
```



\## Suggested fix



Before uploading new versions to CRAN, \*\*data.table\*\* needs to ensure that updates do not break CRAN checks in dependent packages like \*\*YOUR\_PACKAGE\*\*. So can you please submit an updated version of \*\*YOUR\_PACKAGE\*\* to CRAN that fixes this check issue?



In particular, I would suggest to avoid



> DOING\_WHAT\_YOU\_ARE\_DOING\_CURRENTLY



and instead



> DOING\_SOMETHING\_MORE\_ROBUST.



(If we have a more specific suggestion or PR, we will add it here.)



\## Additional context (optional)



\- Minimal reproducible example, if available.

\- Links to previous revdep check runs or CRAN check results for YOUR\_PACKAGE.

\- Any notes that might help debug.



