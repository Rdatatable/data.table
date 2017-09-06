Filing issues
-------------

Please read these points carefully and follow them while filing issues so that we all save time.

- **One issue for one purpose**. Don't add more than one *bug*, *feature request*, *documentation request*, *question*, etc.. on to the same issue. 
- If you've found a *bug*, thanks for reporting. Please read and follow all the instructions at **[Support](https://github.com/Rdatatable/data.table/wiki/Support)** before filing; e.g. check [NEWS](https://github.com/Rdatatable/data.table/blob/master/NEWS.md) first and search existing [Issues](https://github.com/Rdatatable/data.table/issues).
- If you've a *request* of some kind, e.g., *feature request* or *documentation request*, it'd be much appreciated if you could tag appropriately; e.g. **[feature request]**. Please **do not** place keywords at the beginning of the title. This helps us to prioritise easily using tags. 
- If you need *support*, e.g., installation issues or upgrade issues, please add **[Support]** at the beginning of the title. This helps us to easily identify the most common support issues, and provide solutions in a separate page.
- If you have a general *question*, add **[Question]** at the beginning of the title. But note that you're likely to get help faster on the [Mailinglist](https://lists.r-forge.r-project.org/mailman/listinfo/datatable-help) or on [Stackoverflow data.table tag](http://stackoverflow.com/questions/tagged/r+data.table).
- If you've an issue that doesn't fall into any of these categories, then it'd be helpful if you could add **[Misc]** to your title.

It becomes cumbersome to have to ask the same set of questions over and over. So please follow the instructions above for filing issues.

Pull Requests (PR)
------------------

If you are confident, you do not need to file an issue before creating a PR. It's easier for us to accept and merge a self contained PR with everything in one place. If discussion is needed, it can be done in the PR. However, the PR's status must be fully passing automated tests before we will start to look at it. So before you spend the time getting to that stage it may save you time by creating an Issue first to start a discussion and check if your idea will fly.

1. Every new feature or bug fix must have a new test in `inst/tests/tests.Rraw`. You _must please_ check that the test fails _without_ the fix since the test suite only checks that the new test passes _with_ the fix, which is not sufficient. For example, run your new test with the current CRAN version and verify it correctly fails. 
2. Unless the change is very trivial (e.g. typo fix) there must be a new item in NEWS. Please thank yourself by name and include what the thanks is for. Follow the prevailing style at the top of the file; e.g. "Problem with X in Y circumstance is fixed, [#123](issue link). Thanks to <them> for reporting and <me> for fixing, [PR#145](PR link)". These are the release notes that others may quickly skim and search so please use relevant helpful keywords with that in mind.
3. Please create the PR against the `master` branch. Create a branch for that feature/bug fix in the main project and use that as a base for your pull requests.
4. Just one feauture/bug fix per PR please. Small changes are easier to accept than big sweeping changes. Sometimes big sweeping changes are needed and we just have to dicuss those case by case.
5. Separate code formatting changes into a separate "formatting-only" PR. When we look at the unified diff we don't want to see important logic changes mixed up with changes that are just formatting.
6. GitHub enables us to squash commits together when merging, so you don't have to squash yourself. 
7. Your pull request's description is the place to put any benchmark results and be a bit more verbose than the entry in the NEWS file, if you think that's appropriate. Use the text "closes #num" (case insensitive but that space must be present) for GitHub to link and close that issue when we accept and merge. If multiple issues are being closes, "closes" must appear before each one. 
8. Ensure that all tests pass by typing `test.data.table()` after installing your branch. It's also better to `R CMD check --as-cran` against your branch source package archive `.tar.gz` file. You may want to add `--no-manual`, `--no-build-vignettes` or `--ignore-vignettes` (R 3.3.0+) options to reduce dependencies required to perform check. PRs with failed tests can't be merged and it is hard to debug every PR and explain why it fails and how to fix it. The lesser the feedback required, the faster it is likely to be merged. Matt has added his dev cycle script [here](https://github.com/Rdatatable/data.table/blob/master/cc.R) and Pasha has added a Makefile [here](https://github.com/Rdatatable/data.table/blob/master/Makefile).

**References:** If you are not sure how to issue a PR, but would like to contribute, these links should help get you started:

1. **[How to Github: Fork, Branch, Track, Squash and Pull request](https://gun.io/blog/how-to-github-fork-branch-and-pull-request/)**.
2. **[Squashing Github pull requests into a single commit](http://eli.thegreenplace.net/2014/02/19/squashing-github-pull-requests-into-a-single-commit)**.
3. **[Github help](https://help.github.com/articles/using-pull-requests/)** - you'll need the *fork and pull* model.
