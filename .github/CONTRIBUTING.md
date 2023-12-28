Filing issues
-------------

- Please read and follow all the instructions at **[Support](https://github.com/Rdatatable/data.table/wiki/Support)** before filing; e.g. **check [NEWS](https://github.com/Rdatatable/data.table/blob/master/NEWS.md)** first and **search existing [Issues](https://github.com/Rdatatable/data.table/issues)**.
- Please **use tags** rather than words at the beginning of titles; e.g. [feature request], [bug].
- One issue for one purpose. Don't report more than one bug or request several features in the same issue.

**Filing issues is contributing. Thank you!**

Pull Requests (PRs)
-------------------

If you are not fixing an open issue and you are confident, you do not need to file a new issue before submitting the PR. It's easier for us to accept and merge a self-contained PR with everything in one place. If discussion is needed, it can be done in the PR. However, **the PR's status must be passing tests before we will start to look at it**. So, before you spend the time getting to that stage, it may save you time to create an issue first and start a discussion to see if your idea would be accepted in principle. If you are going to spend more than a day on the PR, creating an issue first lets other people know you are working on it to save duplicating effort.

1. Every new feature or bug fix must have one or more new tests in [`inst/tests/tests.Rraw`](https://github.com/Rdatatable/data.table/blob/master/inst/tests/tests.Rraw). You _must please_ check that the tests fail _without_ the fix, since the build system only checks that the new test passes _with_ the fix, which is not sufficient. For example, run your new test with the current DEV version and verify that it actually fails.
2. Unless the change is trivial (e.g. typo fix) there must be a new entry in [NEWS](https://github.com/Rdatatable/data.table/blob/master/NEWS.md). Please thank yourself by name and include what the thanks are for. Follow the prevailing style at the top of the file; e.g. "Problem with X in Y circumstance is fixed, [#123](issue link). Thanks to _(them)_ for reporting and _(me)_ for fixing, [PR#145](PR link)". These are the release notes that others quickly skim and search so please use relevant helpful keywords with that in mind.
3. Please create the PR against the `master` branch. You can do that by forking the repository, creating a new branch for your feature/bugfix in the forked project, and then using that as a base for your pull requests. After your first successful merged PR you will very likely be invited to be a project member. This will allow you to create your next branch directly in the project which is easier and more convenient than forking.
4. Just one feature/bugfix per PR please. Small changes are easier to review and accept than big sweeping changes. Sometimes big sweeping changes are needed and we just have to discuss those case by case.
5. You do not need to separate formatting-only changes. Just make the format changes in your PR. When the PR is passing tests and we look at the PR's unified diff, we will subtract the formatting-only changes and make those to master directly for you. That will reduce your PR to logic changes only so that it can be more easily reviewed now and easier to look back on in future.
6. GitHub enables us to squash commits together when merging, so you don't have to squash yourself.
7. Your pull request's description is the place to put any benchmark results and be a bit more verbose than the entry in the NEWS file, if you think that's appropriate. Include text "Closes #ISSUE_NUM" (case insensitive but the space must be present) for GitHub to link and close the corresponding issue when the PR is merged. If multiple issues are being closed, add that many "Closes #ISSUE" lines.
8. Ensure that all tests pass by typing `test.data.table()` after installing your branch. It's also better to `R CMD check --as-cran` against your branch source package archive `.tar.gz` file. You may want to add `--no-manual`, `--no-build-vignettes` or `--ignore-vignettes` (R 3.3.0+) options to reduce dependencies required to perform check. PRs with failed tests can't be merged and it is hard to debug every PR and explain why it fails and how to fix it. The lesser the feedback required, the faster it is likely to be merged. Matt has added his dev cycle script [here](https://github.com/Rdatatable/data.table/blob/master/cc.R) and Pasha has added a Makefile [here](https://github.com/Rdatatable/data.table/blob/master/Makefile).

Example of a good pull request: [PR#2332](https://github.com/Rdatatable/data.table/pull/2332). It has a NEWS entry. It passed existing tests and added a new one. One test was removed but the PR description clearly explained why upfront (without us having to ask). Benchmark results were included, which made the need for the change compelling. We didn't need to run anything ourselves. Everything was including in one PR in one place. In short, it was a pleasure to review and merge.

**References:** If you are not sure how to issue a PR, but would like to contribute, these links should help get you started:

1. **[How to Github: Fork, Branch, Track, Squash and Pull request](https://gun.io/blog/how-to-github-fork-branch-and-pull-request/)**.
2. **[Squashing Github pull requests into a single commit](http://eli.thegreenplace.net/2014/02/19/squashing-github-pull-requests-into-a-single-commit)**.
3. **[Github help](https://help.github.com/articles/using-pull-requests/)** - you'll need the *fork and pull* model.
