Filing issues
-------------

- Please read and follow all the instructions at **[Support](https://github.com/Rdatatable/data.table/wiki/Support)** before filing; e.g. **check [NEWS](https://github.com/Rdatatable/data.table/blob/master/NEWS.md)** first and **search existing [Issues](https://github.com/Rdatatable/data.table/issues)**.
- One issue for one purpose. Don't report more than one bug or request several features in the same issue.
- Feel free to add reactions to existing issues that are important to you. We monitor this and it helps us prioritize where to devote our efforts! We expect [this issue](https://github.com/Rdatatable/data.table/issues/3189) to be evergreen.

**Filing issues is contributing. Thank you!**

Pull Requests (PRs)
-------------------

If you are not fixing an open issue and you are confident, you do not need to file a new issue before submitting the PR. It's easier for us to accept and merge a self-contained PR with everything in one place. If discussion is needed, it can be done in the PR. However, **the PR's status must be passing tests before we will start to look at it**. So, before you spend the time getting to that stage, it may save you time to create an issue first and start a discussion to see if your idea would be accepted in principle. If you are going to spend more than a day on the PR, creating an issue first lets other people know you are working on it to save duplicating effort.

1. Every new feature or bug fix must have one or more new tests in [`inst/tests/tests.Rraw`](https://github.com/Rdatatable/data.table/blob/master/inst/tests/tests.Rraw); see below for a bit more on how this file works. You _must please_ check that the tests fail _without_ the fix, since the build system only checks that the new test passes _with_ the fix, which is not sufficient. For example, run your new test with the current DEV version and verify that it actually fails. The test's comment should contain `#<issue number>` so we can quickly find the issue in future.
2. Unless the change is trivial (e.g. typo fix) there must be a new entry in [NEWS](https://github.com/Rdatatable/data.table/blob/master/NEWS.md). Please thank yourself by name and include what the thanks are for. Follow the prevailing style at the top of the file; e.g. "Problem with X in Y circumstance is fixed, [#123](issue link). Thanks to _(them)_ for reporting and _(me)_ for fixing." As of Sept 2020 it is discouraged to include more than one link per news item, because CRAN checks the validity of each link and this takes a lot of time (it is preferable to only include an issue link and NOT a PR link). These are the release notes that others quickly skim and search so please use relevant helpful keywords with that in mind.
3. Please create the PR against the `master` branch. You can do that by forking the repository, creating a new branch for your feature/bugfix in the forked project, and then using that as a base for your pull requests. After your first successful merged PR you will very likely be invited to be a project member. This will allow you to create your next branch directly in the project which is easier and more convenient than forking, both for you and for Rdatatable's maintainers. Working on _branches_ on this (Rdatatable) project will _not_ affect the core code, so you can feel free to experiment as a project member; the core code is on the `master` branch, and only Matt & Arun can push/merge code there. Remember to do `git pull upstream your_branch` (where `upstream` is the name of the remote for `Rdatatable/data.table` seen in `git remote -v`) each time you want to add something; this will keep your local branch up to date with remote, in case anyone makes commits you don't yet have locally. This will reduce the number of merge conflicts you will need to deal with. Do not use `git rebase` on a branch where other users are pushing.
4. Just one feature/bugfix per PR please. Small changes are easier to review and accept than big sweeping changes. Sometimes big sweeping changes are needed and we just have to discuss those case by case.
5. You do not need to separate formatting-only changes. Just make the format changes in your PR. When the PR is passing tests and we look at the PR's unified diff, we will subtract the formatting-only changes and make those to master directly for you. That will reduce your PR to logic changes only so that it can be more easily reviewed now and easier to look back on in future.
6. GitHub enables us to squash commits together when merging, so you don't have to squash yourself.
7. Your pull request's description is the place to put any benchmark results and be a bit more verbose than the entry in the NEWS file, if you think that's appropriate. Include text "Closes #ISSUE_NUM" (case insensitive but the space must be present) for GitHub to link and close the corresponding issue when the PR is merged. If multiple issues are being closed, add that many "Closes #ISSUE" lines.
8. Ensure that all tests pass by typing `test.data.table()` after installing your branch. It's also better to `R CMD check --as-cran` against your branch source package archive `.tar.gz` file. You may want to add `--no-manual`, `--no-build-vignettes` or `--ignore-vignettes` (R 3.3.0+) options to reduce dependencies required to perform check. PRs with failed tests can't be merged and it is hard to debug every PR and explain why it fails and how to fix it. The lesser the feedback required, the faster it is likely to be merged. Matt has added his dev cycle script [here](https://github.com/Rdatatable/data.table/blob/master/cc.R) and Pasha has added a Makefile [here](https://github.com/Rdatatable/data.table/blob/master/Makefile).

Example of a good pull request: [PR#2332](https://github.com/Rdatatable/data.table/pull/2332). It has a NEWS entry. It passed existing tests and added a new one. One test was removed but the PR description clearly explained why upfront (without us having to ask). Benchmark results were included, which made the need for the change compelling. We didn't need to run anything ourselves. Everything was including in one PR in one place. In short, it was a pleasure to review and merge.

### Coding Style

A few minor points of style that you should adhere to in your PR:

- Use `=` for assignment (not `<-`); see [here](https://github.com/Rdatatable/data.table/pull/3582#discussion_r287075480), [here](https://github.com/Rdatatable/data.table/issues/3590#issuecomment-495776200), [here](https://stackoverflow.com/questions/1741820/what-are-the-differences-between-and-in-r#comment14293879_1742591) and [here](https://twitter.com/kdpsinghlab/status/1044568690346393606)
- Spacing
  + 2-space indentation
  + No trailing white space
  + In tests use long lines and vertical alignment to easily spot small differences
  + Argument spacing style: `fun(arg1=value1, arg2=val2, ...)`
  + Add a whitespace between `if` and opening bracket, also before opening curly bracket: `if (condition) {`
- Use `L` suffix for integer; i.e. `x[1L]` not `x[1]` (to save coercion)
- Use `error(fmt, arg1, arg2, ...)` not `error(paste(...))` or `error(sprintf(...))` as per [Writing R Extensions#1.7 point 2](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Diagnostic-messages)

### Testing

`data.table` uses a series of unit tests to exhibit code that is expected to work. These are primarily stored in [`inst/tests/tests.Rraw`](https://github.com/Rdatatable/data.table/blob/master/inst/tests/tests.Rraw). They come primarily from two places -- when new features are implemented, the author constructs minimal examples demonstrating the expected common usage of said feature, including expected failures/invalid use cases (e.g., the [initial assay of `fwrite` includes 28 tests](https://github.com/Rdatatable/data.table/blob/master/inst/tests/tests.Rraw#L9123-L9245)). Second, when kind users such as yourself happen upon some aberrant behavior in their everyday use of `data.table` (typically, some edge case that slipped through the cracks in the coding logic of the original author). We try to be thorough -- for example there were initially [141 tests of `split.data.table`](https://github.com/Rdatatable/data.table/blob/master/inst/tests/tests.Rraw#L8493-L8952), and that number has since grown!

When you file a pull request, you should add some tests to this file with this in mind -- for new features, try to cover possible use cases extensively (we use [Codecov](https://codecov.io/gh/Rdatatable/data.table) to make it a bit easier to see how well you've done to minimally cover any new code you've added); for bug fixes, include a minimal version of the problem you've identified and write a test to ensure that your fix indeed works, and thereby guarantee that your fix continues to work as the codebase is further modified in the future. We encourage you to scroll around in `tests.Rraw` a bit to get a feel for the types of examples that are being created, and how bugs are tested/features evaluated.

#### Using `test`

See [`test` function manual](https://rdatatable.gitlab.io/data.table/reference/test.html).

**References:** If you are not sure how to issue a PR, but would like to contribute, these links should help get you started:

1. **[How to Github: Fork, Branch, Track, Squash and Pull request](https://gun.io/blog/how-to-github-fork-branch-and-pull-request/)**.
2. **[Squashing Github pull requests into a single commit](http://eli.thegreenplace.net/2014/02/19/squashing-github-pull-requests-into-a-single-commit)**.
3. **[Github help](https://help.github.com/articles/using-pull-requests/)** - you'll need the *fork and pull* model.

Minimal first time PR
---------------------

```
$ cd /tmp      # or anywhere safe to play
$ git config --global core.autocrlf false   # Windows-only preserve \n in test data
$ git clone https://github.com/Rdatatable/data.table.git
$ cd data.table
$ R CMD build .
$ R CMD check data.table_1.11.5.tar.gz
...
Status: OK
```
Congratulations - you've just compiled and tested the very latest version of data.table in development. Everything looks good. Now make your changes. Using an editor of your choice, edit the appropriate `.R`, `.md`, `NEWS` and `tests.Rraw` files. Test your changes : 
```
$ R CMD build .
$ R CMD check data.table_1.11.5.tar.gz
```
or if your OS supports makefile
```
$ make build && make check
```
Fix the problems and repeat the `build` and `check` steps until you get `Status: OK`.
Now commit the change and push. Since this is a first time PR and you're not a project member, this step should automatically ask you if you wish to fork the project. Say 'yes'. If that's not the case, please edit this wiki page to show what exactly happens for non project members.
```
$ git commit -am "Added/fixed something in somewhere"
$ git push
```
After your first successful non-trivial PR you'll likely be invited to be a project member. When you're a member, before making your changes, you would create a branch first : 
```
$ git checkout -b my_new_branch
```
and then the `commit` and `push` shown above would push to the branch in the main project. The next time you refresh the GitHub page in your browser, a button appears which you can click to create the PR from the branch. And that's all there is to it.

data.table developer utilities
------------------------------

There are few utilities that some of regular `data.table` contributors are using. They are very helpful to speed up testing new changes, and to improve productivity in general. You can read more in [.dev/README.md](https://github.com/Rdatatable/data.table/blob/master/.dev/README.md).

(advanced) Tips for your dev environment
-----------------------------

### `pre-commit` hook 

[`git` hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) are a great way to try and catch common code linting-style problems on your local machine before pushing to GitHub.

The sample pre-commit hook will already do some useful checks for you, e.g. making sure there are no non-ASCII filenames and checking if there are any `git` conflict markers (like `<<<<<<<`) left over in your code.

This can be turned on with `mv .git/hooks/pre-commit.sample .git/hooks/pre-commit` from the `data.table` top-level directory on your clone.

Other helpful tests for common mistakes:

- `grep -Fr "browser()" .` - remove `browser()` calls that were inserted during debugging

We may eventually implement some [pre-receive hooks](https://help.github.com/en/enterprise/2.15/admin/developer-workflow/creating-a-pre-receive-hook-script) to formalize some of these requirements.
