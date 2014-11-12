Filing issues
-------------

Please read these points carefully and follow them while filing issues so that both of us can save time.

- **One issue for one purpose**. Don't add more than one *bug*, *feature request*, *documentation request*, *question*, etc.. on to the same issue. 
- If you've found a *bug*, thanks for reporting. But please read and follow the instructions at **[How to file a bug report](https://github.com/Rdatatable/data.table/wiki/How-to-file-a-bug-report)**. It makes things easier for both of us, and avoids unnecessary and prolonged exchanges on the same set of questions over and over.
- If you've a *request* of some kind, e.g., *feature request* or *documentation request*, it'd be much appreciated if you could add **[Request]** at the beginning of the title. This helps us to prioritise easily without having to go through the entire issue. 
- If you need *support*, e.g., installation issues or upgrade issues, please add **[Support]** at the beginning of the title. This helps us to easily identify the most common support issues, and provide solutions in a separate page.
- If you have a general *question*, add **[Question]** at the beginning of the title. But note that you're likely to get help faster on the [Mailinglist](https://lists.r-forge.r-project.org/mailman/listinfo/datatable-help) or on [Stackoverflow data.table tag](http://stackoverflow.com/questions/tagged/r+data.table).
- If you've an issue that doesn't fall into any of these categories, then it'd be helpful if you could add **[Misc]** to your title.

It becomes cumbersome to have to ask the same set of questions over and over. So please follow the instructions above for filing issues.

Pull Requests
-------------

1. Please create all pull requests (PR) against the `master` branch.
2. Create **one PR per feature/bug fix**.
3. Create a branch for that feature/bug fix, and use that as a base for your pull requests. Pull requests directly against your version of `master` will not be accepted.
4. Unless there's a strong reason against, please **squash all your commits together before issuing a PR**, since you would be working on one issue. 
5. In your pull request's description, please state clearly as to what your PR does, i.e., what FR or bug your PR addresses, along with the issue number. For e.g, "Closes #717, rbindlist segfault on factor columns fixed and added tests."
6. All bug fixes and feature requests should also have **tests** added, to help catch any regressions while fixing another issue some time later. Tests should be added to `inst/tests/tests.Rraw` file. 
7. The `README.md` file also has to be updated while fixing or implementing an issue. It should mention the issue number (along with the link) and what the issue is being closed. And also add a "Thanks to @your_name for the PR".

**References:** If you are not sure how to issue a PR, but would like to contribute, these links should help get you started:

1. **[How to Github: Fork, Branch, Track, Squash and Pull request](https://gun.io/blog/how-to-github-fork-branch-and-pull-request/)**.
2. **[Squashing Github pull requests into a single commit](http://eli.thegreenplace.net/2014/02/19/squashing-github-pull-requests-into-a-single-commit)**.
3. **[Github help](https://help.github.com/articles/using-pull-requests/)** - you'll need the *fork and pull* model.
