Governance for the R data.table project

# Purpose and scope

## This document

The purpose of this document is to define how people related to the project work together, so that the project can expand to handle a larger and more diverse group of contributors.

## The R package

The purpose of the project is to maintain the R data.table package,
which is guided by the following principles:

* Few (if any) dependencies
* Time & memory efficiency
* Concise syntax (minimal redundancy in code)
* Stable code base (easy for users to upgrade to new data.table, and compatible with old R versions)
* Comprehensive and accessible documentation and run-time signals (errors, warnings)
* Clear error and warning messages

To prioritize developer time, we define what is in and out of scope.
The current scope of package functionality includes:
* Data manipulation and analysis 
    * reshaping/pivoting
    * aggregation/summarizing
    * subsetting rows
    * all sorts of joins
    * adding/updating/deleting columns
* Reading/writing of data from/to many file formats

Functionality that is out of scope:
* plotting/graphics (like ggplot2)
* manipulating out-of-memory data, e.g. data stored on disk or remote SQL DB, (as opposed e.g. to sqldf / dbplyr)
* machine learning / modeling (like mlr3)
* regular expression builders (like rex and nc packages) 

# Roles 

## Contributor

* Definition: a user who has written/commented at least one issue, worked to label/triage issues, written a blog post, given a talk, etc. 
* How this role is recognized: there is no central list of contributors / no formal recognition for contributors.

## Project member

* Definition: some one who has submitted at least one PR that has been merged into master. 
* How to obtain this role: any user can become a member by submitting a PR, then having it reviewed and merged into master. Contributors who have written issues should be encouraged to submit their first PR to become a project member.
* How this role is recognized: Members are credited via role="ctb" in DESCRIPTION (so they appear in Author list on CRAN), and they are added to https://github.com/orgs/Rdatatable/teams/project-members so they can create new branches in the Rdatatable/data.table GitHub repo. They also appear on https://github.com/Rdatatable/data.table/graphs/contributors (Contributions to master, excluding merge commits).

## Reviewer

* Definition: a member who has volunteered to do code reviews for some features/files. 
* How to obtain this role: after one or more significant PRs to a given file, a member should be invited to add their name as a reviewer of that file in CODEOWNERS, and after that is merged into master, then they are considered a reviewer. 
* How this role is recognized: same credit in DESCRIPTION as a regular member, role="ctb" (so they appear in Author list on CRAN). 
* Note: having your name in CODEOWNERS does not give any special permission, but it does mean that you will be notified whenever there is a new PR with changes to that file. 

## Committer

* Definition: permission to commit to, and merge PRs into, master branch. 
* How to obtain this role: after a reviewer has a consistent history of careful reviews of others' PRs, then a current GitHub maintainer should ask all other current GitHub maintainers if they approve promoting the reviewer to GitHub maintainer, and it should be done if there is general consensus/agreement. 
* How this role is recognized: credited via role="aut" in DESCRIPTION (so they appear in Author list on CRAN), and added to https://github.com/orgs/Rdatatable/teams/maintainers which gives permission to merge PRs into master branch. 

## CRAN maintainer

* Definition: in charge of communication with CRAN. Responsible for submitting releases to CRAN on a regular basis, and in response to requests from CRAN. 
* How to obtain this role: (1) merge into master a PR adding role="cre" to DESCRIPTION, and (2) submit updated package to CRAN (previous CRAN maintainer will have to confirm change by email to CRAN). 
* How this role is recognized: credited via role="cre" in DESCRIPTION, so they appear as Maintainer on CRAN. 

## Other roles

Each of the below roles is important, and a wiki page should be
created to explain (1) details of responsibilities, (2) who to contact
to obtain this role.

* translation manager (to communicate with translators), no special permission.
* triage manager (to decide which issues/PRs to include in next releases), no special permission.
* performance testing manager (to prevent performance regressions), need permission to edit continuous integration scripts.
* reverse dependency manager (to ensure compatibility with other CRAN packages), need access to a computer that can run revdep checks in parallel.
* binary manager (to build binaries of development branches for user testing before release), need permission to edit continuous deployment scripts.

# Decision-making processes

## Pull Requests

A pull request can be merged, as long as there is one approving review
(ideally from a reviewer of the affected files, different person from
author of PR), and no opposing/blocking comments from
Reviewers/Committers.

## CRAN updates

* Regular CRAN releases should ideally occur twice per year, and can include new features.
* A hotfix/patch CRAN release should occur when CRAN asks for one, at which time the CRAN maintainer should post an issue on github, and ask others to help fix/prepare the release.
* Both kinds of releases should be discussed in an issue, and the release should happen only if there is general consensus that it is ok to go ahead (no Reviewer/Committer has expressed major blocking concerns).
* It is the responsibility of the CRAN maintainer to ensure quality prior to release. This includes CRAN checks, unit tests, performance tests, etc, and these tasks can be delegated to others. 

## Changing this GOVERNANCE.md document

There is no special process for changing this document (submit a PR
and ask for review).

# Code of conduct

As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, religion, etc.

Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. A person with special roles who does not follow the Code of Conduct may have their roles revoked.

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or emailing one or more of the Committers.

This Code of Conduct is adapted from Tidyverse code of conduct.

# Version numbering

data.table Version line in DESCRIPTION typically has the following meanings

* x.y.z where x=major, y=minor, z=patch/hotfix.
* x should be incremented only for major backwards-incompatible changes.
* z is even for CRAN releases, odd for GitHub development.
* z=99 for master branch with new features (for example 1.14.99 or 1.15.99), which eventually becomes a regular CRAN release, with incremented y and z=0 (for example 1.15.0 or 1.16.0).
* patch/hotfix development should occur on GitHub as z=odd (1.15.1) and release to CRAN as z=even (1.15.2).


