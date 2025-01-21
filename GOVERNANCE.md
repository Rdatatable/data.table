Governance for the R data.table project

# Purpose and scope

## This document

The purpose of this document is to define how people related to the project work together, so that the project can expand to handle a larger and more diverse group of contributors.

## The R package

The purpose of the project is to maintain the R data.table package, which is guided by the following principles:

* Time & memory efficiency
* Concise syntax (minimal redundancy in code)
* No external Imports/LinkingTo/Depends dependencies (external meaning those not maintained by the project)
* Few (if any) Suggests/Enhances dependencies
* Stable code base (strong preference for user-friendly back-compatibility with data.table itself and with old versions of R)
* Comprehensive and accessible documentation and run-time signals (errors, warnings)

To prioritize developer time, we define what is in and out of current scope. Feature requests in issues and pull requests that are out of current scope should be closed immediately, because they are not the current priority. If someone wants to contribute code that is currently out of scope, they first have to make a pull request that changes the scope as defined below.

The current scope of package functionality includes:
* data manipulation and analysis 
    * reshaping/pivoting
    * aggregation/summarizing (via `[,, by=...]` and _grouping sets_)
    * filtering rows
    * all sorts of joins
    * adding/updating/deleting columns
    * set operations (union/rbind, intersection, difference)
* high-performance common functions (`frank`, `fcase`, `fifelse`, `transpose`, `chmatch`, `fsort`, `forder`, `uniqueN`, ...)
* common convenience functions (`%like%`, `%notin%`, `timetaken`, `substitute2`, ...)
* ordered data functions (`rleid`, `shift`, `fcoalesce`, _locf_/_nocb_ `nafill`, rolling functions)
* date and time related classes and functions (`IDate`, `ITime`)
* technical functions (`address`, `tables`, `update_dev_pkg`)
* Reading/writing of data from/to flat (plain text) files like CSV

Functionality that is out of current scope:
* Plotting/graphics (like ggplot2)
* Manipulating out-of-memory data, e.g. data stored on disk or remote SQL DB, (as opposed e.g. to sqldf / dbplyr)
* Machine learning (like mlr3)
* Reading/writing of data from/to binary files like parquet

# Roles 

## Contributor

* Definition: a user who has written/commented at least one issue, worked to label/triage issues, written a blog post, given a talk, etc. 
* How this role is recognized: there is no central list of contributors / no formal recognition for contributors.

## Project member

* Definition: some one who has submitted at least one PR with substantial contributions, that has been merged into master. PRs improving documentation are welcome, and substantial contributions to the docs should count toward membership, but minor contributions such as spelling fixes do not count toward membership.
* How to obtain this role: any user/contributor can become a member by submitting a PR with substantial contributions, then having it reviewed and merged into master. Contributors who have written issues should be encouraged to submit their first PR to become a project member. Contributors can look at https://github.com/Rdatatable/data.table/labels/beginner-task for easy issues to work on.
* How this role is recognized: Members are credited via role="ctb" in DESCRIPTION (so they appear in Author list on CRAN), and they are added to https://github.com/orgs/Rdatatable/teams/project-members so they can create new branches in the Rdatatable/data.table GitHub repo. They also appear on https://github.com/Rdatatable/data.table/graphs/contributors (Contributions to master, excluding merge commits).

## Reviewer

* Definition: a member who has volunteered to do code reviews for some features/files. 
* How to obtain this role: after one or more significant PRs to a given file, a member should be invited to add their name as a reviewer of that file in CODEOWNERS, and after that is merged into master, then they are considered a reviewer. 
* How this role is recognized: same credit in DESCRIPTION as a regular member, role="ctb" (so they appear in Author list on CRAN). 
* Note: having your name in CODEOWNERS does not give any special permission, but it does mean that you will be notified whenever there is a new PR with changes to that file. 

## Committer

* Definition: permission to commit to, and merge PRs into, master branch. 
* How to obtain this role: after a reviewer has a consistent history of careful reviews of others' PRs, then a current Committer should ask all other current Committers if they approve promoting the Reviewer to Committer, and it should be done if there is Consensus among active Committers. 
* How this role is recognized: credited via role="aut" in DESCRIPTION (so they appear in Author list on CRAN), and added to https://github.com/orgs/Rdatatable/teams/committers which gives permission to merge PRs into master branch. 

## CRAN maintainer

* Definition: in charge of communication with CRAN. Responsible for submitting releases to CRAN on a regular basis, and for responding to requests from CRAN.
* How to obtain this role: (1) merge into master a PR adding role="cre" to DESCRIPTION, and (2) submit updated package to CRAN (previous CRAN maintainer will have to confirm change by email to CRAN). 
* How this role is recognized: credited via role="cre" in DESCRIPTION, so they appear as Maintainer on CRAN. 

# Decision-making processes

## Definition of Consensus

Most decisions in the project happen by Consensus, which means that no active people (typically Reviewers and/or Committers) have expressed major blocking concerns, in a public discussion (typically in a GitHub issue or pull request). In Consensus, non-response by inactive members indicates tacit agreement.

## Pull Requests

A pull request can be merged by any committer, if there is one approving review, and Consensus from active Reviewers and Committers. 
* approving review must come from someone other than the author of the PR.
* approving review ideally comes from a reviewer of the affected files.
* approving review can and often will be by the committer who merges the PR.

## CRAN updates

* Regular CRAN releases should ideally occur twice per year, and can include new features.
* A hotfix/patch CRAN release should occur when CRAN asks for one, at which time the CRAN maintainer should post an issue on github, and ask others to help fix/prepare the release. It should not include new features.
* Both kinds of releases should be discussed in an issue, and the release should happen only if there is Consensus among active Reviewers and Committers.
* It is the responsibility of the CRAN maintainer to ensure quality prior to release. This includes CRAN checks, unit tests, performance tests, etc, and these tasks can be delegated to others. 

## Changing this GOVERNANCE.md document

There is no special process for changing this document. Submit a PR and ask for review; the group `@Rdatatable/committers` will automatically be assigned to ensure all current Committers are aware of the change.

Please also make a note in the change log under [`# Governance history`](#governance-history)

# Code of conduct

As contributors of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, religion, etc.

Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

Committers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. A person with special roles who does not follow the Code of Conduct may have their roles revoked.

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or emailing one or more of the Committers.

This Code of Conduct is adapted from Tidyverse code of conduct.

# Version numbering

data.table Version line in DESCRIPTION typically has the following meanings

* x.y.z where x=major, y=minor, z=patch/hotfix/devel.
* x should be incremented only for major backwards-incompatible changes.
* z is even for CRAN releases, odd for GitHub development.
* z=99 for master branch with new features (for example 1.14.99 or 1.15.99), which eventually becomes a regular CRAN release, with incremented y and z=0 (for example 1.15.0 or 1.16.0).
* patch/hotfix development should occur on GitHub as z=odd (1.15.1) and release to CRAN as z=even (1.15.2).

# Governance history

Jan 2025: clarify that edits to governance should notify all committers.

Feb 2024: change team name/link maintainers to committers, to be consistent with role defined in governance.

Nov-Dec 2023: initial version drafted by Toby Dylan Hocking and
reviewed by Tyson Barrett, Jan Gorecki, Michael Chirico, Benjamin
Schwendinger.

