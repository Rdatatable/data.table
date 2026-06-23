# Intended for use with a data.table 'where'
# Don't use * or % like SQL's like.  Uses regexpr syntax - more powerful.
# returns 'logical' so can be combined with other where clauses.
like = function(vector, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE) {
  # TODO(R>=4.1.0): grepl itself has "smarter" logic for factor input.
  if (is.factor(vector)) {
    # indexing by factors is equivalent to indexing by the numeric codes, see ?`[` #4748
    ret = grepl(pattern, levels(vector), ignore.case = ignore.case, fixed = fixed, perl = perl)[vector]
    ret[is.na(ret)] = FALSE
    ret
  } else {
    # most usually character, but integer and numerics will be silently coerced by grepl
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed, perl = perl)
  }
}

"%like%" = function(vector, pattern) like(vector, pattern)
# as grep -i -- grep, ignoring case
"%ilike%" = function(vector, pattern) like(vector, pattern, ignore.case = TRUE)
# as grep -F or fgrep -- grep against a fixed pattern (no regex)
#   (more efficient where applicable)
"%flike%" = function(vector, pattern) like(vector, pattern, fixed = TRUE)
# Perl-compatible regex
"%plike%" = function(vector, pattern) like(vector, pattern, perl = TRUE)
