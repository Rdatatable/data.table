# Intended for use with a data.table 'where'
# Don't use * or % like SQL's like.  Uses regexpr syntax - more powerful.
# returns 'logical' so can be combined with other where clauses.
like = function(vector, pattern, ignore.case = FALSE, fixed = FALSE) {
  if (is.factor(vector)) {
    as.integer(vector) %in% grep(pattern, levels(vector), ignore.case = ignore.case, fixed = fixed)
  } else {
    # most usually character, but integer and numerics will be silently coerced by grepl
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed)
  }
}

"%like%" = function(vector, pattern) like(vector, pattern)
# as grep -i -- grep, ignoring case
"%ilike%" = function(vector, pattern) like(vector, pattern, ignore.case = TRUE)
# as grep -F or fgrep -- grep against a fixed pattern (no regex)
#   (more efficient where applicable)
"%flike%" = function(vector, pattern) like(vector, pattern, fixed = TRUE)
