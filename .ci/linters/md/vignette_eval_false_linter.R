# ensure that ```r is preferred to 'eval=FALSE' for plain code chunks
vignette_eval_false_linter = function(md) {
  if (!grepl('[.]Rmd$', md)) return(invisible())
  md = readLines(md)
  bad_lines = grep(R"[eval\s*=\s*F(?:ALSE)?\b]", md)
  if (!length(bad_lines)) return(invisible())
  cat(sprintf(
    "Prefer '```r' chunks to ones using eval=FALSE (lines %s)", toString(bad_lines)
  ))
  stop('Please fix the vignette issues above')
}
