.preprocess = function (f) {
  diff_v_master = system2('git', c('diff', 'master', f), stdout=TRUE)
  if (length(diff_v_master)) f
}
