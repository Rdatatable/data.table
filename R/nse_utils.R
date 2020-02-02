sub_is_fun = function(e, fun) is.call(e) && is.symbol(e[[1L]]) && e[[1L]] == fun
sub_is_fun_length = function(e, fun, n, compare = `==`) is.call(e) && compare(length(e), n) && is.symbol(e[[1L]]) && e[[1L]] == fun
sub_not_fun = function(e, fun) is.call(e) && is.symbol(e[[1L]]) && e[[1L]] != fun
# TODO: chmatch could work on expressions directly for conciseness here
sub_in_funs = function(e, funs) is.call(e) && is.symbol(e[[1L]]) && as.character(e[[1L]]) %chin% funs
sub_in_funs_length = function(e, funs, n, compare = `==`) is.call(e) && compare(length(e), n) && is.symbol(e[[1L]]) && as.character(e[[1L]]) %chin% funs
sub_not_funs = function(e, funs) is.call(e) && is.symbol(e[[1L]]) && !as.character(e[[1L]]) %chin% funs
