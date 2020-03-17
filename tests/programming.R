if (exists("cc")) { ## dev mode
  cc()
} else {
  library(data.table)
  substitute2 = data.table:::substitute2
}

# simple
substitute2(list(var = val), env = list(var="my_var", val=5L))

# AsIs way to handle char.to.name argument
substitute2(list(var = val), env = list(var="my_var", val=I("my_val")))
substitute2(list(var = val), env = I(list(var=as.name("my_var"), val="my_val")))

# test non-scalar char
substitute2(list(var = val), env = list(var="my_var", val=c("a","b")))
substitute2(list(var = val), env = list(var="my_var", val=I(c("a","b"))))
substitute2(list(var = val), env = I(list(var=as.name("my_var"), val=c("a","b"))))

# test non-symbol
substitute2(list(var = val), env = list(var=I("my_var"), val="my_val"))
substitute2(list(var = val), env = I(list(var="my_var", val="my_val")))

# complex use case
substitute2(
  .(fun_ans_var = fun(farg1, farg2=farg2val), timestamp=Sys.time(), col_head = head(head_arg, n=1L)),
  list(
    fun_ans_var = "my_mean_res",
    fun = "mean",
    farg1 = "my_x_col",
    farg2 = "na.rm",
    farg2val = TRUE,
    col_head = "first_y",
    head_arg = "y"
  )
)

# calls of length 0 args
const1 = function() 1L
substitute2(list(nm = fun()), env=list(a="b", fun="const1", nm="int1"))
substitute2(.(), env=list(a="b", fun="const1", nm="int1"))

# some special names
substitute2(.("TRUE" = 1L, "FALSE" = 2L, "1" = 3L, "2" = 4L),
            env=list("FALSE"="col2", "2"="col4"))

# PR example
substitute2(
  .(out_col_name = fun(in_col_name, fun_arg1=fun_arg1val)),
  env = list(
    in_col_name = "x",
    fun = "sum",
    fun_arg1 = "na.rm",
    fun_arg1val = TRUE,
    out_col_name = "sum_x"
  )
)

# re-use inside another function
f = function(expr, env) {
  eval(substitute(
    substitute2(.expr, env),
    list(.expr = substitute(expr))
  ))
}
f(
  .(out_col_name = fun(in_col_name, fun_arg1=fun_arg1val)),
  env = list(
    in_col_name = "x",
    fun = "sum",
    fun_arg1 = "na.rm",
    fun_arg1val = TRUE,
    out_col_name = "sum_x"
  )
)
