library(data.table)

# cc() ## dev mode

# simple
substitute2(list(var = val), env = list(var="my_var", val=5L))

# AsIs way to handle char.to.name argument
substitute2(list(var = val), env = list(var="my_var", val=I("my_val")))
substitute2(list(var = val), env = I(list(var=as.name("my_var"), val="my_val")))

# test non-scalar char
try(
  substitute2(list(var = val), env = list(var="my_var", val=c("a","b")))
)
substitute2(list(var = val), env = list(var="my_var", val=I(c("a","b"))))
substitute2(list(var = val), env = I(list(var=as.name("my_var"), val=c("a","b"))))

# test non-symbol
try(
  substitute2(list(var = val), env = list(var=I("my_var"), val="my_val"))
)
try(
  substitute2(list(var = val), env = I(list(var="my_var", val="my_val")))
)

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

# data.table i, j, by
d = data.table(a = 2:1, b = 1:4)
d[var3%in%values, .(var1 = f(var2)), by=var3,
  env=list(var1="res", var2="b", f="sum", var3="a", values=0:3),
  verbose=TRUE]

# data.table symbols and chars
d = data.table(a = c("b","a"), b = 1:4)
d[var3%in%values, .(var1 = f(var2)), keyby=var3,
  env=list(var1="res", var2="b", f="sum", var3="a", values=I(c("a","b","c"))),
  verbose=TRUE]
d[var3%in%values, .(var1 = f(var2)), keyby=var3,
  env=I(list(var1=as.name("res"), var2=as.name("b"), f=as.name("sum"), var3=as.name("a"), values=c("a","b","c"))),
  verbose=TRUE]

# test that AsIs class removed
class(substitute2(var3%in%values, list(var3="a", values=I(c("a","b","c"))))[[3L]]) == "character"
class(substitute2(var3%in%values, I(list(var3=as.name("a"), values=c("a","b","c"))))[[3L]]) == "character"
class(substitute2(var3%in%values, list(var3="a", values=I(1:3)))[[3L]]) == "integer"
class(substitute2(var3%in%values, I(list(var3=as.name("a"), values=c(1:3))))[[3L]]) == "integer"

# NA tests, especially NA_character_
#TODO

# char.as.name=TRUE but env is AsIs
#TODO
#remove char.as.name arg from API and keep I() interface only?

# get and mget use cases
#TODO

# use DT[, var, env=list(var=quote(.(sum_x=sum(x)))] rather than dt[, eval(var)]?
#TODO
