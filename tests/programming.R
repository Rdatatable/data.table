cc(F)

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
  ),
  char.as.name=TRUE
)

const1 = function() 1L
substitute2(list(nm = fun()), env=list(a="b", fun="const1", nm="int1"), char.as.name=TRUE)
substitute2(.(), env=list(a="b", fun="const1", nm="int1"), char.as.name=TRUE)

substitute2(.("TRUE" = 1L, "FALSE" = 2L, "1" = 3L, "2" = 4L),
            env=list("FALSE"="col2", "2"="col4"),
            char.as.name=TRUE)

substitute2(
  .(out_col_name = fun(in_col_name, fun_arg1=fun_arg1val)),
  env = list(
    in_col_name = "x",
    fun = "sum",
    fun_arg1 = "na.rm",
    fun_arg1val = TRUE,
    out_col_name = "sum_x"
  ),
  char.as.name = TRUE
)
#.(sum_x = sum(x, na.rm = TRUE))