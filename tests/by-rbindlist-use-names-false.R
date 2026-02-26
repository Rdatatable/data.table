require(data.table)

DT1 <- data.table(group = 1:2)[
  , data.table(x = 1, y = 2)[, group, with = FALSE]
  , by = group
]

DT2 <- data.table(group = 2:1)[
  , data.table(x = 1, y = 2)[, group, with = FALSE]
  , by = group
]

stopifnot(
  names(DT1)[2L] == "x",
  names(DT2)[2L] == "y"
)
