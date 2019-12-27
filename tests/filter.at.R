library(data.table)
options(datatable.auto.index=FALSE)

############################### filter.at ################################
set.seed(123)
dt <- data.table(replicate(3, sample(c(T, F), 1E2, replace = T)))
cols <- c('V1', 'V2', 'V3')
cols2 <- c('V1', 'V2')

# dt[filter.at(cols, logic, by, all.ties = T)]

# `&` with no by
#optimized
options(datatable.verbose=TRUE)
dt[filter.at(cols = TRUE, logic = x)]
dt[filter.at(c('V1','V2','V3'), x)]
dt[filter.at(cols, x)]
dt[filter.at(cols2, x)]
dt[filter.at(c('V1'), x)]
dt[filter.at(V1, x)]

#non-optimized
dt[filter.at(patterns('V'), logic = x)]
dt[filter.at(V1:V3, x)]

# to show all identical and not performance:
options(datatable.verbose=FALSE)

#bench::mark(dt[filter.at(cols = TRUE, logic = x)],
#            dt[filter.at(c('V1','V2','V3'), x)],
#            dt[filter.at(patterns('V'), logic = x)],
#            dt[filter.at(V1:V3, x)],
#            dt[V1 & V2 & V3] #creates index with default options
#            )

# `|` with no by
dt[filter.at(cols = TRUE, logic = x, all.vars = F)]

identical(dt[filter.at(TRUE, x, all.vars = F)],
          dt[filter.at(c('V1', 'V2', 'V3'), x, all.vars = F)])

identical(dt[filter.at(TRUE, x, all.vars = F)],
          dt[V1 | V2 | V3])

# which.min and which.max have different properties - they perform Reduce(`union`, ...)
# this functionality should probably be removed as it adds too much complexity

set.seed(123)
dt <- data.table(ID = sample(letters[1:5], 1E2, replace = T), replicate(3, round(runif(1E2),2)))

# without by
dt[filter.at(TRUE, which.min(x))] #warning because of which.min(ID)
dt[filter.at(c('V1', 'V2', 'V3'), which.min(x))]
dt[filter.at(patterns('V'), which.min(x))]
dt[filter.at(V1:V3, which.min(x))]

# with by
dt[filter.at(TRUE, which.min(x), by = ID)]

######### see https://stackoverflow.com/questions/59333424/select-row-from-data-table-in-programming-mode/59338483#59338483 #######
tb = data.table(g_id = c(1, 1, 1, 2, 2, 2, 3),
                item_no = sample(c(24,25,26,27,28,29,30)),
                time_no = c(100, 110, 120, 130, 140, 160, 170)
)

mincol = "item_no"
grp = "g_id"

tb[, .SD[which.min(get(mincol))], grp]
tb[filter.at(mincol, which.min(x), grp)]

################### see https://github.com/Rdatatable/data.table/issues/4105 ##############################
set.seed(1)
foo <- data.table(
  x = as.character(runif(n = 10^6)),
  y = as.character(runif(n = 10^6)),
  z = as.character(runif(n = 10^6))
)

#bench::mark(
#  foo[filter.at(c('x', 'y', 'z'), like(x, '123'))],
#  foo[filter.at(TRUE, like(x, '123'))],
#  foo[filter.at(x:z, like(x, '123'))],
#  foo[like(x, "123")][like(y, "123")][like(z, "123")],
#  foo[like(x, "123") & like(y, "123") & like(z, "123")]
#  )

## A tibble: 5 x 13
#  expression                                              min median `itr/sec` mem_alloc `gc/sec`
#  <bch:expr>                                            <bch> <bch:>     <dbl> <bch:byt>    <dbl>
#1 foo[filter.at(c("x", "y", "z"), like(x, "123"))]      315ms  315ms      3.17     7.9MB     3.17
#2 foo[filter.at(TRUE, like(x, "123"))]                  310ms  313ms      3.20     7.9MB     0   
#3 foo[filter.at(x:z, like(x, "123"))]                   859ms  859ms      1.16   22.95MB     0   
#4 foo[like(x, "123")][like(y, "123")][like(z, "123")]   290ms  292ms      3.43    8.13MB     0   
#5 foo[like(x, "123") & like(y, "123") & like(z, "123")] 858ms  858ms      1.17    22.9MB     0   

# see https://stackoverflow.com/questions/58570110/how-to-delete-rows-for-leading-and-trailing-nas-by-group-in-r
df1<-data.frame(ID=(rep(c("C1001","C1008","C1009","C1012"),each=17)),
                Year=(rep(c(1996:2012),4)),x1=(floor(runif(68,20,75))),x2= 
                  (floor(runif(68,1,100))))

#Introduce leading / tailing NAs
df1[cbind(c(1:5, 18:23, 35:42, 49:51, 66:68),c(rep(c(3, 4, 4, 3, 3), c(5,6,8,3,3))))]<-NA

#introduce "gap"- NAs
set.seed(123); df1$x1[rbinom(68,1,0.1)==1]<-NA; df1$x2[rbinom(68,1,0.1)==1]<-NA

setDT(df1)

df1[filter.at(c('x1','x2'), !(rleid(x) %in% c(1, max(rleid(x))) & is.na(x)), ID)]
df1[filter.at(x1:x2, !(rleid(x) %in% c(1, max(rleid(x))) & is.na(x)), ID)]
df1[filter.at(patterns('x'), !(rleid(x) %in% c(1, max(rleid(x))) & is.na(x)), ID)]

fx = function (x) {!(rleid(x) %in% c(1, max(rleid(x))) & is.na(x))}
df1[filter.at(x1:x2, fx, ID)]

################## see https://github.com/Rdatatable/data.table/issues/2655 ##################################
set.seed(1234)
DT <- data.table(foo = rep(LETTERS[1:2],8),
                 bar = rep(letters[17:20],4),
                 month = rep(month.name[1:4],4),
                 day = seq_len(16),
                 yyy = rnorm(16,mean = 0.5,1.5))

## Expression 1: with hard coded columns
DT[yyy > 0,.(NewCol = paste(month, day),
             bar,
             yyy), by = foo]

## Expression 2: everything passed by reference
A = "yyy"; B = 0; C = "NewCol"; D = "month";E = "day";G = "foo"; H = c("bar","yyy")

# OP wants:
#DT[..A > ..B , .(..C = paste(..D, ..E),
#                 ..H), by = ..G]

# a little closer
DT[filter.at(A, x > B),
   .(NewCol = paste(month, day),
     bar,
     yyy),
   by = G]

