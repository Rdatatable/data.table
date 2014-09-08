\name{joinbyv}
\alias{joinbyv}
\title{ Batch join multiple tables }
\description{
   Perform batch join of multiple tables to one \emph{master} table.
}
\usage{
joinbyv(
  master, join,
  by = lapply(join, key), 
  col.subset = lapply(join, names), 
  row.subset = as.list(rep(TRUE,length(join))),
  nomatch = lapply(row.subset, function(x) if(is.expression(x)) 0 else getOption("datatable.nomatch")),
  allow.cartesian = as.list(rep(getOption("datatable.allow.cartesian"),length(join)))
)
}
\arguments{
  \item{master}{ \code{DT}, eventually also \code{list(DT)}. }
  \item{join}{ \code{list(DT1,DT2,...)} to join multiple tables. }
  \item{by}{ \code{list}, required if \code{join} data.tables do not have keys. A list of character vectors each contains corresponding join table columns join condition. } 
  \item{col.subset}{ \code{list} of character vectors with column names to be kept from corresponding \code{join} tables. By default will all columns from \code{join} table will be kept. }
  \item{row.subset}{ \code{list} of \code{\link{expression}} elements to be sent to corresponding \code{join} tables \code{i} argument (no cross table expressions here). To subset result \code{master} table, use \code{row.subset} together with corresponding \code{nomatch} arg equal to \code{0} (inner join). By default when providing \code{row.subset} list element the corresponding \code{nomatch} will be changed to \code{0} to perform inner join, otherwise it will be \code{getOption("datatable.nomatch")}. If you really want to do outer join to already filtered \code{join} table you need to override corresponding \code{nomatch} arg for \code{NA}. }
    \item{nomatch}{ \code{list} of integer scalar \code{NA, 0} elements corresponding \code{join} tables, indicate outer join for \code{NA} and inner join for \code{0}. }
  \item{allow.cartesian}{ \code{list} of logical scalar elements corresponding to \code{join} tables to define which of the joins are allowed to do cartesian product. }
}
\details{
Any \code{NULL} elements inside the list of arguments \code{by, col.subset, row.subset, nomatch, allow.cartesian} will be replaced by the default value for the argument. Therefore it is possible to pass partially filled lists, \code{length} of each must match to the \code{length(join)}, example \code{col.subset=list(NULL,c("join2_col1","join2_col2"),NULL,c("join4_col1"))}.

Function do not allow cross tables \code{row.subset} filtering expressions. User should apply such filter after \code{joinbyv} as post process: \code{joinbyv(master, join)[join1_colA > join2_colA * 2]}.

Arguments \code{nomatch}, \code{allow.cartesian} by default will be setup according to \code{data.table} options. An exception case described in \code{row.subset} argument will by default override the \code{nomatch} argument to value \code{0} to perform inner join.

This is yet experimental function. Please report any issues to \url{https://github.com/Rdatatable/data.table/issues}.

Possibly the performance might be improved after implementing FR #691 and #692.
}
\value{
    \code{data.table}, denormalized master table joined to defined tables. Columns order according to \code{col.subset}, no key.
}
\author{ Jan Gorecki }
\examples{
\dontrun{
# populate example data BEGIN
product <- 
  data.table(prod_code = 1:16,
             prod_name = paste("prod",letters[1:16]),
             prod_group_code = rep(1:4,4), 
             prod_group_name = paste("prod group",rep(letters[1:4],4)),
             prod_family_code = rep(1:2,8), 
             prod_family_name = paste("prod family",rep(letters[1:2],8)))
customer <- 
  data.table(cust_code = 1:24, 
             cust_name = paste("cust",letters[1:24]),
             cust_mail = paste0("cust",1:24,"@mail.com"),
             cust_hq_code = rep(1:3,8),
             cust_hq_name = paste("cust hq",rep(letters[1:3],8)))
geography <- 
  data.table(state_code = state.abb,
             state_name = state.name,
             division_code = as.integer(state.division),
             division_name = as.character(state.division),
             region_code = as.integer(state.region),
             region_name = as.character(state.region))
time <- 
  data.table(date_code = seq(as.Date(paste0(year(Sys.Date())-1,"-01-01")),
                             as.Date(paste0(year(Sys.Date()),"-12-31")),
                             by = "1 day"))
time[,`:=`(month_code = month(date_code), 
           month_name = months(date_code),
           quarter_code = as.POSIXlt(date_code)$mon \%/\% 3L + 1L,
           year_code = year(date_code))]
n <- 1e3
set.seed(38)
sales <- 
  data.table(prod_code = sample(product[,prod_code], n, TRUE),
             cust_code = sample(customer[,cust_code], n, TRUE),
             state_code = sample(geography[,state_code], n, TRUE),
             date_code = sample(time[,date_code], n, TRUE),
             quantity = rnorm(n, 500, 200),
             value = rnorm(n, 10000,2000))
# populate example data END

# usage of joinbyv

# 1. setkey adhoc in 'by' arg
dt <- joinbyv(master = sales, 
              join = list(product, customer, geography, time),
              by = list("prod_code","cust_code","state_code","date_code"))
print(dt)

# 2. setkeys on 'join' tables before joinbyv
setkeyv(product, "prod_code")
setkeyv(customer, "cust_code")
setkeyv(geography, "state_code")
setkeyv(time, "date_code")
dt <- joinbyv(master = sales, join = list(product, customer, geography, time))
print(dt)

# 3. col.subset - NULL col.subset will results names(join)
dt <- joinbyv(master = sales, join = list(product, customer, geography, time),
              col.subset = list(c("prod_group_name","prod_family_name"),
                                c("cust_name","cust_mail","cust_hq_code"),
                                NULL,
                                c("month_code","quarter_code","year_code")))
print(dt)

# 4. row.subset - will automatically adjust 'nomatch' for inner join on corresponding 'join' DT
dt <- joinbyv(master = sales, join = list(product, customer, geography, time),
              col.subset = list(c("prod_group_name","prod_family_name"),
                                c("cust_name","cust_mail","cust_hq_code"),
                                NULL,
                                c("month_code","quarter_code","year_code")),
              row.subset = list(NULL,
                                NULL,
                                expression(division_name == "West North Central"),
                                NULL))
print(dt)
}
}
\keyword{ data }
