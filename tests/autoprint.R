require(data.table)
# Tests the suppression of := output
# Since this tests autoprinting at the console, it needs to use the .Rout.save mechanism in R CMD check
DT = data.table(a=1:2)                # Should print at console?
DT                                    # yes
DT[1]                                 # yes
DT[2,a:=3L]                           # no
DT                                    # yes
DT[FALSE,a:=3L]                       # no
DT[a==4L,a:=5L]                       # no
DT[a %in% 4:8, a:=5L]                 # no
DT                                    # yes
print(DT[2,a:=4L])                    # yes, as of #6631
print(DT)                             # yes
if (TRUE) DT[2,a:=5L]                 # no. used to print before v1.9.5
if (TRUE) if (TRUE) DT[2,a:=6L]       # no. used to print before v1.9.5
(function(){DT[2,a:=5L];NULL})()      # print NULL
DT                                    # no (from v1.9.5+). := suppresses next auto print (can't distinguish just "DT" symbol alone at the prompt)
DT                                    # yes. 2nd time needed, or solutions below
(function(){DT[2,a:=5L];NULL})()      # print NULL
DT[]                                  # yes. guaranteed print
(function(){DT[2,a:=5L];NULL})()      # print NULL
print(DT)                             # yes. restored in #6631 behavior that had changed in 1.9.6.
(function(){DT[2,a:=5L][];NULL})()    # print NULL
DT                                    # yes. i) function needs to add [] after last one, so that "DT" alone is guaranteed anyway
(function(){DT[2,a:=5L];DT[];NULL})() # print NULL
DT                                    # yes. ii) or as a separate DT[] after the last := inside the function
DT2 = data.table(b=3:4)               # no
(function(){DT[2,a:=6L];DT2[1,b:=7L];NULL})()
DT                                    # yes. last := was on DT2 not DT
{DT[2,a:=6L];invisible()}             # no
print(DT)                             # yes
(function(){print(DT[2,a:=7L]);print(DT);invisible()})()    # yes*2
{print(DT[2,a:=8L]);print(DT);invisible()}                  # yes*2  as at prompt, again as of #6631
DT[1][,a:=9L]      # no (was too tricky to detect that DT[1] is a new object). Simple rule is that := always doesn't print
DT[2,a:=10L][1]                       # yes
DT[1,a:=10L][1,a:=10L]                # no
DT[,a:=as.integer(a)]                 # no
DT[1,a:=as.integer(a)]                # no
DT[1,a:=10L][]                        # yes. ...[] == oops, forgot print(...)

# Test that error in := doesn't suppress next valid print, bug #2376
tryCatch(DT[,foo:=ColumnNameTypo], error=function(e) e$message)         # error: not found.
DT                                    # yes
DT                                    # yes

# Regression test for auto-printing suppression in source(), #2369
local({
  f = tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeLines(c(
    "library(data.table)",
    "DT = data.table(a = 1)",
    "DT[,a:=1]"                       # no
  ), f)
  source(f, local = TRUE, echo = TRUE)
})

# child class of data.table doesn't induce unintended print, #3029
dt = data.table(x = 1)
class(dt) = c("foo", "data.table", "data.frame")
print.foo = function(x, ...) {
  NextMethod("print")
}
dt[, y := 1]                          # no

# withAutoprint() testing (since R3.4.0)
if (!exists("withAutoprint", baseenv())) {
  q("no")
}
if (TRUE) withAutoprint({
  DT                                  # yes
  DT[1L, 1L]                          # yes
  DT[2L, a := 10L]                    # no
})
