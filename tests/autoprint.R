require(data.table)
# Tests the suppression of := output
# Since this tests autoprinting at the console, it needs to use the .Rout.save mechanism in R CMD check
DT = data.table(a=1:2)                # Should print at console?
DT                                    # yes
DT[1]                                 # yes
DT[2,a:=3L]                           # no
DT                                    # yes
print(DT[2,a:=4L])                    # yes
print(DT)                             # yes
(function(){DT[2,a:=5L];NULL})()      # no
DT                                    # yes
{DT[2,a:=6L];invisible()}             # no
print(DT)                             # yes
(function(){print(DT[2,a:=7L]);print(DT);invisible()})()    # yes*2
{print(DT[2,a:=8L]);print(DT);invisible()}                  # yes*2
DT[1][,a:=9L]      # yes (since DT[1] is a new select) due to is.name in := in [.data.table
DT[2,a:=10L][1]    # yes

