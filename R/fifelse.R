fifelse <- function(test, yes, no){
  switch(class(yes)[1L], # Investigate if this can be moved to the C code
         "factor" = factor(.Call(CfifelseR,test, yes, no),levels = levels(yes)),
         .Call(CfifelseR,test, yes, no)
  )
}