fifelse <- function(test, yes, no){
  switch(class(yes)[1L],
         "factor" = factor(.Call(CfifelseR,test, yes, no),levels = levels(yes)),
         "list"   = {
           if(class(no)[1L] != "list") stop("Item 'yes' has different class than item 'no'. Please make sure that they have same class type.")
           as.list(c(.Call(CfifelseR,test, unlist(yes), unlist(no))))
         },
         .Call(CfifelseR,test, yes, no)
  )
}