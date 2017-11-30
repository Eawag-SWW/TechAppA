techapplist.frame=function(techapplist,caselist){ 
 
  #define column for dataframe
  case <- sapply(techapplist,function(z) z$case)
  tech <- sapply(techapplist,function(z) z$tech)
  techapp.score <- sapply(techapplist,function(z) z$techapp.score)
  
  techappframe=data.frame(case=case,tech=tech,techapp.score=techapp.score)
  
  # make loop over all attributes in appropriateness profile
  attrnames=c()
  for (i in 1:length(caselist)) {
      for (j in 1:length(caselist[[1]]$app.fun))  {
      attrnames <- c(attrnames, names(caselist[[i]]$app.fun[j]))
      }
  }
  
  
  for (name in attrnames) {
    
    # helper function to transform empty values (NULLO) to NAs
    f.attrlist <- function(z) {
      if(is.null(z$techapp.profile[[name]])){
        NA
      }else{
        z$techapp.profile[[name]]
      }
    }

    techappframe[[name]] = unlist(sapply(techapplist, f.attrlist))
    
    }
  
  View(techappframe)

  
}
  
  