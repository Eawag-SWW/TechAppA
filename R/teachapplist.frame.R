## -------------------------------------------------------
## File: techapplist.frame.R
##
## December, 2017 ??? Dorothee Spuhler
## dorothee.spuhler@eawag.ch
##
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## ==============================================================================================

# function to transform the techapplist into a data frame

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
  
  