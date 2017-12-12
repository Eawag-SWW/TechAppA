## -------------------------------------------------------
## File: build.list.R
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

build.list <- function(filename,n.info.row=NULL){ 
  # This function reads the technology and case input data stored in a csv file...
  # filename: csv name containing input data for technologies/cases
  # n.info.row: Number of information rows in 'data.csv'.
  #    A information row is one with no attribute, but other information in the beginning (e.g. 'functional.group').
  #   First line containing the 'names' of the technologies is not counted in n.info.row.
  # info.rows have one item per row, attributes have three rows: name, function, and function variables
  
  dat <- read.table(filename, sep=";", stringsAsFactors=FALSE)

  ll <- list()                          # list of technologies or cases

  # determine n.inf.row if not specified as argument
  if (is.null(n.info.row)){
    if ("attr1" %in% dat[,1]){
      n.info.row=which(dat[,1] %in% "attr1")-2 #number of info line is the number of line until attr1 minus 1, we substract again minus 1 for the title line 
    }else{
      stop("No attr1 string in first column")
    }
  }
  
  ## loop over all tech/cases
  for(i in 2:ncol(dat)){
    
    ll.i = list()  
    for(j in 1:n.info.row) {
      ll.i[[dat[j+1,1]]] = unlist(strsplit(sub(" ", "", dat[j+1,i]), ","))
    }
    
    ## loop over all attributes
    ll.i$app.fun  <- list()
    for(att in 1:((nrow(dat)-n.info.row-1)/3)){
      
      if(!is.na(dat[3*att-1+n.info.row,i]) & dat[3*att-1+n.info.row,i]!="") {
        f.string <- paste0("function(x){", dat[3*att+n.info.row,i],
                           "(x, ", dat[3*att+n.info.row+1,i], ")}")
        ll.i$app.fun[[dat[3*att-1+n.info.row,i]]] <- eval(parse(text=f.string))
      }
      
    }
    
    ll[[dat[1,i]]] <- ll.i
  }
  ll
  
}
