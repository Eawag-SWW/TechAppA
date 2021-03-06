## -------------------------------------------------------
## File: compute.techapplist.R
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

compute.techapplist= function(caselist, techlist, lsort=FALSE,lshowplot=FALSE,lpdfplot=FALSE,aggmethod="geomean",n.sample=1000){
  # This function computes for each pair of caselist$case and techlist$case:
  # Usage
  # compute.techapplist(caselist,techlist, [lshowplot=FALSE])
  # Input:
  # caselist: a list of cases with inforows and app.funcitons for each attributes
  # techlist: a list of potential sanitation technologies with inforows and app.functions for each attribute
  # lsort : logical, if TRUE result is sorted by score
  # Variables:
  # See 'compute.techapp(case, tech)': attrapp.score, techapp.profile: all the attrapp.core of a given tech & case, techapp.score
  # Output:
  # techapplist: list containing case,tech, techapp.score, techapp.profile (containing names(tech$app.fun), values)
  
  # Create empty list
  techapplist=list()
  
  # Loop over all case and tech
  for (casename in names(caselist)){
    case=caselist[[casename]]
    case$casename=casename # add casename to the list
    appsublist=list() # store all tech for a given case in a list
    for (techname in names(techlist)){
      tech=techlist[[techname]]
      tech$techname=techname #add techname to the list
      #fu=techlist[[techname]]
      #tech$fu=fu
      if (lshowplot){
        app.item.tmp <- compute.techappscore(case, tech,lshowplot=TRUE,lpdfplot,aggmethod,n.sample)
      } else {
        app.item.tmp <- compute.techappscore(case, tech,lshowplot=FALSE,lpdfplot,aggmethod,n.sample)
      }
      appsublist=append(appsublist,list(app.item.tmp))
    }
    if (lsort){
      appsublist=list.sort(appsublist,(techapp.score)) # parenthesis are telling to sort in descending order
    }
    techapplist=append(techapplist,appsublist)
  }
  #return techapplist
  techapplist
  }