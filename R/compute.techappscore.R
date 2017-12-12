## -------------------------------------------------------
## File: compute.techappscore.R
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

compute.techappscore= function(case,tech,lshowplot=FALSE,lpdfplot=FALSE,aggmethod="geomean",n.sample=1000){
  # This functions computes the attrapp.scores and the techapp.score for a tech in a given case
  # Usage
  # compute.techapp(case, tech, [lshowplot=FALSE])
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. caselist$arbaminch
  # app.fun: both tech and cases contain app.fun, functions for each appropriateness attribute
  # Variables:
  # attrapp.score: the mc.intrgrate(tech$app.fun, case$app.fun)
  # techapp.profile: all the attrapp.core of a given tech & case
  # techapp.score: geometric mean of all attrapp.scores
  # lshowplot: if TRUE plots are generated to illustrate the overall of the case and tech$app.fun
  # Output:
  # tech.app.data: list containing tech, case, techapp.score, techapp.profile (containing names(tech$app.fun), values)

  tech
  
  techapp.profile  = c() # create empty vector to store intermediate result
  attr.names = c() # create empty vectore to store names of used attributes
  n.tech.app.fun=length(names(tech$app.fun)) # number of attributes
  n.case.app.fun=length(names(case$app.fun)) # number of attributes
  n.app.fun=min(n.tech.app.fun,n.case.app.fun)
  
  ## Get tech name and case name
  # From the list if it exists
  if ( ("casename" %in% names(case)) && ("techname" %in% names(tech) ) ){
    techname=tech$techname
    casename=case$casename
  }else{
    # Form the function call argument otherwise (assuming call with case$casename)
    techname=unlist(strsplit(deparse(substitute(tech)),"\\$"))
    techname=techname[2]
    casename=unlist(strsplit(deparse(substitute(case)),"\\$"))
    casename=casename[2]
  }
  
  # in case there are no attributes, n.app.fun is 0. In this case the function retruns a score of 1
  if (n.app.fun==0){
    techapp.data=list()
    techapp.data$tech=techname
    techapp.data$case=casename
    techapp.data$techapp.score=1
    techapp.data$techapp.profile=as.list(c("attr"="NULL"))
    return(techapp.data)
  }
  
  ## Prepare plot for visual interpretation of results if lshowplot=TRUE
  # parameters
  techcolor="darkorange"
  casecolor="dodgerblue"
  
  if(lshowplot==TRUE){
    # Create multiple plot table
    par(mfcol=c(n.app.fun,1)) # dimensions of plot, the raw number is equal to the number of appropriateness functions
    par(mar = c(3, 3, 3, 2), oma = c(2, 1, 2, 4)) # margins for plot window
  }
  if(lpdfplot==TRUE){
    #initiating a pdf plot
    pdf(file=paste0(getwd(),"/plots/","techapp.score - ",casename,", ",techname,".pdf"),width=7,height=9)
    par(mfrow=(c(4,2)), oma=c(2,1,2,4)) # margins for plot window
  }
  # ****Finally its gowing to becoming interesting: Compute the attribute appropriateness scores and profiles
  # This loup provides the techapp.profiles using a function to integrate the tech and case app functions by sampling (see mc.integrate.R)
  

  for(attr in names(tech$app.fun)){

    # Check that this attribute also exist in case$app.fun, otherwise skip
    if (attr %in% names(case$app.fun)){
      # Store attribute names
      attr.names=c(attr.names,attr)
      # Calculate app.score
      f1 = tech$app.fun[[attr]]
      f2 = case$app.fun[[attr]]
      attrapp.score = mc.integrate(f1,f2,n.sample)
      techapp.profile = c(techapp.profile, attrapp.score)

  # Now create the plots for visual analysis of the results (only if lshowplot=TRUE)

      # Compute total technology appropriatness score 
      if(aggmethod=="geomean"){
      l=length(na.omit(techapp.profile))
      techapp.score=(prod(na.omit(techapp.profile)))^(1/l) 

      }
      if(aggmethod=="mean"){
        techapp.score=mean(na.omit(techapp.profile))
      }
      if(lshowplot==TRUE){
        # define plot xlim using max value
        maxxlim=40000 # max possible value
        xval=seq(0,maxxlim,1) # vector of values to evaluate the last non-zero point in the intevall 1:maxxlim
        # only used for plotting as not precise because uses only integer values
        # for functions with max smaller than 1, put 0.1 as intervall in seq
        techval=tech$app.fun[[attr]](xval) # computing tech$app.fun for xval
        caseval=case$app.fun[[attr]](xval) # computing case$app.fun for xval
        Xmaxtech=xval[max(which(techval>0))]   # find the highest xval for which techval is nonzero
        # in case of warning message "In max(which(techval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxtech>=maxxlim) Xmaxtech=0.0001 # if tech$app.fun is const until Inf set Xmaxtech to very small in order not to be considered
        Xmaxcase=xval[max(which(caseval>0))]   # find the highest xval for which caseval is nonzero
        # in case of warning message "In max(which(caseval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxcase>=maxxlim) Xmaxcase=0.0001 # if tech$app.fun is const until Inf set Xmaxcase to very small in order not to be considered
        Xmaxplot=max(Xmaxtech,Xmaxcase)+1 # set the higher of Xmaxcase and Xmactech as Xmax for plots
        # plot
        plot(tech$app.fun[[attr]], main=paste(attr,"-","attrapp.score",attrapp.score), xlab=" ", ylab=" ", xlim=c(0,Xmaxplot), col=techcolor)
        cex_label= par("cex")*par("cex.lab")
        axis(side = 2,col=techcolor)
        par(new = T)
        plot(case$app.fun[[attr]], col=casecolor, axes = FALSE, xlab = " ", ylab = " ",xlim=c(0,Xmaxplot))
        axis(side = 4,col=casecolor)
        legend(x="topleft",legend=c(paste("caseapp.fun=",c(case$app.fun[[attr]])),paste("techapp.fun=",c(tech$app.fun[[attr]]))),col=c(casecolor,techcolor),
               inset=.02, lwd=3, lty=c(1,1),cex = 0.7,text.width = NULL, bty="n")
      }
      if(lpdfplot==TRUE){
        
        # define plot xlim using max value
        maxxlim=40000 # max possible value
        xval=seq(0,maxxlim,1) # vector of values to evaluate the last non-zero point in the intevall 1:maxxlim
        # only used for plotting as not precise because uses only integer values
        # for functions with max smaller than 1, put 0.1 as intervall in seq
        techval=tech$app.fun[[attr]](xval) # computing tech$app.fun for xval
        caseval=case$app.fun[[attr]](xval) # computing case$app.fun for xval
        Xmaxtech=xval[max(which(techval>0))]   # find the highest xval for which techval is nonzero
        # in case of warning message "In max(which(techval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxtech>=maxxlim) Xmaxtech=0.0001 # if tech$app.fun is const until Inf set Xmaxtech to very small in order not to be considered
        Xmaxcase=xval[max(which(caseval>0))]   # find the highest xval for which caseval is nonzero
        # in case of warning message "In max(which(caseval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxcase>=maxxlim) Xmaxcase=0.0001 # if tech$app.fun is const until Inf set Xmaxcase to very small in order not to be considered
        Xmaxplot=max(Xmaxtech,Xmaxcase)+1 # set the higher of Xmaxcase and Xmactech as Xmax for plots
        # plot
        plot(tech$app.fun[[attr]], main=paste(attr,"-","attrapp.score",attrapp.score), xlab=" ", ylab=" ", xlim=c(0,Xmaxplot), col=techcolor)
        cex_label= par("cex")*par("cex.lab")
        axis(side = 2,col=techcolor)
        par(new = T)
        plot(case$app.fun[[attr]], col=casecolor, axes = FALSE, xlab = " ", ylab = " ",xlim=c(0,Xmaxplot))
        axis(side = 4,col=casecolor)
        legend(x="topleft",legend=c(paste("caseapp.fun=",c(case$app.fun[[attr]])),paste("techapp.fun=",c(tech$app.fun[[attr]]))),col=c(casecolor,techcolor),
               inset=.02, lwd=3, lty=c(1,1),cex = 0.7,text.width = NULL, bty="n")
        
    }  
    }
    else{ 
    techapp.profile = c(techapp.profile, NA)
    attr.names=c(attr.names,attr)
    }

  }
  if(length(na.omit(techapp.profile))==0) {
    techapp.score=1     # defines the techapp.score to be 1 if no attribute from this technology is listed in the casedata.
    plot.new()
    } 
  if(lpdfplot==TRUE){
    title(paste(techname,", ",casename,"- techapp.score",techapp.score), outer=TRUE)
    dev.off() #beenden des pdf plots
  }

  ## Create a list with the results
  techapp.profile=setNames(techapp.profile,attr.names)
  techapp.data=list(case=casename, tech=techname, techapp.score=techapp.score, techapp.profile=as.list(techapp.profile))
  techapp.data
}

