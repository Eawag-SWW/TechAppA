## -------------------------------------------------------
## File: compute.sysapplist.R
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


#this code uses the function: compute.techapplist as well as mc.integrate to 
#calculate the system appropiatness score for each system (from syslist)
#the output is a list (sysapplist) that contains information about the system appropiatness
#the aggregation method as well as the plots you want to generate (pdf or viewer) has to the 
#choosen as inpt of the function.


compute.sysapplist= function(syslist,caselist,techlist,lsort,lshowplot,lpdfplot,aggmethod){

  if(aggmethod=="product"){ #if aggregation mothod is set to "product" the following code is used
    applist<-compute.techapplist(caselist,techlist,lsort,lshowplot,lpdfplot,aggmethod) #running compute.techapplist to generate the "applist" which is used in the following code
  #get cases names  
  cases=names(caselist)
  #start a loop over each case  
  ll=list()  #initiate a empty list to fill during the loops
  for(g in 1:length(sapply(names(caselist),length))){
    #initiate vektors and lists which are filled during the loop   
    sysapp.score=c()
    techapp.score=c()
    systechapp.score=c()
    sysattrapp.score=c()
    sysapp.score.tot=c()
    ll.sys=list()
    ll.case=list()
    ll.sys2=list()
    ll.case=cases[g]
    #start a loop over all systems enbeded in syslist
    for(j in 1:length(sapply(syslist,length))){
      #initiate vektor which are filled during the loop     
      sysattrapp.profile=c() #vektor containing the system atrribute appropriatness scores
      sysattrapp.profile.summary=list()
      systechapp.profile.summary=list()
      ###calculating the system attribute appropriatness score for each system attribute   
      for(attr in names(syslist[[j]]$app.fun)){
        if(attr %in% names(caselist[[g]]$app.fun)){
          f1 = syslist[[j]]$app.fun[[attr]]
          f2 = caselist[[g]]$app.fun[[attr]]
          attrapp.score = mc.integrate(f1,f2)
          sysattrapp.profile = c(sysattrapp.profile, attrapp.score)
          sysattrapp.profile.summary[[attr]]=attrapp.score
        }
      }
      if(length(sysattrapp.profile)==0){sysattrapp.profile=1}  #assuming,that if the system attribute is not in the casedata, it is 100% appropriate 
      #calculating the total system attribute appropriatness score by product  
      l.sysattr=length(sysattrapp.profile)
      totsysattrapp.score=(prod(sysattrapp.profile))^(1/l.sysattr)
      
      ###calculating the technology appropriatness score    
      for(i in 1:length(sapply(applist,length))){ 
        
        if(applist[[i]]$case == cases[[g]] &  
           applist[[i]]$tech %in% names(syslist[[j]]$techs)){
          
          techapp.score[i]=applist[[i]]$techapp.score  #gives you the techapp.score calculated my campute.techapp 
          # in a vektor (product of attr scores) 
          systechapp.profile.summary[[applist[[i]]$tech]]=techapp.score[i]
        }
      }
      systechappscore.profile=techapp.score[!is.na(techapp.score)]  #deletes empty (NA) cells from vector
      #calculating the system technology appropriatness score (summarizes all technology appropriatnes scores (by product) into one score)  
      l.systech=length(systechappscore.profile)
      totsystechapp.score=(prod(systechappscore.profile))^(1/l.systech)
      #combining system technology appropriatness score and system attribute appropriatness score inro one score by product
      sysapp.score[[j]]=totsysattrapp.score*totsystechapp.score   
      #ceating a list containing the system appropriatnes scores for each system and case (one list for each case)  
      ll.sys2$Case=cases[g]
      ll.sys2$System=names(syslist)[[j]]
      ll.sys2$sysappscore=sysapp.score[[j]]
      ll.sys2$sysattrapp.profile=sysattrapp.profile.summary
      ll.sys2$systechapp.profile=systechapp.profile.summary
      ll.sys[[j]]=ll.sys2
      ll[[ll.case]]=ll.sys #writing the list of each case into a global list containing all cases
    }
    if(lpdfplot==TRUE){
    #creating a pdf plot sowing the system appropiatness of all systems for each case in a barplot 
    pdf(file=paste0(getwd(),"/plots/","System appropriatnes score (by prod.)  ",cases[[g]],".pdf"))
    barplot(sysapp.score, main=cases[g], ylim= c(0:1),names.arg=names(syslist),
            ylab="System appropriatnes score")
    dev.off()
    }
  }
  ll
  }
  if(aggmethod=="mean"){  #if aggregation mothod is set to "mean" the following code is used
    applist<-compute.techapplist(caselist,techlist,lsort,lshowplot,lpdfplot,aggmethod)
    #get cases names  
    cases=names(caselist)
    #start a loop over each case  
    ll=list() 
    for(g in 1:length(sapply(names(caselist),length))){
      #initiate vektors which are filled during the loop   
      sysapp.score=c()
      systechapp.score=c()
      sysattrapp.score=c()
      sysapp.score.tot=c()
      ll.sys=list()
      ll.case=list()
      ll.sys2=list()
      ll.case=cases[g]
      #start a loop over all systems enbeded in syslist
      for(j in 1:length(sapply(syslist,length))){
        
        ##################################   system attribute appropriatness score
        
        #initiate vektors which are filled during the loop     
        sysattrapp.profile=c() #vektor containing the system atrribute appropriatness scores
        sysattrapp.profile.summary=list()
        systechapp.profile.summary=list()
        #calculating the system attribute appropriatness score for each system attribute   
        for(attr in names(syslist[[j]]$app.fun)){
          if(attr %in% names(caselist[[g]]$app.fun)){
            f1 = syslist[[j]]$app.fun[[attr]]
            f2 = caselist[[g]]$app.fun[[attr]]
            attrapp.score = mc.integrate(f1,f2)
            sysattrapp.profile = c(sysattrapp.profile, attrapp.score)
            sysattrapp.profile.summary[[attr]]=attrapp.score
          }
        }
        if(length(sysattrapp.profile)==0){sysattrapp.profile=1}  #assuming,that if the system attribute is not in the casedata, it is 100% appropriate 
        #calculating the total system attribute appropriatness score by mean
        totsysattrapp.score=mean(sysattrapp.profile)
        
        ################################  technology appropriatness score 
        
        techapp.score=c()
        # calculating the technology appropriatness score    
        for(i in 1:length(sapply(applist,length))){ 
          
          if(applist[[i]]$case == cases[[g]] &  
             applist[[i]]$tech %in% names(syslist[[j]]$techs)){
            
            techapp.score[i]=applist[[i]]$techapp.score  #gives you the techapp.score calculated my campute.techapp 
           
            systechapp.profile.summary[[applist[[i]]$tech]]=techapp.score[i]
          }
        }
        systechappscore.profile=techapp.score[!is.na(techapp.score)]  #deletes empty (NA) cells from vector
        #calculating the system technology appropriatness score (summarizes all technology appropriatnes scores (by mean) into one score)  
        totsystechapp.score=mean(systechappscore.profile)
        ###################################      
        
        # calculating the total system attribute appropriatness score by mean
        sysapp.score[[j]]=totsysattrapp.score*totsystechapp.score   #combining system technology appropriatness score and system 
        #attribute appropriatness score inro one score by product
        #ceating a list containing the system appropriatnes scores for each system and case (one list for each case)  
        ll.sys2$Case=cases[g]
        ll.sys2$System=names(syslist)[[j]]
        ll.sys2$sysappscore=sysapp.score[[j]]
        ll.sys2$sysattrapp.profile=sysattrapp.profile.summary
        ll.sys2$systechapp.profile=systechapp.profile.summary
        ll.sys[[j]]=ll.sys2
        ll[[ll.case]]=ll.sys #writing the list of each case into a global list containing all cases
      }
      if(lpdfplot==TRUE){
      #creating a pdf plot sowing the system appropiatness of all systems for each case in a barplot 
      pdf(file=paste0(getwd(),"/plots/","System appropriatnes score (by mean)  ",cases[[g]],".pdf"))
      barplot(sysapp.score, main=cases[g], ylim= c(0:1),names.arg=names(syslist),
              ylab="System appropriatnes score")
      dev.off()  
      }
    }
    ll
  }
ll
}