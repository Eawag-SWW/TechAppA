
## =======================================================

## File: appropriateness.runfile.r

## November 29, 2017 -- Dorothee Spuhler
## dorothee.spuhler@eawag.ch

## =======================================================
## PREPARATION
## =======================================================

rm(list=ls()) # Clear your the R memory

### Set the working directory
setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/AppropriatenessDorothee/TechAppA/")

### Load required library packages
library(triangle) # extra package for triangular distribution
library (trapezoid) # extra package for trapezoidial distribution
library(rlist)  # extra package to manupulate/filter app list
library(gridExtra)
library(ColorPalette)
library(ggplot2)
library(greshape2) 

### Load required functions (all part of the Appropriateness folder)
source("R/build.list.r")   # This function reads input data. The file format is csv. The model requires one techdata.csv and one casedata.csv. 
# See below for more details on the format of the input data files.
# build.list(filename,n.info.row)
source("R/appfunctions.r") # contains functions that are not provided in R but can be used to compute attribute values
  # prange(x, lower=-Inf, upper=Inf)
  # drange(x, lower=-Inf, upper=Inf)
  # rrange(x, lower=-Inf, upper=Inf)
  # ptrapez(x, a, b=(d-a)/2+a, c=b, d)
  # dtrapez(x, a, b=(d-a)/2+a, c=b, d)
  # rtrapez(x, a, b=(d-a)/2+a, c=b, d)
  # pcat(x, probs)
  # dcat(x, probs), probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6) !NB. the sum of probs has to be =1
  # rcat(x, probs)
source("R/mc.integrate.r") # This functions computes a monte carlo integration of two continous functions
  # mc.integrate(case.app.fun, tech.app.fun, n.sample=10000)
source("R/compute.techappscore.r") # Returns app.profile and app.score (aggregated profile)
  # compute.techappscore(tech, case,lshowplot=FALSE)
  # plots provide a graphical representaiton of the two functions and the overall
source("R/compute.techapplist.r") # Returns a list of app.profiles & app.score for all the techs and caes of a techlist and caseplist
  # compute.techapplist(techlist, caselist, listsep=" ", filename="")
source("R/techapplist.write.r") # writes applist either to screen or to a file if listsep and filename are provided
 # function(applist, listsep=" ", filename="") 

source("R/techapplist.frame.R")


## =======================================================
## EXAMPLES
## =======================================================

### Read data input files using build.list to generate a caselist and techlist
caselist_demo<- build.list("demo/casedata_demo2.csv")
techlist_demo<- build.list("demo/techdata_demo2.csv")
# alternatively use:
# caselist_demo<- build.list("casedata_demo.csv")
# techlist_demo<- build.list("techdata_demo.csv")

### Usinge compute.techapp.r to compute app.proiles for a pair of case and tech (caselist$case, techlist$tech)
applist_demo=list()
app.item.tmp <- compute.techappscore(caselist_demo$Katarniya, techlist_demo$uddt,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$Katarniya, techlist_demo$dry.toilet,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$Katarniya, techlist_demo$application.compost,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))                    
# Write the list to the screen
techapplist.write(applist_demo)      
# Write to file
techapplist.write(applist_demo, listsep=";", filename="app_list_demo.csv") #giving a list separation charachter and a filename creates a csv file with the results


### Using compute.techapplistto compute the entire appropriateness profiles for a list of cases and a list of technologies
applist_demo<-compute.techapplist(caselist_demo,techlist_demo, lshowplot = F, lpdfplot = F) 
# use lshowplot=F and lpdfplot to display the match of the case attribute function and the tech attribute functions
#write the list to the screen
techapplist.write(applist_demo)

### Convert to dataframe and write to csv
appframe_demo=techapplist.frame(applist_demo,techlist_demo, caselist_demo)
View(appframe_demo)
write.table(appframe_demo, file = "appframe_demo.csv", sep = ";",row.names=F)


## =======================================================
## EXAMPLES PLOTS
## =======================================================

### Define order of functional groups
appframe_demo$functional.group = factor(appframe_demo$functional.group, levels=c('U','Uadd','S','C','T','D'))
appframe_demo_long = melt(appframe_demo, id=c("case", "tech", "functional.group"))

### Plot a histogramm per tech of all scores
ggplot(appframe_demo_long, aes(x=variable, y=value, fill=functional.group)) +
  geom_boxplot() +
  facet_wrap( ~ functional.group) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# -- plot histogram of tech app scores per functional group
ggplot(appframe_demo, aes(x=techapp.score, fill=functional.group)) + geom_histogram(show.legend=F) + facet_wrap( ~ functional.group)
# -- plot histogram of all tech app scores (coloured per functional group)
ggplot(appframe_demo, aes(x=techapp.score, fill=functional.group, order=functional.group)) + geom_histogram()
# -- boxplot of scores per functional group
ggplot(appframe_demo, aes(y=techapp.score, x=functional.group, fill=functional.group)) +  geom_boxplot()


## =======================================================
## GUDIELINES ON THE PREPARATION OF INPUT DATA
## =======================================================
# Each data files contains a list of items (either techs or cases in the columns)
# Each items has a few information attributes (info.rows), build.list automatically detects the number of info rows, so you do not need to provide this.
# This is followed by a list of appropriateness attributes (attr1,...., attrn).
# Info rows can be used to provide comments about the case or the technology.
# For Technologies, the info.rows are also used to provide the functional groups and the products which are used to generate sanitation systems by a different model module.
# Predefined functional groups:
# User interface (U), Collection and Storage (S), Conveyance (C), (Semi-)centralized Treatement (T), Reuse and/or Disposal (D) (see also http://ecompendium.sswm.info)
# Pre-defined poducts:
# urine, faeces, excreta, blackwater, greywater, stormwater, storedurine, driedfaeces, pit humus, compost, sludge, effluent, stabilizedsludge, secondaryeffluent, biogas
# Appropriateness attributes are defined by three rows:
# 1 Name of the attributes: e.g. bod, water, temp, omskil, etc.
# 2 Name of attribute appropriateness function describing the technology/case requirement/capactiy 
# 3 Parameters required for this function
# Each distinct attriute is described by a pair of functions, one for the case and one for the tech.
# !!! A pair has always to consits of one density function ('d...') and one distribution function ('p...')
# Which of the two functions is used to describe the case or the technology attribute value can vary
# Generally density functions are used to describe probability that the attribute takes a certain value (e.g. temperature)
# ... and distribution functions are used to describe the performance given the attribute (e.g. the performance of a technology given a certain temperature)
# Recommended functions are:
# p or drange(x, lower=-Inf, upper=Inf) 
# p or dtrapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dunif(x, min, max)
# dcat(x, probs)  # probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6)
# Other that might work are: dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.

