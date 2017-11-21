#############################################################
# Date	:	04/DEC/07						#
# Author	:	BRC							#
# Program	:	randomisation.R					#
# Descriptio:	Calculates the balance statistic using the#
#			minimisation algorithm.  This is calculate#
#			d by:  						#
#			sum^M_(j=1) {sum^N_(i=1){w_(ij)z_(ij)}^2} #
#										#
#			Where : 						#
#			x_(ij) is the raw stratification score    #
#			z_(ij)={x_(ij)-mean(x_(.j)}/stdev(x_(.j)) # 
#			w_(ij) is the allocation matrix, i.e      #
#                 for 4 units, 2 arms this is.			#
#										#
#			Allo  W0001 W0002 W0003 W0004		      #
#			 1       1     1     0     0              # 
#			 2	   1     0     1     0              #
#                  3       1     0     0     1              #
# requires	:	the number of units, arms, and the raw    #
#                 stratification variables,see star_dummy.cs#  
# 			change the working directory and buffer   #  
#############################################################
# History									#
#############################################################
# Date	:	Description						#
# 18MAY08   :     Added uneven arm split randomly selected  #
#           :     into first block                          #
#############################################################
#############################################################
#memory.limit(size=4095)
#memory.size(TRUE)
###############################
# Input data                  #
###############################
# Dummy Data
# folder	<-	"D:\\CARTER\\randomisation_Allocation_Training_Area"
# covariates	<-	"star_dummy_1.csv"
# unit		<-	18
# outfile	<-	"block_one"
# figure	<-	"blk1_B"


random.allocation	<-	function(covariates, unit) {

units			<-  	unit
arms  		<-  	2

#######################################################
# This determines the number of units selected if there is an uneven split
select 		<-	ifelse(units/arms == round(units/arms,0),units/arms,units/arms + ifelse(rnorm(1,mean=0,sd=1)>0,0.5,-0.5))
# Note number of arms is fixed to two in this program #
#######################################################
#  Prelims                          
random	<-	combn(letters[1:units], select)
random	<-	t(random)
random	<-	random[1:(nrow(random)/2),]
##########################################
# Generation of the randomisation matrix #

rand		<-	matrix(c(rep(0,nrow(random)*units)),byrow=T,ncol=units)
colnames(rand)		<-	letters[1:units]
   for(j in 1:units) {	
		rand[1:(nrow(random)/2),j]			<-	apply(ifelse(random[1:(nrow(random)/2),] == letters[j],1,0),1,sum)
		rand[round(nrow(random)/2):nrow(random),j]	<-	apply(ifelse(random[round(nrow(random)/2):nrow(random),] == letters[j],1,0),1,sum)
		print(paste("Completed block ", j," of ",units))
   }

#####################################
# Remove no longer required objects #
rm(random) 
#################################################
# Use the STAR covariate info as dummy for this #
#################################################
# 
# Name the practices #
rownames(covariates)	<-	as.character(covariates[,1])
# select first n (units) for practice #
covariates			<-	data.frame(covariates[1:units,])
# remove practive details
covariates			<-	data.frame(covariates[,2:ncol(covariates)])

### Calculate the standardized Z-scores ###
z     <-    (  covariates -  t(matrix(apply(covariates,2,mean),ncol(covariates),nrow(covariates)))  )  / t(matrix(sqrt(apply(covariates,2,var)),ncol(covariates),nrow(covariates)))
#### Calculate the balance statistic ####
#########################################
gc(TRUE)
balance			<-	cbind(rand,apply( ( as.matrix(rand) %*% as.matrix(z) )**2,1,sum))
rm(rand)
gc(TRUE)
colnames(balance)		<-	c(rownames(covariates),"balance")
##  Sort the data ##
balance	<-	balance[order(balance[,"balance"]),]

##############################################
# Plots the density of the balance statistic #
# hist(balance[,"balance"],xlab="Imbalance",main=" ")
# savePlot(filename = paste(area,figure,sep="\\"),type = c("jpeg"),device = dev.cur())

### Max number of obs presented ###
maxn		<-	ifelse(unit > 17,1000,ifelse(unit > 11, 100,ifelse(unit ==11,58,ifelse(unit==10,32,ifelse(unit==9,18,ifelse(unit==8,10,nrow(balance)))))))
#### Sends out the data ####

bal.out	<-	balance[1:maxn,]
return(bal.out)
# write.csv(bal.out,paste(area,paste(outfile,"csv",sep="."),sep="/"),row.names=F)
}
