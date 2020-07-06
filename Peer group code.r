
peergroups<-function(timeindex, analystindex, firmindex, interval=0.99, repetitions=1000) {
	

	######################################################################
	#The following rows check that the inputs for the function are correct
	######################################################################
	
	#Check that the parameter vectors are of equal length
	if(length(timeindex) != length(analystindex)) {
		stop("Parameter vectors have different length")
	}
	if(length(timeindex) != length(firmindex)) {
		stop("Parameter vectors have different length")
	}
	if(length(firmindex) != length(analystindex)) {
		stop("Parameter vectors have different length")
	}
	
	#Check that the optional confidence interval is between 0 and 1
	if(interval < 0) {
		stop("interval must be between 0 and 1")
	}
	if(interval > 1) {
		stop("interval must be between 0 and 1")
	}
	
	#Check that there are no NAs in parameter vectors
	if(sum(is.na(timeindex)> 0 )) {
		stop("timeindex contains NA values")
	}
	
	if(sum(is.na(analystindex)> 0 )) {
		stop("analystindex contains NA values")
	}
	
	if(sum(is.na(firmindex)> 0 )) {
		stop("firmindex contains NA values")
	}
	
	#######################################################################################################################################################
	#This part of the code consists of a nested function that is used to calculate the peer criterion of a specific firm (it will be used later in the code)
	#######################################################################################################################################################
	
	
	analystsimulation<-function(firmsfollowedbyanalyst_nested_input, totalfirms_nested_input, interval_nested_input, repetitions_nested_input) {
		#firmsfollowedbyanalyst_nested_input is a vector consisting of the number of firms followed by each of the analysts following firm i
		#totalfirms_nested_input is the total number of firms with analysts during a given time period
		#interval_nested_input is the confidence interval for the simulation
		#repetitions_nested_input is the number of simulation rounds
		
		possiblechoices = totalfirms_nested_input - 1 #The number of possible other firms that the analysts can choose. The number is set to total number of firms - 1 to exclude firm i itself
		criterion<-rep(0, repetitions_nested_input) #Vector containing the values of peer criterion in each simulation round (consists of zeros at this point)
		if(max(firmsfollowedbyanalyst_nested_input) > 1) { #Checks that there is at least one analyst that follows another firm
			for(round in 1:repetitions_nested_input) {
				otherchoices = firmsfollowedbyanalyst_nested_input[firmsfollowedbyanalyst_nested_input>1] #Vector of choices that the analysts make, analysts following only firm i are excluded
				simulatedchoices = sapply(otherchoices, function(x) sample(1:possiblechoices, x - 1, replace = FALSE)) #Analysts following firm i choose the other firms they follow at random (x-1 because they follow x-1 other firms in addition to firm i)
				simulatedchoices = unlist(simulatedchoices) #Gathers all choices in one vector
				simulatedcoveredfirms = unique(simulatedchoices) #Generates a list of unique randomly selected firms among the analysts' choices
				criterion[round] = max(sapply(simulatedcoveredfirms, function(x) length(which(simulatedchoices ==x)))) #Criterion[round] is the highest number of common analysts firm i has with another random firm
			}
		}
		
		#Determining the final value of the peer criterion by going through the simulated values and identifying the non-random value at the given confidence level
		
		threshold = repetitions_nested_input * (1 - interval_nested_input) #Identify the threshold number of simulation rounds for the peer criterion
		highestcriterion = max(criterion) #Identifies the highest value for the simulated peer criterion among different simulation rounds
		l = highestcriterion #The highest value is given as the starting value for a while loop
		while(l > 0) { #This loop starts with the highest number of common analysts with another random firm in the simulation and reduces it until the confidence level is reached
			if (length(which(criterion==l))>=threshold ) {
				l = l + 1 #Add 1 to get the value of the criterion that is above the threshold
				break
			}
			l = l - 1
		}
		return(l) #Return the value of the peer criterion
	}
	
	
	
	##################################
	#Preparing data for the simulation
	##################################
	
	
	
	
	#Print out text "Initializing..."
	Sys.sleep(0.1)
	progresstext = "Initializing..."
	print(progresstext)
	print(Sys.time())
	flush.console()
	
	#Create data frames of the input data 
	alldata<-data.frame(timeindex, firmindex, analystindex, stringsAsFactors=F)
	alldata<-unique(alldata) #Removes possible duplicate observations

	
	#Count the number of time periods in the data
	years<-unique(alldata[,1]) 
	numberofyears<-length(years) #This variable gives the number of time periods in the data
	
	
	#Count the number of firm-year observations in the data	
	firmyearobs<-data.frame(timeindex, firmindex, stringsAsFactors=F)
	firmyearobs<-unique(firmyearobs)
	firmyearobs<-firmyearobs[order(firmyearobs$timeindex),]
	totalrows = nrow(firmyearobs) #totalrows = number of firm-year observations in the data
	
	#Count the number of firms in the data during each separate time period
	firmsperyear<-rep(NA, length(years))
	for(y in 1:numberofyears) {
		year = years[y] #timeindex
		subdata = subset(firmyearobs, firmyearobs[,1]==year) 
		firmsperyear[y]=nrow(subdata)
	}
	

	oldprogressnumber = 0 #For simulation progress reporting
	counter = 0 #For simulation progress reporting
	
	#Count the (time period-specific) number of firms followed by each analyst in the data
	analystyearobs<-data.frame(timeindex, analystindex, stringsAsFactors=F)
	analystyearobs<-unique(analystyearobs)
	analystyearobs<-analystyearobs[order(analystyearobs$timeindex),]
	firmsfollowedbyanalyst = rep(NA, length(analystyearobs))
	for(i in 1:nrow(analystyearobs)) {
		yearidentifier = analystyearobs[i, 1]
		analystidentifier = analystyearobs[i, 2]
		if(i == 1) {
			yearspecificanalystdata = subset(alldata, alldata[,1] == yearidentifier)
			oldyear=yearidentifier
		}
		if(yearidentifier != oldyear) {
			yearspecificanalystdata = subset(alldata, alldata[,1] == yearidentifier)
			oldyear=yearidentifier
		}
		analystspecificdata = subset(yearspecificanalystdata, yearspecificanalystdata[,3] == analystidentifier)
		firmsfollowedbyanalyst[i] = nrow(analystspecificdata) #Vector containing the number of firms followed by each analyst
	}
	analystyearobs=cbind(analystyearobs, firmsfollowedbyanalyst) #A matrix combining analyst-year observations with the corresponding number of firms followed by each analyst
	totaloutput<-vector('list') #This list will contain the final output of the function
	outputrow = 1 #Needed for printing the output
	
	
	#################################################################################
	#Run the simulation and identify peers for each firm-year observation in the data
	#################################################################################
	
	#Print out text "Starting simulation..."
	Sys.sleep(0.1)
	progresstext = "Starting simulation..."
	print(progresstext)
	print(Sys.time())
	flush.console()

	
	for(i in 1:totalrows) { #The loop goes through all the firm-year observations one at a time
		yearidentifier = firmyearobs[i, 1] #Time index of the observation
		firmidentifier = firmyearobs[i, 2] #Firm index of the observation
		totalfirms = firmsperyear[match(yearidentifier, years)] #Total number of firms during the time period
		if (i == 1) {
			yearspecificfirmanalystdata = subset(alldata, alldata[,1] == yearidentifier) #Contains all the firm-analyst observations of the time period
			oldyear=yearidentifier
		}
		if (yearidentifier != oldyear) {
			yearspecificfirmanalystdata = subset(alldata, alldata[,1] == yearidentifier) #Contains all the firm-analyst observations of the time period
			oldyear=yearidentifier
		}
		firmspecificanalystdata = subset(yearspecificfirmanalystdata, yearspecificfirmanalystdata[,2] == firmidentifier) #Contains the analyst-firm data for all analysts following the firm
		yearspecificanalystdata = subset(analystyearobs, analystyearobs[,1] == yearidentifier) #Contains the analyst data of the time period
		analystrows = sapply(firmspecificanalystdata[,3], function(x) match(x, yearspecificanalystdata[,2])) #Identify which row in "yearspecificanalystdata" corresponds to each analyst following firm i
		firmsfollowedbyanalyst = sapply(analystrows, function(x) yearspecificanalystdata[x,3]) #A vector containing the number of firms followed by each analyst following firm i
		
		#Calculate the peer criterion
		peercriterion = analystsimulation(firmsfollowedbyanalyst_nested_input=firmsfollowedbyanalyst, totalfirms_nested_input=totalfirms, interval_nested_input=interval, repetitions_nested_input=repetitions) #Uses the nested function to calculate the value of the peer criterion
		
		
		#Identify which firms have at least the peer criterion number of common analysts with firm i in the data
		if(peercriterion >= 2) { #Firms with peer criterion < 2 cannot have a peer group
			
			#The following for loop creates a data frame that contains data on all other firms followed by each of the analysts following firm i
			for(j in 1: nrow(firmspecificanalystdata)) {
				analystid = firmspecificanalystdata[j,3]
				if( j == 1) {
					otherfirmsfollowedbyanalyst_nested_input = subset(yearspecificfirmanalystdata, yearspecificfirmanalystdata[, 3] == analystid & yearspecificfirmanalystdata[, 2] != firmidentifier)
				} else {
					otherfirmsfollowedbyanalyst_nested_input = rbind(otherfirmsfollowedbyanalyst_nested_input, subset(yearspecificfirmanalystdata, yearspecificfirmanalystdata[, 3] == analystid & yearspecificfirmanalystdata[, 2] != firmidentifier))
				}
			}
			otherfirmids=unique(otherfirmsfollowedbyanalyst_nested_input[,2]) #A vector of different firms followed by the analysts
			commonanalystswithotherfirms = sapply(otherfirmids, function(x) length(which(otherfirmsfollowedbyanalyst_nested_input[,2]==x))) #A vector containing the number of common analysts with each of the unique firms
			potentialpeers=data.frame(otherfirmids, commonanalystswithotherfirms, stringsAsFactors=FALSE) #Created a data frame combining the two vectors
			peergroup=subset(potentialpeers, potentialpeers[,2]>=peercriterion) #Identify analyst-based peers: the other firms with at least the peer criterion number of common analysts with firm i 
			
			#If the firm has peers, the peer group is stored in the list "totaloutput"
			if (nrow(peergroup) > 0) { 
				firmspecificoutput=data.frame(yearidentifier, firmidentifier, peergroup, peercriterion, stringsAsFactors=FALSE)
				totaloutput[[outputrow]]=firmspecificoutput
				outputrow=outputrow + 1
			}
		}
		
		#This part of the code periodically prints progress information on the screen
		counter = counter + 1
		newprogressnumber = round(counter / totalrows, digits = 2) * 100
		if (newprogressnumber > oldprogressnumber) {
			oldprogressnumber = newprogressnumber
			progresstext = paste(c(newprogressnumber, "% Complete"), collapse = " ")
			Sys.sleep(0.1)
			print(progresstext)
			print(Sys.time())
			flush.console()
		}
	}
	
	#########################################
	#Prepare the final output of the function
	#########################################
	
	progresstext = "Preparing Output"
	Sys.sleep(0.1)
	print(progresstext)
	print(Sys.time())
	flush.console()
	
	
	#Create a single data frame out of all the rows in "totaloutput" and name the columns:
	combinedframe = do.call('rbind', totaloutput) 
	outputframe = data.frame(combinedframe$yearidentifier, combinedframe$firmidentifier, combinedframe$otherfirmids, combinedframe$commonanalystswithotherfirms, combinedframe$peercriterion, stringsAsFactors=FALSE)
	names(outputframe)[1]<-"year"
	names(outputframe)[2]<-"firm"
	names(outputframe)[3]<-"peer_firm"
	names(outputframe)[4]<-"common_analysts"
	names(outputframe)[5]<-"peer_criterion"
	
	return(outputframe)
}

		
		