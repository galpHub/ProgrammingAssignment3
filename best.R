best <- function(state, outcome) {
	state<-as.character(state)
	outcome<-as.character(outcome)
	data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
	colNames <- names(data)
	

	statesAsListed <- data[["State"]]
	funcs <- functionCreator(statesAsListed,colNames)
	### 

	
	#### Check that state and outcome are valid	
	funcs$checkValidInput(state,outcome)

	#### Return hospital name in that state with lowest 30-day death
	#### rate
	data <- funcs$restrictRelevant(data,state,outcome)
	bestHospitals <- funcs$minMortList(data)

	## Select the 'minimum' hospital name given by alphabetic (i.e. lexicograhical)
	## ordering
	
	## The only part missing is the alphabetical sorting.
	#bestHospitals <- bestHospitals[order(bestHospitals$Hospital.Name),]
	print("The number of the best hospitals is " )
	print(length(as.list(bestHospitals)))
	print(head(bestHospitals))
	min(as.character(bestHospitals))
}

functionCreator <- function(statesAsListed,colNames){
	outcome_names <- list("heart attack","heart failure","pneumonia")
	## The outcomeIndices variable is a list of the indices of the
	## columns of the table containing the percentage mortality rates.
	outcomeIndices <- list( 11,17,23)
	names(outcomeIndices) <- outcome_names

	checkValidInput <- function(state,outcome){
		if ( !(state %in% statesAsListed)){
			stop( c("invalid state")  )
			return
		}else if( !(as.list(outcome) %in% outcome_names) ){
			stop( c("invalid outcome")  )
			return
		}
	}
	restrictRelevant <- function(data,state,outcome){
		relevantState <- statesAsListed %in% state
		columnIndex <- outcomeIndices[[outcome]]
		data <- data[c("Hospital.Name",colNames[columnIndex],"State" )][relevantState,]
		##data <- data[relevantState,]
	}
	minMortList <- function(hosp.List){
		hosp.List[,2] <- suppressWarnings(as.numeric(hosp.List[,2]))
		minMortality <- min(hosp.List[,2],na.rm=TRUE)
		isBestHospital <- hosp.List[,2]==minMortality
		isBestHospital[is.na(isBestHospital)] <- FALSE
		hosp.shortList <- hosp.List[isBestHospital,]
	}
	list( checkValidInput = checkValidInput,restrictRelevant=restrictRelevant,
		minMortList=minMortList)
}






	## Alphabetically sorts a vector of characters
minHospital <- function( hospitalList = character() ){
	lengthName <- max(sapply(hospitalList,nchar))
	hospitalNumbers <- sapply(hospitalList, )
}
hospitalNumber <- function( hospital = char()){
	nameLength <- nchar(hospital)
	if( nameLength<1){
		return(0)
	}
	hospital <- paste(hospital,collapse =" ")
	letterList <- as.list(0:26)
	names(letterList)<- c(" ",LETTERS)
	letter2num <- function(x) letterList[[substring(hospital,x,x)]]
	numberCode <- vapply(1:nameLength, letter2num)
}
