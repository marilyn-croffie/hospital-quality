best <- function(state, outcome) {
        ## Read outcome data
        dataset <- read.csv("outcome-of-care-measures.csv")
        hospital_data <- dataset[,c(2,7,11,17,23)]
        
        ## Check that state and outcome are valid
        valid_states <- unique(hospital_data$State)
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(sum(state%in%valid_states)!=1){stop("invalid state")}
        if(sum(outcome%in%valid_outcomes)!=1){stop("invalid outcome")}
        
        ## Filter data for that state
        shortlist <- subset(hospital_data, State == state)
        
        ## Create a pointer to the outcome data of interest for that state
        if(outcome == "heart attack"){Outcome  <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack}
        else if(outcome == "heart failure"){Outcome <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure}
        else if(outcome == "pneumonia"){Outcome <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia}
        
        ## Find outcome's lowest 30-day death rate
        # typecast data to numeric to change 'Not Available' values to NA for easy processing
        Outcome_numeric <- as.numeric(Outcome)
        
        # get a cleaner shortlist dataset for that state and outcome by removing rows of missing values
        filter <- complete.cases(Outcome_numeric)
        shortlist <- shortlist[filter,]
        
        # reset pointer to the outcome of interest for the newly cleaned dataset
        if(outcome == "heart attack"){Outcome  <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack}
        else if(outcome == "heart failure"){Outcome <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure}
        else if(outcome == "pneumonia"){Outcome <- shortlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia}
        
        # sort shortlisted dataset according to outcome's 30-day death rate in ascending order
        shortlist <- sort_by(shortlist, list(format(as.numeric(Outcome), nsmall = 1), shortlist$Hospital.Name))
        
        ## Return the topmost hospital name (alphabetical order) in that state with lowest 30-day death rate
        shortlist$Hospital.Name[1]
}