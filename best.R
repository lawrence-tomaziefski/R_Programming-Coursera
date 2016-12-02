##Best.R
best = function(state,outcome){
        oocm_data = read.csv("outcome-of-care-measures.csv",na.strings ="Not Availible",stringsAsFactors = FALSE)
        my_data = oocm_data[,c(2,7,11,17,23)]
        colnames(my_data) = c("Hospital_Name","State","Heart_Attack","Heart_Failure","Pneumonia")
        possible_outcomes = c("heart attack","heart failure","pneumonia")
        check_outcome = outcome %in% possible_outcomes
        if(check_outcome == FALSE){
                stop("invalid outcome")
        }
        check_state = state %in% my_data$State
        if(check_state == FALSE){
                stop("invalid state")
        }
        state_data = filter(my_data,State == state)
        state_data$Heart_Attack = as.numeric(state_data$Heart_Attack)
        state_data$Heart_Failure = as.numeric(state_data$Heart_Failure)
        state_data$Pneumonia = as.numeric(state_data$Pneumonia)
        state_data = arrange(state_data,Hospital_Name)
        if (outcome == "heart attack"){
                state_data = select(state_data,Hospital_Name,State,Heart_Attack)
                state_data = arrange(state_data,Heart_Attack)
        }else if (outcome == "heart failure"){
                state_data = select(state_data,Hospital_Name,State,Heart_Failure)
                state_data = arrange(state_data,Heart_Failure)
        }else{
                state_data = select(state_data,Hospital_Name,State,Pneumonia)
                state_data = arrange(state_data,Pneumonia)
        }
        return (state_data[1,1])
}
