##rankhospital.R
rankhospital = function(state,outcome,num){
        possible_outcomes = c("heart attack","heart failure","pneumonia")
        check_outcome = outcome %in% possible_outcomes
        if(check_outcome == FALSE){
                stop("invalid outcome")
        }
         oocm_data = read.csv("outcome-of-care-measures.csv",na.strings ="Not Availible",stringsAsFactors = FALSE)
        outcomes = c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        my_data = oocm_data[,c(2,7,outcomes[outcome])]
        colnames(my_data)= c("Hospital","State","Outcome")
        check_state = state %in% my_data$State
        if(check_state == FALSE){
                stop("invalid state")
        }
        state_data = filter(my_data,State == state)
        state_data$Outcome = as.numeric(state_data$Outcome)
        state_data = arrange(state_data,Hospital)
        state_data = arrange(state_data,Outcome)
        state_data = select(state_data,Hospital,State,Outcome)
        state_data = filter(state_data,!is.na(Outcome))
        x = data.frame("Rank"=1:length(state_data$Outcome))
        ranked_data = data.frame(bind_cols(state_data,x))
        if(num == "best"){
                rank = 1 
        }else if (num == "worst"){
                rank = length(ranked_data$Rank)
        }else{
                rank = num   
        }
        return (ranked_data[rank,1])
}

          