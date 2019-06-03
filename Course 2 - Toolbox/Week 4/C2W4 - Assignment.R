best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <- read.csv("~/R Works/Course 2/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character") 
        
        ## Check that state and outcome are valid
        st<-unique(outcome_data[,7])
        if (!is.element(state,st)) stop("invalid state")
        if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death rate
        col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        st_data<-outcome_data[outcome_data[,7]==state & outcome_data[,col[outcome]]!="Not Available",c(2,col[outcome])]
        st_data[,2]<-as.numeric(st_data[,2])                   
        best_hos<-st_data[which(st_data[,2]==min(st_data[,2],na.rm = TRUE)),1]
        sort(best_hos)
        best_hos[1]
        
}

rankhospital <- function(state, outcome, num ="best") {
        ## Read outcome data
        outcome_data <- read.csv("~/R Works/Course 2/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
        ## Check that state and outcome are valid
        st<-unique(outcome_data[,7])
        if (!is.element(state,st)) stop("invalid state")
        if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
        
        col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        
        #st_data<-outcome_data[which(complete.cases(outcome_data) & outcome_data[,7]==state),c(2,col[outcome])]   
        # why does the complete.cases function not work here when two or more columns are subset??
        st_data<-outcome_data[outcome_data[,col[outcome]]!="Not Available" & outcome_data[,7]==state,c(2,col[outcome])]
        colnames(st_data) <- c("name","outcome")
        w<-c("best"=1,"worst"=nrow(st_data))
        if (is.numeric(num) ) {
                if (num>nrow(st_data)) {
                      return("NA")  
                }   else {
                          n<-num
                }  
                
        }  else {
                if(is.element(num, c("best","worst"))) {
                        n<-w[num]
                }   else {
                        stop("Invalid Rank")
                }
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        st_data<-transform(st_data,outcome = as.numeric(as.character(outcome)),name = as.character(name))
        ranked<-st_data[order(st_data$outcome,st_data$name),]
        ranked[n,1]
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("~/R Works/Course 2/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")

        ## Check that outcome and rank are valid
        if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
        
        col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        st_data<-outcome_data[outcome_data[,col[outcome]]!="Not Available",c(2,7,col[outcome])]
        colnames(st_data) <- c("name","state","outcome")
        
        ## For each state, find the hospital of the given rank
        st_data<-transform(st_data,outcome = as.numeric(as.character(outcome)),name = as.character(name),state=as.character(state))
        ranked<-st_data[order(st_data$state,st_data$outcome,st_data$name),]
                #ranked$rank<-unlist(with(ranked,tapply(ranked,ranked$state,rank)))

        ## Return a data frame with the hospital names and the (abbreviated) state name
        st_name<-unique(ranked$state)
        st_rank<-data.frame(state=st_name,name=character(length(st_name)),stringsAsFactors = FALSE) 
                # when StringsAsFactors is not set to FALSE, values can't be assisgned to the data frame. The default mode for data frame is factor.
        for (i in 1:length(st_name)) {
                ind_st<-ranked[ranked$state==st_name[i],c(1,2)]
                st_rank[i,1]<-st_name[i]
                
                if (num=="best") {
                        n=1
                }  else if (num=="worst") {
                        n=nrow(ind_st)
                }  else {
                        n=num
                }
                
                if (n>nrow(ind_st)) {
                        st_rank[i,2]<-"NA"
                }   else {
                        st_rank[i,2]<-ind_st[n,1]
                }
        }
        st_rank
}
