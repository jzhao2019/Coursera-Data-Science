pollutantmean<-function(directory, pollutant, id=1:332) {
        total<-0
        cnt<-0
        for (i in 1:length(id)){
                data<-read.csv(file.path(directory,paste(formatC(id[i],width = 3,flag = "0"),"csv",sep = ".")))
                total<-total+sum(data[!is.na(data[,pollutant]),pollutant])
                cnt<-cnt+sum(!is.na(data[,pollutant]))
        }
        total/cnt
}

complete <- function(directory, nfile=1:332) {
        com_cases<-data.frame("id"=integer(0),"nobs"=integer(0))
        
        for(i in 1:length(nfile)) {
                data<-read.csv(file.path(directory,paste(formatC(nfile[i],width = 3,flag = "0"),"csv",sep = ".")))
                n<-sum(complete.cases(data))
                if (n>0) {
                        com_cases[i,"id"]<-nfile[i]
                        com_cases[i,"nobs"]<- sum(complete.cases(data))        
                }
                
        }
        com_cases
        
}

corr<-function(directory, threshold = 0) {
        j<-1
        correlation<-vector(mode = "numeric", length = 0)
        for(i in 1:332) {
                data<-read.csv(file.path(directory,paste(formatC(i,width = 3,flag = "0"),"csv",sep = ".")))
                n<-sum(complete.cases(data))
                if (n>threshold) {
                        correlation[j]<-cor(data$sulfate,data$nitrate, use = "complete.obs")
                        j<-j+1
                }
                
        }
        correlation
}


