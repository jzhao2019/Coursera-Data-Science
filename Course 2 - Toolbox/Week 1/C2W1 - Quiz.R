QuizAns<-function() {
        hw1_data<-read.csv(file.path("~","R Works","Course 2","hw1_data.csv")) ##Read file to workspace

        head(hw1_data,2) ## 11-12. Read the first 2 rows

        

        nrow(hw1_data)   ## 13.number of observations in the data frame

        

        tail(hw1_data,2) ## 14.last 2 rows

        

        hw1_data[47, "Ozone"]  ## 15. Ozone value in 47th row

        

        sum(is.na(hw1_data$Ozone)) ##16. count of missing values in Ozone

        

        mean(na.omit(hw1_data$Ozone)) ## 17. mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.

        
        mean(hw1_data[which(hw1_data$Ozone>31 & hw1_data$Temp>90),"Solar.R"]) ## 18.Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?


        mean(hw1_data[which(hw1_data$Month == 6), "Temp"],na.rm = TRUE) ## 19. What is the mean of "Temp" when "Month" is equal to 6?

        

        max(hw1_data[hw1_data$Month == 5 , "Ozone"], na.rm=TRUE) ##20. What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
        
        
}

