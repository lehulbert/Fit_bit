#throughout the program, Mon=1, Tues=2, Wed=3, Thurs=4, Fri=5, Sat=6, Sun=0


# Uncomment this code to install the package:
# install.packages("fitbitScraper")
# library("fitbitScraper")

# Uncomment this code to get access to your data:
# mypassword<- readLines("pw.txt")
# cookie<- login(email="lehulbert@gmail.com",password=mypassword)


today<-"2016-08-08"   #Set this to whatever and the program will still work
# I can change the start date too as long as the start date is a Monday. 
df<- get_daily_data(cookie,what="steps",start_date="2014-08-18",end_date=today)
total_steps<- sum(df[,2])
total_days<- nrow(df)
total_avg<- floor(total_steps/total_days)


weekday_names<-c("Mon","Tues","Wed","Thu","Fri","Sat","Sun")
total_weeks<-floor(total_days/7)
extra_days<-total_days%%7
day_of_the_week<-replicate(total_weeks,weekday_names)             #repeats the days list as many times as there are weeks
day_of_the_week<-c(day_of_the_week,weekday_names[1:extra_days])   #adds the days of the current week

day_number<-1:total_days
df3<- data.frame(day_number,day_of_the_week,df)
active_days<-nrow(subset(df3,steps>2000))


avg_dotw<-function(day_of_week,x=1){ #finds the average for each day of the week and adds that value to a list.x=1 is the default. IF you want to use the t-test function, use any x!=1
    sum=0          
    day_count=0
    observations<-c()
    for (i in 1:total_days){
        if (df3[i,1]%%7==day_of_week &df3[i,4]>=2000 )  {  #only count the days where I got more than 2000 steps
            sum=sum + df3[i,4]
            day_count=day_count+1
            observations<-c(observations,df3[i,4])
            }
    }
    avg=round(sum/day_count)

    if(x==2){
        return (observations)
    }
    else {return (avg)}
    
}

make_frame<-function(day_of_week){   #returns just the part of the dataframe for the day of the week you want
    df3[which(df3$day_number %%7== day_of_week),]
}

avgs_for_each_day <- function(){   #prints the average number of steps for each day of the week
    avgs<-c(avg_dotw(1),avg_dotw(2),avg_dotw(3),avg_dotw(4),avg_dotw(5),avg_dotw(6),avg_dotw(0))
    avg_frame<-data.frame(days_of_the_week,avgs) 
    print(avg_frame[order(avgs),]) 
}
   
 
#View(df3)

get_inactive_days<-function(){
    for (i in 1:total_days){
        if (df3[i,4]<=2000)  { 
            print(df3[i,])
        }
    }
}

# Use this function to see if there's a statistically significant difference
# in the average number of daily steps between one day of the week and another.
difference_btwn_daysoftheweek<- function(day_of_week1,day_of_week2){    #enter the number corresponding to its day of the week
    t.test(avg_dotw(day_of_week1,2),avg_dotw(day_of_week2,2))     #the 2 is to get the avg_dotw function to return the observations instead of the avg
}


# Use this function to see if there's a statistically significant difference
# in the average number of daily steps from year 1 to year 2 of fitbit usage.
difference_btwn_years<- function(){     #enter the years to compare
    year1<-get_daily_data(cookie,what="steps",start_date="2014-08-18",end_date="2015-08-17")
    year2<-get_daily_data(cookie,what="steps",start_date="2015-08-18",end_date="2016-08-17")
    year3<-get_daily_data(cookie,what="steps",start_date="2016-08-18",end_date="2017-08-17")
    
    year1_cleaned<-subset(year1,steps>2000)
    year2_cleaned<-subset(year2,steps>2000)
    year3_cleaned<-subset(year3,steps>2000)
    
    year1_avg<-sum(year1_cleaned[,2])/nrow(year1_cleaned)
    year2_avg<-sum(year2_cleaned[,2])/nrow(year2_cleaned)
    difference<-year2_avg-year1_avg
     

    x<-t.test(year2_cleaned[,2],year1_cleaned[,2])      #just compares year 1 and year 2 right now

    cat ("Significant difference between average year 2 and year 1 steps?\n")
    if (x$p.value <.05)    {cat ("   t-test shows that yes, there's a statistically significant difference.\n")}
    else {cat("   no, there's isn't a statistically significant difference.") }
    cat("   On average I walk",round(difference),"more steps per day in year 2 than year 1.\n")
    
    x
}

most_and_least <-function(nobs){    #nobs=how many of the highest and lowest days do you want to see
    df3_ordered<-df3[order(df3$steps),]
    df3_ordered<- df3_ordered[which(df3_ordered$steps>1000),]
    cat("days with the fewest steps...\n")
    print(head(df3_ordered,nobs))
    cat ("\n")
    cat("days with the most steps...\n")
    print(tail(df3_ordered,nobs))
}

above_30k<- function(full_list=FALSE){
    x<-df3[which(df3$steps>30000),]
    if(full_list==TRUE){            #set the argument to true to get a full list of all the days I walked more than 30k steps
        print (x[order(x$steps),])
    }
    cat("number of times I've walked more than 30,000 steps=",nrow(x),"\n")
}


above_20k<- function(full_list=FALSE){
    x<-df3[which(df3$steps>20000),]
    if(full_list==TRUE){                #set the argument to true to get a full list of all the days I walked more than 20k steps
        print (x[order(x$steps),])
    }
    cat("number of times I've walked more than 20,000 steps=",nrow(x),"\n")
}


above_15k<- function(full_list=FALSE){
    x<-df3[which(df3$steps>15000),]
    if(full_list==TRUE){            #set the argument to true to get a full list of all the days I walked more than 15k steps
        print (x[order(x$steps),])
    }
    cat("number of times I've walked more than 15,000 steps=",nrow(x),"\n")
    cat("I walk 15,000 steps", round(nrow(x)/active_days*100,1),"% of the time","\n")
}

above_10k<- function(full_list=FALSE){
    x<-df3[which(df3$steps>10000),]
    if(full_list==TRUE){            #set the argument to true to get a full list of all the days I walked more than 15k steps
        print (x[order(x$steps),])
    }
    cat("number of times I've walked more than 10,000 steps=",nrow(x),"\n")
    cat("I walk 10,000 steps", round(nrow(x)/active_days*100,1),"% of the time","\n")
}



plotting<- function(){      #NEEDS WORK!
    ggplot(subset(df3,lots_day_names %in% c("Sun","Mon")),
           aes(x=day_number,y=steps,color=lots_day_names))+geom_point()
}


# Write the tests I ran and my conclusions to the file fitbit_results.txt... 
writeResults<- function(){
    sink("fitbit_results.txt")
    
    cat("My total steps over the past",total_days,"days is:",total_steps,"\n")
    cat("Which means overall I walk an average of", total_avg,"steps per day.", "\n","\n")
    
    cat ("My average # of steps broken down by day of the week:\n")
    avgs_for_each_day()
    cat("\n")
    most_and_least(6)
    cat("\n")
    above_10k()
    cat("\n")
    above_15k()
    cat("\n")
    above_20k()
    cat("\n")
    above_30k()
    cat("\n","\n")

    cat("T-TESTS... \n")
    cat ("Significant difference between average Mondays and Sundays steps?\n")
    ttest1<-difference_btwn_daysoftheweek(1,0)  
    if (ttest1$p.value <.05)    {cat ("   t-test shows that yes, there's a statistically significant difference.")}
    else {cat("   no, there's isn't a statistically significant difference.") }
    cat("\n","\n")
    
    cat ("Significant difference between average Mondays and Saturdays steps?\n")
    ttest1<-difference_btwn_daysoftheweek(1,6)  
    if (ttest1$p.value <.05)    {cat ("   t-test shows that yes, there's a statistically significant difference.")}
    else {cat("   no, there's isn't a statistically significant difference.") }
    cat("\n","\n")

    difference_btwn_years()
    
    sink()
}
