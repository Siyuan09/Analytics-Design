# # ===================================================
# RFM analysis on CDNOW data
# Data: CDNOW customer data (this time full data)
# Source: provided by Professor Bruce Hardie on
#   http://www.brucehardie.com/datasets/CDNOW_sample.zip
# ===================================================

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/s/xxfloksp0968mgu/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month
     library(lubridate)
     df.raw$date<-ymd(df.raw$date)
     df.raw$year<-year(df.raw$date)
     df.raw$month<-month(df.raw$date)

# b) aggregate into monthly data with number of trips and total expenditure
     df <- aggregate(x=list("qty"=df.raw$qty,"expd"=df.raw$expd),
                     by=list("id"=df.raw$id,"year"=df.raw$year,"month"=df.raw$month),
                     FUN = sum)
     trips<-aggregate(x=list("trips"=df.raw$id),
                      by=list("id"=df.raw$id,"year"=df.raw$year,"month"=df.raw$month),
                      FUN = length)

# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calcualte RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.

     df<-merge(df,trips,by=c("id","year","month"),all = T)
     df.final<-expand.grid(id=c(1:1000),year=c(1997,1998),month=(1:12))
     df.final<-df.final[!(df.final$year==1998 & df.final$month>6),]
     df.final<- merge(df.final,df,by=c("id","year","month"),all.x = T)
     df.final$trips[is.na(df.final$trips)]<-0
     df.final$qty[is.na( df.final$qty)]<-0
     df.final$expd[is.na( df.final$expd)]<-0

# now we should have the dataset we need; double check to make sure that every consumer is in every period


# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency
     
     # assign 0 to recency in convenience
     df.final$recency<-0
     
     #calculate recency of year 1997
     #since we only need 6 months in 1998, these two years cannot loop together
     for(i in 1:1000){
         for(m in 1:12){
             if(df.final$trips[df.final$id==i&df.final$year==1997&df.final$month==m]==0){
                 df.final$recency[df.final$id==i&df.final$year==1997&df.final$month==m+1]<-
                     df.final$recency[df.final$id==i&df.final$year==1997&df.final$month==m]+1
             }else{
                 df.final$recency[df.final$id==i&df.final$year==1997&df.final$month==m+1]<-1
             }
         }
     }
     
     #calculate recency of January 1998 in order to calculate the rest in 1998
    
     #df.final$trips[df.final$id==1&df.final$year==1997&df.final$month==12]==0
     #df.final$recency[df.final$id==1&df.final$year==1998&df.final$month==1]<-1
     for(i in 1:1000){ 
         if(df.final$trips[df.final$id==i&df.final$year==1997&df.final$month==12]==0){
                 df.final$recency[df.final$id==i&df.final$year==1998&df.final$month==1]<-
                     df.final$recency[df.final$id==i&df.final$year==1997&df.final$month==12]+1
                 }else{
                 df.final$recency[df.final$id==i&df.final$year==1998&df.final$month==1]<-1
         }
     }
        
     
     #calculate rest of the recency in 1998
     for(i in 1:1000){
         for(m in 1:6){ 
            if(df.final$trips[df.final$id==i&df.final$year==1998&df.final$month==m]==0){
                     df.final$recency[df.final$id==i&df.final$year==1998&df.final$month==m+1]<-
                       df.final$recency[df.final$id==i&df.final$year==1998&df.final$month==m]+1
                 }else{
                     df.final$recency[df.final$id==i&df.final$year==1998&df.final$month==m+1]<-1
            }
         }
     }
     
     #adjust the value of unknown recency
     df.final$recency[df.final$year==1997&df.final$month==1]<- NA

# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 8 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency

    df.final$quarters[df.final$month==1:3&df.final$year==1997]<-1 
    df.final$quarters[df.final$month==4:6&df.final$year==1997]<-2
    df.final$quarters[df.final$month==7:9&df.final$year==1997]<-3
    df.final$quarters[df.final$month==10:12&df.final$year==1997]<-4
    df.final$quarters[df.final$month==1:3&df.final$year==1998]<-5
    df.final$quarters[df.final$month==4:6&df.final$year==1998]<-6
  
    df.final$frequency[df.final$quarters==1]<-0
    for (i in 1:1000){
        for (q in 2:6){
            df.final$frequency[df.final$id==i&df.final$quarters==q]<-
                sum(df.final$trips[df.final$id==i&df.final$quarters==q-1])
        }
    }
    
    #adjust the value of unknown frequency
    df.final$frequency[df.final$quarters==1]<-NA
   
# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the beginning to 
#   the PAST MONTH. Call this df$monvalue
    
    #calculate monthly value of year 1997
    #since we only need 6 months in 1998, these two years cannot loop together
    df.final$monvalue[1]<-0
    for (i in 1:1000) {
        a<-0
        for (m in 1:12) {
            if(df.final$trips[df.final$id==i&df.final$year==1997&df.final$month==m]==0){
                    df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==m+1]<-
                        df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==m]
                }else{
                    df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==m+1]<-
                        (df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==m]*a+
                             df.final$expd[df.final$id==i&df.final$year==1997&df.final$month==m])/(a+1)
                    a<-a+1
            }
        }
    }
    
   
    #calculate monthly value of January 1998 in order to calculate the rest in 1998
    for(i in 1:1000){
        b<-length(df.final$trips[df.final$id==i&df.final$year==1997&!df.final$trips==0])
        if(df.final$trips[df.final$id==i&df.final$year==1997&df.final$month==12]==0){
            df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==1]<-
                df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==12]
        }else{
            df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==1]<-
                (df.final$monvalue[df.final$id==i&df.final$year==1997&df.final$month==12]*(b-1)+
                     df.final$expd[df.final$id==i&df.final$year==1997&df.final$month==12])/b
        }
    }
     
    #calculate rest of the monthly value in 1998
    for (i in 1:1000) {
        b<-length(df.final$trips[df.final$id==i&df.final$year==1997&!df.final$trips==0])
        for (m in 1:6) {
            if(df.final$trips[df.final$id==i&df.final$year==1998&df.final$month==m]==0){
                df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==m+1]<-
                    df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==m]
            }else{
                df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==m+1]<-
                    (df.final$monvalue[df.final$id==i&df.final$year==1998&df.final$month==m]*b+
                         df.final$expd[df.final$id==i&df.final$year==1998&df.final$month==m])/(b+1)
               b<-b+1
            }
        }
    }
    
    #adjust the value of unknown monthly value
    df.final$monvalue[df.final$year==1997&df.final$month==1]<-NA
  
# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

    b1 <- -0.05
    b2 <- 3.5
    b3 <- 0.05

    df.final$index <- b1*df.final$recency + b2*df.final$frequency + b3*df.final$monvalue


# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-trip revenue that these consumers generate and comment on 
#   whether the RFM index help you segment which set of customers are "more valuable"

    df.final<-df.final[order(-df.final$index),]
    groups<-quantile(df.final$index,probs = seq(1,0,length = 11),na.rm = T,names = T)
  
    average_expd<-c()
  
    for(i in 1:9){
      df_group<-subset(df.final,index>groups[i+1]&index<=groups[i])
      average_expd<- append(average_expd,sum(df_group$expd)/length(df_group$month[!df_group$month==0]))
    }
    
    df_group10<-subset(df.final,index>=groups[11]&index<=groups[10])
    average_expd<- append(average_expd,sum(df_group10$expd)/length(df_group10$month[!df_group$month==0]))
  
    barplot(average_expd, 
            main="Average expenditure by deciles in the RFM index",
            xlab = "deciles in the RFM index",
            ylab = "average expenditure",las=T)
    
    ###############
    # Comment: I think the RFM index does help us segment 
    # which customers are "more valueble".
    # Since customers who have higher index also spend much more each month in average,
    # so customers with higher RFM index are who we should target at.
