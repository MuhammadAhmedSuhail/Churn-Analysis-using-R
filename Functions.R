#install.packages("stringr")
library(stringr)

Cal_Total_Revenue<- function(single_cus,ref_date)
{
  converted_ref_date = as.Date(ref_date,format = ("%m/%d/%Y"),)
  
  rows_before_ref_date = single_cus[as.Date(single_cus$Visit_Date,format = ("%m/%d/%Y"),) < converted_ref_date,]  #Removes ref_date row and below
  
  
  return(sum(rows_before_ref_date$Total_Purchases_In_USD))
  
}

Cal_Max_Purchase_Per_Day <- function(single_cus)
{
  all_purch <- single_cus$Total_Purchases_In_USD
  
  max_purch <- 0
  
  for ( x in 1:length(all_purch))
  {
    if(all_purch[x] > max_purch)
    {
      max_purch = all_purch[x]
    }
  }
  return(max_purch)  
}

Cal_Min_Purchase_Per_Day <- function(single_cus)
{
  all_purch <- single_cus$Total_Purchases_In_USD
  
  min_purch <- all_purch[1]
  
  for ( x in 1:length(all_purch))
  {
    if(all_purch[x] < min_purch)
    {
      min_purch = all_purch[x]
    }
  }
  return(min_purch)  
}

Cal_Per_Week_Total_Sales <- function(single_cus,date_start,date_end)
{
  temp <- single_cus[as.Date(single_cus$Visit_Date, format="%m/%d/%Y")<=date_end & as.Date(single_cus$Visit_Date, format="%m/%d/%Y") >= date_start,]
  
  return(sum(temp$Total_Purchases_In_USD))
  
}

Cal_Per_Week_Visits <- function(single_cus,date_start,date_end)
{
  temp <- single_cus[as.Date(single_cus$Visit_Date, format="%m/%d/%Y")<=date_end & as.Date(single_cus$Visit_Date, format="%m/%d/%Y") >= date_start,]
  
  return(length(temp$Visit_Date))
  
}

Cal_Per_Day_Visits <- function(single_cus)
{
  templis <- weekdays(as.Date(single_cus$Visit_Date, format="%m/%d/%y"))
  
  countlist <- list()
  mon_count <- 0
  tues_count <- 0
  wed_count <- 0
  thur_count <- 0
  fri_count <- 0
  sat_count <- 0
  sun_count <- 0
  
  for ( x in 1:length(templis))
  {
    if ( templis[x] == "Monday" )
    {
        mon_count <- mon_count + 1
    }
    else if ( templis[x] == "Tuesday" )
    {
        tues_count <- tues_count + 1
    }
    else if ( templis[x] == "Wednesday" )
    {
      wed_count <- wed_count + 1
    }
    else if ( templis[x] == "Thursday" )
    {
      thur_count <- thur_count + 1
    }
    else if ( templis[x] == "Friday" )
    {
      fri_count <- fri_count + 1
    }
    else if ( templis[x] == "Saturday" )
    {
      sat_count <- sat_count + 1
    }
    else if ( templis[x] == "Sunday" )
    {
      sun_count <- sun_count + 1
    }
  }
  
  countlist <- append(countlist,mon_count)
  countlist <- append(countlist,tues_count)
  countlist <- append(countlist,wed_count)
  countlist <- append(countlist,thur_count)
  countlist <- append(countlist,fri_count)
  countlist <- append(countlist,sat_count)
  countlist <- append(countlist,sun_count)
  
  return (countlist)
}

Cal_Per_Day_Sales <- function(single_cus)
{
  
  mon_sales <- 0
  tues_sales <- 0
  wed_sales <- 0
  thur_sales <- 0
  fri_sales <- 0
  sat_sales <- 0
  sun_sales <- 0
  
  all_sales<-list()
  
  tempp <- weekdays(as.Date(single_cus$Visit_Date, format="%m/%d/%Y"))
  
  lis <- which(tempp == "Monday")
  
  for ( x in  lis )
  {
    mon_sales <- mon_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Tuesday")
  
  for ( x in  lis )
  {
    tues_sales <- tues_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Wednesday")
  
  for ( x in  lis )
  {
    wed_sales <- wed_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Thursday")
  
  for ( x in  lis )
  {
    thur_sales <- thur_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Friday")
  
  for ( x in  lis )
  {
    fri_sales <- fri_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Saturday")
  
  for ( x in  lis )
  {
    sat_sales <- sat_sales + single_cus$Total_Purchases_In_USD[x]
  }
  
  lis <- which(tempp == "Sunday")
  
  for ( x in  lis )
  {
    sun_sales <- sun_sales + single_cus$Total_Purchases_In_USD[x]
  }

  all_sales <- append(all_sales,mon_sales)
  all_sales <- append(all_sales,tues_sales)
  all_sales <- append(all_sales,wed_sales)
  all_sales <- append(all_sales,thur_sales)
  all_sales <- append(all_sales,fri_sales)
  all_sales <- append(all_sales,sat_sales)
  all_sales <- append(all_sales,sun_sales)
  
  return(all_sales)
}

Cal_Visit_Flag <- function(single_cus,before_date)
{
  if( length(which(as.Date(single_cus$Visit_Date, format="%m/%d/%Y") == before_date) == 0) == 0 )
  {
    return (0)
  }
  else
  {
    return (1)
  }
}

Cal_Sales_Amount <- function(single_cus,before_date)
{
  return(single_cus$Total_Purchases_In_USD[which(as.Date(single_cus$Visit_Date, format="%m/%d/%Y") == before_date )] )
}

Cal_Label <- function(single_cus,df)
{
  if ( max(as.Date(single_cus$Visit_Date, format="%m/%d/%Y")) < max(as.Date(df$Visit_Date, format="%m/%d/%Y")) - 7  )
  {
    return(1)
  }
  return(0)
}




