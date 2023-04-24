#install.packages("stringr")
#install.packages("data.table")
library(stringr)
library(data.table)

source('Functions.R')

df <- read.csv("Retail_Customer.csv")
allCustomers <- unique(df$CustomerID)

Days <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

Total_Revenue <- list()

Max_Purchase_In_A_Day <- list()
Min_Purchase_In_A_Day <- list()

Total_Visit_Days <- list()
Standard_Deviation_In_Sales <- list()

W1_Total_Sales <- list()
W1_Visit_Days <- list()

W2_Total_Sales <- list()
W2_Visit_Days <- list()

W3_Total_Sales <- list()
W3_Visit_Days <- list()

W4_Total_Sales <- list()
W4_Visit_Days <- list()

W5_Total_Sales <- list()
W5_Visit_Days <- list()

Monday_Visit_Days <- list()
Tuesday_Visit_Days <- list()
Wednesday_Visit_Days <- list()
Thursday_Visit_Days <- list()
Friday_Visit_Days <- list()
Saturday_Visit_Days <- list()
Sunday_Visit_Days <- list()

Monday_Total_Sales <- list()
Tuesday_Total_Sales <- list()
Wednesday_Total_Sales <- list()
Thursday_Total_Sales <- list()
Friday_Total_Sales <- list()
Saturday_Total_Sales <- list()
Sunday_Total_Sales <- list()

Favourite_Visit_Day_Revenue_Wise <- list()
Favourite_Visit_Day_Visit_Wise <- list()

D4_Visit_Flag <- list()
D0_Visit_Flag <- list()
D2_Visit_Flag <- list()
D5_Visit_Flag <- list()
D3_Visit_Flag <- list()
D1_Visit_Flag <- list()

D4_Sales_Amount <- list()
D0_Sales_Amount <- list()
D2_Sales_Amount <- list()
D5_Sales_Amount <- list()
D3_Sales_Amount <- list()
D1_Sales_Amount <- list()

Days_Since_Last_Visit <- list()
Label <- list()

for ( x in 1:length(allCustomers) )
{
  single_cus <- df[df$CustomerID == allCustomers[x],] #Selects records of only one Customer
  single_cus <- single_cus[order(as.Date(single_cus$Visit_Date, format="%m/%d/%Y")), ]  #Orders according to Date
  
  single_cus_dates <- single_cus$Visit_Date
  ref_date = as.Date(single_cus_dates[1], format="%m/%d/%Y") + 34
  
  Total_Revenue <- append(Total_Revenue,Cal_Total_Revenue(single_cus,ref_date))
  Max_Purchase_In_A_Day<- append(Max_Purchase_In_A_Day,Cal_Max_Purchase_Per_Day(single_cus))
  
  Min_Purchase_In_A_Day<-append(Min_Purchase_In_A_Day,Cal_Min_Purchase_Per_Day(single_cus))
  Total_Visit_Days<- append(Total_Visit_Days,length(single_cus$Visit_Date))
  
  Standard_Deviation_In_Sales <- append(Standard_Deviation_In_Sales,sd(single_cus$Total_Purchases_In_USD))
  W1_Total_Sales <- append(W1_Total_Sales,Cal_Per_Week_Total_Sales(single_cus,ref_date-6,ref_date-1))
  W1_Visit_Days <- append(W1_Visit_Days,Cal_Per_Week_Visits(single_cus,ref_date-6,ref_date-1))
  
  W2_Total_Sales <- append(W2_Total_Sales,Cal_Per_Week_Total_Sales(single_cus,ref_date-13,ref_date-7))
  W2_Visit_Days <- append(W2_Visit_Days,Cal_Per_Week_Visits(single_cus,ref_date-13,ref_date-7))
  
  W3_Total_Sales <- append(W2_Total_Sales,Cal_Per_Week_Total_Sales(single_cus,ref_date-20,ref_date-14))
  W3_Visit_Days <- append(W2_Visit_Days,Cal_Per_Week_Visits(single_cus,ref_date-20,ref_date-14))
  
  W4_Total_Sales <- append(W2_Total_Sales,Cal_Per_Week_Total_Sales(single_cus,ref_date-27,ref_date-21))
  W4_Visit_Days <- append(W2_Visit_Days,Cal_Per_Week_Visits(single_cus,ref_date-27,ref_date-21))
  
  W5_Total_Sales <- append(W2_Total_Sales,Cal_Per_Week_Total_Sales(single_cus,ref_date-34,ref_date-28))
  W5_Visit_Days <- append(W2_Visit_Days,Cal_Per_Week_Visits(single_cus,ref_date-34,ref_date-28))
  
  visit_count_list <- Cal_Per_Day_Visits(single_cus)
  
  Monday_Visit_Days <- append(Monday_Visit_Days,visit_count_list[1])
  Tuesday_Visit_Days <- append(Tuesday_Visit_Days,visit_count_list[2])
  Wednesday_Visit_Days <- append(Wednesday_Visit_Days,visit_count_list[3])
  Thursday_Visit_Days <- append(Thursday_Visit_Days,visit_count_list[4])
  Friday_Visit_Days <- append(Friday_Visit_Days,visit_count_list[5])
  Saturday_Visit_Days <- append(Saturday_Visit_Days,visit_count_list[6])
  Sunday_Visit_Days <- append(Sunday_Visit_Days,visit_count_list[7])
  
  sales_count_list <- Cal_Per_Day_Sales(single_cus)
  
  Monday_Total_Sales <- append(Monday_Total_Sales,sales_count_list[1])
  Tuesday_Total_Sales <- append(Tuesday_Total_Sales,sales_count_list[2])
  Wednesday_Total_Sales <- append(Wednesday_Total_Sales,sales_count_list[3])
  Thursday_Total_Sales <- append(Thursday_Total_Sales,sales_count_list[4])
  Friday_Total_Sales <- append(Friday_Total_Sales,sales_count_list[5])
  Saturday_Total_Sales <- append(Saturday_Total_Sales,sales_count_list[6])
  Sunday_Total_Sales <- append(Sunday_Total_Sales,sales_count_list[7])
  
  Favourite_Visit_Day_Revenue_Wise <- append( Favourite_Visit_Day_Revenue_Wise,weekdays( as.Date(single_cus$Visit_Date[which(single_cus$Total_Purchases_In_USD == max(single_cus$Total_Purchases_In_USD))],format="%m/%d/%Y") ) )
  Favourite_Visit_Day_Visit_Wise <- append(Favourite_Visit_Day_Visit_Wise,Days[which.max(visit_count_list)])
  
  D4_Visit_Flag <- append(D4_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 4))
  D0_Visit_Flag <- append(D0_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y")))
  D2_Visit_Flag <- append(D2_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 2))
  D5_Visit_Flag <- append(D5_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 5))
  D3_Visit_Flag <- append(D3_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 3))
  D1_Visit_Flag <- append(D1_Visit_Flag,Cal_Visit_Flag(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 1))
  
  D4_Sales_Amount <- append(D4_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 4))
  D0_Sales_Amount <- append(D0_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y")))
  D2_Sales_Amount <- append(D2_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 2))
  D5_Sales_Amount <- append(D5_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 5))
  D3_Sales_Amount <- append(D3_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 3))
  D1_Sales_Amount <- append(D1_Sales_Amount,Cal_Sales_Amount(single_cus,as.Date(ref_date, format="%m/%d/%Y") - 1))
  
  Days_Since_Last_Visit <- append(Days_Since_Last_Visit,as.numeric(max(as.Date(df$Visit_Date, format="%m/%d/%Y")) - max(as.Date(single_cus$Visit_Date, format="%m/%d/%Y") )))
  Label <- append(Label,Cal_Label(single_cus,df))
}

#Task1

sum(W1_Total_Sales)
sum(W2_Total_Sales)
sum(W3_Total_Sales)
sum(W4_Total_Sales)
sum(W5_Total_Sales)

#The Week with the highest sales has the highest earning

#Task2

allCustomers[which(Total_Revenue == max(Total_Revenue))]

#Task3


detailed <- data.frame(
  CustomerID = allCustomers,
  TotalRevenue = unlist(Total_Revenue),
  MaxPurchaseIn_A_Day = unlist(Max_Purchase_In_A_Day),
  MinPurchase_In_A_Day = unlist(Min_Purchase_In_A_Day),
  TotalVisit_Days = unlist(Total_Visit_Days),
  STD_In_Sales = unlist(Standard_Deviation_In_Sales),
  W1_Total_Sales = unlist(W1_Total_Sales),
  W1_Visit_Days = unlist(W1_Visit_Days),
  W2_Total_Sales = unlist(W2_Total_Sales),
  W2_Visit_Days = unlist(W2_Visit_Days),
  W3_Total_Sales = unlist(W3_Total_Sales),
  W3_Visit_Days = unlist(W3_Visit_Days),
  W4_Visit_Days = unlist(W4_Visit_Days),
  W4_Total_Sales = unlist(W5_Visit_Days),
  W5_Total_Sales = unlist(W5_Total_Sales),
  W5_Visit_Days = unlist(W5_Visit_Days),
  Friday_Visit_Days = unlist(Friday_Visit_Days),
  Thursday_Visit_Days = unlist(Thursday_Visit_Days),
  Saturday_Visit_Days = unlist(Saturday_Visit_Days),
  Tuesday_Visit_Days = unlist(Tuesday_Visit_Days),
  Wednesday_Visit_Days = unlist(Wednesday_Visit_Days),
  Sunday_Visit_Days = unlist(Sunday_Visit_Days),
  Monday_Visit_Days = unlist(Monday_Visit_Days),
  
  Friday_Total_Sales = unlist(Friday_Total_Sales),
  Thursday_Total_Sales = unlist(Thursday_Total_Sales),
  Saturday_Total_Sales = unlist(Saturday_Total_Sales),
  Tuesday_Total_Sales = unlist(Tuesday_Total_Sales),
  Wednesday_Total_Sales = unlist(Wednesday_Total_Sales),
  Sunday_Total_Sales = unlist(Sunday_Total_Sales),
  Monday_Total_Sales = unlist(Monday_Total_Sales),
  
  Favourite_Visit_Day_Revenue_Wise = unlist(Favourite_Visit_Day_Revenue_Wise),
  Favourite_Visit_Day_Visit_Wise = unlist(Favourite_Visit_Day_Visit_Wise),
  D4_Visit_Flag = unlist(D4_Visit_Flag),
  D4_Sales_Amount = unlist(D4_Sales_Amount),
  D0_Visit_Flag = unlist(D0_Visit_Flag),
  D0_Sales_Amount = unlist(D0_Sales_Amount),
  D2_Visit_Flag = unlist(D2_Visit_Flag),
  D2_Sales_Amount = unlist(D2_Sales_Amount),
  D5_Visit_Flag = unlist(D5_Visit_Flag),
  D5_Sales_Amount = unlist(D5_Sales_Amount),
  D3_Visit_Flag = unlist(D3_Visit_Flag),
  D3_Sales_Amount = unlist(D3_Sales_Amount),
  D1_Visit_Flag = unlist(D1_Visit_Flag),
  D1_Sales_Amount = unlist(D1_Sales_Amount),
  
  Days_Since_Last_Visit = unlist(Days_Since_Last_Visit),
  Label = unlist(Label)
)


write.csv(detailed,"Detailed_Dataset.csv", row.names = FALSE)







