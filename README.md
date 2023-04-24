# Churn Analysis using R

## Project Description

In today’s retail industry, understanding customers better is critical for businesses to stay competitive and profitable. Retail data is growing exponentially in variety, volume, velocity & value with each passing year. Smart retailers are well aware that this data can be utilized and eventually holds the prospective for profit. As a result, retailers are becoming more conscious about utilization of data and information kept in their repositories, so they can integrate and analyze these large volumes of data to come up with results that can support the quality of their decision-making, in order to stay at a competitive advantage and to increase profits.

This project analyzes a dataset provided by a food store in Pakistan dated between September 17, 2014, and October 26, 2014 (6 weeks approx.) to understand customer churn behavior. The dataset involves only three columns, and each row represents the purchasing done by customers in each day.

## Data Summary
The dataset (Retail_Customer.csv.zip) provided for this project has a total of 1058198 rows and includes the following columns:
- customer_id: unique identifier for each customer
- date: date of purchase
- sales: total sales amount for the purchase
The reference date is the last date of the 5th week, and a customer will churn if he/she has no visit in a week.

## Tasks
The following tasks were performed in the project:
- Calculated the week with the highest earnings.
- Identified the most valued customer.
- Categorized the customers into three groups (Poor, Mediocre, Rich) based on their total spending during the 6-week period.

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites
You will need to have R installed on your machine. You can download the latest version of [R](https://cran.r-project.org/bin/windows/base/) here.
</br>
You will also need to have R Studio on your machine. You can download the latest version of [R Studio](https://posit.co/download/rstudio-desktop/) here.

### Installing
Clone this repository onto your local machine.
```
git clone https://github.com/MuhammadAhmedSuhail/Churn-Analysis-using-R.git
```
Run `analysis.r` on RStudio to analyze the data and retrieve valuable information.

## Conclusion
This project provides insights into customer churn behavior using retail data. Understanding customer behavior and identifying the most valuable customers can help businesses create targeted marketing strategies to increase profits.
