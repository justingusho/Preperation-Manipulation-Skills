---
title: "Preperation/Manipulation Skills"
author: "Justin Gusho"
output: html_notebook
---





library(data.table)
library(stringr)
```

```{r}
dt_customers <- fread(file = "Datasets/customers.csv",  sep=";")
dt_payments <- fread(file = "Datasets/payments.csv",  sep=";")

str(dt_customers)
summary(dt_customers)
str(dt_payments)
summary(dt_payments)

dt_customers[, `:=` (sex = as.factor(sex), region = as.factor(region))]
setnames(dt_customers, "iiincome", "income")

dt_payments[, "sales" := str_replace(sales, ",", ".")]    
dt_payments[, "sales" := str_replace(sales, "\\$", "")]
dt_payments[, "sales" := as.numeric(sales)] 

dt_payments[, `:=` (last=str_split(name, "_", simplify = TRUE)[,1],     
                    first=str_split(name, "_", simplify = TRUE)[,2])]
dt_payments[, "name" := NULL]

dt_payments[, "date" := as.Date(date, format = "%Y/%m/%d")]

na.omit(dt_customers)
dt_customers[is.na(income), "income" := dt_customers[, median(income, na.rm = TRUE)]]   


dt_customers[,.N]-uniqueN(dt_customers)

dt_payments[,.N]-uniqueN(dt_payments)
dt_payments <- unique(dt_payments)

uniqueN(dt_customers, by = c("last", "first"))
uniqueN(dt_payments, by = c("last", "first"))

dt_payments_aggregated <- dt_payments[,.(total_sales=sum(sales),
                                         date=max(date)),
                                         by=c("last", "first")]

dt_payments_aggregated[, "indicator_vintage" := 0]
dt_payments_aggregated[total_sales >= 2500 & date < "2018-12-24",
                       "indicator_vintage" := 1]

dt_customers[,"regional_customers" := .N, by="region"]

dt_customers[, quantile(income)]

dt_customers[income <= (quantile(income)["25%"]), 
             "income_group" := "low"]
dt_customers[(quantile(income)["25%"]) < income & income < (quantile(income))["75%"], 
             "income_group" := "medium"]
dt_customers[income >= (quantile(income))["75%"], 
             "income_group" := "high"]
dt_customers[, income_group := as.factor(income_group)]


dt_customers[, "avg_regional_group_income" := mean(income), 
             by = c("region","income_group")]

dt_customers[, "income_standardized" := (income - mean(income)) /sd(income), by="region"]
dt_customers[, .(average_income=mean(income)), by = c("region","income_group")][order(income_group, region)]

dt_complete_aggregated <- dt_payments_aggregated[dt_customers, on=.(last=last, first=first), ] 
head(dt_complete_aggregated)

dt_complete <- dt_payments[dt_customers, on=.(last=last, first=first), nomatch=NULL] 
dt_complete[, "id" := .GRP, by=c("last","first")]
dt_complete[, "year" := as.factor(year(date))]
head(dt_complete[, .(id, year, sex, sales, income)][order(id,year)])

dt_reshape <- dcast(dt_complete, 
                    id ~ year, 
                    value.var = c("sales","income"))

dt_melt <- melt(dt_reshape, 
                id.vars = "id", 
                measure.vars = patterns("sales_","income_"),
                variable.name = "year", 
                value.name = c("sales","income"))


dt_complete[sex=="male",]


dt_complete[income_group == "high" & region == "north",]

dt_complete[, total_regional_income := regional_customers*avg_regional_group_income]

dt_complete[, sales_standardized := (sales-mean(sales)) / sd(sales)]

dt_complete[, quarter := quarter(date)]

dt_complete[, year_t1 := year(date)-1] [, year_t2 := year(date)-2 ]


sales_aggr_per_quarter <- dt_complete[, .(total_sales = sum(sales)), by = .(quarter)]
sales_aggr_per_quarter

avg_sales_per_Q_per_year <- dt_complete[, . (average_sales = mean(sales)), by=c("quarter", "year")] [order(quarter, year)]
avg_sales_per_Q_per_year

avg_sales_for_females <- dt_complete[sex== "female", .(average_sales_female = mean(sales)), by=c("quarter", "year")] [order(quarter, year)]
avg_sales_for_females

mean_income_females <- mean(dt_complete[sex == "female", income])
avg_sales_for_females2 <- dt_complete[sex == "female" & income > mean_income_females, .(av_high_income_females = mean(sales)),
                                      by=c("quarter", "year")] [order(quarter, year)]

avg_sales_for_females2

dt_complete[, .N, by = c("income_group", "region")][N < 300, ]
```
