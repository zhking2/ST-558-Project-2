library(readxl)
library(tidyverse)

raw_data <- read_excel("data/ussuperstoredata.xls",skip = 1, col_names = 
                         c("RowID","OrderID","OrderDate","ShipDate","ShipMode","CustomerID","CustomerName",
                           "Segment","Country","City","State","PostalCode","Region","ProductID",
                           "Category","Subcategory","ProductName","Sales","Quantity","Discount","Profit"))
clean_data <- raw_data |>
  select(-"RowID") |>
  mutate(across(c(ShipMode, Segment, Country, City, State, PostalCode, Region, Category,Subcategory), as.factor))

saveRDS(clean_data,file="data/store_df.rds")
