library(dplyr)
library(stringr)

# Created a csv file with all VVV transactions starting with postalcode 62
transactions <-  read_excel("62_transactions_VVV_A&P.xlsx")
head(transactions)

# Use descriptive analytics (BI) to find the most popular shops/postalcodes
# when you open the map on the app

transactions %>%
  filter(TransactionType == "P") %>%
  count(PostalCode_clean) %>% 
  mutate(rank = min_rank(-n)) %>%
  arrange(desc(n)) %>% 
  rename(number = PostalCode_clean, times = n)

# How much time is needed to run this query?
system.time(transactions %>%
              filter(TransactionType == "P") %>%
              count(PostalCode_clean) %>% 
              mutate(rank = min_rank(-n)) %>%
              arrange(desc(n)) %>% 
              rename(number = PostalCode_clean, times = n))

# Use descriptive analytics (BI) redefine the most popular shops/postalcodes
# when you zoom in on your map on the app

rank621 <- transactions %>%
  filter(TransactionType == "P") %>%
  # Changed filter to postal codes starting with 621 instead of 62
  # This reflects zooming in on the map
  filter(str_detect(PostalCode_clean, "^621")) %>%
  count(PostalCode_clean) %>% 
  mutate(rank = min_rank(-n)) %>%
  arrange(desc(n)) %>% 
  rename(number = PostalCode_clean, times = n)

print(rank621)

# How much time is needed to run this query?  
system.time(transactions %>%
              filter(TransactionType == "P") %>%
              # Changed filter to postal codes starting with 621 instead of 62
              # This reflects zooming in on the map
              filter(str_detect(PostalCode_clean, "^621")) %>%
              count(PostalCode_clean) %>% 
              mutate(rank = min_rank(-n)) %>%
              arrange(desc(n)) %>% 
              rename(number = PostalCode_clean, times = n))