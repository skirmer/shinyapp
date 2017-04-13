# Investigation of cases of double-counting contributions #
# Data 4 Democracy - Chicago Lobbyists Project #
# Stephanie Kirmer #
# April 13, 2017 #

library(tidyverse)

# If the date and the amount and the recipient and the lobbyist are the same, 
# is the contribution ID the same, even if different clients?

df <- read.csv("https://query.data.world/s/ebgm9pltjmv22p1w9lmkdx46e",header=T, stringsAsFactors=FALSE)

# Assume that the same amount, from the same lobbyist ID, on the same date, 
# to the exact same recipient organization/LLC, is likely a duplicate

#Filter being used on current output:
df3 <- unique(df[,c("Year","CLIENT_NAME", "CONTRIBUTION_DATE", "recipient_surname","AMOUNT", "CLIENT.INDUSTRY")])

#Backtracking to remove client from the math but add the lobbyist
df2 <- unique(df[,c("Year","CONTRIBUTION_DATE", "recipient_surname","RECIPIENT","AMOUNT", "LOBBYIST_ID")])


df2 <- df[,c("Year","CONTRIBUTION_DATE", "recipient_surname", "RECIPIENT","AMOUNT", "LOBBYIST_ID")]

# Summarize the list of records that are
dupecheck <- df2 %>%
  group_by(Year,CONTRIBUTION_DATE, recipient_surname, RECIPIENT,
           AMOUNT, LOBBYIST_ID) %>% 
  filter(!is.na(recipient_surname)) %>%
  summarize(records = n(), 
            value = max(AMOUNT),
            total_excess_val = (value * records) - value) %>%
  arrange(CONTRIBUTION_DATE, AMOUNT, recipient_surname)

View(dupecheck)

dupecheck_byperson <- dupecheck %>%
  group_by(recipient_surname) %>%
  summarize(total_excess = sum(total_excess_val))

withdupes <- df2 %>%
  filter(!is.na(recipient_surname)) %>%
  group_by(recipient_surname) %>%
  summarize(total_given = sum(AMOUNT))

viz_now <- df3 %>%
  filter(!is.na(recipient_surname)) %>%
  group_by(recipient_surname) %>%
  summarize(viz_total = sum(AMOUNT))

compare_both <- cbind(dupecheck_byperson, withdupes[,2], viz_now[,2])
compare_both$actual_amt <- compare_both$total_given - compare_both$total_excess
compare_both$viz_error <- compare_both$viz_total - compare_both$actual_amt
View(compare_both)


write.csv(compare_both, "../chilobby/excess_value.csv")
