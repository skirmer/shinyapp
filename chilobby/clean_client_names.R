# Client Name Cleaning #
# Data 4 Democracy - Chicago Lobbyists Project #
# Stephanie Kirmer #
# April 13, 2017 #

# Liberally draws from the work of Matt E. Sweeney #

library(tidyverse)
library(tm)

starting <- read.csv("../chilobby/Lobbyist_Data_-_Lobbyist__Employer__Client_Combinations.csv", stringsAsFactors = FALSE)

#Save the old name
starting$CLIENT_NAME_ORIG <- starting$CLIENT_NAME

# text pre-processing of CLIENT_NAME -- these correct for common entry mistakes
starting$CLIENT_NAME <- toupper(starting$CLIENT_NAME) #make all characters upper case
starting$CLIENT_NAME <- gsub(".COM$", "", starting$CLIENT_NAME) # drop .com if it appears at the end
starting$CLIENT_NAME <- removePunctuation(starting$CLIENT_NAME)
starting$CLIENT_NAME <- trimws(starting$CLIENT_NAME, which = "both") #Might want to run this at repeated points

# Remove abbreviations like LLC #
starting$CLIENT_NAME <- gsub("AND ITS AFFILIATES|AN ILLINOIS CORPORATION| LLC| INC| LLP| CORPORATE| CORPORATION| CORP", "", starting$CLIENT_NAME)
starting$CLIENT_NAME <- gsub(" CO$", "", starting$CLIENT_NAME) #Only cut CO when it's at the end of a string alone

starting$CLIENT_NAME <- trimws(starting$CLIENT_NAME, which = "both") #Might want to run this at repeated points
starting$CLIENT_NAME <- gsub(" ST$", "", starting$CLIENT_NAME) #Only cut ST or STREET when it's at the end of a string alone
starting$CLIENT_NAME <- gsub(" STREET$", "", starting$CLIENT_NAME) 

#Fix a spelling screwup or two
starting$CLIENT_NAME <- gsub("SERVIES", "SERVICES", starting$CLIENT_NAME) 
starting$CLIENT_NAME <- gsub(" ASSOC$", " ASSOCIATION", starting$CLIENT_NAME) 

# Remove multiple spaces
starting$CLIENT_NAME <- gsub("  ", " ", starting$CLIENT_NAME)
starting$CLIENT_NAME <- trimws(starting$CLIENT_NAME, which = "both")

sum(duplicated(starting$CLIENT_NAME))
sum(duplicated(starting$CLIENT_NAME_ORIG))

# Explore names and look for similarities

tester <- starting %>%
  dplyr::arrange(CLIENT_NAME)

write.csv(tester, "../chilobby/cleaned_clients.csv")
