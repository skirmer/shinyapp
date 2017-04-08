# Hackathon project - lobbyist data analysis #

contribs <- read.csv("https://query.data.world/s/8j1cyy979bgv6eeficekno5m7",header=T)

head(contribs)


library("httr")
library("rvest")
zero <- httr::GET("https://en.wikipedia.org/wiki/Chicago_City_Council")
tables <- rvest::html_table(content(zero), fill=TRUE)
aldermen <- (tables[[1]])
cleannames <- gsub(", Jr.| Jr.", "",aldermen$Name)
name_aldermen <- str_split(cleannames, " ")
lastnames <- lapply(name_aldermen, tail, 1)
lastnames_u <- unlist(lastnames)

library(tidyverse)
library(stringr)

for(i in 1:length(contribs)){
  contribs[i, "alderman"] <- ifelse(grepl(lastnames_u[n], contribs[i, "RECIPIENT"]) == TRUE, lastnames_u[n], NA)
}


write.csv(aldermen, "./Documents/R_Projects/aldermen_to_ward.csv")
