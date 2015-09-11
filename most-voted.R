# Analyze election results from Canadian Federal Elections 2011

library(readr)
library(dplyr)

setwd(file.path(normalizePath("~"),"elections"))

## Get data
remote <- 
        "http://www.elections.ca/scripts/OVR2011/34/data_donnees/table_tableau12.csv"
local<-basename(remote)
if(!file.exists(local)) download.file(remote, local)
e <- read_csv(local)

## Clean data

# Vectorized function to drop everything after first slash - normally French
# location names - and replace spaces with dots

Englishize <- function(s, replace.spaces = FALSE) {
        sapply(s, 
               function(s) {
                        x <- strsplit(s, "/", fixed=TRUE) %>% unlist
                        x <- trimws(x[1])
                        if (replace.spaces) x <- gsub(" ", ".", x, fixed=TRUE)
                        return(x)
               }
        )
}

colnames(e) %>% Englishize(replace.spaces=TRUE) -> colnames(e)
e$Province %>% Englishize -> e$Province
e$Candidate %>% Englishize -> e$Candidate
e$Candidate.Residence %>% Englishize -> e$Candidate.Residence

numerize <- function(x) {
        x %>% trimws %>% as.numeric
}

e$Majority %>% numerize -> e$Majority
e$Majority.Percentage %>% numerize -> e$Majority.Percentage

## What are the most partisan places?

e %>% 
        arrange(desc(Percentage.of.Votes.Obtained)) %>%
        head(10) %>%
        glimpse

## What about BC?

e[e$Province=="British Columbia",] %>%
        arrange(desc(Percentage.of.Votes.Obtained)) %>%
        head(10) %>%
        glimpse
