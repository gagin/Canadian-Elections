# Analyze election results from Canadian Federal Elections 2011

library(readr)
library(dplyr)

setwd(file.path(normalizePath("~"),"elections"))

## Get data
remote <- 
        "http://www.elections.ca/scripts/OVR2011/34/data_donnees/table_tableau12.csv"
local<-basename(remote)
if(!file.exists(local)) download.file(remote, local, mode="wb")
e <- read_csv(local)
e <- read_csv(remote)

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

e[e$Province=="British Columbia", ] %>%
        arrange(desc(Percentage.of.Votes.Obtained)) %>%
        head(10) %>%
        glimpse

stdev <- sd(e$Percentage.of.Votes.Obtained)
aver  <- mean(e$Percentage.of.Votes.Obtained)
cat(paste("68% of candidates get",
          round(aver - stdev),
          "to",
          round(aver + stdev),
          "percents of votes"))
hist(e$Percentage.of.Votes.Obtained,
     main = "Is it hard to get 50% of votes?",
     xlab = "Percentage of votes",
     ylab = "Number of candidates")

# Let's see what distribution is for winners

winners <- tapply(e$Percentage.of.Votes.Obtained,
                  e$Electoral.District.Number,
                  max)

hist(winners,
     main = "Lead of winners",
     xlab = "Percentage of votes",
     ylab = "Number of candidates")

stdev <- sd(winners)
aver <- mean(winners)
cat(paste("68% of winning candidates get",
          round(aver - stdev),
          "to",
          round(aver + stdev),
          "percents of votes"))
