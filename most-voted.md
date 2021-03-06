# Election results from Canadian Federal elections in 2011

How signigicant was conservative lead?


```r
# Analyze election results from Canadian Federal Elections 2011

library(readr)
library(dplyr)

#setwd(file.path(normalizePath("~"),"elections"))
```


```r
## Get data
remote <- 
        "http://www.elections.ca/scripts/OVR2011/34/data_donnees/table_tableau12.csv"
#local<-basename(remote)
#if(!file.exists(local)) download.file(remote, local, mode="wb")
#e <- read_csv(local)
# Use remote directly to skip mode setting problem
e <- read_csv(remote)



## Clean data

# Vectorized function to drop everything after first slash - normally French
# location names - and replace spaces with dots

vEnglishize <- function(strings.vector, replace.spaces = FALSE) {
        sapply(strings.vector, 
               function(s) {
                        x <- strsplit(s, "/", fixed=TRUE) %>% unlist
                        x <- trimws(x[1])
                        if (replace.spaces) x <- gsub(" ", ".", x, fixed=TRUE)
                        return(x)
               }
        )
}

colnames(e) %>% vEnglishize(replace.spaces=TRUE) -> colnames(e)
e$Province %>% vEnglishize -> e$Province
e$Candidate %>% vEnglishize -> e$Candidate
e$Candidate.Residence %>% vEnglishize -> e$Candidate.Residence

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
```

```
## Observations: 10
## Variables:
## $ Province                     (chr) "Alberta", "Alberta", "Alberta", ...
## $ Electoral.District.Name      (chr) "Crowfoot", "Wetaskiwin", "Vegrev...
## $ Electoral.District.Number    (int) 48010, 48026, 48024, 48025, 48020...
## $ Candidate                    (chr) "Kevin A Sorenson ** Conservative...
## $ Candidate.Residence          (chr) "Killam, Alta.", "Lacombe, Alta."...
## $ Candidate.Occupation         (chr) "Parliamentarian/", "College Inst...
## $ Votes.Obtained               (int) 44115, 37756, 39145, 32652, 40007...
## $ Percentage.of.Votes.Obtained (dbl) 84.0, 81.4, 79.8, 77.8, 77.5, 77....
## $ Majority                     (dbl) 39310, 32475, 33584, 27549, 34672...
## $ Majority.Percentage          (dbl) 74.8, 70.0, 68.5, 65.7, 67.1, 64....
```

```r
## What about BC?

e[e$Province=="British Columbia", ] %>%
        arrange(desc(Percentage.of.Votes.Obtained)) %>%
        head(10) %>%
        glimpse
```

```
## Observations: 10
## Variables:
## $ Province                     (chr) "British Columbia", "British Colu...
## $ Electoral.District.Name      (chr) "Abbotsford", "Langley", "Vancouv...
## $ Electoral.District.Number    (int) 59001, 59013, 59030, 59022, 59023...
## $ Candidate                    (chr) "Ed Fast ** Conservative", "Mark ...
## $ Candidate.Residence          (chr) "Abbotsford, B.C.", "Langley, B.C...
## $ Candidate.Occupation         (chr) "Parliamentarian/", "Parliamentar...
## $ Votes.Obtained               (int) 32493, 35569, 27794, 23946, 25109...
## $ Percentage.of.Votes.Obtained (dbl) 65.0, 64.5, 62.8, 62.1, 58.4, 57....
## $ Majority                     (dbl) 22404, 24292, 19433, 14070, 17082...
## $ Majority.Percentage          (dbl) 44.8, 44.1, 43.9, 36.5, 39.7, 35....
```

```r
stdev <- sd(e$Percentage.of.Votes.Obtained)
aver  <- mean(e$Percentage.of.Votes.Obtained)
cat(paste("If the distribution would be normal, 68% of winning candidates would get",
          round(aver - stdev),
          "to",
          round(aver + stdev),
          "percents of votes"))
```

```
## If the distribution would be normal, 68% of winning candidates would get 0 to 38 percents of votes
```

```r
hist(e$Percentage.of.Votes.Obtained,
     main = "Is it hard to get 50% of votes?",
     xlab = "Percentage of votes",
     ylab = "Number of candidates")
```

![](most-voted_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Let's see what distribution is for winners

winners <- tapply(e$Percentage.of.Votes.Obtained,
                  e$Electoral.District.Number,
                  max)

hist(winners,
     main = "Lead of winners",
     xlab = "Percentage of votes",
     ylab = "Number of candidates")
```

![](most-voted_files/figure-html/unnamed-chunk-2-2.png) 

```r
stdev <- sd(winners)
aver <- mean(winners)
cat(paste("If the distribution would be normal, 68% of winning candidates would get",
          round(aver - stdev),
          "to",
          round(aver + stdev),
          "percents of votes"))
```

```
## If the distribution would be normal, 68% of winning candidates would get 40 to 61 percents of votes
```

Let's check Benford's law

```r
vFirstDigit <- function(numbers.vector) {
        sapply(numbers.vector, function(x) {
                if (x > 0) {
                        while (x < 1) x <- x*10
                        x %>% as.character %>% substr(1,1) %>% as.integer -> x
                        }
                return(x)
                })
}

hist(vFirstDigit(e$Percentage.of.Votes.Obtained),
     main="Benford's law tested for percentages of votes for all candidates",
     xlab="First digits of vote percentages",
     breaks=1:9,
     xaxt="n")
axis(1,at=1:9,hadj=-4)
```

![](most-voted_files/figure-html/unnamed-chunk-3-1.png) 

```r
hist(vFirstDigit(winners),
     main="Benford's law tested for percentages of votes for winning candidates",
     xlab="First digits of vote percentages",
     breaks=1:9,
     xaxt="n")
axis(1,at=1:9,hadj=-4)
```

![](most-voted_files/figure-html/unnamed-chunk-3-2.png) 

Actually, aren't these essentially the same as previous histograms,
although rounded? The difference is in candidates who got less than 1% of votes.
How many of these were there?

```r
(sum(e$Percentage.of.Votes.Obtained<1) / nrow(e)) %>% signif(2)
```

```
## [1] 0.16
```
More than I expected, actually, - one in six.

Anyway, what this tells us, is that votes distribution isn't normal, but rather
is governed by something close to Benford's law. It makes sense, because it's
non-linearly harder process to get more votes. Exponential diffuculty instead
of exponential growth.
