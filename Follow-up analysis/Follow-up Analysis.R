### Wrangling
library(descr)
## Load data
d <- read.csv("RD.csv", fill = T)
d1 <- d
## Recode Missingness
d1[d1 == "N/A"] <- NA
## Delete second row
d1 <- d1[-1, ]
## Generate ID number
d1$ID <- 1:nrow(d1)

### Variable recoding
## Select only international student
d1$Q4 <- ifelse(d1$Q4 == "Yes", 1, 0)

## Academic support satisfaction
# Overall relationship with supervisor/PI
d1$Q21.f <- as.character(d1$Q21.f)
d1$Q21.f[d1$Q21.f == "1 = Not at all satisfied"] <- "1"
d1$Q21.f[d1$Q21.f == "7 = Extremely satisfied"] <- "7"
d1$Q21.f[d1$Q21.f == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q21.f <- as.numeric(d1$Q21.f)
d1$Q21.f[d1$Q21.f == 1] <- 0
d1$Q21.f[d1$Q21.f == 2] <- 1
d1$Q21.f[d1$Q21.f == 3] <- 2
d1$Q21.f[d1$Q21.f == 4] <- 3
d1$Q21.f[d1$Q21.f == 5] <- 4
d1$Q21.f[d1$Q21.f == 6] <- 5
d1$Q21.f[d1$Q21.f == 7] <- 6

# Guidance received from other mentors in lab/research
d1$Q22.d <- as.character(d1$Q22.d)
d1$Q22.d[d1$Q22.d == "1 = Not at all satisfied"] <- "1"
d1$Q22.d[d1$Q22.d == "7 = Extremely satisfied"] <- "7"
d1$Q22.d[d1$Q22.d == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q22.d <- as.numeric(d1$Q22.d)
d1$Q22.d[d1$Q22.d == 1] <- 0
d1$Q22.d[d1$Q22.d == 2] <- 1
d1$Q22.d[d1$Q22.d == 3] <- 2
d1$Q22.d[d1$Q22.d == 4] <- 3
d1$Q22.d[d1$Q22.d == 5] <- 4
d1$Q22.d[d1$Q22.d == 6] <- 5
d1$Q22.d[d1$Q22.d == 7] <- 6

## Social satisfaction
# Social environment
d1$Q21.c <- as.character(d1$Q21.c)
d1$Q21.c[d1$Q21.c == "1 = Not at all satisfied"] <- "1"
d1$Q21.c[d1$Q21.c == "7 = Extremely satisfied"] <- "7"
d1$Q21.c[d1$Q21.c == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q21.c <- as.numeric(d1$Q21.c)
d1$Q21.c[d1$Q21.c == 1] <- 0
d1$Q21.c[d1$Q21.c == 2] <- 1
d1$Q21.c[d1$Q21.c == 3] <- 2
d1$Q21.c[d1$Q21.c == 4] <- 3
d1$Q21.c[d1$Q21.c == 5] <- 4
d1$Q21.c[d1$Q21.c == 6] <- 5
d1$Q21.c[d1$Q21.c == 7] <- 6

# Opportunities to collaborate
d1$Q21.g <- as.character(d1$Q21.g)
d1$Q21.g[d1$Q21.g == "1 = Not at all satisfied"] <- "1"
d1$Q21.g[d1$Q21.g == "7 = Extremely satisfied"] <- "7"
d1$Q21.g[d1$Q21.g == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q21.g <- as.numeric(d1$Q21.g)
d1$Q21.g[d1$Q21.g == 1] <- 0
d1$Q21.g[d1$Q21.g == 2] <- 1
d1$Q21.g[d1$Q21.g == 3] <- 2
d1$Q21.g[d1$Q21.g == 4] <- 3
d1$Q21.g[d1$Q21.g == 5] <- 4
d1$Q21.g[d1$Q21.g == 6] <- 5
d1$Q21.g[d1$Q21.g == 7] <- 6

## Financial satisfaction
# Availability of funding
d1$Q21.a <- as.character(d1$Q21.a)
d1$Q21.a[d1$Q21.a == "1 = Not at all satisfied"] <- "1"
d1$Q21.a[d1$Q21.a == "7 = Extremely satisfied"] <- "7"
d1$Q21.a[d1$Q21.a == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q21.a <- as.numeric(d1$Q21.a)
d1$Q21.a[d1$Q21.a == 1] <- 0
d1$Q21.a[d1$Q21.a == 2] <- 1
d1$Q21.a[d1$Q21.a == 3] <- 2
d1$Q21.a[d1$Q21.a == 4] <- 3
d1$Q21.a[d1$Q21.a == 5] <- 4
d1$Q21.a[d1$Q21.a == 6] <- 5
d1$Q21.a[d1$Q21.a == 7] <- 6

# Benefits (health care, leave, etc.)
d1$Q22.b <- as.character(d1$Q22.b)
d1$Q22.b[d1$Q22.b == "1 = Not at all satisfied"] <- "1"
d1$Q22.b[d1$Q22.b == "7 = Extremely satisfied"] <- "7"
d1$Q22.b[d1$Q22.b == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q22.b <- as.numeric(d1$Q22.b)
d1$Q22.b[d1$Q22.b == 1] <- 0
d1$Q22.b[d1$Q22.b == 2] <- 1
d1$Q22.b[d1$Q22.b == 3] <- 2
d1$Q22.b[d1$Q22.b == 4] <- 3
d1$Q22.b[d1$Q22.b == 5] <- 4
d1$Q22.b[d1$Q22.b == 6] <- 5
d1$Q22.b[d1$Q22.b == 7] <- 6

## Work satisfaction
# Number of publications
d1$Q21.h <- as.character(d1$Q21.h)
d1$Q21.h[d1$Q21.h == "1 = Not at all satisfied"] <- "1"
d1$Q21.h[d1$Q21.h == "7 = Extremely satisfied"] <- "7"
d1$Q21.h[d1$Q21.h == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q21.h <- as.numeric(d1$Q21.h)
d1$Q21.h[d1$Q21.h == 1] <- 0
d1$Q21.h[d1$Q21.h == 2] <- 1
d1$Q21.h[d1$Q21.h == 3] <- 2
d1$Q21.h[d1$Q21.h == 4] <- 3
d1$Q21.h[d1$Q21.h == 5] <- 4
d1$Q21.h[d1$Q21.h == 6] <- 5
d1$Q21.h[d1$Q21.h == 7] <- 6

# Teaching duties
d1$Q22.c <- as.character(d1$Q22.c)
d1$Q22.c[d1$Q22.c == "1 = Not at all satisfied"] <- "1"
d1$Q22.c[d1$Q22.c == "7 = Extremely satisfied"] <- "7"
d1$Q22.c[d1$Q22.c == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q22.c <- as.numeric(d1$Q22.c)
d1$Q22.c[d1$Q22.c == 1] <- 0
d1$Q22.c[d1$Q22.c == 2] <- 1
d1$Q22.c[d1$Q22.c == 3] <- 2
d1$Q22.c[d1$Q22.c == 4] <- 3
d1$Q22.c[d1$Q22.c == 5] <- 4
d1$Q22.c[d1$Q22.c == 6] <- 5
d1$Q22.c[d1$Q22.c == 7] <- 6

## Career support satisfaction
# Career pathway guidance and advice
d1$Q22.i <- as.character(d1$Q22.i)
d1$Q22.i[d1$Q22.i == "1 = Not at all satisfied"] <- "1"
d1$Q22.i[d1$Q22.i == "7 = Extremely satisfied"] <- "7"
d1$Q22.i[d1$Q22.i == "4 = Neither satisfied nor dissatisfied"] <- "4"
d1$Q22.i <- as.numeric(d1$Q22.i)
d1$Q22.i[d1$Q22.i == 1] <- 0
d1$Q22.i[d1$Q22.i == 2] <- 1
d1$Q22.i[d1$Q22.i == 3] <- 2
d1$Q22.i[d1$Q22.i == 4] <- 3
d1$Q22.i[d1$Q22.i == 5] <- 4
d1$Q22.i[d1$Q22.i == 6] <- 5
d1$Q22.i[d1$Q22.i == 7] <- 6

## Distal outcomes
# Which of the following sectors would you most like to work in (beyond a postdoc) when you complete your degree?
# Academia
d1$Q37.a <- as.character(d1$Q37.a)
d1$Q37.a <- ifelse(d1$Q37.a == "1st", 1, 0)
d1$Q37.a <- as.numeric(d1$Q37.a)

# How much more likely are you now to pursue a research career than when you launched your PhD program?
d1$Q43.a <- as.character(d1$Q43.a)
d1$Q43.a[d1$Q43.a == "Somewhat more likely"] <- "1"
d1$Q43.a[d1$Q43.a == "Much more likely"] <- "1"
d1$Q43.a[d1$Q43.a == "Much less likely"] <- "0"
d1$Q43.a[d1$Q43.a == "Somewhat less likely"] <- "0"
d1$Q43.a[d1$Q43.a == "Equally likely"] <- "0"
d1$Q43.a[d1$Q43.a == "Unsure"] <- "0"
d1$Q43.a <- as.numeric(d1$Q43.a)

# I feel that my program is preparing me well for a research career.
d1$Q51.a <- as.character(d1$Q51.a)
d1$Q51.a[d1$Q51.a == "Agree"] <- "1"
d1$Q51.a[d1$Q51.a == "Strongly agree"] <- "1"
d1$Q51.a[d1$Q51.a == "Strongly disagree"] <- "0"
d1$Q51.a[d1$Q51.a == "Disagree"] <- "0"
d1$Q51.a[d1$Q51.a == "Neither agree nor disagree"] <- "0"
d1$Q51.a[d1$Q51.a == "I don't know"] <- "0"
d1$Q51.a <- as.numeric(d1$Q51.a)

# I feel that my program is preparing me well for a non-research science-related career.
d1$Q51.b <- as.character(d1$Q51.b)
d1$Q51.b[d1$Q51.b == "Agree"] <- "1"
d1$Q51.b[d1$Q51.b == "Strongly agree"] <- "1"
d1$Q51.b[d1$Q51.b == "Strongly disagree"] <- "0"
d1$Q51.b[d1$Q51.b == "Disagree"] <- "0"
d1$Q51.b[d1$Q51.b == "Neither agree nor disagree"] <- "0"
d1$Q51.b[d1$Q51.b == "I don't know"] <- "0"
d1$Q51.b <- as.numeric(d1$Q51.b)

## Covariates
# Ethnicity
d1$Q58.1 <- ifelse(d1$Q58.1 == "Caucasian", 1, 0) # Reference group
d1$Q58.2 <- ifelse(d1$Q58.2 == "Latino/Hispanic", 1, 0)
d1$Q58.3 <- ifelse(d1$Q58.3 == "Middle Eastern", 1, 0)
d1$Q58.4 <- ifelse(d1$Q58.4 == "African", 1, 0)
d1$Q58.5 <- ifelse(d1$Q58.5 == "Caribbean", 1, 0)
d1$Q58.6 <- ifelse(d1$Q58.6 == "South Asian", 1, 0)
d1$Q58.7 <- ifelse(d1$Q58.7 == "East Asian", 1, 0)
d1$Q58.8 <- ifelse(d1$Q58.8 == "Pacific Islander", 1, 0)
d1$Q58.9 <- ifelse(d1$Q58.9 == "American Indian", 1, 0)
d1$Q58.10 <- ifelse(d1$Q58.10 == "Mixed ethnicity", 1, 0)
d1$Q58.11 <- ifelse(d1$Q58.11 == "Other, please specify", 1, 0)
d1$Q58.12 <- ifelse(d1$Q58.12 == "Prefer not to say", 1, 0)
d1$OtherEth <- ifelse(d1$Q58.5 == 1 | d1$Q58.8 == 1 | d1$Q58.9 == 1 | d1$Q58.10 == 1 | d1$Q58.11 == 1 | d1$Q58.12 == 1, 1, 0)

# Region
d1$Q5 <- as.character(d1$Q5)
d1$Q5.1 <- ifelse(d1$Q5 == "Africa", 1, 0)
d1$Q5.2 <- ifelse(d1$Q5 == "Asia (including Middle East)", 1, 0)
d1$Q5.3 <- ifelse(d1$Q5 == "Australasia", 1, 0)
d1$Q5.4 <- ifelse(d1$Q5 == "Europe", 1, 0) # Reference
d1$Q5.5 <- ifelse(d1$Q5 == "North or Central America", 1, 0)
d1$Q5.6 <- ifelse(d1$Q5 == "South America", 1, 0)

# Age
d1$Q56 <- ifelse(d1$Q56 == "18 - 24" | d1$Q56 == "25 - 34", 0, 1)

# Gender
d1$Q57 <- ifelse(d1$Q57 == "Female (including trans female)", 1, 0)

# Which was the most important reason you decided to enroll in a PhD program?
d1$Q3 <- ifelse(d1$Q3 == "I want to pursue an academic career", 1, 0)

# On average, how many hours a week do you typically spend on your PhD program?
d1$Q24 <- ifelse(d1$Q24 == "51-60 hours" | d1$Q24 == "61-70 hours" | d1$Q24 == "71-80 hours" | d1$Q24 == "More than 80 hours", 1, 0)

# On average, how much one-on-one contact time do you spend with your supervisor each week?
d1$Q25 <- ifelse(d1$Q25 == "Between one and three hours" | d1$Q25 == "More than three hours", 1, 0)
library(tidyverse)
#install.packages("remotes")
#remotes::install_github("GerkeLab/grkmisc")
library(grkmisc)
d1$Q25 <- as.character(d1$Q25)
d1 <- d1 %>%
  mutate(Q25 = recode_if(Q25,ID == 6803 | 
                           ID == 6666 |  
                           ID == 6380 | 
                           ID == 5896 | 
                           ID == 5806 | 
                           ID == 5541 | 
                           ID == 5312 | 
                           ID == 5230 | 
                           ID == 4489 | 
                           ID == 4302 | 
                           ID == 4272 | 
                           ID == 2977 | 
                           ID == 871 | 
                           ID == 805 | 
                           ID == 265 | 
                           ID == 6362 | 
                           ID == 6318 | 
                           ID == 6292 | 
                           ID == 5569 | 
                           ID == 3836 | 
                           ID == 3624 | 
                           ID == 3465 | 
                           ID == 3434 | 
                           ID == 3301 | 
                           ID == 3239 | 
                           ID == 789 | 
                           ID == 630, "0" = "1"))

d1$Q25 <- as.numeric(d1$Q25)

# Have you ever sought help for anxiety or depression caused by PhD study?
d1$Q28 <- ifelse(d1$Q28 == "Yes", 1, 0)

# Do you feel that you have experienced bullying in you PhD program?
d1$Q31 <- ifelse(d1$Q31 == "Yes", 1, 0)

# Do you feel that you have experienced discrimination or harassment in your PhD program?
d1$Q34 <- ifelse(d1$Q34 == "Yes", 1, 0)

# Do you have any caring responsibilities?
d1$Q59.4 <- ifelse(d1$Q59.4 == "No", 0, 1)

# Subset
d2 <- cbind(d1$ID, d1$Q4, 
            d1$Q21.f, d1$Q22.d, d1$Q21.c, d1$Q21.g, d1$Q21.a, d1$Q22.b, d1$Q21.h, d1$Q22.c, d1$Q22.i, 
            d1$Q37.a, d1$Q43.a, d1$Q51.a, d1$Q51.b, 
            d1$Q56, d1$Q57, d1$Q58.1, d1$Q58.2, d1$Q58.3, d1$Q58.4, d1$Q58.6, d1$Q58.7, d1$OtherEth, d1$Q5.1, d1$Q5.2, d1$Q5.3, d1$Q5.4, d1$Q5.5, d1$Q5.6, d1$Q3, d1$Q24, d1$Q25, d1$Q28, d1$Q31, d1$Q34, d1$Q59.4)
d2 <- as.data.frame(d2)
colnames(d2) <- c("ID", "HomeCountry", 
                  "relationship", "guidance", "social", "collaboration", "funding", "benefits", "publications", "teaching", "careersupport",
                  "Academia", "DeltaA", "PrepR", "PrepNR", 
                  "Age", "Gender", "Caucasian", "Latino/Hispanic", "Middle Eastern", "African", "South Asian", "East Asian", "OtherEth", "Africa", "Aisa (including Middle East)", "Australasia", "Europe", "North or Central America", "South America", "PhDReason", "HoursWorked", "HoursMeeting", "Anxiety", "Bullying", "Discrimination", "Caring")

# International Doctoral Students
d3 <- subset(d2, d2$HomeCountry == 0)

### Follow-up analysis

## Statistics for covariates and distal outcomes 
##Covariates
# Get covariates data from Mplus
dcov <- read.csv("CovExcel.csv", fill = T)

# Subset only covariate data for each class
d8 <- dcov[, c(10:29, 36)]
d9 <- d8

# Gather vertical data
library(tidyr)
d10 <- d9 %>%
  gather("Covariate", "Value", -21)
class(d10$Class)
d10$Class <- as.factor(d10$Class)

# Covariates means by group
colMeans(d9[d9$Class == 1, ])
colMeans(d9[d9$Class == 2, ])
colMeans(d9[d9$Class == 3, ])
colMeans(d9[d9$Class == 4, ])
colMeans(d9[d9$Class == 5, ])
colMeans(d9[d9$Class == 6, ])

# Covariates standard error by group
# install.packages("plotrix")                 
library("plotrix") 
apply(d9[d9$Class == 1,], 2, std.error)
apply(d9[d9$Class == 2,], 2, std.error)
apply(d9[d9$Class == 3,], 2, std.error)
apply(d9[d9$Class == 4,], 2, std.error)
apply(d9[d9$Class == 5,], 2, std.error)
apply(d9[d9$Class == 6,], 2, std.error)

## Distal outcomes
# Get distal outcomes data from Mplus
ddis <- read.csv("DisExcel.csv", fill = T)

# Subset only distal outcomes data for each class
d11 <- ddis[, c(10:13, 20)]
d12 <- d11

# Gather vertical data
d13 <- d12 %>%
  gather("Distal", "Value", -5)
class(d10$Class)
d13$Class <- as.factor(d13$Class)

# Distal outcomes means by group
colMeans(d12[d12$Class == 1,])
colMeans(d12[d12$Class == 2,])
colMeans(d12[d12$Class == 3,])
colMeans(d12[d12$Class == 4,])
colMeans(d12[d12$Class == 5,])
colMeans(d12[d12$Class == 6,])

# Distal outcomes standard error by group
apply(d12[d12$Class == 1,], 2, std.error)
apply(d12[d12$Class == 2,], 2, std.error)
apply(d12[d12$Class == 3,], 2, std.error)
apply(d12[d12$Class == 4,], 2, std.error)
apply(d12[d12$Class == 5,], 2, std.error)
apply(d12[d12$Class == 6,], 2, std.error)


# Correlation Table
library(Hmisc)
dx3 <- as.matrix(d3[, 16:37])
rcorr(dx3)

# First two columns of correlation table
colMeans(d3[, 16:37])
apply(d3[, 16:37], 2, sd)

# Indicators means
# Subset only Indicators data for each class
d14 <- ddis[, c(1:9, 20)]
d15 <- d14

# Gather vertical data
d16 <- d15 %>%
  gather("Indicators", "Value", -10)
d16$Class <- as.factor(d16$Class)

# Indicators means by group
colMeans(d15[d15$Class == 1,], na.rm = T)
colMeans(d15[d15$Class == 2,], na.rm = T)
colMeans(d15[d15$Class == 3,], na.rm = T)
colMeans(d15[d15$Class == 4,], na.rm = T)
colMeans(d15[d15$Class == 5,], na.rm = T)
colMeans(d15[d15$Class == 6,], na.rm = T)

# Indicators standard deviation by group
apply(d15[d15$Class == 1,], 2, std.error, na.rm = T)
apply(d15[d15$Class == 2,], 2, std.error, na.rm = T)
apply(d15[d15$Class == 3,], 2, std.error, na.rm = T)
apply(d15[d15$Class == 4,], 2, std.error, na.rm = T)
apply(d15[d15$Class == 5,], 2, std.error, na.rm = T)
apply(d15[d15$Class == 6,], 2, std.error, na.rm = T)



