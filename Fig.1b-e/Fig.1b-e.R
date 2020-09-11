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

# Internaitonal Doctoral Students
d3 <- subset(d2, d2$HomeCountry == 0)

### Fig.1 

## Covariates data preparation
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

# Relable the covariates and groups
d10$Covariate <- factor(d10$Covariate, 
                        levels = c("Caring", "Discrimination", "Bullying", "Anxiety", "HoursMeeting", "HoursWorked", "PhDReason",      
                                   "SouthAmerica", "NorthCentralAmerica", "Australasia", "Asia", "Africa",    
                                   "OtherEth", "EastAsian", "SouthAsian", "African", "MiddleEastern", "LatinoHispanic",   
                                   "Gender", "Age"),
                        labels = c( "Have caring responsibilities",
                                    "Experienced discrimination or harassment",
                                    "Experienced bullying",
                                    "Sought help for anxiety or depression",
                                    "Spend > 1 hour with supervisor per week",
                                    "Work > 50 hrs. per week",
                                    "Pursue Ph.D. for academic career",
                                    "South America", "North or Central America", "Australasia", "Asia (including Middle East)", "Africa", 
                                    "Other Ethnicities", "East Asian", "South Asian", "African", "Middle Eastern", "Latino / Hispanic",     
                                    "Female",
                                    "Age > 34 yrs."))

d10$Class <- factor(d10$Class, levels=c("1", "2", "3", "4", "5", "6"), 
                    labels = c("Dissatisfied", 
                               "Ambivalent", 
                               "Low Spirit", 
                               "Satisfactorily Advised", 
                               "Teaching Striver", 
                               "Satisfied"))

## Distal outcomes data preparation
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

# Relable the covariates and groups
d13$Distal <- factor(d13$Distal, 
                     levels = c("Academia", "PrepR", "PrepNR", "DeltaA"), 
                     labels = c("Want to work in academia", 
                                "Prepared for research career", 
                                "Prepared for non-research career",
                                "More likely to research"))

d13$Class <- factor(d13$Class, 
                    levels=c("1", "2", "3", "4", "5", "6"), 
                    labels = c("Dissatisfied", 
                               "Ambivalent", 
                               "Low Spirit", 
                               "Satisfactorily Advised", 
                               "Teaching Striver", 
                               "Satisfied"))

bd1 <- d13

### Plots
## Plot for Fig.1b Distal outcomes means by profile 
library(ggplot2)
library(wesanderson)
library(stringr)
cbbPalette <- c("#FFCC00", "#FF9900", "#FF6600", "#990000")
posn.d <- position_dodge(width=0.95)

ggplot(data=bd1, mapping=aes(x=Class, y=Value, fill=str_wrap(Distal,18))) +
  labs(title="", x="Graduate student typology", y="Level of intention") + 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  stat_summary(fun.data=mean_sdl, geom="bar", position=posn.d) + 
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", position=posn.d, width=0.3) +
  coord_flip() +  
  ylim(0,1) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(text = element_text(size = 12)) +
  theme(legend.text=element_text(size=11), legend.position = c(0.7,0.5), legend.direction ="vertical") +
  theme(legend.key.height=unit(1, "cm")) +
  guides(fill=guide_legend(ncol=1, reverse = TRUE)) +
  scale_fill_manual(values=cbbPalette, name = "Distal outcomes")

ggplot(data=bd1, mapping=aes(x=Class, y=Value, fill=str_wrap(Distal,18))) +
  labs(title="", x="Graduate student typology", y="Level of intention") + 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  stat_summary(fun.data=mean_sdl, geom="bar", position=posn.d) + 
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", position=posn.d, width=0.3) +
  coord_flip() +  
  ylim(0,1) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "none")+
  scale_fill_manual(values=cbbPalette)


### Figure were later rearranged with modified position for legends.

## Covariates data (significant personal status variables)
# Data preparation
da7 <- dcov[, c(23:29, 36)]
da8 <- da7

# Gather vertical data
da8 <- da8 %>%
  gather("PersonalStatus", "Value", -8)
da8$Class <- as.factor(da8$Class)

# Relable the covariates and groups
da8$Class <- factor(da8$Class, 
                    levels=c("1", "2", "3", "4", "5", "6"), 
                    labels=c("Dissatisfied", 
                             "Ambivalent", 
                             "Low Spirit", 
                             "Satisfactorily Advised", 
                             "Teaching Striver", 
                             "Satisfied"))

da8$PersonalStatus <- factor(da8$PersonalStatus, 
                             levels=c("PhDReason", 
                                      "HoursWorked", 
                                      "HoursMeeting", 
                                      "Anxiety", 
                                      "Bullying", 
                                      "Discrimination", 
                                      "Caring"), 
                             labels = c( "Pursue Ph.D. for academic career (enroll reason)",
                                         "Work > 50 hrs. per week",
                                         "Spend >= 1 hour with supervisor per week",
                                         "Sought help for anxiety or depression",
                                         "Experienced bullying", 
                                         "Experienced discrimination or harassment",
                                         "Have caring responsibilities"))

# Gather only significant covariates
ad8 <- da8
ad8<-ad8[!(ad8$PersonalStatus=="Work > 50 hrs. per week" | ad8$PersonalStatus=="Have caring responsibilities"),]
bd4 <- ad8

### Plots
## Plot for Fig.1c Covariates means of personal status by profile 
cbPalette <- c("#33CCFF", "#66CCFF", "#6699FF", "#3366FF", "#0033CC")

ggplot(data=bd4, mapping=aes(x=Class, y=Value, fill=PersonalStatus)) +
  labs(x="Graduate student typology", y="Level of agreement") + 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  stat_summary(fun.data=mean_sdl, geom="bar", position=posn.d) + 
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", position=posn.d, width=0.3) +
  ylim(0,1)+ 
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  theme(text = element_text(size = 12)) +
  theme(legend.text=element_text(size=11), legend.position = c(0.70, 0.5)) +
  scale_fill_manual(values=cbPalette, name = "Personal status covariates")

ggplot(data=bd4, mapping=aes(x=Class, y=Value, fill=PersonalStatus)) +
  labs(x="Graduate student typology", y="Level of agreement") + 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  stat_summary(fun.data=mean_sdl, geom="bar", position=posn.d) + 
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", position=posn.d, width=0.3) +
  ylim(0,1)+ 
  coord_flip() +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "none") +
  scale_fill_manual(values=cbPalette)

### Figure were later rearranged with modified position for legends.

## Fig.1d plot data preparation
df1 <- cbind(d1$ID, d1$Q31,  d1$Q32.1, d1$Q32.2, d1$Q32.3, d1$Q32.4, d1$Q32.5, d1$Q32.6, d1$Q32.7, d1$Q32.a)
df1 <- as.data.frame(df1)
colnames(df1) <- c("ID", "Bullied","Supervisor", "AnotherStudent", "Postdoc", "OtherAcademicStaffMember", "OnlineTroll", "OtherPleaseSpecify", "PreferNotToSay", "OtherAnswer")
df2 <- df1
# Note: Other academic staff member has a space
library(tidyverse)
df2 <- mutate(df2, BulliedByWhom = case_when(
  Bullied == "0" ~ 'None',
  Bullied == "1" & OtherAcademicStaffMember == "Other academic staff member " ~ 'Other academic staff member',
  Bullied == "1" & Supervisor == "Supervisor" ~ 'Supervisor',
  Bullied == "1" & AnotherStudent == "Another student" ~ 'Another student',
  Bullied == "1" & Postdoc == "Postdoc" ~ 'Postdoc',
  Bullied == "1" & OnlineTroll == "Online troll" ~ 'Online troll',
  Bullied == "1" & OtherPleaseSpecify == "Other, please specify" ~ 'Other, please specify',
  Bullied == "1" & PreferNotToSay == "Prefer not to say" ~ 'Prefer not to say'),
  BulliedByWhom = as.factor(BulliedByWhom))

df3 <- df2
df3 <- df3[df3$ID %in% dcov$ID, ]

# Combined class
df4 <- cbind(df3, dcov$Class)
names(df4)[12] <- "Class"

# For multiple selection, calculate counts
dx1 <- df4
library(reshape2)
require(plyr)

# Create dummy variables
dx1$Supervisor <-  ifelse(dx1$Supervisor == "Supervisor", 1, 0)
dx1$AnotherStudent <-  ifelse(dx1$AnotherStudent == "Another student", 1, 0)
dx1$Postdoc <-  ifelse(dx1$Postdoc == "Postdoc", 1, 0)
dx1$OtherAcademicStaffMember <-  ifelse(dx1$OtherAcademicStaffMember == "Other academic staff member ", 1, 0)
dx1$OnlineTroll <-  ifelse(dx1$OnlineTroll == "Online troll", 1, 0)
dx1$OtherorPreferNotToSay <-  ifelse(dx1$OtherPleaseSpecify == "Other, please specify" | dx1$PreferNotToSay == "Prefer not to say", 1, 0)
dx1$None <- ifelse(dx1$Bullied == 0, 1, 0)

# Create data for multiple selection
dx2 <- dx1

dx2 <- dx2 %>%
  select(Supervisor, AnotherStudent, Postdoc, OtherAcademicStaffMember, OnlineTroll, OtherorPreferNotToSay, Class)

dx2$Class <- factor(dx2$Class, 
                    levels=c("1", "2", "3", "4", "5", "6"), 
                    labels = c("Dissatisfied", 
                               "Ambivalent", 
                               "Low Spirit", 
                               "Satisfactorily Advised", 
                               "Teaching Striver", 
                               "Satisfied"))

dx3 <- ddply(dx2,.(Class),numcolwise(sum))

dx4 <- melt(dx3,id.var="Class")

dx4$variable <- factor(dx4$variable, 
                       levels = c( "OtherorPreferNotToSay", 
                                   "OnlineTroll", 
                                   "OtherAcademicStaffMember", 
                                   "Postdoc", 
                                   "AnotherStudent", 
                                   "Supervisor"), 
                       labels = c("Other or Prefer not to say", 
                                  "Online troll", 
                                  "Other academic staff member", 
                                  "Postdoc", 
                                  "Another student", 
                                  "Supervisor"))

# Calculate within group percentage
dxx2 <- dx4
dxx2$ClassValue <- rep(c(153, 435, 179, 725, 301, 665),6)
dxx2 <- group_by(dxx2, Class) %>% mutate(percent = value/ClassValue)

bd2 <- dxx2

### Plots
## Plot for Fig.1d Bullied by whom (Percentage within groups) 
ggplot() + 
  geom_bar(
    aes(x=Class,fill=variable,y=percent),
    data=bd2,
    stat="identity", position="dodge") + 
  labs(title="", x="Graduate student typology", y="Percentage within groups") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  coord_flip() +
  ggtitle("") +
  theme(legend.position = "right") +
  scale_fill_grey(start=0.8, end=0.2, name = "Bullied by whom") +
  theme(legend.position = c(0.8, 0.5)) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  guides(fill = guide_legend(reverse = TRUE, ncol=1)) +
  theme(text = element_text(size = 12)) +
  theme(legend.text=element_text(size=11))

ggplot() + 
  geom_bar(
    aes(x=Class,fill=variable,y=percent),
    data=bd2,
    stat="identity", position="dodge") + 
  labs(title="", x="Graduate student typology", y="Percentage within groups") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  coord_flip() +
  ggtitle("") +
  theme(legend.position = "none") +
  scale_fill_grey(start=0.8, end=0.2) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  theme(text = element_text(size = 20))

### Figure were later rearranged with modified position for legends.

## Fig.1e plot data preparation
df5 <- cbind(d1$ID, d1$Q34,  d1$Q35.1, d1$Q35.2, d1$Q35.3, d1$Q35.4, d1$Q35.5, d1$Q35.6, d1$Q35.7, d1$Q35.8, d1$Q35.9, d1$Q35.a)
df5 <- as.data.frame(df5)
colnames(df5) <- c("ID", 
                   "Discriminated",
                   "RacialDiscriminationOrHarassment", 
                   "SexualHarassment", 
                   "AgeDiscrimination",
                   "GenderDiscrimination", 
                   "LGBTQDiscriminationOrHarassment",
                   "ReligiousDiscrimination", 
                   "DisabilityDiscrimination", 
                   "OtherPleaseSpecify", 
                   "PreferNotToSay", 
                   "OtherAnswer")
df6 <- df5

df6 <- mutate(df6, DiscriminationType = case_when(
  Discriminated == "0" ~ 'None',
  Discriminated == "1" & RacialDiscriminationOrHarassment == "Racial discrimination or harassment" ~ 'Racial discrimination or harassment',
  Discriminated == "1" & SexualHarassment == "Sexual harassment" ~ 'Sexual harassment',
  Discriminated == "1" & AgeDiscrimination == "Age discrimination" ~ 'Age discrimination',
  Discriminated == "1" & GenderDiscrimination == "Gender discrimination" ~ 'Gender discrimination',
  Discriminated == "1" & LGBTQDiscriminationOrHarassment == "LGBTQ discrimination or harassment" ~ 'LGBTQ discrimination or harassment',
  Discriminated == "1" & ReligiousDiscrimination == "Religious discrimination" ~ 'Religious discrimination',
  Discriminated == "1" & DisabilityDiscrimination == "Disability discrimination" ~ 'Disability discrimination',
  Discriminated == "1" & OtherPleaseSpecify == "Other, please specify" ~ 'Other, please specify',
  Discriminated == "1" & PreferNotToSay == "Prefer not to say" ~ 'Prefer not to say'),
  DiscriminationType = as.factor(DiscriminationType))

df7 <- df6
df7 <- df7[df7$ID %in% dcov$ID, ]

# Combined class
df8 <- cbind(df7, dcov$Class)
names(df8)[14] <- "Class"

### For multiple selection, calculate counts
dx5 <- df8

dx5$RacialDiscriminationOrHarassment <-  ifelse(dx5$RacialDiscriminationOrHarassment == "Racial discrimination or harassment", 1, 0)
dx5$SexualHarassment <-  ifelse(dx5$SexualHarassment == "Sexual harassment", 1, 0)
dx5$AgeDiscrimination <-  ifelse(dx5$AgeDiscrimination == "Age discrimination", 1, 0)
dx5$GenderDiscrimination <-  ifelse(dx5$GenderDiscrimination == "Gender discrimination", 1, 0)
dx5$LGBTQDiscriminationOrHarassment <-  ifelse(dx5$LGBTQDiscriminationOrHarassment == "LGBTQ discrimination or harassment", 1, 0)
dx5$ReligiousDiscrimination <-  ifelse(dx5$ReligiousDiscrimination == "Religious discrimination", 1, 0)
dx5$DisabilityDiscrimination <-  ifelse(dx5$DisabilityDiscrimination == "Disability discrimination", 1, 0)
dx5$OtherorPreferNotToSay <-  ifelse(dx5$OtherPleaseSpecify == "Other, please specify" | dx5$PreferNotToSay == "Prefer not to say", 1, 0)
dx5$None <- ifelse(dx5$Discriminated == 0, 1, 0)

# Create data for multiple selection
dx6 <- dx5

dx6 <- dx6 %>%
  select(RacialDiscriminationOrHarassment, AgeDiscrimination, GenderDiscrimination, LGBTQDiscriminationOrHarassment, ReligiousDiscrimination, DisabilityDiscrimination, OtherorPreferNotToSay, Class)

dx6$Class <- factor(dx6$Class, 
                    levels=c("1", "2", "3", "4", "5", "6"), 
                    labels = c("Dissatisfied", 
                               "Ambivalent", 
                               "Low Spirit", 
                               "Satisfactorily Advised", 
                               "Teaching Striver", 
                               "Satisfied"))

dx7 <- ddply(dx6,.(Class),numcolwise(sum))

dx8 <- melt(dx7,id.var="Class")

dx8$variable <- factor(dx8$variable, 
                       levels=c("OtherorPreferNotToSay", 
                                "LGBTQDiscriminationOrHarassment",  
                                "DisabilityDiscrimination", 
                                "ReligiousDiscrimination", 
                                "AgeDiscrimination",
                                "GenderDiscrimination", 
                                "RacialDiscriminationOrHarassment"), 
                       labels = c("Other or Prefer not to say",  
                                  "LGBTQ discrimination or harassment",  
                                  "Disability discrimination", 
                                  "Religious discrimination", 
                                  "Age discrimination",
                                  "Gender discrimination",
                                  "Racial discrimination or harassment"))

# Calculate within group percentage
dxx4 <- dx8
dxx4$ClassValue <- rep(c(153, 435, 179, 725, 301, 665),7)
dxx4 <- group_by(dxx4, Class) %>% mutate(percent = value/ClassValue)

bd3 <- dxx4

### Plots
## Plot for Fig.1e Discrimination type (Percentage within groups)
ggplot() + 
  geom_bar(
    aes(x=Class,fill=variable,y=percent),
    data=bd3,
    stat="identity", position="dodge") + 
  labs(title="", x="Graduate student typology", y="Percentage within groups") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  coord_flip() +
  ggtitle("") +
  theme(legend.position = "right") +
  scale_fill_grey(start=0.8, end=0.2, name = "Discrimination types") +
  theme(legend.position = c(0.8, 0.5)) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  guides(fill = guide_legend(reverse = TRUE, ncol=1)) +
  theme(text = element_text(size = 12)) +
  theme(legend.text=element_text(size=11))

ggplot() + 
  geom_bar(
    aes(x=Class,fill=variable,y=percent),
    data=bd3,
    stat="identity", position="dodge") + 
  labs(title="", x="Graduate student typology", y="Percentage within groups") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=12,  family="Arial", colour="black")) +
  coord_flip() +
  ggtitle("") +
  theme(legend.position = "none") +
  scale_fill_grey(start=0.8, end=0.2) +
  theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) +
  theme(text = element_text(size = 20))

### Figure were later rearranged with modified position for legends.





