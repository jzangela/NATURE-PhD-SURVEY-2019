lrd <- read.table("MS1.txt")
colnames(lrd) <- c("relationship", "guidance", "social", "collaboration", "funding", "benefits", "publications", "teaching", "careersupport",
                   "Academia", "DeltaA", "PrepR", "PrepNR",
                   "Age", "Female",
                   "LatinoHispanic", "MiddleEastern", "African", "SouthAsian", "EastAsian", "OtherEth",
                   "Africa", "Asia", "Australasia", "NorthCentralAmerica", "SouthAmerica",
                   "PhDReason", "HoursWorked", "HoursMeeting", "Anxiety", "Bullying", "Discrimination", "Caring",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4", "CPROB5", "CPROB6",
                   "Class",
                   "ID")

d1 <- lrd
d1$C1 <- ifelse(d1$Class == 1, 1, 0)
d1$C2 <- ifelse(d1$Class == 2, 1, 0)
d1$C3 <- ifelse(d1$Class == 3, 1, 0)
d1$C4 <- ifelse(d1$Class == 4, 1, 0)
d1$C5 <- ifelse(d1$Class == 5, 1, 0)
d1$C6 <- ifelse(d1$Class == 6, 1, 0)
sum(d1$C1)
sum(d1$C2)
sum(d1$C3)
sum(d1$C4)
sum(d1$C5)
sum(d1$C6)

lgAcademia <- glm(Academia ~ Age + Female +
                  LatinoHispanic + MiddleEastern + African + SouthAsian + EastAsian + OtherEth +
                  Africa + Asia + Australasia + NorthCentralAmerica + SouthAmerica +
                  PhDReason + HoursWorked + HoursMeeting + Anxiety + Bullying + Discrimination + Caring +
                  C1 + C2 + C3 + C4 + C5, data = d1, family = "binomial")
summary(lgAcademia)

lgDeltaA <- glm(DeltaA ~ Age + Female +
                    LatinoHispanic + MiddleEastern + African + SouthAsian + EastAsian + OtherEth +
                    Africa + Asia + Australasia + NorthCentralAmerica + SouthAmerica +
                    PhDReason + HoursWorked + HoursMeeting + Anxiety + Bullying + Discrimination + Caring +
                  C1 + C2 + C3 + C4 + C5, data = d1, family = "binomial")
summary(lgDeltaA)

lgPrepR <- glm(PrepR ~ Age + Female +
                  LatinoHispanic + MiddleEastern + African + SouthAsian + EastAsian + OtherEth +
                  Africa + Asia + Australasia + NorthCentralAmerica + SouthAmerica +
                  PhDReason + HoursWorked + HoursMeeting + Anxiety + Bullying + Discrimination + Caring +
                 C1 + C2 + C3 + C4 + C5, data = d1, family = "binomial")
summary(lgPrepR)

lgPrepNR <- glm(PrepNR ~ Age + Female +
                 LatinoHispanic + MiddleEastern + African + SouthAsian + EastAsian + OtherEth +
                 Africa + Asia + Australasia + NorthCentralAmerica + SouthAmerica +
                 PhDReason + HoursWorked + HoursMeeting + Anxiety + Bullying + Discrimination + Caring +
                  C1 + C2 + C3 + C4 + C5, data = d1, family = "binomial")
summary(lgPrepNR)







