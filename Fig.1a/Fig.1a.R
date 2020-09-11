### Copy from Excel file 
library(tidyverse)
x <- read.delim(pipe("pbpaste"))
x1 <- x %>% gather("Class", "Value", -1)
x1$Class <- as.factor(x1$Class)
levels(x1$Class)
x1$Class <- factor(x1$Class, 
                   levels = c("Love.Doctoral.Study..27.2..", 
                              "Teach.to.Live..12.0..", 
                              "Good.Advisor.Middle.Across..28.4..", 
                              "Middle.Adivsor.Low.Across..7.8..", 
                              "Middle.Advisor.Middle.Across..18.2..",
                              "Hate.Doctoral.Study..6.4.."), 
                    labels = c("Satisfied",
                               "Teaching Striver",
                               "Satisfactorily Advised",
                               "Low Spirit", 
                               "Ambivalent", 
                               "Dissatisfied"))
class(x1$X)
x1$X <- as.factor(x1$X)
levels(x1$X)
x1$X <- factor(x1$X, 
               levels = c("Overall relationship with supervisor/PI", 
                          "Guidance received from other mentors in lab/research",
                          "Social environment ",
                          "Opportunities to collaborate",
                          "Availability of funding ",
                          "Benefits (health care, leave, etc.) ",
                          "Number of publications",
                          "Teaching duties ",
                          "Career Advise Received"),
               labels = c("Overall relationship", # with supervisor/PI", 
                          "Guidance received", # from other mentors in lab/research",
                          "Social environment",
                          "Opportunities to collaborate",
                          "Availability of funding",
                          "Benefits", # (health care, leave, etc.)",
                          "Number of publications",
                          "Teaching duties",
                          "Career pathway"))# guidance and advice"))


x1$Type <- ifelse(x1$Class == "Satisfied" | x1$Class == "Satisfactorily Advised" | x1$Class == "Low Spirit", 0, 1)
x1$Type <- as.factor(x1$Type)

# Fig.1a Plot
ggplot(x1, aes(x=X, y=Value, group=Class)) +
  labs(title="", x="Indicators of satisfaction", y="Level of satisfaction") +
  geom_line(aes(color=Class, linetype=Type), lwd=1.5)+ 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 60, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        text=element_text(size=12,  family="Arial", colour="black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(breaks=seq(0, 6, 1), limits=c(0, 6)) +
  theme(axis.text.y = element_text(size = 22), axis.title = element_text(size = 22), axis.text.x = element_text(size = 14))
