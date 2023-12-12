# www.isacajournal-digital.org/isacajournal/2019_volume_3/MobilePagedArticle.action?articleId=1485390#articleId1485390
# Figure 3—R Code to Plot the Bar Chart in Figure 2 
library(ggplot2) 
 
n <- 1000000            #iterations 
lp <- c(0, 0.3)       #low probability range
li <- c(0, 10000)     #low impact range
mp <- c(0.3, 0.7)     #medium probability range   
mi <- c(10000, 20000) #medium impact range
hp <- c(0.7, 1.0)     #high probability range   
hi <- c(20000,30000)  #high impact range

slp = runif(n, min(lp), max(lp))   #random numbers in low probability range 
sli = runif(n, min(li), max(li))   #random numbers in low impact range
smp = runif(n, min(mp), max(mp))   #random numbers in med probability range 
smi = runif(n, min(mi), max(mi))   #random numbers in med impact range
shp = runif(n, min(hp), max(hp))   #random numbers in high probability range 
shi = runif(n, min(hi), max(hi))   #random numbers in highimpact range

lr = c(slp*sli, slp*smi, smp*sli)  #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(smp*smi, slp*shi, shp*sli)  #med risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(shp*shi, smp*shi, shp*smi)  #high risks defined as red region in risk matrix and its possible variants, R=P*I

lr1 = c(slp*sli)                   #low risks with rating 1 (low P x low I) marked as green region in risk matrix, R=P*I
lr2 = c(slp*smi, smp*sli)          #low risks with rating 2 (low P x med I and med P x low I) marked as green region in risk matrix, R=P*I
mr3 = c(slp*shi, shp*sli)          #med risks with rating 3 (low P x high I and high P x low I) marked as yellow region in risk matrix, R=P*I
mr4 = c(smp*smi)                   #med risks with rating 4 (med P x med I) marked as yellow region in risk matrix, R=P*I
hr6 = c(smp*shi, shp*smi)          #high risks with rating 6 (med P x high I and high P x med I) marked as red region in risk matrix, R=P*I
hr9 = c(shp*shi)                   #high risks with rating 9 (high P x high I) marked as red region in risk matrix, R=P*I


dlr = data.frame(x=lr)             #data frame with low risks and label "Low"
dlr$risk = "Low"

dmr = data.frame(x=mr)             #data frame with medium risks and label "Med"  
dmr$risk = "Med"

dhr = data.frame(x=hr)             #data frame with high risks and label "High"  
dhr$risk = "High"

df = rbind(dlr, dmr, dhr)          #bind all data frames into one dataset


dlr1 = data.frame(x=lr1)           #data frame with low risks and label "R1" (rating 1)
dlr1$risk = "R1"

dlr2 = data.frame(x=lr2)           #data frame with low risks and label "R2" (rating 2)
dlr2$risk = "R2"

dmr3 = data.frame(x=mr3)           #data frame with medium risks and label "R3" (rating 3)  
dmr3$risk = "R3"

dmr4 = data.frame(x=mr4)           #data frame with medium risks and label "R4" (rating 4)  
dmr4$risk = "R4"

dhr6 = data.frame(x=hr6)           #data frame with high risks and label "R6" (rating 6)  
dhr6$risk = "R6"

dhr9 = data.frame(x=hr9)           #data frame with high risks and label "R6" (rating 9)  
dhr9$risk = "R9"

dfr = rbind(dlr1, dlr2, dmr3, dmr4, dhr6, dhr9)  #bind all data frames into one rating dataset


# Plotting using ggplot2 with color marks

ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=risk),
colour="white", binwidth = 200)+
scale_fill_manual(values = c("#CC0033","#33FF00", "#FFFF33")) +
  xlab(label = "Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk Matrix Colors")

# Plotting using ggplot2 with risk ratings
ggplot(dfr, aes(x = x, fill = risk)) + 
  geom_histogram(binwidth = 200, colour = "white") +
  scale_fill_manual(values = c("R1" = "#90EE90", 
                               "R2" = "#33FF00", 
                               "R3" = "#FFFF33", 
                               "R4" = "#FFBF00", 
                               "R6" = "#FF6666", 
                               "R9" = "#CC0033")) +
  xlab("Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk Colored Ratings")

library(dplyr)


# Create a dataset for the cumulative distribution
cdf_data <- df %>%
  arrange(x) %>%
  mutate(cumulative = cumsum(..count..)/sum(..count..)) %>%
  group_by(x) %>%
  summarise(cumulative = max(cumulative))

# Plotting histogram with CDF
ggplot(df, aes(x = x, y = ..count..)) +
  geom_histogram(aes(y = ..count..), binwidth = 1000, fill = "skyblue", colour = "white") +
  geom_line(data = cdf_data, aes(y = cumulative), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "CDF")) +
  xlab("Yc") +
  ylab("Count") +
  ggtitle("Histogram with Cumulative Distribution Function")

