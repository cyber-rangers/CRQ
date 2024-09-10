# www.isacajournal-digital.org/isacajournal/2019_volume_3/MobilePagedArticle.action?articleId=1485390#articleId1485390
# Figure 3—R Code to Plot the Bar Chart in Figure 2 
library(ggplot2) 
 
n <- 1000000          #iterations  
lp <- c(0, 0.25)      #low probability range
li <- c(0, 7500)      #low impact range
mp <- c(0.25, 0.5)    #medium probability range   
mi <- c(7500, 15000)  #medium impact range
sp <- c(0.5, 0.75)    #significant probability range   
si <- c(15000, 22500)  #significant impact range
hp <- c(0.7, 1.0)     #high probability range   
hi <- c(22500,30000)  #high impact range

slp = runif(n, min(lp), max(lp))   #random numbers in low probability range 
sli = runif(n, min(li), max(li))   #random numbers in low impact range
smp = runif(n, min(mp), max(mp))   #random numbers in med probability range 
smi = runif(n, min(mi), max(mi))   #random numbers in med impact range
ssp = runif(n, min(sp), max(sp))   #random numbers in sign probability range 
ssi = runif(n, min(si), max(si))   #random numbers in sign impact range
shp = runif(n, min(hp), max(hp))   #random numbers in high probability range 
shi = runif(n, min(hi), max(hi))   #random numbers in high impact range

lr = c(slp*sli, slp*smi, slp*ssi, slp*shi, smp*sli, ssp*sli, shp*sli)  #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(smp*smi, smp*ssi, smp*shi, ssp*smi, shp*smi)  #med risks defined as yellow region in risk matrix and its possible variants, R=P*I
sr = c(ssp*ssi, ssp*shi, shp*ssi)  #significant risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(shp*shi)                    #high risks defined as red region in risk matrix and its possible variants, R=P*I

lr1 = c(slp*sli)                   #low risks with rating 1 marked as green region in risk matrix, R=P*I
lr2 = c(slp*smi, smp*sli)          #low risks with rating 2 marked as green region in risk matrix, R=P*I
lr3 = c(slp*ssi, ssp*sli)          #low risks with rating 3 marked as yellow region in risk matrix, R=P*I
lr4 = c(slp*shi, shp*sli)          #low risks with rating 4 marked as yellow region in risk matrix, R=P*I
mr4 = c(smp*smi)                   #med risks with rating 2 marked as green region in risk matrix, R=P*I
mr6 = c(ssp*smi, smp*ssi)          #med risks with rating 6 marked as amber region in risk matrix, R=P*I
mr8 = c(shp*smi, smp*shi)          #med risks with rating 8 marked as amber region in risk matrix, R=P*I
sr9 = c(ssp*ssi)                   #significant risks with rating 9 marked as amber region in risk matrix, R=P*I
sr12 = c(ssp*shi, shp*ssi)         #significant risks with rating 12 marked as red region in risk matrix, R=P*I
hr16 = c(shp*shi)                  #high risks with rating 16 marked as red region in risk matrix, R=P*I

dlr = data.frame(x=lr)             #data frame with low risks and label "Low"
dlr$risk = "Low"

dmr = data.frame(x=mr)             #data frame with medium risks and label "Med"  
dmr$risk = "Med"

dsr = data.frame(x=sr)             #data frame with medium risks and label "Med"  
dsr$risk = "Sig"

dhr = data.frame(x=hr)             #data frame with high risks and label "High"  
dhr$risk = "High"

df = rbind(dlr, dmr, dsr, dhr)      #bind all 4 data frames into one dataset


dlr1 = data.frame(x=lr1)           #data frame with low risks and label "R1" (rating 1)
dlr1$r_rating = "R1"

dlr2 = data.frame(x=lr2)           #data frame with low risks and label "R2" (rating 2)
dlr2$r_rating = "R2"

dlr3 = data.frame(x=lr3)           #data frame with low risks and label "R3" (rating 3)  
dlr3$r_rating = "R3"

dlr4 = data.frame(x=lr4)           #data frame with low risks and label "R4" (rating 4)  
dlr4$r_rating = "R4L"

dmr4 = data.frame(x=mr4)           #data frame with medium risks and label "R4" (rating 4)  
dmr4$r_rating = "R4M"

dmr6 = data.frame(x=mr6)           #data frame with medium risks and label "R6" (rating 6)  
dmr6$r_rating = "R6"

dmr8 = data.frame(x=mr8)           #data frame with medium risks and label "R8" (rating 8)  
dmr8$r_rating = "R8"

dsr9 = data.frame(x=sr9)           #data frame with significant risks and label "R9" (rating 9)  
dsr9$r_rating = "R9"

dsr12 = data.frame(x=sr12)         #data frame with significant risks and label "R12" (rating 12)  
dsr12$r_rating = "R12"

dhr16 = data.frame(x=hr16)           #data frame with high risks and label "R16" (rating 16)  
dhr16$r_rating = "R16"

dfr = rbind(dlr1, dlr2, dlr3, dlr4, dmr4, dmr6, dmr8, dsr9, dsr12, dhr16)  #bind all data frames into one rating dataset


# Plotting using ggplot2 with color marks

ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=risk),
colour="white", binwidth = 1000)+
scale_fill_manual(values = c("#CC0033","#33FF00","#FFFF33","#FFBF00")) +
  xlab(label = "Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk 4x4 Weakly Consistent Matrix 2x2 Yellow Colors")

# Plotting using ggplot2 with risk ratings
ggplot(dfr, aes(x = x, fill = r_rating)) + 
  geom_histogram(binwidth = 1000, colour = "white") +
  scale_fill_manual(values = c("R1" = "#98FB98", 
                               "R2" = "#32CD32", 
                               "R3" = "#228B22", 
                               "R4L" = "#006400",
                               "R4M" = "#33FF00",
                               "R6" = "#FFFACD",
                               "R8" = "#FFFF33",
                               "R9" = "#FFBF00",
                               "R12" = "#FF8C00", 
                               "R16" = "#CC0033")) +
  xlab("Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk Weakly Consistent Colored Ratings 2x2 Yellow")

