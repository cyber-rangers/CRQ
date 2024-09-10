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

lr = c(slp*sli, slp*smi, smp*sli)                                               #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(slp*ssi, slp*shi, ssp*sli, shp*sli, smp*smi)                             #med risks defined as yellow region in risk matrix and its possible variants, R=P*I
sr = c(smp*ssi, smp*shi, ssp*smi, shp*smi, ssp*ssi)                             #significant risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(ssp*shi, shp*ssi, shp*shi)                                               #high risks defined as red region in risk matrix and its possible variants, R=P*I

lr1 = c(slp*sli)                   #low risks with rating 1 marked as green region in risk matrix, R=P*I
lr2 = c(slp*smi, smp*sli)          #low risks with rating 2 marked as green region in risk matrix, R=P*I
mr3 = c(slp*ssi, ssp*sli)          #med risks with rating 3 marked as green region in risk matrix, R=P*I
mr4 = c(slp*shi, shp*sli, smp*smi) #med risks with rating 4 marked as green region in risk matrix, R=P*I
sr6 = c(ssp*smi, smp*ssi)          #significant risks with rating 6 marked as amber region in risk matrix, R=P*I
sr8 = c(shp*smi, smp*shi)          #significant risks with rating 8 marked as amber region in risk matrix, R=P*I
sr9 = c(ssp*ssi)                   #significant risks with rating 9 marked as amber region in risk matrix, R=P*I
hr12 = c(ssp*shi, shp*ssi)         #high risks with rating 12 marked as red region in risk matrix, R=P*I
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

dmr3 = data.frame(x=mr3)           #data frame with low risks and label "R3" (rating 3)  
dmr3$r_rating = "R3"

dmr4 = data.frame(x=mr4)           #data frame with low risks and label "R4" (rating 4)  
dmr4$r_rating = "R4"

dsr6 = data.frame(x=sr6)           #data frame with medium risks and label "R6" (rating 6)  
dsr6$r_rating = "R6"

dsr8 = data.frame(x=sr8)           #data frame with medium risks and label "R8" (rating 9)  
dsr8$r_rating = "R8"

dsr9 = data.frame(x=sr9)           #data frame with significant risks and label "R9" (rating 9)  
dsr9$r_rating = "R9"

dhr12 = data.frame(x=hr12)         #data frame with significant risks and label "R12" (rating 12)  
dhr12$r_rating = "R12"

dhr16 = data.frame(x=hr16)           #data frame with high risks and label "R16" (rating 16)  
dhr16$r_rating = "R16"

dfr = rbind(dlr1, dlr2, dmr3, dmr4, dsr6, dsr8, dsr9, dhr12, dhr16)  #bind all data frames into one rating dataset


# Plotting using ggplot2 with color marks

ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=risk),
colour="white", binwidth = 1000)+
scale_fill_manual(values = c("#CC0033","#33FF00","#FFFF33","#FFBF00")) +
  xlab(label = "Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk 4x4 Matrix Colors")

# Plotting using ggplot2 with risk ratings
ggplot(dfr, aes(x = x, fill = r_rating)) + 
  geom_histogram(binwidth = 1000, colour = "white") +
  scale_fill_manual(values = c("R1" = "#90EE90", 
                               "R2" = "#33FF00", 
                               "R3" = "#FFFACD", 
                               "R4" = "#FFFF33",
                               "R6" = "#FFBF00",
                               "R8" = "#FF8C00",
                               "R9" = "#C46210",
                               "R12" = "#FF6666", 
                               "R16" = "#CC0033")) +
  xlab("Yc") +
  ylab("Count") +
  ggtitle("Histogram of Cyber Risk Colored Ratings")

