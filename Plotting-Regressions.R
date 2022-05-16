getwd()
setwd("C:/Users/anton/OneDrive/Dokument/R/thesis/scraper-for-thesis")

library(readr)
library(ggplot2)

df3 <- read_csv("df-for-plot2.csv")

drops <- c("Unnamed: 0")
df3 <- df3[ , !(names(df3) %in% drops)]


#####

t <- df3 %>% 
  dplyr::count(Topic_labels, Labels) %>%
  ggplot(aes(x = reorder(Topic_labels, n, sum), y = n, fill = Labels)) + 
  geom_col() +
  geom_text(aes(label = n), vjust = 0, size=7)+
  xlab("Topics") + ylab("Frequency count") +
  labs(fill = "Type of language: \n (Non-Toxic = FALSE, Toxic = TRUE) ")+
  coord_flip()+
  ggtitle("Count of toxic comments over topics") +
  theme_bw()

t = t + theme(axis.text=element_text(size=15),
              axis.title=element_text(size=15,face="bold"),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 15))


library(data.table)
dt <- setDT(df3)[,list(count = .N), by = .(Topic_labels,Labels)][,list(Labels = Labels, count = count,
                                                                       percent_fmt = paste0(formatC(count*100/sum(count), digits = 2), "%"),
                                                                       percent_num = count/sum(count)
), by = Topic_labels]

topicsF <- ggplot(dt, aes(x=Topic_labels, y=percent_num, fill = factor(Labels))) +
  geom_bar(position = position_fill(reverse=FALSE), stat="identity",  width=0.7) +
  xlab("Topics") + ylab("Proportion") +
  geom_text(aes(label = percent_fmt), size=7)+
  labs(fill = "Type of language: \n (Non-Toxic = FALSE, Toxic = TRUE) ") +
  coord_flip() +
  ggtitle("Proportion of toxic comments over topics") +
  theme_bw()

topicsF = topicsF + theme(axis.text=element_text(size=15),
                          axis.title=element_text(size=15,face="bold"),
                          legend.text = element_text(size = 15),
                          legend.title = element_text(size = 15))

library(ggpubr)

figureH <- ggarrange(t, topicsF,
                     ncol = 1, nrow = 2)

figureH

#####

Nt <- nrow(df3) #N of comments
Nps <- unique(df3$id)
Ntps <- subset(df3, Labels == TRUE)
Ntps <- unique(Ntps$id)
Ntps <- length(Ntps)
Np <- length(Nps) #N of posters
Nq <- sum(df3$quotedummy == 1) # N of quoted replies

MeanPpP <- round(mean(df3$recur)) #Mean number of comments per poster
MedianPpP <- round(median(df3$recur)) #Median number of comments per poster
SdPpP <- round(sd(df3$recur)) #Sd
MinPpP <- round(min(df3$recur)) #Min
MaxPpP <- round(max(df3$recur))#Max

MeanYear <- round(mean(df3$reg2)) #Mean registration year
MinYear <- round(min(df3$reg2)) #Min
MaxYear <- round(max(df3$reg2)) #Max

n_occur <- dim(df3[duplicated(df3$id),])[1]
Fin <- n_occur/Nt # Percent Posting >Once

NonT <- subset(df3, Labels == 0)
MeanComLen <- round(mean(NonT$n)) # Mean length of comment (words)
MedianComLen <- round(median(NonT$n)) #Median length of comment
SdComLen <- round(sd(NonT$n)) # Sd 
MinComLen <- round(min(NonT$n)) #Min
MaxComLen <- round(max(NonT$n)) #Max

Tx <- subset(df3, Labels == TRUE) 
MeanTCom <- round(mean(Tx$n))
MedianTCom <- round(median(Tx$n))
SdTCom <- round(sd(Tx$n))
MinTCom <- round(min(Tx$n))
MaxTCom <- round(max(Tx$n))

table = matrix(NA, nrow=9, ncol=6)
colnames(table) = c("Count", "Mean","Median", "Standard deviation", "Min", "Max")
rownames(table) = c("N of Comments", "N of Users", "N of Toxic Users", "N of Quoted replies", "Registration Year", "Comments per User", "Proportion Commenting > Once", "Non-Toxic Comment Length (words)", "Toxic Comment Length (words)")
table[1,1] = Nt
table[2,1] = Np
table[3,1] = Ntps
table[4,1] = Nq

table[5,2] = MeanYear
table[6,2] = MeanPpP
table[7,2] = Fin
table[8,2] = MeanComLen
table[9,2] = MeanTCom

table[6,3] = MedianPpP
table[8,3] = MedianComLen
table[9,3] = MedianTCom

table[6,4] = round(SdPpP)
table[8,4] = round(SdComLen)
table[9,4] = round(SdTCom)

table[5,5] = MinYear
table[6,5] = MinPpP
table[8,5] = MinComLen
table[9,5] = MinTCom

table[5,6] = MaxYear
table[6,6] = MaxPpP
table[8,6] = MaxComLen
table[9,6] = MaxTCom

library(kableExtra)

options(knitr.kable.NA = '')

table %>%
  kbl(digits=2, caption = "Descriptive statistics for Flashback discussion (4/7/2017 - 5/14/2017)") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#####

labs <- c("Labels")
labels_prop <- df3[labs]
labs_prop_by_bin <- aggregate(labels_prop, by = list(bin = df3$date),FUN=sum)
labs_prop_by_bin['cumuT'] <- cumsum(labs_prop_by_bin$Labels)
labs_prop_by_bin['p.per.d'] <- table(df3['date'])
labs_prop_by_bin['cumu.ppd'] <- cumsum(labs_prop_by_bin$p.per.d)
labs_prop_by_bin$avgT <- labs_prop_by_bin$Labels / labs_prop_by_bin$p.per.d
labs_prop_by_bin$SD_score = sd(labs_prop_by_bin$avgT)

ply <- function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 2)}

labs_prop_by_bin$r.avgT <- ply(labs_prop_by_bin$avgT)

labs_prop_by_bin$norm <- (labs_prop_by_bin$p.per.d - min(labs_prop_by_bin$p.per.d))/(max(labs_prop_by_bin$p.per.d) - min(labs_prop_by_bin$p.per.d))
labs_prop_by_bin$norm <- as.numeric(labs_prop_by_bin$norm)

df3$C <- 1
labs_prop_by_C <- aggregate(x = df3[c("C")], by = list(bin = df3$date),FUN=sum)

g <- ggplot(labs_prop_by_C, aes(x = bin, y = C)) + 
  geom_line(size = 2) + 
  xlab("Time") + ylab("Comments") +
  scale_x_date(breaks = labs_prop_by_C$bin[seq(1, length(labs_prop_by_C$bin), by = 2)])+
  ggtitle("Thread activity (4/7/2017 - 5/14/2017)")+
  theme_bw()

g + theme(axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          plot.title = element_text(size=22))

d <- ggplot(data = labs_prop_by_bin, aes(x=bin, y=avgT, label=bin, size = norm, weight = p.per.d)) + #Average toxicity
  geom_point()+
  scale_size("Comments per day", range=c(1, 15))+
  geom_text(size=3, aes(label = bin), hjust = 0.4,  vjust = -2)

d <- d + geom_smooth(method = "loess", span = 0.2, show.legend =FALSE) +
  xlab("Time") + ylab("Average toxicity") +
  ggtitle("Average Toxicity by daily bins (4/7/2017 - 5/14/2017)")+
  theme_bw()

d + theme(axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="bold"),
          plot.title = element_text(size=22),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20))

#####

df3['IncrDate2'] = df3$IncrDate**2
df3['IncrDate3'] = df3$IncrDate**3

df3$Topic1[df3$Topic1 == 0.00] <- 0.00001
df3$Topic2[df3$Topic2 == 0.00] <- 0.00001
df3$Topic3[df3$Topic3 == 0.00] <- 0.00001
df3$Topic4[df3$Topic4 == 0.00] <- 0.00001
df3$Topic5[df3$Topic5 == 0.00] <- 0.00001
df3$Topic6[df3$Topic6 == 0.00] <- 0.00001
df3$Topic7[df3$Topic7 == 0.00] <- 0.00001
df3$Topic8[df3$Topic8 == 0.00] <- 0.00001
df3$Topic9[df3$Topic9 == 0.00] <- 0.00001
df3$Topic10[df3$Topic10 == 0.00] <- 0.00001
df3$Topic11[df3$Topic11 == 0.00] <- 0.00001
df3$Topic12[df3$Topic12 == 0.00] <- 0.00001
df3$Topic13[df3$Topic13 == 0.00] <- 0.00001
df3$Topic14[df3$Topic14 == 0.00] <- 0.00001

df3['LogTopic1'] = log(df3$Topic1)
df3['LogTopic2'] = log(df3$Topic2)
df3['LogTopic3'] = log(df3$Topic3)
df3['LogTopic4'] = log(df3$Topic4)
df3['LogTopic5'] = log(df3$Topic5)
df3['LogTopic6'] = log(df3$Topic6)
df3['LogTopic7'] = log(df3$Topic7)
df3['LogTopic8'] = log(df3$Topic8)
df3['LogTopic9'] = log(df3$Topic9)
df3['LogTopic10'] = log(df3$Topic10)
df3['LogTopic11'] = log(df3$Topic11)
df3['LogTopic12'] = log(df3$Topic12)
df3['LogTopic13'] = log(df3$Topic13)
df3['LogTopic14'] = log(df3$Topic14)

library(ggcorrplot)
library(corrplot)
library(GGally)


myvars <- c("IncrDate", "UserGroup", "n", "recur", "quotedummy", "reg2inc","Topic1", "Topic3", "Topic4", "Topic5", "Topic6",
            "Topic7", "Topic8", "Topic9", "Topic10", "Topic11", "Topic13","Topic14")
sub <- df3[myvars]
sub <- na.omit(sub)

model.matrix(~0+., data=sub) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=4)

#####

df3 <- na.omit(df3)
min(df3$Freq)


df3$UserGroup <- cut(df3$Freq,
                     breaks=c(0, 100, 500, 2000, 10000, 72289),
                     labels=c('Very low previous activity', 'Low previous activity', 'Moderate previous activity', 'High previous activity',
                              'Very high previous activity'))

df3$UserGroup <- factor(df3$UserGroup)
table(df3$UserGroup)
table(df3$UserGroup, df3$Labels)

avgToxic <- dplyr::summarise(group_by(df3, id), totalPosts = n(),
                             score = sum(Labels)/totalPosts)

df3 <- merge(df3, avgToxic, by = "id")

df3 <- df3[order(df3$post.nr), ]


d <- ggplot(df3) +
  geom_bin2d(aes(x=date, y=log(Freq), color=UserGroup), drop=TRUE, bins = 35)+
  geom_smooth(aes(x=date, y=log(Freq)),method="loess",span = 2)+
  scale_fill_continuous(type = "viridis") +
  xlab("Time") + ylab("Log transformed N of previous comments") +
  labs(fill = "Count of users") +
  ggtitle("Thread activity of users by their natural log of previous participation on Flashback (4/7/2017 - 5/14/2017)")+
  scale_x_date(breaks = df3$date[seq(1, length(df3$date), by = 2)])+
  theme_bw()

d + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        plot.title = element_text(size=22),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

#####

j <- ggplot(df3) +
  geom_point(aes(x=log(Freq), y=score, color=UserGroup), size = 3)+
  geom_smooth(aes(x=log(Freq), y=score), method="loess", span=2)+
  xlab("Log transformed N of previous comments") + ylab("Average toxicity of user") +
  ggtitle("Average toxicity of users by their natural log of previous participation on Flashback (4/7/2017 - 5/14/2017)")+
  coord_trans()+
  theme_bw()


j + theme(axis.text.x = element_text(size=20)) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        plot.title = element_text(size=22),
        legend.text = element_text(size = 20, face="bold"),
        legend.title = element_text(size = 20, face="bold"))

df3$UserGroup <- relevel(df3$UserGroup, ref='High previous activity')

#####
library(ggbeeswarm)
library(broom)
library(tidyverse)

m <- glm(Labels ~ IncrDate + IncrDate2 + UserGroup + UserGroup:IncrDate + UserGroup:(IncrDate2) , family=binomial(link="logit"),
         na.action = na.exclude, data = df3)
m$rank

length(m$coefficients)


modC <- glm(Labels ~ log(n) + log(recur) + reg2inc + quotedummy + LogTopic1 + LogTopic3 + LogTopic4 + LogTopic5 + LogTopic6 + LogTopic7
            + LogTopic8 + LogTopic9 + LogTopic10 + LogTopic11 + LogTopic13 + LogTopic14 + IncrDate + IncrDate2 + UserGroup + UserGroup:IncrDate + UserGroup:(IncrDate2), family=binomial(link="logit"),
            na.action = na.exclude, data = df3)

exp(coef(modC))

exp(confint(modC))

plot(modC, which = 4, id.n = 3)

model.data <- augment(modC, newdata=df3) %>% 
  mutate(index = 1:n())

standard_res <- rstandard(modC)

model.data <- cbind(model.data, standard_res)

st <- ggplot(model.data, aes(index, standard_res)) + 
  geom_point(aes(color = Labels), alpha = .5) +
  ggtitle("Standard residual error (above 3 represent possible outliers)")+
  theme_bw()

st + theme(axis.text=element_text(size=20),
           axis.title=element_text(size=22,face="bold"),
           plot.title = element_text(size=22))

mdf <- model.data %>% 
  filter(abs(standard_res) < 3)

modCn <- glm(Labels ~ log(n) + log(recur) + reg2inc + quotedummy + LogTopic1 + LogTopic3 + LogTopic4 + LogTopic5 + LogTopic6 + LogTopic7
             + LogTopic8 + LogTopic9 + LogTopic10 + LogTopic11 + LogTopic13 + LogTopic14 + IncrDate + IncrDate2 + UserGroup + UserGroup:IncrDate + UserGroup:I(IncrDate2), family=binomial(link="logit"),
             na.action = na.exclude, data = mdf)

plot(modCn, which = 4, id.n = 3)

model.data2 <- augment(modCn, newdata=mdf) %>% 
  mutate(index = 1:n())

standard_res2 <- rstandard(modCn)

model.data2 <- cbind(model.data2, standard_res2)

st2 <- ggplot(model.data2, aes(index, standard_res2)) + 
  geom_point(aes(color = Labels), alpha = .5) +
  ggtitle("Standard residual error (above 3 represent possible outliers)")+
  theme_bw()

st2 + theme(axis.text=element_text(size=20),
            axis.title=element_text(size=22,face="bold"),
            plot.title = element_text(size=22))


proba <- predict(modC, type="response")
logit <- log(proba/(1-proba))

p1 <- ggplot(df3, aes(logit, log(n)))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p2 <- ggplot(df3, aes(logit, log(recur)))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p4 <- ggplot(df3, aes(logit, IncrDate2))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p7 <- ggplot(df3, aes(logit, LogTopic3))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p8 <- ggplot(df3, aes(logit, LogTopic4))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p9 <- ggplot(df3, aes(logit, LogTopic5))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p10 <- ggplot(df3, aes(logit, LogTopic6))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()


p12 <- ggplot(df3, aes(logit, LogTopic8))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()


p14 <- ggplot(df3, aes(logit, LogTopic10))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p15 <- ggplot(df3, aes(logit, LogTopic11))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p16 <- ggplot(df3, aes(logit, LogTopic13))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

p17 <- ggplot(df3, aes(logit, LogTopic14))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()

figureP <- ggarrange(p1, p2, p4, p7, p8
                     , p9, p10, p12, p14, p15, p16, p17,
                     ncol = 4, nrow = 4)

figureP






