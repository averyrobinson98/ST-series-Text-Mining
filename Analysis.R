################################
################################
################################ Chi Square Test

chTest <- Chars %>% group_by(Episode) %>% summarize(charsIn =sum(chars %in% MainChars2)) %>% data.frame()
chTest$charsOut <- Chars %>% group_by(Episode) %>% summarize(charsOut = sum(!chars %in% MainChars2)) %>% select(charsOut) %>% unlist %>% unname

chTest <- chTest[,-1]
chisq.test(chTest)
chTest1 <- chTest[-15,]
chisq.test(chTest1)


chTest$Season <-factor(EpsbySeason$Season)
chTest2 <- chTest %>%group_by(Season) %>% summarize(AvgIn = mean(charsIn))
chTest2$AvgOut <- chTest %>% group_by(Season) %>% summarize(AvgOut = mean(charsOut)) %>% select(AvgOut) %>% unlist %>% unname
chTest2 <- chTest2[,-1]
chisq.test(chTest2)

chTest$Thirds <- factor(c(rep(1,3),rep(2,3),rep(3,2), rep(1,3),rep(2,3),rep(3,3), rep(1,3),rep(2,3),rep(3,2)))
chTest3 <- chTest %>% group_by(Thirds) %>% summarize(AvgIn = mean(charsIn))
chTest3$AvgOut <- chTest %>% group_by(Thirds) %>% summarize(AvgOut = mean(charsOut)) %>% select(AvgOut) %>% unlist %>% unname
chTest3 <- chTest3[,-1]
chisq.test(chTest3)


################################
################################
################################ Chi Square Test for proportion of speaking per Season

charwords <- data.frame(S1=season1,S2=season2,S3=season3)
chisq.test(charwords)


################################
################################
################################  LR  for predicting IMDB score by percent of main chars speaking

# With the outlier episode

percByMain.lm <- lm(Rating~percent,data=LinesByMainChars)
#Significant at the 0 level, 5.13e-05

# Without the outlier episode

percByMain2.lm<- lm(Rating~percent,data=LinesByMainChars[-15,])
# Not significant


################################
################################
################################  LR for predicting IMDB score by lines per ep

# With the outlier episode

temp <- LinesPerEp
temp[is.na(temp)] <- 0
temp$Rating <- ratings$Ratings
lines.lm <- lm(Rating~Joyce+Mike+Hopper+Nancy+Dustin+Lucas+Jonathan+Steve+Will+Eleven ,data=temp)
#Dustin and Joyce are significant at the 0.05 level, Mike at the 0.1 level, but the F stat is only significant at a .10 level, so the 



################################
################################
################################  LR for predicting IMDB score by words per ep

# With the outlier episode

temp <- WordsPerEp
temp[is.na(temp)] <- 0
temp$Rating <- ratings$Ratings
word.lm <- lm(Rating~ Joyce+Mike+Hopper+Nancy+Dustin+Lucas+Jonathan+Steve+Will+Eleven,data=temp)
# The F stat is significant at a 0.05 alpha level.Dustin and Joyce are significant at a 0.001 and 0.05 level, respectively.

# Without the outlier episode
word2.lm <- lm(Rating~ Joyce+Mike+Hopper+Nancy+Dustin+Lucas+Jonathan+Steve+Will+Eleven,data=temp[-15,])
# F stat is extremeley not significant
# R^2 dropped from 0.6669 to 0.3566



################################
################################
################################  LR for predicting IMDB score by sentiment in each ep
ComWords1 <- tidy_all 
nrcSents <- get_sentiments("nrc")
ComWords1 <- ComWords1 %>% inner_join(nrcSents,by="word")

ComWords1 <- ComWords1 %>% count(Episode,sentiment) %>% filter(!sentiment %in% c("negative","positive"))
ComWords1$EpTot <- rep(ComWords1 %>% group_by(Episode) %>% summarize(epTot=sum(n)) %>% select(epTot) %>% unlist %>% unname,each=8)
ComWords1 <- ComWords1 %>% group_by(Episode,sentiment) %>% summarize(perc=n/EpTot)
temp <- data.frame(ComWords1)
ratings$Epnum <- factor(1:25)
temp <- temp %>% left_join(ratings[,c(2,4)], by=c("Episode"="Epnum"))

sent.lm <- lm(Ratings~sentiment,data=temp)


################################
################################
################################  LR for predicting IMDB score by episode placement in season

# With the outlier ep

temp <- data.frame(Rating=ratings$Ratings, Ep=c(1:8,1:9,1:8))
epPlace.lm <- lm(Rating~Ep,data=temp)
# slightly significant

# Without the outlier ep
epPlace2.lm <- lm(Rating~Ep,data=temp[-15,])
#Extremely significant



################################
################################
################################ General Episode Stat Analysis With Anova and post hoc analysis


GenEpStats2 <- GenEpStats %>% select(!Episode) %>% pivot_longer(!Season)
GenEpStats2$name <- factor(GenEpStats2$name)

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="numberOfLines") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
#Season 1 and Season 3 have significantly different number of Lines, Season 2 and Season 3 are partially significant

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="AvgLineLength") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
# At an alpha level of .10, Season 3 and Season 2 have significantly different Average Line Length

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="numberOfWords") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
#Season 3 and Season 2 are partically significantly different in number of words per episode

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="Rating") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
#The ratings are not statistically significantly different

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="NumChars") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
#The number of characters are not statistically significantly different

GenEpStats2Temp <- GenEpStats2 %>% filter(name=="LinelengthSD") 

aov.results <- aov(value~Season,data=GenEpStats2Temp)
TukeyHSD(aov.results)
#Variation in line length is not statistically significantly different



