library(tidytext)
library(stringi)
library(tidyr)
# Main Chars
MainChars <-c("Joyce","Mike","Hopper","Nancy","Dustin","Lucas","Jonathan","Steve","Will","Eleven")


# Clean through the scripts
cleanlines <- function(set){
                index <- str_which(set,":")
                lines <- set[index]
                lines <- str_remove_all(lines,"\\(.*\\)")
                lines <- str_remove_all(lines,"\\[.*\\]")
                lines <- str_remove_all(lines,"\\\n")
                lines <- gsub(".*(?=(\\b\\w+:))","",lines,perl=T)
                lines <- gsub("\\.\\.\\.","",lines)
                lines <- gsub("\\s{2,}"," ",lines) %>% trimws()
                return(lines)
}

######################
######################
# AllLines : Gather All Lines from every character, one line = one observation

AllLines <- NULL
temp <- data.frame("Episode"=NA,"Lines"=NA)
for(i in 1:length(scripts)){
                temp <- cleanlines(scripts[[i]])
                temp <- data.frame("Episode"=rep(factor(i),length(temp)),"Lines"=temp)
                AllLines <- bind_rows(AllLines,temp)
}


######################
######################
# Charlines : Gather the lines from the main characters

Charlines <- vector("list",length=length(MainChars))
names(Charlines) <- MainChars

#Have each component of the inner list be named the list of episodes #epNamesSimp
for(i in 1:length(MainChars)){
                Charlines[[i]] <- vector("list",length(epNamesSimp))
                names(Charlines[[i]]) <- epNamesSimp
                
}

#Get Lines for all Main Chars
for(j in 1:length(MainChars)){
                name <- MainChars[j]
                for(i in 1:length(epNamesSimp)){
                                Dialogue <- cleanlines(scripts[[i]])   
                                sub <- str_subset(Dialogue,paste0(name,":"))
                                Charlines[[j]][i] <- list(sub)
                                
                }
}

######################
######################
# tidy_all : Tidy format for All Lines, every single character

temp <- AllLines
temp$Lines <- str_remove_all(temp$Lines,"\\w*(?=:)")
tidy_all <- temp %>% unnest_tokens(word,Lines) %>% anti_join(stop_words)
temp <- NULL


######################
######################
#  Boundaries for graphs
SeasonCuts <- data.frame(lab=c("Season 1 ","Season 2","Season 3"), values=c(8,17,25),stringsAsFactors = F)
EpsbySeason <- data.frame(Episode=factor(1:25), Season=factor(c(rep(1,8),rep(2,9),rep(3,8))), epName=epNamesSimp)

######################
######################
# Tidy_Chars : Tidy format for the lines from the ten main characters

tidy_Chars <- data.frame(.=NA, Episode=factor(NA), Season=factor(NA),Char=factor(NA))
for(i in 1:10){
                temp <- Charlines[[i]]
                temp <- temp %>% unlist %>% data.frame()
                temp$Ep <- rownames(temp)
                temp$Ep <- str_remove_all(temp$Ep,"[:digit:]")
                temp <- temp %>%left_join(EpsbySeason, by=c("Ep"="epName")) %>% select(-Ep)
                temp$Char <- factor(MainChars[i])
                tidy_Chars <- bind_rows(tidy_Chars,temp)
}
tidy_Chars <- tidy_Chars[-1,]

tidy_Chars$. <- str_remove_all(tidy_Chars$.,"\\w*(?=:)")
names(tidy_Chars) <- c("Lines","Episode","Season","Character")
tidy_Chars <- tidy_Chars %>% unnest_tokens(word,Lines)


######################
######################
# General Episode Data

#1
LinesPerEp <- matrix(,nrow=25,ncol=10)

for(i in 1:length(MainChars)){
                temp <- Charlines[[i]]
                temp <- sapply(temp,length) %>% unname
                temp[temp==0] <- NA
                LinesPerEp[,i] <- temp
}
LinesPerEp <- data.frame(LinesPerEp)
colnames(LinesPerEp) <- MainChars
rownames(LinesPerEp) <- epNamesSimp
LinesPerEp$Season <- factor(EpsbySeason$Season)

#2
WordsPerEp <- matrix(,nrow=25,ncol=10)
for(i in 1:length(MainChars)){
                temp <- Charlines[[i]]
                for(j in 1:length(temp)){
                                temp2 <- temp[[j]]
                                if(identical(temp2,character(0))){
                                                WordsPerEp[j,i] <- NA
                                                
                                }
                                else{
                                                temp2 <- (sapply(strsplit(temp2," "),length)- 1) %>% sum
                                                WordsPerEp[j,i] <- temp2  
                                }
                }
}
WordsPerEp <- data.frame(WordsPerEp)
colnames(WordsPerEp) <- MainChars
rownames(WordsPerEp) <- epNamesSimp

#3
LettersPerEp <- matrix(,nrow=25,ncol=10)

for(i in 1:length(MainChars)){
                temp <- Charlines[[i]]
                for(j in 1:length(temp)){
                                temp2 <- temp[[j]]
                                if(identical(temp2,character(0))){
                                                LettersPerEp[j,i] <- NA
                                }
                                else{
                                                temp2 <- str_remove_all(temp2,"\\w*(?=:)") %>% str_remove_all(.,"[:punct:]") %>% str_remove_all(.,"[:blank:]")
                                                
                                                LettersPerEp[j,i] <- nchar(temp2) %>% sum()
                                }
                                
                                
                                
                }
}
LettersPerEp <- data.frame(LettersPerEp)
colnames(LettersPerEp) <- MainChars
rownames(LettersPerEp) <- epNamesSimp

#4
WordLengthSD <- NULL
for(i in 1:length(MainChars)){
                temp <- Charlines[[i]] %>% unlist() 
                temp <- str_remove_all(temp,"\\w*(?=:)") %>% str_remove_all(.,"[:punct:]")
                temp <- str_split(temp,"\\s+") %>% unlist
                temp <- temp[temp !=""]
                WordLengthSD <- append(WordLengthSD,nchar(temp) %>% sd)
}

names(WordLengthSD) <- MainChars


######################
######################
# General Stats Per Episode

GenEpStats <-AllLines %>% group_by(Episode) %>% summarize(numberOfLines = n())
temp <-AllLines %>% unnest_tokens(word,Lines) %>% group_by(Episode) %>% summarize(numWords=n()) %>% data.frame() %>% select(numWords)
GenEpStats$numberOfWords <- temp[,1]
GenEpStats <- GenEpStats %>% mutate(AvgLineLength = numberOfWords/numberOfLines)
AllLines$LineNum <- 1:dim(AllLines)[1]

temp <- AllLines %>% unnest_tokens(word,Lines) %>% group_by(Episode,LineNum) %>% summarize(wordInLine=n())
temp <- temp %>% group_by(Episode) %>% summarize(lineChange=sd(wordInLine)) %>% data.frame() %>% select(lineChange)
GenEpStats$LinelengthSD <- temp[,1]
GenEpStats$Rating <- ratings$Ratings

#Number of Characters in each ep
temp <- NULL
GenEpStats$NumChars <- 0
for(i in 1:length(scripts)){
                temp <- str_extract_all(scripts[[i]],pattern="\\w*(?=:)") %>% unlist() %>% stri_omit_empty() %>% unique() %>% length()
                GenEpStats$NumChars[i] <- temp
}

GenEpStats$Season <- factor(EpsbySeason$Season)
GenEpStats$Episode <- factor(epNamesSimp)


######################
######################
# General Stats per Season

SeasonStats <- data.frame(Season=factor(1:3))


#Find Average Line Lengths 
SeasonStats$`Average Line Length` <- GenEpStats %>% select(AvgLineLength,Season) %>% group_by(Season) %>% summarize("Average Line Length"=mean(AvgLineLength)) %>% select("Average Line Length") %>% unlist %>% unname
#Find Average Variations
SeasonStats$`Average Line Length Variation` <- GenEpStats %>% select(LinelengthSD,Season) %>% group_by(Season) %>% summarize("Average SD"=mean(LinelengthSD)) %>% select("Average SD") %>% unlist %>% unname

#Find Average Ratings
SeasonStats$`Average Rating` <- GenEpStats %>% select(Rating,Season) %>% group_by(Season) %>% summarize("Average Rating"=mean(Rating)) %>% select("Average Rating") %>% unlist %>% unname

#Find Average Words per Ep Averages
SeasonStats$`Average Number of Words Per Episode` <- GenEpStats %>% select(numberOfWords,Season) %>% group_by(Season) %>% summarize("Average #Words"=mean(numberOfWords)) %>% select("Average #Words") %>% unlist %>% unname

#Find Number of Lines Averages
SeasonStats$`Number of Lines` <- GenEpStats %>% select(numberOfLines,Season) %>% group_by(Season) %>% summarize("Total Lines"=sum(numberOfLines)) %>% select("Total Lines") %>% unlist %>% unname

#Find Number of Speakers  Averages
temp <- NULL
temp2<- data.frame("Char"=NA,"Ep"=NA)
for(i in 1:length(scripts)){
                temp <- str_extract_all(scripts[[i]],pattern="\\w*(?=:)") %>% unlist() %>% stri_omit_empty() %>% unique()
                temp <- data.frame("Char"=temp,"Ep"=factor(rep(i,length(temp))))
                temp2 <- bind_rows(temp2,temp)
}
temp2 <- temp2 %>% left_join(EpsbySeason,by=c("Ep"="Episode"))  
temp2 <- temp2 %>% select(!Ep) %>% distinct() 
temp2 <-temp2 %>% count(Season)
SeasonStats$`Number of Speakers` <- temp2[-4,] %>% select(n) %>% unlist %>% unname




######################
######################
# More General Stats per Character
UniqueWords <- tidy_Chars %>% group_by(Character) %>% select(word,Character) %>% summarize(unq=unique(word)) %>% count(Character,sort=T) 
UniqueWords <- data.frame(UniqueWords)


Char_Stats <- data.frame(Characters = MainChars)
Char_Stats$TotalLines <- LinesPerEp[-15,-11] %>% colSums(na.rm=T) 
Char_Stats$AvgLinePerEp <- LinesPerEp[-15,-11] %>% colMeans(na.rm=T)
Char_Stats$AvgLineLength <- (WordsPerEp[-15,-11]/LinesPerEp[-15,-11]) %>% colMeans(na.rm=T)
Char_Stats$AvgWordLength <- (LettersPerEp[-15,-11]/WordsPerEp[-15,-11]) %>% colMeans(na.rm=T)
Char_Stats$Vocab <- UniqueWords$n

Char_Stats <- Char_Stats %>% pivot_longer(!Characters)
names(Char_Stats) <- c("Characters","Category","Value")
Char_Stats$Category <- factor(Char_Stats$Category)
Char_Stats$Characters <- factor(Char_Stats$Characters)



forGraph <- data.frame(LinesPerEp)
forGraph$Season <- factor(EpsbySeason$Season)
forGraph <- forGraph %>% pivot_longer(!Season)

forGraph <- forGraph %>% group_by(name,Season) %>% summarize(TotLines=sum(value,na.rm=T)) %>% data.frame()




