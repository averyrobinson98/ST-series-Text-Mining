# Load libraries
library(dplyr)
library(rvest)
library(xml2)
library(stringr)



# get urls for all episode scripts
mainurl <- "https://transcripts.fandom.com/wiki/Stranger_Things"
epNames <- read_html(mainurl) %>% html_nodes("#mw-content-text a") %>% as.character()
epNames<-str_extract(epNames,".*(?=(title))") %>% str_sub(.,10) %>% str_remove_all(.,"\"") %>% trimws()
urls <- paste0("https://transcripts.fandom.com",epNames)
epNamesSimp <- str_remove_all(epNames,"(/wiki/)") %>% str_remove_all(.,"(%3F)")


# retrieve all scripts
mylist1<-list()
for(i in urls){
                temp <- read_html(i) %>% html_nodes(.,"#mw-content-text") 
                mylist1 <- append(mylist1, list(temp))
}

# general clean up
scripts <- list()
for(i in 1:25){
                temp <- mylist1[[i]]
                temp<- gsub("<.*?>","",temp)
                temp <- strsplit(temp,"\n\n") %>% unlist()
                scripts <- append(scripts,list(temp))  
}
mylist1<-NULL


#Scrape IMBD Data for episode ratings
base <-  "https://www.imdb.com/title/tt4574334/episodes?season="
urls2 <- paste0(base,c(1:3))
ratings <-NULL
for(i in 1:3){
                temp <- read_html(urls2[i]) %>% html_nodes(.,".ipl-rating-star.small .ipl-rating-star__rating") %>% html_text %>% as.numeric()
                ratings <- append(ratings,temp)
}
ratings <- data.frame("Episode"=epNamesSimp ,"Ratings"=ratings)


