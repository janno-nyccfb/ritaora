library(rvest)
library(xml2)
library(tm)
library(wordcloud)
library(tidyverse)

#scrape links (first page of 'rita ora' search on dailymail.co.uk)
url1<-'https://www.dailymail.co.uk/home/search.html?offset=0&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url2<-'https://www.dailymail.co.uk/home/search.html?offset=50&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url3<-'https://www.dailymail.co.uk/home/search.html?offset=100&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url4<-'https://www.dailymail.co.uk/home/search.html?offset=150&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url5<-'https://www.dailymail.co.uk/home/search.html?offset=200&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url6<-'https://www.dailymail.co.uk/home/search.html?offset=250&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url7<-'https://www.dailymail.co.uk/home/search.html?offset=300&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url8<-'https://www.dailymail.co.uk/home/search.html?offset=350&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url9<-'https://www.dailymail.co.uk/home/search.html?offset=400&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url10<-'https://www.dailymail.co.uk/home/search.html?offset=450&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url11<-'https://www.dailymail.co.uk/home/search.html?offset=500&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url12<-'https://www.dailymail.co.uk/home/search.html?offset=550&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url13<-'https://www.dailymail.co.uk/home/search.html?offset=600&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
url14<-'https://www.dailymail.co.uk/home/search.html?offset=650&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'

#function for scraping links
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data.frame(link = link_, url = url_))
}
#return links on all search pages
ritaora_search_1<-scraplinks(url1)
ritaora_search_2<-scraplinks(url2)
ritaora_search_3<-scraplinks(url3)
ritaora_search_4<-scraplinks(url4)
ritaora_search_5<-scraplinks(url5)
ritaora_search_6<-scraplinks(url6)
ritaora_search_7<-scraplinks(url7)
ritaora_search_8<-scraplinks(url8)
ritaora_search_9<-scraplinks(url9)
ritaora_search_10<-scraplinks(url10)
ritaora_search_11<-scraplinks(url11)
ritaora_search_12<-scraplinks(url12)
ritaora_search_13<-scraplinks(url13)
ritaora_search_14<-scraplinks(url14)

#bind all searchtables
ritaora_search<-bind_rows(ritaora_search_1,ritaora_search_10,ritaora_search_11,ritaora_search_11,ritaora_search_12,ritaora_search_13,ritaora_search_14,ritaora_search_2,ritaora_search_3,ritaora_search_5,ritaora_search_6,ritaora_search_4,ritaora_search_7,ritaora_search_8,ritaora_search_9)

#filter for only headlines that contain 'Rita Ora'
ritaora_search<-ritaora_search%>%filter(str_detect(ritaora_search$link,"Rita Ora"))
#add start of url in each cell
ritaora_search["start"]<-"https://www.dailymail.co.uk"
ritaora_search<-ritaora_search%>%mutate(url2=paste0(start,url))

#create a test
test<-ritaora_search%>%slice(1:3)

#scrape test text using lapply
testtext<-lapply(ritaora_search$url2,function(i) {
  webpage<-read_html(i)
  draft_page<-html_nodes(webpage,'p')
  draft<-html_text(draft_page)
  draft<-head(draft,n=-13L)
})

finaltext<-do.call(c,testtext)

#find frequency of words
nont<-c("\n","\t","\r")
finaltext<-gsub(paste(nont,collapse = "|")," ",finaltext)
finaltext<-removeWords(finaltext,c("Facebook","comment","comments","MailOnline","Rita","Ora","post"))
Corpus<-VCorpus(VectorSource(finaltext))
ctrl<-list(stopwords=T,
           removePunctuation=T,
           removeNumbers=T)
tdm<-TermDocumentMatrix(Corpus,ctrl)
tdm.m<-as.matrix(tdm)
counts<-rowSums(tdm.m)
rita.ora.words<-data.frame(cbind(names(counts),as.numeric(counts)),stringsAsFactors = F)
names(rita.ora.words)<-c('term','frequency')
rita.ora.words$frequency<-as.numeric(rita.ora.words$frequency)
which(grepl("edt",rita.ora.words$term)) #4184
rita.ora.words<-rita.ora.words[-4184,]
head(rita.ora.words[with(rita.ora.words,order(-frequency)),],100)

#wordcloud
rita.ora.wordcloud<-wordcloud(words=rita.ora.words$term,freq=rita.ora.words$frequency,min.freq=1,max.words=200,random.order=F,rot.per=0.35,colors=brewer.pal(8,"Dark2"))
