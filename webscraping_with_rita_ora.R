library(downloader)
library(stringr)
library(rvest)
library(xml2)
library(tm)

url1<-'https://www.dailymail.co.uk/home/search.html?offset=0&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
pg1<-read_html(url1)
head(html_attr(html_nodes(pg1,"a"), "href"),n=100L)

scraplinks <- function(url1){
  # Create an html document from the url
  webpage <- xml2::read_html(url1)
  # Extract the URLs
  url1_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data_frame(link = link_, url = url1_))
}


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

ritaora_search<-rbind(ritaora_search_1,ritaora_search_10,ritaora_search_11,ritaora_search_11,ritaora_search_12,ritaora_search_13,ritaora_search_14,ritaora_search_2,ritaora_search_3,ritaora_search_5,ritaora_search_6,ritaora_search_4,ritaora_search_7,ritaora_search_8,ritaora_search_9)

ritaora_search<-ritaora_search%>%filter(grepl("Rita Ora",ritaora_search$link))
#add start of url in each cell
ritaora_search["start"]<-"https://www.dailymail.co.uk"
ritaora_search<-ritaora_search%>%mutate(url2=paste0(start,url))
#form corpus
txturls <- ritaora_search$url

#scrape text
for (i in test$url2) #may not have set up loop correctly
{
  testtext<-read_html(i) 
} #seems to return pure html
readtext_parsed<-html_nodes(testtext,css='p')%>%html_text()#text from only last url





