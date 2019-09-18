library(downloader)
library(stringr)
library(rvest)
library(xml2)
library(tm)

url1<-'https://www.dailymail.co.uk/home/search.html?offset=0&size=50&sel=site&searchPhrase=Rita+Ora&sort=recent&channel=tvshowbiz&channel=femail&channel=news&type=article&days=last365days'
pg1<-read_html(url1)
head(html_attr(html_nodes(pg1,"a"), "href"),n=100L)
