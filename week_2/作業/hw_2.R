library(rvest)
library(xml2)
library(magrittr)
library(decoder)

base_url1 =  "http://www.stat-nba.com/award.php?item=13&keyItem="
item = "per"
base_url2 = "&keyTable=advanced&isnba=1&season="
year= "-1"
res = read_html(url)
name = res%>% html_nodes("td")%>%html_text()
indexs = res%>% html_nodes("th")%>%html_text()

indexs[1]="num"


name = matrix(name, ncol = 24 , byrow = TRUE)
name = data.frame(name)
colnames(name) = indexs

nba_data = function(year,item){
  base_url1 =  "http://www.stat-nba.com/award.php?item=13&keyItem="
  base_url2 = "&keyTable=advanced&isnba=1&season="
  url = paste(base_url1, item, base_url2, year, sep="")
  res = read_html(url)
  name = res%>% html_nodes("td")%>%html_text()
  indexs = res%>% html_nodes("th")%>%html_text()
  
  indexs[1]="num"
  
  
  name = matrix(name, ncol = 24 , byrow = TRUE)
  name = data.frame(name)
  colnames(name) = indexs
  return(name)