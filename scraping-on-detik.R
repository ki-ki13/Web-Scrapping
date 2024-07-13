library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
# 
# sortByDate <- function(remDr){
#   sortButton <- remDr$findElement(using = "css selector", ".gsc-option-menu-container.gsc-inline-block")
#   sortButton$clickElement()
#   
#   dateButton <- sortButton$findChildElement(using = "css selector", ".gsc-option-menu .gsc-option-menu-item:last-child")
#   dateButton$getElementText()
#   remDr$mouseMoveToLocation(webElement = dateButton)
#   remDr$click()
# }

getNews <- function(df, remDr) {
  # remDr$navigate(pageUrl)
  Sys.sleep(5)
  links = list()
  while(length(links) == 0){
    links <- remDr$findElements(using = "css selector", ".list.media_rows.list-berita a")
    print("getting links ...")
  }
  
  urls <- sapply(links, function(x) x$getElementAttribute("href"))
  
  titles <- remDr$findElements(using = "css selector", ".list.media_rows.list-berita a h2")
  titles <- sapply(links, function(x) x$getElementText())
  
  for(i in 1:length(urls)){
    title <- titles[[i]]
    url <- urls[[i]]
    news <- read_html(url) %>%
      html_elements(".detail__body-text.itp_bodycontent p") %>%
      html_text2() %>%
      paste(collapse = " ")
    df <- df %>%
      add_row(title, url, news)
  }
  df  
}





rD <- rsDriver(browser = "firefox", chromever = NULL)
remDr <- rD$client

df <- tibble(title = character(), url = character(), news = character())

remDr$navigate("https://detik.com")
query <- "maritim"
search <- remDr$findElement(using = "css selector", "input[name = 'query']")
search$getElementAttribute("placeholder")
search$clearElement()
search$sendKeysToElement(list(query, key = "enter"))

i <- 1
max <- 3
for(i in c(1:max)){
  df <- getNews(df, remDr)
  print(paste0("process of ", i, "/", "10 done!"))

  if(i < max){
    nextPage <- remDr$findElement(using = "css selector", ".paging a:last-child")
    nextPage$clickElement() 
    Sys.sleep(5)
  }
                

  i <- i + 1
}
length(df$news)


view(df)
# 
date_pattern <-"\\b\\d{1,2} [A-Za-z]{3} \\d{4} \\d{2}:\\d{2}\\b"

df <- df %>%
  mutate(name = "Deny",
         waktu = str_extract(title,date_pattern))%>%
  filter(news != "")

# write.table(df, "./kompas-news.csv", row.names = FALSE, sep = "~")
write.csv(df, "./detik-news.csv", row.names = FALSE)
remDr$close()
rD$server$stop()



