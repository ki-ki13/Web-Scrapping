library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)

sortByDate <- function(remDr){
  sortButton <- remDr$findElement(using = "css selector", ".gsc-option-menu-container.gsc-inline-block")
  sortButton$clickElement()
  
  dateButton <- sortButton$findChildElement(using = "css selector", ".gsc-option-menu .gsc-option-menu-item:last-child")
  dateButton$getElementText()
  remDr$mouseMoveToLocation(webElement = dateButton)
  remDr$click()
}

getNews <- function(df, pageUrl, remDr) {
  remDr$navigate(pageUrl)
  Sys.sleep(5)
  links = list()
  while(length(links) == 0){
    links <- remDr$findElements(using = "css selector", ".gsc-expansionArea .gsc-thumbnail-inside a:first-child")
    print("getting links ...")
  }
  
  
  # dates <- remDr$findElements(using = "css selector", ".gs-bidi-start-align.gs-visibleUrl.gs-visibleUrl-breadcrumb span:nth-child(3)")
  # 
  # dates <- sapply(dates, function(x) x$getElementText())
  
  
  
  titles <- sapply(links, function(x) x$getElementText())
  urls <- sapply(links, function(x) x$getElementAttribute("href"))

  for(i in 1:length(urls)){
    title <- titles[[i]]
    url <- urls[[i]]
    news <- read_html(url) %>%
      html_elements(".read__content p") %>%
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

remDr$navigate("https://kompas.com")
query <- "maritim"
search <- remDr$findElement(using = "css selector", "input#search")
search$getElementAttribute("placeholder")
search$clearElement()
search$sendKeysToElement(list(query, key = "enter"))

sortByDate(remDr)

# 
page2 <- remDr$findElement(using = "css selector", ".gsc-cursor-page[aria-label = 'Page 2']")

page2$clickElement()
Sys.sleep(3)

searchableQuery <- remDr$getCurrentUrl() %>%
  str_sub(., 1, -2)

urlQueries <- sapply(c(1:10), function(i) {
  paste0(searchableQuery, i)
})
urlQueries

i <- 1
for(urlQ in urlQueries){
  df <- getNews(df, pageUrl = urlQ, remDr)
  print(paste0("process of ", i, "/", length(urlQueries), " done!"))
  i <- i + 1
}
length(df$news)

df <- read_csv("./kompas-news.csv")

# date_pattern <-"\\d{2}\/\\d{2}/\\d{4}"
date_pattern <- "\\b\\d{4}/\\d{2}/\\d{2}\\b"

df <- df %>%
  mutate(name = "Deny",
         waktu = str_extract(url,date_pattern))%>%
  filter(news != "")

view(df)

# write.table(df, "./kompas-news.csv", row.names = FALSE, sep = "~")
write.csv(df, "./kompas-news.csv", row.names = FALSE)
remDr$close()
rD$server$stop()



