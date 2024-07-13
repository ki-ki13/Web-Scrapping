library(tidyverse)
library(DBI)
library(textclean)

# koneksi database
con <- DBI::dbConnect(RMySQL::MySQL(),
                      host = "localhost",
                      dbname = "navy_scraping",
                      user = "root", 
                      password = "")

#cek koneksi
dbGetQuery(con, "Show tables")

#list files
list.files("Kelompok 3")
df <- tibble(title = character(), link = character(), news = character(), tanggal = character())
nama_kel <- 'Kelompok 3/'
for (i in list.files(nama_kel)) {
  df2<- read_csv(paste0(nama_kel,i))
  if("waktu" %in% colnames(df2)) {
    df2<- df2 %>% select(title,link,news,waktu) %>% rename(tanggal = waktu)
    } 
  else {
    if("tanggal" %in% colnames(df2)){
      df2<- df2 %>% select(title,link,news,tanggal)  
    }
    df2 <- df2 %>% select(title,link,news) %>% mutate(tanggal = "")
  }  
  # kelompok1
  # df2 <- df2 %>% mutate(tanggal = as.character(tanggal), 
  #                       keyword = "kegiatan masyarakat di pesisir pantai",
  #                       nama = "Redhy Eri Erdiansyah",
  #                       kelompok = '1')
  
  # kelompok3
  df2 <- df2 %>% mutate(tanggal = as.character(tanggal),
                        keyword = "Situasi keamanan pantai",
                        nama = str_extract(i, pattern = "_(.*?)_"),
                        kelompok = '3')
  df<-rbind(df,df2)
}


glimpse(df)

kelompok1 <- df 
glimpse(kelompok1)

df2 <- read_csv("Kelompok 2/2_viarmadamarinowaliadi_27112023.csv") 
df2 <- df2 %>% select(title,link,news,tanggal) %>% 
  mutate(tanggal = as.character(tanggal), 
         keyword = "pengawasan pantai oleh masyarakat",
         nama = "viarmada marino",
         kelompok = '2')

glimpse(df2)
kelompok2 <- df2

df<-rbind(df,df2)

view(df)

write.csv(df,"kel1-3.csv")

data <- read_csv("kel1-3.csv")
glimpse(data)

view(data_clean)

clean_data <- function(data) {
  data <- data %>%
    distinct(link, .keep_all = T) %>%
    filter(news != "")  %>%
    select(-'...1')
  data
}

data_clean<-clean_data(data) 
glimpse(data_clean)

write.csv(data_clean,'kel1-3(distinct).csv')

data1 <- data_clean
data2 <- read_csv('kelompok_4-6_cleaned.csv')

glimpse(data1)
view(data1)

data1 <- data1 %>% mutate(media = str_extract(link,'\\b\\w+\\.\\w+\\.\\w+\\b'))
data2 <- data2 %>% select(-'...1') %>% mutate(kelompok = as.character(kelompok)) %>% rename(tanggal=waktu,
                                                                                            nama = name)
glimpse(data2)

data_gabung <- rbind(data1,data2)
glimpse(data_gabung)

write.csv(data_gabung,'hasil_data.csv')

data_new <- read_csv('hasil_data.csv')

glimpse(data_new)

data_new <- data_new %>% 
  mutate(kelompok = as.character(kelompok)) %>% 
  select(-'...1')

sum(!is.na(data_new$tanggal))

view(data_new)

data_new <- data_new %>% 
  mutate(tanggal = if_else(is.na(tanggal),'01/01/2020',tanggal))

data_new %>% mutate(
  tanggal = case_when(str_detect(.,'\\d{4}-\\d{2}-\\d{2}') ~ as.Date(tanggal))
)

data_clean$news <- data_clean$news%>% 
  replace_url() %>% 
  replace_date() %>% 
  replace_time()

data_clean <- data_clean %>% 
  mutate(news = news %>%  str_to_lower(.) %>% str_replace_all(.,'[^a-zA-Z\\s]',''))  

#cek data clean
data_clean$news[1]

head(data_clean)

# ganti nama tabel 
res <- dbWriteTable(con, 'test', data_clean,  overwrite = TRUE, append=FALSE, row.names=TRUE)
res

generate_wordcloud <- function(data){
  wordcloud2(data=data, size=1.6, color='random-dark')
}

generate_pie <- function(data){
  pie(data$count, labels = data$count)
}