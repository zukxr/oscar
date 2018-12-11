library("rvest")
library("knitr")
library("stringr")

movies <- read.csv("movies.csv")
movies <- movies$movie

movie_revenues <- data.frame(
  movie = character(50),
  opening_revenue = numeric(50),
  total_revenue = numeric(50),
  stringsAsFactors = FALSE
)

index <- 0

for (movie in movies) {
  
  cat(paste("Pulling data for",movie))
  
  
  
  search_movie_title <- gsub(" ","%20", movie)
  search_movie_title <- gsub(":","",search_movie_title)
  search_movie_title <- gsub("\\(", "", search_movie_title)
  search_movie_title <- gsub("\\)", "", search_movie_title)
  
  
  start_url <- paste0("https://www.boxofficemojo.com/search/?q=",search_movie_title,"&sort=gross")
  
  session <- html_session(start_url)
  url_page <- read_html(start_url)
  title_from_page <- html_nodes(url_page,"b font a") %>% html_text()
  title_from_page <- title_from_page[1]
  
  movie_bom_page <- follow_link(session, title_from_page)
  bom_page <- read_html(movie_bom_page)
  domestic_revenue <- html_nodes(bom_page, "center font b") %>% html_text()
  opening_weekend_revenue <- html_nodes(bom_page, "td td td td .mp_box+ .mp_box table:nth-child(1) tr:nth-child(1) td+ td") %>% html_text()
  
  
  opening_weekend_revenue <- str_trim(opening_weekend_revenue, side = "left")
  
  opening_weekend_revenue <- gsub("\\$","", opening_weekend_revenue)
  opening_weekend_revenue <- gsub(",","", opening_weekend_revenue)
  opening_weekend_revenue <- as.numeric(opening_weekend_revenue)
  
  domestic_revenue <- gsub("\\$","", domestic_revenue)
  domestic_revenue <- gsub(",","", domestic_revenue)
  domestic_revenue <- as.numeric(domestic_revenue)
  
  movie_revenues$movie[index] <- title_from_page
  movie_revenues$opening_revenue[index] <- opening_weekend_revenue
  movie_revenues$total_revenue[index] <- domestic_revenue
  
  index <- index + 1
  
}

write.csv(movie_revenues, file = "movie_revenues.csv")