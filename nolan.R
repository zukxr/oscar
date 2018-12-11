library("rvest")
library("RedditExtractoR")

movies <- read.csv("movies.csv", header=TRUE, sep = ",")

final_movies <- data.frame(
  "movie" = character(2),
  "movie_score" = integer(2),
  stringsAsFactors = FALSE
)

movieCounter <- 1


for (movie in movies$movie_title) {
  
  movieTitle <- movie
  
  print(paste0("Gathering reddit data for ", movie))
  
  redditData <- get_reddit(search_terms = movieTitle, subreddit = "movies", page_threshold = 2, sort_by = "comments")
  
  totalPostsCount <- 0
  
  print("Seperating out posts...")
  
  for (id in redditData$id) {
    if (id == 1) {
      
      totalPostsCount = totalPostsCount + 1
    }
    
  }
  
  
  posts <- data.frame(
    "post" = character(totalPostsCount),
    "number_of_comments" = integer(totalPostsCount),
    "post_score" = integer(totalPostsCount),
    "average_comment_score" = integer(totalPostsCount),
    "total_comment_score" = integer(totalPostsCount),
    "weighted_post_score" = integer(totalPostsCount),
    stringsAsFactors = FALSE
    
    #totalPostsCount
  )
  
  postsIndex <- 0
  redditDataRowCounter <- 0
  
  comment_count <- 0
  commentAverage <- 0
  
  print("Analyzing comments...")
  
  for (id in redditData$id) {
    if (id == 1) {
      
      posts$post[postsIndex] <- redditData$title[redditDataRowCounter]
      posts$number_of_comments[postsIndex] <- redditData$num_comments[redditDataRowCounter]
      posts$post_score[postsIndex] <- redditData$post_score[redditDataRowCounter]
      
      postsIndex = postsIndex + 1
      comment_count <- 0
      commentAverage <- 0
    }
    
    
    
    if (id != 1) {
      
      commentAverage <- commentAverage + redditData$comment_score[redditDataRowCounter]
      posts$average_comment_score[postsIndex] <- commentAverage / comment_count
      posts$total_comment_score[postsIndex] <- commentAverage
      comment_count = comment_count + 1
      
      
    }
    
    
    posts$weighted_post_score[postsIndex] <- sum((redditData$num_comments[redditDataRowCounter] * 0.4),(redditData$post_score[redditDataRowCounter] * 0.3),((commentAverage / comment_count) * 0.2), (commentAverage * 0.1))
    
    redditDataRowCounter = redditDataRowCounter + 1
  }
  
  print("Adding to final roster...")
  
  
  final_movies$movie[movieCounter] <- movieTitle
  final_movies$movie_score[movieCounter] <- sum(posts$weighted_post_score)/50
  
  movieCounter <- movieCounter + 1
  
}



write.csv(final_movies, file="final.csv")

