library("dplyr")

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

prepare_data_for_genres = function(){
	rated_movies = ratings %>%
     		group_by(MovieID) %>%
  		summarize(ratings_per_movie = n(),
            	ave_ratings = round(mean(Rating), dig=3)) %>%
  		inner_join(movies, by = 'MovieID')

	genres = as.data.frame(rated_movies$Genres, stringsAsFactors=FALSE)
	tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                        type.convert=TRUE),
                    	stringsAsFactors=FALSE)

	m = length(genre_list)
	genre_matrix = matrix(0, nrow(rated_movies), length(genre_list))
	for(i in 1:nrow(tmp)){
  	    genre_matrix[i,genre_list %in% tmp[i,]]=1
	}

	colnames(genre_matrix) = genre_list
	remove("tmp", "genres")
	rated_movies$image_url = sapply(rated_movies$MovieID,
                          function(x) paste0('MovieImages/', x, '.jpg'))
	
	list("rated" = rated_movies, "genre_matrix" = genre_matrix)

}

prepare_model_for_prediction = function(){
  
  user_ids = paste0('u', ratings$UserID)
  movie_ids = paste0('m', ratings$MovieID)
  ratings = ratings$Rating
  tmp = data.frame(user_ids, movie_ids, ratings, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$user_ids), as.integer(tmp$movie_ids), x = tmp$ratings)
  rownames(Rmat) = levels(tmp$user_ids)
  colnames(Rmat) = levels(tmp$movie_ids)
  Rmat = new('realRatingMatrix', data = Rmat)
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(method = 'Cosine',
                                          normalize = "Z-score",
                                          nn = 25))
  list("Rmat" = Rmat, "model" = rec_UBCF)
}


get_top_movies_by_genre = function(genre, n){
    tmp = rated_movies[genre_matrix[, genre] == 1, ]
    tmp = arrange(tmp, -ratings_per_movie)
    tmp[1:n, ]
}

get_movie_recs_ubcf = function(user_ratings){
 
  n = nrow(user_ratings)
  
  if (n > 0){
    movieIDs = colnames(Rmat)
    n.item = ncol(Rmat)
    new.ratings = rep(NA, n.item)
    for (i in 1:n){
      movie_id =  paste('m', user_ratings[i, "MovieID"], sep='')
      rating   =  as.numeric(user_ratings[i, "Rating"])
      print(movie_id, rating)
      new.ratings[which(movieIDs == movie_id)] = rating
    }
    new.user = matrix(new.ratings, 
                      nrow=1, ncol=n.item,
                      dimnames = list(
                        user="user",
                        item=movieIDs
                      ))
    new.Rmat = as(new.user, 'realRatingMatrix')
    
    recom1 = predict(ubcf_model, new.Rmat, type = 'ratings')
    
    result = order(as(recom1, "matrix"), decreasing = TRUE)[1:10]
    
  }else {
    sample(movies$MovieID, 10)
  }
  
 
}

##############
# read in data
##############
ratings = read.csv("ratings.dat", 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines('movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$image_url = sapply(movies$MovieID,
                                function(x) paste0('MovieImages/', x, '.jpg'))

genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

prepared = prepare_data_for_genres()
rated_movies = prepared$rated
genre_matrix = prepared$genre_matrix

model_prep = prepare_model_for_prediction()
Rmat = model_prep$Rmat
ubcf_model = model_prep$model


shinyServer(function(input, output, session) {
  
  
  output$genre <- renderUI({
    
    n = as.numeric(input$choices)
    num_rows <- n / 5
    num_movies <- 5 # movies per row
    top = get_top_movies_by_genre(input$genre, n)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = top$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(paste((i - 1) * num_movies + j, ". ", top$Title[(i - 1) * num_movies + j])))))
      })))
    })
  })
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    tmp_movie_idx = sample(1:nrow(movies), num_rows * num_movies)
    tmp_movies = movies[tmp_movie_idx, ]
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = tmp_movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(tmp_movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", tmp_movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the submit is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list = reactiveValuesToList(input)
      user_ratings = get_user_ratings(value_list)
      print(nrow(user_ratings))
      
      movie_ids = get_movie_recs_ubcf(user_ratings)
      user_predicted_ids = which(movies$MovieID %in% movie_ids)
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = movies$MovieID[user_predicted_ids],
                                  Title = movies$Title[user_predicted_ids])
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center",
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%",
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function

