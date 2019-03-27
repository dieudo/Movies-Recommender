library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv")
str(movies)
str(ratings)
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres_splited <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres_splited) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,9743,18) #empty matrix, 9743=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres_splited)) {
  for (c in 1:ncol(genres_splited)) {
    genmat_col = which(genre_matrix[1,] == genres_splited[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix_df<- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix_df)) {
  genre_matrix_df[,c] <- as.integer(genre_matrix_df[,c])#conversion to integers  
} 

head(genre_matrix_df)
