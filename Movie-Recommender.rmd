
```{r}
#title: "Movie Recommender"
#output: html_document
#Author: "Goutham Muguluvalli-Niranjan"
```

#Question 2.2
```{r}
rm(list=ls())

library(data.table)

movies <- read.csv("movies.csv", stringsAsFactors=FALSE)

# get the data of user
user67 <- read.csv("ratings.csv")
user67data <- subset(user67, user67[,1]==67)

# extract only genres coloumn
movies67 <- as.data.frame(movies[which(movies$movieId %in% user67data$movieId),3], stringsAsFactors=FALSE)

# split the list of genres of movie
genres <- as.data.frame(tstrsplit(movies67[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:ncol(genres))

profile67 <- matrix(0,nrow(user67data)+1,20)

genres_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "IMAX", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", "(no genres listed)")

profile67[1,] <- genres_list
colnames(profile67) <- genres_list

#iterate through matrix
for (i in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    profile67_col = which(profile67[1,] == genres[i,c])
    profile67[i+1,profile67_col] <- 1
  }
}

#convert into dataframe
profile67_new <- as.data.frame(profile67[-1,], stringsAsFactors=FALSE) 

#remove first row, which was the genre list
for (c in 1:ncol(profile67_new)) {
  profile67_new[,c] <- as.integer(profile67_new[,c])
} #convert from characters to integers

user67Prfile_vec <- colMeans(profile67_new)

# extract only genres coloumn
movies.genres <- as.data.frame(movies[sample(nrow(movies), 10),], stringsAsFactors=FALSE)

# split the list of genres of movie
genres2 <- as.data.frame(tstrsplit(movies.genres[,3], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:ncol(genres2))

M.profile <- matrix(0,nrow(genres2)+1,20)

M.profile[1,] <- genres_list
colnames(M.profile) <- genres_list
#M.profile

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    profile67_col = which(M.profile[1,] == genres2[i,c])
    M.profile[i+1,profile67_col] <- 1
  }
}
#M.profile

#convert into dataframe
M.profile_new <- as.data.frame(M.profile[-1,], stringsAsFactors=FALSE) 

#remove first row, which was the genre list
for (c in 1:ncol(M.profile_new)) {
  M.profile_new[,c] <- as.integer(M.profile_new[,c])
} #convert from characters to integers

M.profile_new <- cbind(mTitle=movies.genres$title,M.profile_new )
#M.profile_new
#movies.genres

result <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("similarity")
colnames(result) <- x

cosine <- function(x, y) {
  # Need to do error checking:
  # 1) Ensure x and y are vectors.
  
  sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
}

for (i in 1:nrow(M.profile_new)) {
    movieid.matrix <-M.profile_new[i,2:21]
    similarity = cosine(user67Prfile_vec,movieid.matrix)
    result[nrow(result)+1,] = similarity
}

result <- cbind(mTitle=movies.genres$title,result )
result <- result[with(result, order(similarity, decreasing = TRUE)), ]
result<- data.frame(lapply(result, as.character), stringsAsFactors=FALSE)



for (i in 1:5) {
  if(i==1){
    cat("For User ID 67, the following movies are recommended: \n\n")
  }
    cat("Movie ",result[i,][,1],", similarity score",round( as.numeric(result[i,][,2]),3))
    cat("\n")
  }

```
