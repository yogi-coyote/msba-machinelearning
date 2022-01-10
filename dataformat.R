#Formatting
movies <- read.csv("./archive/IMDb movies.csv", header = TRUE)
ratings <- read.csv("./archive/IMDb ratings.csv", header = TRUE)
names <- read.csv("./archive/IMDb names.csv", header = TRUE)
principals <- read.csv("./archive/IMDb title_principals.csv", header = TRUE)

dim(movies)
dim(names)
dim(ratings)
dim(principals)

hist(ratings$weighted_average_vote)

movie_ratings <- merge(movies, ratings, by = "imdb_title_id")

# DNR - Creates data frame with 850,000 rows
#movie_ratings_exp <- merge(movie_ratings, principals, by = "imdb_title_id")
#head(movie_ratings_exp)
#write.csv(movie_ratings_exp, file = 'movie_ratings_exp.csv', sep = ',')

write.csv(movie_ratings, file = 'movie_ratings.csv', sep = ',')


#Reading 
movie_ratings <- read.csv("movie_ratings.csv", sep = ",")

head(movie_ratings)
dim(movie_ratings)
str(movie_ratings)

#mrt_director <- movie_ratings[movie_ratings$director == 'Ruben Fleischer' | #movie_ratings$production_company == "Columbia Pictures", ]

#Plots
library(ggplot2)

ggplot(movie_ratings, aes(x = avg_vote)) +
  geom_histogram(alpha = 0.7) + 
  labs(x = "Average Rating", y = "Count") +
  theme_minimal()

ggplot(movie_ratings, aes(x = avg_vote, y = votes)) +
  geom_point(alpha = 0.3, color = 'blue') + 
  geom_smooth(color = "red") +
  labs(x = "Average Rating", y = "Number of Votes") +
  theme_minimal()


is.na()