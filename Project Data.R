imdb_full<-read.csv("C:/Users/micha/OneDrive/Desktop/IMDB_Data.csv",header=TRUE, na.strings=c("","NA"))

imdb_data<-read.csv("C:/Users/micha/OneDrive/Desktop/movie_ratings.csv",header=TRUE, na.strings=c("","NA"))



imdb_full$year <- as.numeric(imdb_full$year) 

imdb_data$year <- as.numeric(imdb_data$year) 
colnames(imdb_data)
dim(imdb_data)
head(imdb_data)




imdb_data$budget = as.numeric(gsub("\\$", "", imdb_data$budget))
imdb_data$usa_gross_income=as.numeric(gsub("\\$", "", imdb_data$usa_gross_income))
imdb_data$worlwide_gross_income=as.numeric(gsub("\\$", "", imdb_data$worlwide_gross_income))
imdb_data$year <- as.numeric(imdb_data$year) 



str(imdb_data)

imdb_holland1<- imdb_data %>%
  filter(str_detect(actors, "%Tom Holland%"))


imdb_data <- imdb_data  %>%
  filter(country=="USA")

##Notes: Create a column for director experience 
#(i.e., arrange by year, then have a column add the number of occurrences of directors name in a column before that row)

#install.packages("separate")
library(tidyr)
library(dplyr)

imdb_exp<-imdb_data[!is.na(imdb_data$budget) & !is.na(imdb_data$usa_gross_income),] %>%
  separate(col=actors,into=c("actor1","actor2","actor3","actor4","actor5"), sep="," ) %>%
  separate(col=director, into=c("director1","director2"), sep = ",") %>%
  separate(col=writer, into=c("writer1","writer2","writer3"), sep=",") %>%
  separate(col=genre, into = c("genre1","genre2","genre3"), sep=",") # %>%
  #separate(col=language, into =
# filter(str_detect(original_title,"Prince of Persia"))


imdb_dataset<- imdb_exp  %>% 
  select(-c("X","imdb_title_id","title","date_published","description")) %>%
  mutate(domestic_profit = usa_gross_income - budget, 
       worldwide_profit = worlwide_gross_income - budget) %>%
  mutate(domestic_profitability = domestic_profit/budget,
         worldwide_profitability = worldwide_profit/budget, 
         profit = case_when(worldwide_profit > 0 ~ 1,
                            worldwide_profit <= 0  ~0),
         language_number = count.fields(textConnection(language), sep = ",")) %>%
  mutate(language_bool = case_when(language_number > 1 ~ 1,
                                   language_number == 1 ~ 0))

imdb_dataset[is.na(imdb_dataset)] <- ""  
imdb_dataset$profit <- as.factor (imdb_dataset$profit)


total_obs_imdb <- dim(imdb_dataset)[1]
sample_index <- sample(1:total_obs_imdb, 0.7*total_obs_imdb)
train_data_imdb <- imdb_dataset[sample_index,]
test_data_imdb <- imdb_dataset[-sample_index,]

lm_imdb<-lm(profit~., data=train_data_imdb, type="binomial")

typeof(imdb_dataset$profit)



save(imdb_dataset, file="C:/Users/micha/OneDrive/Desktop/Mod 2 Notes/Machine Learning/Project/imdb_data.rda")



load("C:/Users/micha/OneDrive/Desktop/Mod 2 Notes/Machine Learning/Project/actor_res.rda")
load("C:/Users/micha/OneDrive/Desktop/Mod 2 Notes/Machine Learning/Project/director_db.rda")
load("C:/Users/micha/OneDrive/Desktop/Mod 2 Notes/Machine Learning/Project/writer_db.rda")





detach(package:plyr)
imdb_holland <- imdb_dataset %>%
  filter(actor1 == "Tom Holland" | actor2== "Tom Holland" | actor3== "Tom Holland" | actor4== "Tom Holland" | actor5== "Tom Holland")
  filter(across(c(actor1, actor2,actor3, actor4,actor5), ~"Tom Holland"))
  
imdb_holland <- imdb_dataset[which(imdb_dataset$actor1 == "Tom Holland" | imdb_dataset$actor2== "Tom Holland" | 
                                     imdb_dataset$actor3== "Tom Holland" | imdb_dataset$actor4== "Tom Holland" | 
                                     imdb_dataset$actor5== "Tom Holland"),]

imdb_Wahlberg <- imdb_dataset %>%
  filter(actor1== "Mark Wahlberg" | actor2== "Mark Wahlberg" | actor3 == "Mark Wahlberg" | actor4== "Mark Wahlberg" | actor5 =="Mark Wahlberg")

imdb_dataset %>%
  filter(str_detect(original_title, "Transformers"))

imdb_Tuturro <- imdb_dataset %>%
  filter(actor1=="John Turturro" | actor2=="John Turturro" | actor3=="John Turturro" | actor4=="John Turturro" | actor1=="John Turturro")

dim(imdb_dataset)

head(imdb_dataset)
tail(imdb_dataset)



imdb_full[1244, ]
imdb_exp[1244, ]

a<- count.fields(textConnection(imdb_dataset$production_company), sep = ",")

sum(is.na(a))


##Uncharted

#Year:    2022
#Genre1:  Action
#Genre2:  Adventure
#Duration: NA
#Country: United States
#Language: English
#Director1: Ruben Fleischer
#Writer1: Rafe Judkins
#Writer2: Art Marcum
#Writer3: Matt Holloway
#Budget: 120,000,000


total_data_imdb <- total_data_imdb[, !duplicated(colnames(total_data_imdb))]

save(total_data_imdb, file="C:/Users/micha/OneDrive/Desktop/Mod 2 Notes/Machine Learning/Project/imdb_data_cleaned.rda")




setdiff(total_data_imdb, imdb_exp2)




detach(package:plyr)

uncharted_db <- total_data_imdb %>%
  filter("Adventure" == 1)



#Renaming columns

names(total_data_imdb) <- make.names(total_data_imdb, unique=TRUE)
names(total_data_imdb)

