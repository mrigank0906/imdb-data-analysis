
#install the required packages

Needed <- c("stringr", "tm", "RWeka", "corpus")
install.packages(Needed, dependencies = TRUE)

# Libraries used
library(tm)
library(stringr)

#import the dataset from the csv file
movie_data_train <- read.csv(file.choose())
movie_data_test <- read.csv(file.choose())

#combine test and train to perform data cleaning
movie_data_test$Source <- "Test"
movie_data_train$Source <- "Train"

#add a column revenue to test data set and fill it with NA
#test data set would be required to test the final model to predict the revenue and thus revenue column is missing
movie_data_test$revenue <- NA
colnames(movie_data_test)[1] <- colnames(movie_data_train)[1]

comb_data <- rbind(movie_data_train, movie_data_test)
str(comb_data)


################################################################################################
#Dropping unwanted attributes



# Genres

comb_data$genres <- as.character(comb_data$genres)

#1. Removing the id and name tags. Also, filtering out any punctuation or number.
#Adding the newly generated genres in newcolumn : P_genres

for (i in 1:nrow(comb_data)) {
  comb_data$P_genres[i] <-
    gsub(']', "", gsub(
      "+(id)+",
      "",
      gsub("[[{'':0-9},]|+(name)+", "", comb_data$genres[i])
    ))
}

#2. Adding the text data to a corpus

new <- VCorpus(VectorSource((comb_data$P_genres)))
mt <- as.matrix(DocumentTermMatrix(new))

#3. Adding the list of genres to the main dataset :

comb_data <- cbind(comb_data, mt)


#PART B
# Spoken Languages
# removing punctuation

corpus <- VCorpus(VectorSource(comb_data$spoken_languages))
corpus <- tm_map(corpus, removePunctuation)

# Creating a list of tags to remove
code <-
  c(
    "en",
    "es",
    "ko",
    "ar",
    "ru",
    "sv",
    "sr",
    "de",
    "fr",
    "la",
    "it",
    "nl",
    "cn",
    "ja",
    "hi",
    "cs",
    "pt",
    "de",
    "am"
  )
code_new <- NULL
for (i in 1:length(code)) {
  code_new[i] <- paste("+", "(", code[i], ")", "+", sep = "")
}
code_new <- data.frame(code_new)

# Using gsub to filter tags “ iso__,  “ iso6391” and “name”

for (i in 1:length(corpus)) {
  corpus[[i]] <- gsub("iso__ ", "", corpus[[i]])
  corpus[[i]] <- gsub("iso6391 ", "", corpus[[i]])
  corpus[[i]] <- gsub("+(name)+", "", corpus[[i]])
  
  # replacing language code
  for (j in 1:nrow(code_new)) {
    corpus[[i]] <- gsub(code_new[j, ], "", corpus[[i]])
  }
}

##################
corpus_1 <- tm_map(corpus, PlainTextDocument)
dtm3 <- DocumentTermMatrix(corpus_1)

nn <- as.matrix(dtm3)

# FInding most frequent spoken languages
freq <- colSums(nn)
head(sort(freq, decreasing = TRUE), 10)
colnames(nn)

# removing garbage ( Lot of garbage values picked up by document term matrix )
nn <- nn[, -c(1:8)]
nn <- nn[, -c(2:5)]
nn <- nn[, -c(8:12)]
nn <- nn[, -c(3)]
nn <- nn[, -c(13:14)]
nn <- nn[, -c(17:19)]
nn <- nn[, -c(49:51)]
nn <- nn[, -c(45:46)]
nn <- nn[, -c(29:32)]
nn <- nn[, -c(36)]
nn <- nn[, -c(1:7)]
nn <- nn[, -c(33:34)]
nn <- nn[, -c(18:19)]
nn <- nn[, -c(8:12)]
nn <- nn[, -c(5)]
nn <- nn[, -c(9)]
##########################
# Final data stored the comb_data

comb_data <- cbind(comb_data, nn)


###########################################################################################

#fill budget==0 with mean budget value
summary(comb_data$budget)

for (i in 1:nrow(comb_data)) {
  if (comb_data$budget[i] == 0) {
    comb_data$budget[i] = mean(comb_data$budget)
  }
}



list_na <- colnames(comb_data)[apply(comb_data, 2, anyNA)]
list_na

#fill missing values as well as where runtime=0 with mean runtime
mean_missing <- apply(comb_data[, colnames(comb_data) %in% list_na],
                      2,
                      mean,
                      na.rm =  TRUE)
mean_missing

mean_runtime <- mean_missing[1]

for (i in 1:nrow(comb_data)) {
  if (is.na(comb_data$runtime[i]) | comb_data$runtime[i] == 0) {
    comb_data$runtime[i] = mean_runtime
  }
}


#fill missing dates
#convert date into month, year and day

comb_data$release_date <- as.character(comb_data$release_date)
comb_data$release_date <- as.Date(comb_data$release_date, "%m/%d/%Y")

#extract month and create a month column
comb_data$month <- format(comb_data$release_date, format = "%b")

#extract year and create new column year
comb_data$year <- format(comb_data$release_date, format = "%Y")

#extract day and create another coumn day
comb_data$day <- format(comb_data$release_date, format = "%d")

#get the weekday for each date
comb_data$weekday <- format(comb_data$release_date, format = "%a")

#find mode for month, year, day, weekday
#create a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Calculate the mode for weekday, day and month using the user function and impute the result in place of missing values.
result_weekday <- getmode(comb_data$weekday)
result_day <- getmode(comb_data$day)
result_month <- getmode(comb_data$month)

#calculate median for year and impute
result_year <- median(comb_data$year, na.rm = TRUE)

#impute missing weekday
for (i in 1:nrow(comb_data)) {
  if (is.na(comb_data$weekday[i])) {
    comb_data$weekday[i] = result_weekday
  }
}

#impute missing months
for (i in 1:nrow(comb_data)) {
  if (is.na(comb_data$month[i])) {
    comb_data$month[i] = result_month
  }
}

#impute missing day
for (i in 1:nrow(comb_data)) {
  if (is.na(comb_data$day[i])) {
    comb_data$day[i] = result_day
  }
}

#impute missing year
for (i in 1:nrow(comb_data)) {
  if (is.na(comb_data$year[i])) {
    comb_data$year[i] = result_year
  }
}

#summary of the variables budget, runtime, weekday, day, month
summary(comb_data$budget)
summary(comb_data$runtime)

sort(table(comb_data$weekday), decreasing = TRUE)
sort(table(comb_data$day), decreasing = TRUE)
sort(table(comb_data$month), decreasing = TRUE)

#count the number of keywords for each movie

#extract each keyword from the list of characters and remove all the white spaces and special characters

comb_data$Keywords <- as.character(comb_data$Keywords)
for (i in 1:nrow(comb_data)) {
  comb_data$keyword_count[i] <-
    gsub(']', "", gsub(
      "+(id)+",
      "",
      gsub("[[{'':0-9},]|+(name)+", "", comb_data$Keywords[i])
    ))
}

#count keywords
for (i in 1:nrow(comb_data)) {
  comb_data$keyword_count[i] <-
    str_count(comb_data$keyword_count[i], '\\w+')
}

#convert it into numeric
comb_data$keyword_count <- as.numeric(comb_data$keyword_count)

#impute mean number of keywords where count=0
for (i in 1:nrow(comb_data)) {
  if (comb_data$keyword_count[i] == 0) {
    comb_data$keyword_count[i] <- ceiling(mean(comb_data$keyword_count))
  }
  
}

##########################################################################################################

#Production companies

comb_data$production_companies <-
  as.character(comb_data$production_companies)

for (i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <-
    gsub(']', "", gsub('"', "", strsplit(substring(
      gsub(
        "[[{'':0-9},] | +(name)+",
        "",
        comb_data$production_companies[i]
      ),
      10,
      60
    ), "id")))
  
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <-
    gsub(",.*", "", comb_data$production_companies[i])
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <-
    str_sub(comb_data$production_companies[i], 3, -3)
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <-
    str_replace(comb_data$production_companies[i], "aracter", "NA,")
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <-
    gsub(",.*", "", comb_data$production_companies[i])
}



#Production countries
comb_data$production_countries <-
  as.character(comb_data$production_countries)

comb_data$production_countries <-
  as.character(comb_data$production_countries)

for (i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <-
    gsub(']', "", gsub('"', "", strsplit(substring(
      gsub(
        "[[{'':0-9},] | +(name)+",
        "",
        comb_data$production_countries[i]
      ),
      26,
      60
    ), "id")))
  
  
}
for (i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <-
    gsub("\'.*", "", comb_data$production_countries[i])
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <-
    str_replace(comb_data$production_countries[i], "character", "NA,")
}

for (i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <-
    gsub(",.*", "", comb_data$production_countries[i])
}



#drop unwanted columns

up_comb_data <-
  comb_data[,-which(
    names(comb_data) %in% c(
      "belongs_to_collection",
      "homepage",
      "imdb_id",
      "original_title",
      "overview",
      "popularity",
      "poster_path",
      "release_date",
      "genres",
      "spoken_languages",
      "status",
      "tagline",
      "title",
      "Keywords",
      "Source"
    )
  )]

#create visualization
vis_genre <- as.data.frame(colSums(up_comb_data[, 11:31]))
colnames(vis_genre) <- c("Frequency")
#vis_genre$Genre<-rownames(vis_genre)

par(mar = c(2, 6, 2, 0.5) + .1)
barplot(
  vis_genre$Frequency,
  horiz = TRUE,
  las = 1,
  names.arg = rownames(vis_genre)
)
title("Genre Preference", adj = 0)

write.csv(up_comb_data,"up_comb_data.csv")
