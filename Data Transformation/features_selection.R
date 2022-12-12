


library(readr)
library(dplyr)
library (stringr)


# Song Data
songs <- readRDS("C:/Users/Alimat sadia/Desktop/MSC DS &  AI/Data visualization/wasabi data22/OneDrive_1_11-19-2022/data/songs_all_artists_3000.rds")

colnames(songs)
colSums(is.na(songs))

song_features <- songs %>% select(id, id_album,publicationDate, genre,availableCountries)
song_features<- song_features %>% filter(!is.na(availableCountries)) %>%
  mutate(count_availablecountry = str_count(availableCountries, ",")+1)


song_features[song_features== "NULL"|song_features== ""] <- NA



# Album
albums <- readRDS("C:/Users/Alimat sadia/Desktop/MSC DS &  AI/Data visualization/wasabi data22/OneDrive_1_11-19-2022/data/albums_all_artists_3000.rds")


albums <- rename(albums, "id_album" = "_id")
colnames(albums)
colSums(is.na(albums))
albums_features <- albums %>% select(id_artist, id_album, genre, publicationDate, deezerFans)
albums_features[albums_features== "NULL"|albums_features== ""] <- NA
sum(is.na(albums_features))
colSums(is.na(albums_features))

# Artists
# Load all data

artists<- readRDS("C:/Users/Alimat sadia/Desktop/MSC DS &  AI/Data visualization/wasabi data22/OneDrive_1_11-19-2022/data/wasabi_all_artists_3000.rds")
artists_features <- artists %>% select(id,gender, lifeSpan.ended,locationInfo)
artists_features[artists_features== "NULL"|artists_features== ""] <- NA
sum(is.na(artists_features))
colSums(is.na(artists_features))

write.csv(song_features, "song_features_final.csv")
write.csv(albums_features, "albums_features_final.csv")
write.csv(artists_features, "artists_features_final.csv")
