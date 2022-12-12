library(dplyr)
  
# Preprocess Data
## 1) Data Cleaning
#### · Artist Data

   
#load Artist data
artist_data1= read.csv("artists_features_final.csv")
  
# Check missing value
sum(is.na(artist_data1))
colSums(is.na(artist_data1))

# Check duplicated values
artist_data1<-artist_data1[!duplicated(artist_data1),]

    

#1. Splitting location column into 3 column – country, state, and city
   


countries=c()
states=c()
cities=c()

for (i in seq(1,length(artist_data1$locationInfo),1)){
  #print(i)
  if (!is.na(artist_data1$locationInfo[i])) {
    location= strsplit(artist_data1$locationInfo[i], "[,]")[[1]]
    country=location[1]
    state=location[2]
    city=location[3]
    #print(i)
    
    artist_data1$country[i]<-country
    artist_data1$states[i]<-state
    artist_data1$cities[i]<-city
    
  }
  else{
    
    artist_data1$country[i]<-NA
    artist_data1$states[i]<-NA
    artist_data1$cities[i]<-NA
    
  }
}
    

#2.Replacing name of some contries for easy geographical location 
   
# Replacing England, Scotland, Wales and Northern Ireland by united kingdom for easy geographical location 
artist_data1$country[artist_data1$country == 'England'] <- 'United Kingdom'
artist_data1$country[artist_data1$country == 'Scotland'] <- 'United Kingdom'
artist_data1$country[artist_data1$country == 'Wales'] <- 'United Kingdom'
artist_data1$country[artist_data1$country == 'Northern Ireland'] <- 'United Kingdom'
# Replacing the netherlands by the  netherlands for easy geographical location 
artist_data1$country[artist_data1$country == 'The Netherlands'] <- 'Netherlands'
table(artist_data1$country)


    

#3. Deleting used locationInfo column
   
artist_data1 <- select(artist_data1, -locationInfo)
artist_data1
    

#4. rename several columns – id
   
artist_data1 <- rename(artist_data1, "id_artist" = "id")
artist_data1
    



####  SONG Data


   
# load SONG data
song_data1= read.csv("song_features_final.csv")
song_data1
    

   
# Check missing value
colSums(is.na(song_data1))
song_data1
    

#1. Extract year from publication data 
   
for (i in seq(1,length(song_data1$id),1)){
  #print(i)
  
  if(!is.na(song_data1$publicationDate[i])) {
    
    lifeSpan_end= strsplit(song_data1$publicationDate[i], "[-]")[[1]]
    year_end=lifeSpan_end[1]
    
    #print(i)
    
    song_data1$publicationDate[i]<-year_end
  }
  else{
    
    song_data1$publicationDate[i]<-NA}
}

    

#2. Rename several columns with tag from song data 
   
song_data1 <- rename(song_data1, "id_song" = "id")
song_data1 <- rename(song_data1, "publicationDate_song" = "publicationDate")
song_data1 <- subset(song_data1, select=-availableCountries)
colSums(is.na(song_data1))
song_data1
    

#### · ALBUMS


   
#load Album data
album_data1= read.csv("albums_features_final.csv")
album_data1
    

   
# Check missing value
colSums(is.na(album_data1))

    


#1. Extract year from publication data 
   
for (i in seq(1,length(album_data1$id_album),1)){
  
  
  if(!is.na(album_data1$publicationDate[i])) {
    year= strsplit(album_data1$publicationDate[i], "[-]")[[1]]
    
    
    album_data1$publicationDate[i]<-year[1]
  }
  
  else{
    
    album_data1$publicationDate[i]<-album_data1$publicationDate[i]
    
  }
}

#Compare missng value 

colSums(is.na(album_data1))
album_data1
    




#2. Rename several columns with tag from album data - publicationDate, genre, deezerFans
   
album_data1 <- rename(album_data1, "publicationDate_album" = "publicationDate")
album_data1 <- rename(album_data1, "genre_album" = "genre")
album_data1 <- rename(album_data1, "deezerFans_album" = "deezerFans")

    

## 2) Merge Data

#1. Joining Song data and Album data per album_id
   
album_song<- merge(song_data1, album_data1, by = "id_album")
album_song
colSums(is.na(album_song))
    

   
# filtering data by country, publicationDate
album_song<- album_song %>% 
  filter(country != "") %>% filter(!is.na(country)  & !is.na(publicationDate_song) & !is.na(publicationDate_album) & !is.na(id_song))
album_song$publicationDate<- as.numeric(album_song$publicationDate_song)
album_song$publicationDate<- as.numeric(album_song$publicationDate_album)

# count number of songs per yer 
album_song %>% 
  group_by(publicationDate_album) %>% 
  summarize(count_song = n_distinct(id_song))
    

#2. Joining Album_Song data and Artist data per id_artist
   
# merge all the data
all_data<- merge(album_song, artist_data1, by = "id_artist")

# convert missing value in gender to "Unknown"
all_data$gender[is.na(all_data$gender)] <- "Unknown"

# rename of the column publicationDate_album to year
all_data<- rename(all_data,"year"="publicationDate_album")

# check missing value
all_data
colSums(is.na(all_data))

    


## 4)	Transforming Data

#1. Group and summarise data by year,gender,country,genre_album,lifeSpan.ended,continent
   
global_data1<-all_data %>% group_by(year,gender,country,genre_album,lifeSpan.ended) %>%
  summarize(count_song = n_distinct(id_song),
            count_album = n_distinct(id_album),
            count_artist = n_distinct(id_artist),
            deezer_fans=sum(deezerFans_album),
            average_availableCountries = mean(count_availablecountry,trim = 1),
            average_fans = mean(deezerFans_album,trim = 1))

global_data1<- rename(global_data1,"genre"="genre_album") # rename column genre
global_data1

    


#2. Categorize values of genre – 182 to 9
   
# check the variables of genre
table(global_data1$genre) 
    

   
# create categorizing lists

rock_list<- list("Rock","Soft Rock","Rap Rock","Pop Rock","Piano Rock","Symphonic Rock","Electronic Rock",
                 "Progressive Rock","Indie Rock","Industrial Rock","Gothic Rock","Glam Rock","Hard Rock","Deutschrock","Experimental Rock","Folk Rock","Art Rock","J-Rock","Psychedelic Rock","Alternative Rock")
hip_hop_list<- list("Hip Hop","Trip Hop","Southern Hip Hop","Trip Hop",
                    "Hardcore Hip Hop","Christian Hip Hop","Australian Hip Hop")
pop_list<-list("Pop","Electropop","Synthpop","Psychedelic Pop","Power Pop","Experimental Pop","Pop Punk","Indie Pop","French Pop")
folk_list<- list("Folk","Filk")
punk_list<- list("Steampunk","Post-Punk","Punk Rock","Hardcore Punk","Horror Punk")

metal_list<-list("Black Metal","Glam Metal","Melodic Death Metal","Metalcore","Progressive Metal","Power Metal",
                 "Folk Metal","Death Metal","Heavy Metal","Gothic Metal","Nu Metal")
    


   
# replace original value to categorized value
for (i in 1:nrow(global_data1)){
  if(!is.na(global_data1$genre[i])){
    if(global_data1$genre[i] %in% rock_list ){
      global_data1$genre[i]<-"Rock"
    }
    else if (global_data1$genre[i] %in% hip_hop_list ){
      global_data1$genre[i]<-"Hip Hop"
    }
    else if (global_data1$genre[i] %in% pop_list ){
      global_data1$genre[i]<-"Pop"
    }
    else if (global_data1$genre[i] %in% metal_list ){
      global_data1$genre[i]<-"Metal"
    }
    else if (global_data1$genre[i] %in% folk_list ){
      global_data1$genre[i]<-"Folk"
    }
    else if (global_data1$genre[i] == "Jazz"){
      global_data1$genre[i]<-"Jazz"
    }
    else if (global_data1$genre[i] =="Country" ){
      global_data1$genre[i]<-"Country"
    }
    else if (global_data1$genre[i] =="R&amp;B"|global_data1$genre[i] =="Contemporary R&amp;B"){
      global_data1$genre[i]<-"R&B"
    }
    else{
      global_data1$genre[i]<-"Others"
    }
  }
}
table(global_data1$genre)

    

#3. Create New column related to location – lat, lon, continent by combining external data
   
global_data1$lat <- NA
global_data1$lon <- NA
global_data1$continent <- NA

global_data1<- data.frame(global_data1)



coord_cap <- read.csv(file = "Capitals_lon_lat.csv")
coord_cap$CapitalLatitude <- as.character(coord_cap$CapitalLatitude)
coord_cap$CapitalLongitude <- as.character(coord_cap$CapitalLongitude)


# get coordinates for each country
for (i in 1:nrow(global_data1)) {
  if ((global_data1[i,3] %in% coord_cap$CountryName) == TRUE){
    global_data1$lat[i] <- coord_cap[which(coord_cap$CountryName == as.character(global_data1[i,3])),3]
    global_data1$lon[i] <- coord_cap[which(coord_cap$CountryName == as.character(global_data1[i,3])),4]
    global_data1$continent[i] <- coord_cap[which(coord_cap$CountryName == as.character(global_data1[i,3])),6]
  } else {
    global_data1$lat[i] <- "Unknown"
    global_data1$lon[i] <- "Unknown"
  }
}

# replace missing value in genre and country to Unknown
global_data1$genre[is.na(global_data1$genre)] <- "Unknown"
global_data1$country[is.na(global_data1$country)] <- "Unknown"

colSums(is.na(global_data1))
global_data1
table(global_data1$continent)
    
#4. Re-Categorize values of continent – Central America to North America
   
# check the value of continent
# here we can see that  Canada  Guatemala Mexico United States are considered as central america country. 
dd=global_data1%>% filter(continent=="Central America")%>% select(country)
table(dd)
# lets change that to north america
global_data1$continent[global_data1$continent == "Central America"] <- 'North America'
table(global_data1$continent)
    

   
# check other values of continent. 
dd=global_data1%>% filter(continent=="South America")%>% select(country)
table(dd)

    

   
# our final dataset
global_data1
    


   
#save the file
write.csv(global_data1,"final_data.csv")
    










