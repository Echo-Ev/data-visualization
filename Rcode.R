library(readr)
library(tidyverse)

data1<- read_tsv("D:\\Aut_semester\\introduction to data science\\dataset\\title.basics.tsv") # Read the file
data2<- read_tsv("D:\\Aut_semester\\introduction to data science\\dataset\\title.ratings.tsv")
View(data1)
View(data2)
result <- left_join(data2, data1, by = "tconst")  #Perform a left join of data1 to data2
View(result)
result_1<-select(result,tconst,titleType,primaryTitle,originalTitle,startYear,genres,averageRating,numVotes) #Select the relevant columns
View(result_1)

result_1<-result_1[result_1$titleType=="movie",]
result_1$genres <- as.character(result_1$genres) # Ensure that the genres column is of string type
result_1$genres[result_1$genres == "\\N"] <- NA #Replace \N with NA
result_1 <- result_1[!is.na(result_1$genres), ] #Remove rows containing NA
View(result_1)

genre_seperate<-result_1 %>% separate_rows(genres,sep=",") #A single work may belong to multiple genres. These should be split, and then the average ratings for each genre should be calculated.
View(genre_seperate)

genre_seperate_rating_average<-group_by(genre_seperate,genres) %>% summarise(count=n(),AverageRating=mean(averageRating, na.rm=TRUE))
View(genre_seperate_rating_average)

library(ggplot2)
p1<-ggplot(genre_seperate_rating_average, aes(x = reorder(genres, count), y = count, fill = genres)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Movie Genres by Count",
       x = "Genres",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p1

library(treemapify)
p2<-ggplot(genre_seperate_rating_average, aes(area = count, fill = AverageRating, label=genres)) +
  geom_treemap() + geom_treemap_text(fontface="italic", colour="white", place="centre") +
  labs(title="counts and averagerating of different movie genres",
       fill="Averagerating",
       caption="IMDB Dataset")
p2

different_titletype<-select(result,tconst,titleType,primaryTitle,originalTitle,startYear,genres,averageRating,numVotes) #Select the relevant columns
View(different_titletype)

different_titletype <- different_titletype[!is.na(different_titletype$startYear), ]  # Remove rows where the 'startYear' column contains NA.
different_titletype <- different_titletype[different_titletype$startYear != "\\N", ]         # Remove rows where the 'startYear' column contains "\\N".

different_titletype <- different_titletype %>%
  mutate(startYear = as.numeric(startYear))

divided_decade<-different_titletype %>%mutate(decade=floor(startYear/10)*10)
View(divided_decade)

# Calculate the total box office for each genre in each decade.
titleType_by_decade <- divided_decade %>%
  group_by(decade, titleType) %>%
  summarise(totalVotes = sum(numVotes),AverageRating=mean(averageRating, na.rm=TRUE), .groups = 'drop') %>%
  arrange(decade, desc(totalVotes))
View(titleType_by_decade)

library(ggplot2)


p3<-ggplot(titleType_by_decade, aes(x = decade, y = totalVotes, colour = titleType, size = AverageRating, alpha = 0.5)) +
  geom_point() +
  scale_y_log10() +  # logarithmic scaling
  scale_size(range = c(1, 6), breaks = seq(5, 8, 0.5)) +  # Adjust the size range and rating interval of the points
  scale_alpha(guide = FALSE) +  
  theme_minimal() +
  labs(x = "Decade",
       y = "Total Votes (log scale)",
       size = "Average Rating",
       colour = "Title Type") +
  theme(legend.position = "right")  
p3

result_1<-select(result,tconst,titleType,primaryTitle,originalTitle,startYear,genres,averageRating,numVotes) #Select the relevant columns
result_1<-result_1[result_1$titleType=="videoGame",] # Filter the movie rows
View(result_1)

install.packages("textcat")
library(textcat)

result_1$Language = sapply(result_1$originalTitle, textcat)
View(result_1)

gather_country<-group_by(result_1,Language) %>% summarise(count=n(),TotalVotes = sum(numVotes, na.rm = TRUE))
View(gather_country)
gather_country$Language[gather_country$Language == "\\N"] <- NA #Replace \N with NA
gather_country <- gather_country[!is.na(gather_country$Language), ]
View(gather_country)

language_to_country <- data.frame(
  Language = c("afrikaans", "albanian", "basque", "bosnian", "breton", 
               "catalan", "croatian-ascii", "czech-iso8859_2", "danish", "dutch", 
               "english", "esperanto", "estonian", "finnish", "french","frisian", "german", "hungarian", "icelandic", "indonesian",
               "irish", "italian", "latin", "latvian", "lithuanian",
               "malay", "manx", "middle_frisian", "nepali", "norwegian","polish", "portuguese", "romanian", "rumantsch", "sanskrit", 
               "scots", "scots_gaelic", "serbian-ascii", "slovak-ascii","slovak-windows1250","slovenian-ascii","slovenian-iso8859_2", 
               "spanish", "swahili", "swedish","tagalog", "turkish", "ukrainian-koi8_r", "welsh"),
  Country = c("South Africa", "Albania", "Spain", "Bosnia", "France",
              "Spain", "Croatia", "Czech Republic", "Denmark", "Netherlands",
              "United Kingdom", "N/A", "Estonia", "Finland", "France","Netherlands", "Germany", "Hungary", "Iceland", "Indonesia",
              "Ireland", "Italy", "Vatican City", "Latvia", "Lithuania",
              "Malaysia", "Isle of Man", "Netherlands", "Nepal", "Norway","Poland", "Brazil", "Romania", "Switzerland", "India", 
              "Scotland", "Scotland", "Serbia", "Slovakia", "Slovakia","Slovenia","Slovenia",
              "Mexico", "Tanzania", "Sweden","Philippines", "Turkey", "Ukraine", "United Kingdom")
)

gather_country <- gather_country %>%
  left_join(language_to_country, by  = "Language") %>%
  rename(Country = Country)
View(gather_country)
#Translate language into country

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)

# Obtain map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")


# Merge into map data
world_map <- left_join(world_map, gather_country, by = c("name" = "Country"))

library(ggplot2)
library(sf)
library(viridis)  # Import viridis package for more beautiful color schemes

p4<-ggplot(data = world_map) +
  geom_sf(aes(fill = TotalVotes), color = "white") +
  scale_fill_viridis_c(name = "Total Votes", trans = "log10",  # Add logarithmic conversion
                       labels = scales::comma) +  # Format tags with commas
  labs(title = "World Map Colored by Movie Votes",
       fill = "Total Votes") +
  theme_minimal()  
p4

library(patchwork)
plot<-(p1 | p2) / 
(p3 | p4)

ggsave("combined_plot.png", plot, width = 20, height = 10, units = "cm")

