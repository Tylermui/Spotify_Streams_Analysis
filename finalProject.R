library(tidyverse)
library(tidymodels)
library(dplyr)
#This is my path to my CSV file, so each one of yours will be a little different
setwd('~/School_Work/CS229 Intro to Data Science with R/FinalProject/Data')
getwd()
spotifyData = read_csv('spotify-2023.csv')
view(spotifyData)
#changing variable types of columns
spotifyData$streams = as.integer(spotifyData$streams)
spotifyData$bpm = as.integer(spotifyData$bpm)
spotifyData$key = as.factor(spotifyData$key)
#checking to make sure the new types are set
class(spotifyData$streams)
class(spotifyData$bpm)
class(spotifyData$key)
view(spotifyData)

#things we want to aggregate
# *note streams is what makes a song popular, the more streams the more more popular
#spotify and apple playlists making it popular?
#what bpm makes it popular?
#time of the year song is released?

#If a song is in an apple playlist
ApplePlaylists_Streams = spotifyData %>% 
  select(track_name,in_apple_playlists, streams) %>% 
  na.omit()
ApplePlaylists_Streams
#you can see a positive correlation coefficient
ApplePlaylists_Streams_Plot = ggplot(ApplePlaylists_Streams, aes(x = in_apple_playlists, y = streams)) +
  scale_y_continuous(
    breaks = seq(0,2500000000, by = 500000000),
    labels = c("0","500M", "1B", "1.5B", "2B", "2.5B")) +
  scale_x_continuous(breaks = seq(0,600, by = 100)) +
  xlab("Counts in Apple Music Playlists") +
  ylab("Streams") +
  ggtitle("Streams When Found in an Apple Music Playlist") +
  geom_point()
ApplePlaylists_Streams_Plot

#If a song is in a spotify playlist
SpotifyPlaylists_Streams = spotifyData %>% 
  select(track_name,in_spotify_playlists, streams) %>% 
  na.omit()
SpotifyPlaylists_Streams
#you can see a positive correlation coefficient
SpotifyPlaylists_Streams_Plot = ggplot(SpotifyPlaylists_Streams, aes(x = in_spotify_playlists, y = streams)) +
  scale_y_continuous(
    breaks = seq(0,2500000000, by = 500000000),
    labels = c("0","500M", "1B", "1.5B", "2B", "2.5B")) +
  scale_x_continuous(breaks = seq(0,60000, by = 10000)) +
  xlab("Counts in Spotify Playlists") +
  ylab("Streams") +
  ggtitle("Streams When Found in a Spotify Playlist") +
  geom_point()
SpotifyPlaylists_Streams_Plot

#bpm compared to streams
BPM_Streams = spotifyData %>% 
  select(track_name, bpm, streams) %>% 
  na.omit() %>% 
  group_by(bpm) %>% 
  summarise(meanstreams = mean(streams))
view(BPM_Streams)
#can't really see too much, correlation coefficient is 0, kinda surprised
BPM_Streams_Plot = ggplot(BPM_Streams, aes(x = bpm, y = meanstreams)) +
  scale_y_continuous(
    breaks = seq(0,2500000000, by = 500000000),
    labels = c("0","500M", "1B", "1.5B", "2B", "2.5B")) +
  scale_x_continuous(breaks = seq(50,225, by = 25)) +
  xlab("Beats Per Minute (BPM)") +
  ylab("Streams") +
  ggtitle("Average Streams per Beats Per Minute") +
  geom_col()
BPM_Streams_Plot

#Key to streams
#creating the data frame of keys and streams
key_2023 = spotifyData %>%
  select(key, streams) %>% 
  na.omit 
#creating the avergae of each stream
key_summary = key_2023 %>% 
  group_by(key) %>% 
  summarise(meanstreams = mean(streams))
view(key_summary)
#average streams per key graph, we can see that songs in D# are the most popular, whilst songs in A are the least popular, on average
KeyPlot = ggplot(key_summary, aes(x = key, y = meanstreams)) +
  scale_y_continuous(
    breaks = seq(100000000,600000000, by = 100000000),
    labels = c("100M", "200M", "300M", "400M", "500M", "600M")) +
  ylab("Average Streams") +
  xlab("Key")+
  ggtitle("Average Streams per Key") +
  geom_col()
KeyPlot


#Release month compare to Streams
Month_Mean_Streams = spotifyData %>% 
  select(released_month, streams) %>% 
  na.omit() %>% 
  group_by(released_month) %>% 
  summarise(meanstreams = mean(streams))
#plotting the mean streams for each month
Month_Mean_Streams_Plot = ggplot(Month_Mean_Streams, aes(x = released_month, y = meanstreams)) +
  scale_x_continuous(
    breaks = seq(1,12, by = 1),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(
    breaks = seq(100000000,700000000, by = 100000000),
    labels = c("100M", "200M", "300M", "400M", "500M", "600M", "700M")) +
  ylab("Average Streams") +
  xlab("Release Month")+
  ggtitle("Average Streams per Release Month") +
  geom_col()
Month_Mean_Streams_Plot
