 ---
title: "Predicting the variation of song's popularity through different variables from 2000-2020"
author: "Yash Baliyan"
output: 
  html_document:
    theme: cerulean
    toc: true
date: '2022-08-17' 
---

```{r,echo = F,message=F}
library(readxl)
library(prettydoc)
library(spotifyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotrix)
library(kableExtra)
library(esquisse)
```

```{r, echo = F}
setwd("/users/yashbaliyan/downloads")
songs <- read.csv("song_data.csv")
```


# Introduction {.tabset}
## About Data

This data set contains of all the songs from 2000-2020, there are certain song attributes which will help me predict the popularity of songs. The song attributes like valence, danceabiolity, key can help judge the popularity and trends with the help of the GG plots.


## Summary
```{r,echo = F}
summary(songs)
str(songs)
```

## Story
My study helps people figure out what are the primary components to figure out the high song popularity of the songs, which has 15 columns and about 18000 rows of a range of songs with diverse song features. I took six song properties into account: audio valence, audio duration, key, danceability, liveness, and energy, all of which are non-binary in nature. Each of these variables contributes to the creation of a ggplot with song popularity to demonstrate the relationship between them. The histogram is the sole figure that takes into account only one variable, x; all other charts take into account two or three variables. The story's main goal is to demonstrate a link between the statistics and the tables.
 
# Figure interpretation of data {.tabset}
## Histogram
```{r}
ggplot(data = songs,aes(x=song_popularity))+geom_bar(colour="blue")
```

This histogram shows the shape of the data. It is partially skewed towards the left. most of the song popularity lie between 50 to 75. Aim of the artists is to somehow get a rating above 85.


## Scatterplot 
```{r}
sone <- songs%>%
  mutate(song_duration_minutes = (song_duration_ms/100)/60/60)

ggplot(sone) +
  aes(x = song_popularity, y = song_duration_minutes) +
  geom_point(shape = "circle", size = 1.5, colour = "#3C5074") +
  theme_light() 
```


The following is a scatter plot between song duration and its popularity, there can be seen that songs above the popularity of 80 have a smaller duration between 3 seconds to 1 minute 30 seconds.


## Line graph and scatterplot for energy and song_popularity
```{r}
 ggplot(songs) +
  aes(x = song_popularity, y = energy, fill = energy) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  scale_fill_gradient() +
  theme_minimal()

ggplot(data = songs,aes(x=song_popularity, y=energy))+geom_smooth()

```


The inference drawn here is too much energy is not good for a song. it can be seen from the songs that have a  less popularity level the energy is much higher than the songs that are extremely popular.  


## Line graph for audio_valence and song_popularity.
```{r}
ggplot(songs)+
  aes(x=song_popularity,y= audio_valence, fill=audio_valence)+
  geom_smooth(shape="circle", size=1.5, colour =" red")+
  scale_fill_gradient()+
  theme_minimal()
```


It is clear here that lesser the audio valence, the more would be the popularity of your the, eventually.


## Box plot analysis for audio_mode, key and danceability
```{r}
ggplot(data = songs,aes(x=factor(key),y=danceability,fill = factor(key)))+
  geom_boxplot()
ggplot(data = songs,aes(x=factor(key),y=danceability,fill = factor(audio_mode)))+
         geom_boxplot()
```


This is a box plot between the dacebility, key and audio mode. when the key is below 3, having an audio mode increases the dancebility on average and when key is above 3, having no audio mode does the job. 


## line graph to show the relationship b/w song_popularity and key.
```{r}
ggplot(data = songs,aes(x=song_popularity, y=key))+geom_smooth( color = "red")
Focus <- songs%>%
  filter(song_popularity>60)
ggplot(data= Focus,aes(x=song_popularity,y=key,stat_identity(na.rm=F)))+
geom_smooth(color="red")+labs(title = "focused inference", 
                   caption = "figure", x = "Song_popularity", y = "key")


```


The figure here shows the indirect relationship of key and song_popularity,i.e song_popularity depends on less keys used for a song.


## Line graph analysis for song_popularity,liveness and danceability
```{r}
ggplot(data = songs,aes(x=song_popularity, y=danceability))+geom_smooth( color = "blue")
dance <- songs%>%
filter(song_popularity>60)
ggplot(data= dance,aes(x=song_popularity,y=danceability,stat_identity(na.rm=F)))+
geom_smooth(color="red")+labs(title = "focused inference", 
                   caption = "figure", x = "Song_popularity", y = "danceability")

ggplot(data = songs,aes(x=song_popularity, y=liveness))+geom_smooth(color = "red")
live <- songs%>%
filter(song_popularity>60)
ggplot(data= live,aes(x=song_popularity,y=liveness,stat_identity(na.rm=F)))+
geom_smooth(color="red")+labs(title = "focused inference", 
                   caption = "figure", x = "Song_popularity", y = "liveness")

```


Increasing the dancebility does have an effect on song popularity. keeping it lower would restrict the song from pushing it above 60 rating. Once a song increases it, so does its danceability 

Liveliness and song_popularity has an indirect relation as you increase the livliness the popularity of the song decreases. 
 
 
## Inference drawn for danceability through scatterplot and line graph both
```{r}
ggplot(data= songs,aes(x=song_popularity,y=danceability,stat_identity(na.rm=F)))+geom_point(aes(color=danceability))+
geom_smooth(color="red")+labs(title = "inference", 
                   caption = "figure", x = "Song_popularity", y = "danceability")

zoom <- songs%>%
  filter(song_popularity>75)
ggplot(data= zoom,aes(x=song_popularity,y=danceability,stat_identity(na.rm=F)))+geom_point(aes(color=danceability))+
geom_smooth(color="red")+labs(title = "focused inference", 
                   caption = "figure", x = "Song_popularity", y = "danceability")


```


The following figure shows that popularity is constant till song_popularity is 75 i.e. from this number ownwards the ratings increases as the danceability increases.
 
 
This dataset shows that **happy** is the most popular song.


# Table {.tabset}
## Repetition of top songs. (above 95).
```{r}
sume <- songs%>%
  group_by(song_name)%>%
  filter(song_popularity>95)%>%
  summarise(song_na=n())%>%
  kbl(caption="repeated top songs",align="cc")
sume%>% kable_styling(bootstrap_options = c("striped","bordered"))%>%
  kable_material(html_font="times new roman",font_size=12) %>% 
  kable_paper(full_width=F)
```


## Top 15 songs with the help of song_popularity
```{r}
Top_Songs <- songs%>%
  filter(song_popularity>95)%>%
  select(song_name,song_popularity,danceability,key,liveness,energy,audio_valence)%>%
  arrange(desc(song_popularity),10)%>%
  kbl(caption="Top songs",align="cc")
Top_Songs%>% kable_styling(bootstrap_options = c("striped","bordered"))%>%
  kable_material(html_font="times new roman",font_size=12) %>% 
  kable_paper(full_width=F)
```


## Popularity with Danceability
```{r,}
son <- songs%>%
  select(song_name,song_popularity,danceability,liveness,audio_valence,speechiness,tempo,key,energy)%>%
  mutate(popular_dance=round((song_popularity*danceability),2))%>%
  filter(popular_dance>=80,song_popularity>95)%>%
   kbl(caption="popular_dance",align="cc")
son%>% kable_styling(bootstrap_options = c("striped","bordered"))%>%
  kable_material(html_font="times new roman",font_size=12) %>% 
  kable_paper(full_width=F)
```


# Figure interpretation of data through Regression 
```{r}
Regression <- lm(song_popularity~danceability+liveness+audio_valence+energy, data = songs)
summary(Regression)

```
