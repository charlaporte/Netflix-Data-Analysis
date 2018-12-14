library(tidyverse)
library(readr)
library(stringr)

netflix <- read_csv("../NetflixViewingHistory.csv") 

netflix$Date <- parse_date(netflix$Date, '%d/%m/%y')

#View by count by date
netflix %>%
  count(Date) %>%
  arrange(desc(n))%>%
  ggplot(aes(Date,n, color = n))+
  geom_col()

  

#What's the last episode I watched?
netflix %>%
  filter(str_detect(netflix$Title,'Big Bang'))%>%
  separate(col = Title, into = c("title","season","title_episode"), sep = ':')%>%
  map_df(str_trim)
  
#Separate & find what's the serie I watched the most ?
net_serie <- netflix %>%
              separate(col = Title, into = c("title","season","title_episode"), sep = ': ')%>%
              map_df(str_trim)%>%
              count(by= title)%>%
              arrange(desc(n))%>%
              select('title' = by, everything())%>%
              mutate(title = recode(.$title,'Star Wars'= 'Star Wars: The Clone War'))


#plot the top 5 series watched
net_serie %>%
  top_n(5)%>%
  ggplot(aes(reorder(title,-n),n))+
  geom_col()+
  labs(x = 'Title', y ='Number of episodes viewed', title= 'Top 5 series viewed on Netflix')+
  theme_classic()
