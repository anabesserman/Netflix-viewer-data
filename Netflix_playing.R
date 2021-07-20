library(tidyr)
library(dplyr)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(extrafont)


# Read in my Netflix viewing history up until Jan 29 2021 
# (you can download yours from your Netflix account!)

netflix <- read.csv("NetflixViewingHistory.csv")
glimpse(netflix)
head(netflix)


# Using lubridate to format date (from Month/Day/Year to Year-Month-Day)

netflix$Date <- lubridate::mdy(netflix$Date)
head(netflix)


# Organizing show info: Name, Season, and Episode into their own columns 

netflix <- netflix %>% separate(Title, sep = ": ", into = c("Name", "Season", "Episode"))
head(netflix)
tail(netflix)

# Selecting TV shows only

tv <- netflix %>% 
  filter(!is.na(Episode)) 


# Top 10! (By number of episodes watched; Netflix Data starts at 2016)

tv.show <- tv %>% 
  group_by(Name) %>% 
  summarize(
    episodes_n = n()) %>% 
  arrange(desc(episodes_n)) %>% 
  head(10)

tv.show

# Adding Genre to the all around Top 10

tv.show$Genre[tv.show$Name %in% c("Gilmore Girls", "Shameless (U.S.)", "The Fosters")] <- "Drama"
tv.show$Genre[tv.show$Name %in% c("The Office (U.S.)", "The Good Place", "Schitt's Creek", "Parks and Recreation", "Grace and Frankie")] <- "Comedy"
tv.show$Genre[tv.show$Name %in% c("Terrace House", "The Great British Baking Show")] <- "Reality TV"

tv.show

# Making top 10 all time graph

tv.show %>% 
  ggplot(aes(x = reorder(Name, episodes_n, sum), y = episodes_n, fill = Genre)) +
  geom_bar(position="stack", stat="identity") + 
  coord_flip() + 
  theme(legend.position = "right", text = element_text(size=20, family="Georgia")) +
  scale_fill_manual(values = c("#E7B800","#00AFBB","#FC4E68")) +
  labs(
    title = "Ana's most watched TV shows since 2016", 
    subtitle = "ranked by number of episodes watched", 
    caption = "Data: Ana's Netflix© Viewing History",
    y = "Total Number of Episodes Watched", 
    x = "") 


ggsave("netflix_plot_tv.png", width = 16, height = 9, units = "in")


# number of episodes per day 

glimpse(tv)

daily.ep <- tv %>% 
  group_by(Name, Date) %>% 
  summarize(
    episodes_n = n(), 
    year = lubridate::year(Date)) %>% 
  group_by(Name, Date, year, month) %>% 
  summarize(
    episodes = n()) %>% 
  arrange(desc(Date))

head(daily.ep)

daily.ep %>% 
  ggplot(aes(x = Date, y = episodes)) + 
  geom_col(aes(fill = as.factor(year))) + 
  theme(text = element_text(size=16, family="Georgia"),
        axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "none", 
        axis.title.y = element_text(family="Georgia", size = 16, color = "#222222"),
        plot.caption = element_text(family="Georgia", size = 12, color = "#222222")) +
  labs(
    title = "Episodes per day (2016-2021)", 
    caption = "Data: Ana's Netflix© Viewing History",
    y = "Episodes Watched", 
    x = "")

ggsave("daily_ep.png", width = 16, height = 9, units = "in")


# Viewing by day of the week

netflix.week <- tv %>% 
  mutate(Month = month(Date, label = T),
         Year = year(Date),
         Weekdays = wday(Date, label = T, week_start = getOption("lubridate.week.start", 1))) 
netflix.week

netflix.week %>% 
  group_by(Weekdays) %>%
  summarise(
    n = n()) %>% 
  ggplot(aes(x = Weekdays, y = n)) +
  geom_col(aes(fill = if_else(Weekdays %in% c("Fri", "Sat", "Sun"), "#FC4E68", "#00AFBB"))) + 
  scale_fill_identity() + 
  labs(x="", y="",
       title = "Every day is Netflix day for Ana - especially weekends!", 
       subtitle = "Number of episodes watched per day of the week (2016-2021)",
       caption = "Data: Ana's Netflix© Viewing History") +
  theme_minimal() +
  theme(text = element_text(size=16, family="Georgia"))

ggsave("weekdays.png",  width = 12, height = 9, units = "in")



