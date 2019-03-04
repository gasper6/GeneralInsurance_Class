# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
library(dplyr)
library(ggplot2)

dt_KPI <- read.csv("./Data/lesson2_KPI.csv")

# Záporné a NA poistné nahradíme nulou

dt_KPI$Premium[dt_KPI$Premium < 0] = 0
# Nemám rád dplyr

dt_KPI %>%
  mutate(Profit = Premium - Expenses - Losses) %>% 
  group_by(Unit) %>% 
  summarize(Profit = sum(Profit, na.rm = TRUE)) %>% 
  arrange(desc(Profit))
# Najlepšia unit bola 7

dt_KPI %>% filter(Unit == "Unit7") %>%
  mutate(Profit = Premium - Expenses - Losses) %>% 
  group_by(Year) %>% 
  summarize(Profit = sum(Profit, na.rm = TRUE)) %>% 
  arrange(Profit)
# najhorší rok bol 2014

dt_KPI %>% filter(Unit == "Unit7") %>%
  mutate(Profit = Premium - Expenses - Losses) %>% 
  group_by(Year) %>% 
  summarize(Profit = sum(Profit, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = Profit)) + 
  geom_col()
# Pekne nakreslíme

# Your Explanation about analysis:

# Netuším ako súvisela príprava dát s týmto cvičením, ani ako sa mala správne urobiť. Blbé dáta sa mali vynechať? Mali sa nhradiť priemerom? Čo tam bolo treba urobiť?
# Záporné premium som nahradil nulami. Pekne po starom, ako R-ku
# Potom som zisťoval ktorá unit mala najväčší zisk. Zistil som, že to bola Unit7
# Dal som si vykresliť profit Unit7 podľa rokov. Aj na grafe vidno, že najhorší rok bol 2014.
