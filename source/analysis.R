#install.packages("maps")
#install.packages("mapproj")
library("mapproj")
library("maps")
library("dplyr")
library("ggplot2")
library("leaflet")
library("tidyverse")
library("plotly")

incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)

#introduction questions answers
# black_pop_in_1990 <- black_people_jail_population %>% 
#   slice(which.max(percentage)) %>% 
#   pull(percentage)
# print(black_pop_in_1990)


# Chart 1
black_people_jail_population <- incarceration_trends %>%
  filter(state == "WA") %>% 
  select(year, state, county_name, total_pop_15to64,black_pop_15to64)

black_people_jail_population <- mutate(
  black_people_jail_population,
  percentage = replace_na(black_pop_15to64/total_pop_15to64),
  )


chart1 <- plot_ly(
  data = black_people_jail_population,
  x = ~year,
  y = ~percentage,
  type = "bar"
) %>% 
  layout(title = "The incarceration rate of black people in Washington",
    yaxis = list(title = "percentage of black people in jail"),
    xaxis = list(title ="Years"))
chart1


# Chart 2

BvW_people_jail_population <- incarceration_trends %>%
  filter(state == "NY") %>%
  filter(county_name =="New York County") %>%
  filter(year <= 2000) %>%
  summarise(year, total_pop_15to64,
            white_pop_15to64,
            black_pop_15to64)%>%
  drop_na()

chart2 <- ggplot(BvW_people_jail_population) +
  geom_line(mapping = aes(x=year, y=white_pop_15to64),color = "darkred") +
  geom_line(aes(x=year, y=black_pop_15to64),color="steelblue") +
  labs(
    title = "white and black incarsation rates in Newyork",
    x = "Years",
    y = "incarceration rates",
  )
chart2 + annotate("text", x = 1992.5, y = 2000000, label = "White population")+
  annotate("text", x = 1992.5, y = 1300000, label = "Black population")

# # Map


large_jails <- incarceration_trends  %>%
  filter(year == 2018) %>% 
  select(year, county_name, total_pop_15to64,black_pop_15to64, fips=fips) %>% 
  mutate(percentage = replace_na(black_pop_15to64/total_pop_15to64)) %>% 
  drop_na()

map_data <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = "," )

join <- full_join(map_data, county.fips, by = "polyname")

join_map <- left_join(large_jails, join, by = "fips")

blank_theme <- theme_bw() +
   theme(
     axis.line = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     axis.title = element_blank(),
     plot.background = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank()
   )

VIZ <- ggplot(data = join_map) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = percentage ),
    color = "white",
    size = .1 
  ) +
  coord_map() + 
  scale_fill_continuous(low = "Black", high = "Red") +
  labs(fill = "Ratio",
       title = "Ratio of black people in jail",
       subtitle = "southern states have much higher ratio of black people in jail") +
  blank_theme
VIZ
