#libraries

install.packages("tidyverse")
library(tidyverse)
library(gapminder)
library(dplyr)
library(ggplot2)
install.packages("plotly")
library(plotly)
install.packages("shiny")
library(scales)


#data de base

names(unicef_indicator_1.V3)[names(unicef_indicator_1.V3) == "vv"] <- "Child_labor_in_percentage"
names(unicef_metadata)[names(unicef_metadata) == "Life.expectancy.at.birth..total..years."] <- "Life_expectancy_at_birth"
data_join_alpha <- full_join(unicef_metadata, unicef_indicator_1.V3, by = join_by(country == country, year == year))
data_join_alpha <- full_join(data_join_alpha, countryContinent, by = join_by(country == country, numeric_code == country_code))
data_join_beta <- full_join(unicef_indicator_1.V3, unicef_metadata, by = join_by(country == country, year == year))
data_join_beta <- full_join(data_join_beta, countryContinent, by = join_by(country == country, numeric_code == country_code))

data_join_alpha <- data_join_alpha %>%
  mutate(continent = ifelse(country == "Macedonia, the former Yugoslav Republic of", "Europe", continent),
         sub_region = ifelse(country == "Macedonia, the former Yugoslav Republic of", "Southern Europe", sub_region))

data_join_alpha <- data_join_alpha %>%
  mutate(continent = ifelse(country == "Palestinian Territory, Occupied", "Asia", continent),
         sub_region = ifelse(country == "Palestinian Territory, Occupied", "Western Asia", sub_region))

data_join_beta <- data_join_beta %>%
  mutate(continent = ifelse(country == "Macedonia, the former Yugoslav Republic of", "Europe", continent),
         sub_region = ifelse(country == "Macedonia, the former Yugoslav Republic of", "Southern Europe", sub_region))

data_join_beta <- data_join_beta %>%
  mutate(continent = ifelse(country == "Palestinian Territory, Occupied", "Asia", continent),
         sub_region = ifelse(country == "Palestinian Territory, Occupied", "Western Asia", sub_region))

data_join_alpha_nosex <- filter(data_join_alpha, sex == "Total")
data_join_beta_nosex <- filter(data_join_beta, sex == "Total")
data_join_beta_nototal <- filter(data_join_beta, sex != "Total")




map_world <- map_data("world")

# Carte 1 

  ## Data pour carte 1
map_1<- full_join(data_join_alpha_nosex, map_world, by = c("country" = "region"))

  ## script carte 1


map_1_subset <- subset(map_1, long > -170 & long < 180 & lat > -55 & lat < 90)

map_1_subset %>%
  ggplot() +
  scale_fill_viridis(option = "C", na.value = "grey") +
  aes(x = long, y = lat, group = group, fill = Child_labor_in_percentage) +
  labs(
    title = "Differences between countries regarding child labor",
    subtitle = "Countries in grey have no data",
    caption = "Source: Unicef",
    x = "Longitude",
    y = "Latitude",
    fill = "Child labor in %"
  ) +
  geom_polygon(color = "black") +
  theme_bw() +
  guides(fill = guide_colorbar(title = "Child labor in %"))


#barchart 1 

## Data pour barchart 1
data_join_alpha_sex <- filter(data_join_alpha, sex != "Total")
data_join_alpha_sex <- filter(data_join_alpha, continent != "NA")


## Plots monde et continents

plot_europe <- data_join_beta %>%
  filter(continent == "Europe", sex != "Total", !is.na(Child_labor_in_percentage), !is.na(sex)) %>%
  ggplot(aes(x = sex, y = Child_labor_in_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_region, scales = "fixed") +
  labs(title = "Europe", x = "Sexe", y = "Child labor in %") +
  theme_minimal()

ggplotly(plot_europe, tooltip = "Child_labor_in_percentage")


plot_africa <- data_join_beta2 %>%
  filter(continent == "Africa", sex != "Total", !is.na(Child_labor_in_percentage), !is.na(sex)) %>%
  ggplot(aes(x = sex, y = Child_labor_in_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_region, scales = "fixed") +
  labs(title = "Africa", x = "Sex", y = "Child labor in %") +
  theme_minimal()

ggplotly(plot_africa)

plot_asia <- data_join_beta2 %>%
  filter(continent == "Asia", sex != "Total", !is.na(Child_labor_in_percentage), !is.na(sex)) %>%
  ggplot(aes(x = sex, y = Child_labor_in_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_region, scales = "fixed") +
  labs(title = "Asia", x = "Sex", y = "Child labor in %") +
  theme_minimal()

ggplotly(plot_asia)

plot_americas <- data_join_beta2 %>%
  filter(continent == "Americas", sex != "Total", !is.na(Child_labor_in_percentage), !is.na(sex)) %>%
  ggplot(aes(x = sex, y = Child_labor_in_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_region, scales = "fixed") +
  labs(title = "Americas", x = "Sex", y = "Child labor in %") +
  theme_minimal()

ggplotly(plot_americas)


plot_oceania <- data_join_beta2 %>%
  filter(continent == "Oceania", sex != "Total", !is.na(Child_labor_in_percentage), !is.na(sex)) %>%
  ggplot(aes(x = sex, y = Child_labor_in_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_region, scales = "fixed") +
  labs(title = "Oceania", x = "Sex", y = "Child labor in %") +
  theme_minimal()

ggplotly(plot_oceania)

data_aggregated <- data_join_beta %>%
  filter(sex != "Total") %>%
  group_by(continent, sub_region, sex) %>%
  summarise(average_childlabor = round(mean(Child_labor_in_percentage, na.rm = TRUE), 2))

data_aggregated_clean <- na.omit(data_aggregated)

plot_continents <- data_aggregated_clean %>%
  ggplot(aes(x = sex, y = average_childlabor, fill = sex)) +
  guides(fill ="none") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~continent, scales = "fixed") +
  labs(title = "Average Child labor par Sex et Continent", x = "", y = "Average child labor in %") +
  theme_minimal()

ggplotly(plot_continents)

##scaterplot pop x lifeExp 

scaterplot_popandlifeexp <- data_join_alpha %>%
  filter(!is.na(Population..total) & !is.na(Life_expectancy_at_birth)) %>%
  ggplot(aes(x = Population..total, y = Life_expectancy_at_birth, color = continent, group = country)) +
  geom_point() +
  labs(title = "titre à définir", x ="Child labor in %" , y = "Life expectancy at birth", color ="Continents") +
  theme_minimal()

scaterplot_popandlifeexp

## scaterplot vv x lifeExp

scaterplot_vvandlifeexp <- data_join_beta_nosex %>%
  filter(!is.na(Child_labor_in_percentage) & !is.na(Life_expectancy_at_birth)) %>%
  ggplot(aes(x = Child_labor_in_percentage, y = Life_expectancy_at_birth, color = continent, group = country)) +
  geom_point(alpha = 0.8, size = 4) +
  labs(title = "titre à définir", x ="Child labor in %" , y = "Life expectancy at birth", color ="Continents") +
  theme_minimal()


ggplotly(scaterplot_vvandlifeexp)



## Évolution 

data_timeseries <- full_join(unicef_metadata, countryContinent, by = join_by(country == country, numeric_code == country_code))
data_timeseries <- filter(data_timeseries, continent %in% c("Africa", "Americas", "Asia", "Europe", "Oceania"))

ggplot(data_timeseries) +
  aes(x = year, y = Life_expectancy_at_birth, color = continent) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", colour = "black", linetype = 2) +
  facet_wrap(~ continent, nrow = 1) +
  theme_classic()


time_serie_lifeexp_continents <- data_timeseries %>%
  ggplot(aes(x = year, y = Life_expectancy_at_birth, fill = country, color = continent)) +
  geom_point(alpha = 0.4, size = 1) +
  theme_bw() +
  facet_wrap(~continent, nrow = 1) +
  scale_x_continuous(breaks = c(1960, 1980, 2000)) +
  guides(color = "none", fill = "none") +
  labs(x = "Time evolution", y = "Life expectancy at birth") 
  
  ggplotly(time_serie_lifeexp_continents, tooltip = c("country", "year", "Life_expectancy_at_birth"))
  

time_serie_lifeexp_world <- data_timeseries %>%
  ggplot(aes(x = year, y = Life_expectancy_at_birth, color = continent)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", aes(color = "Global")) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  labs(x = "Time evolution", y = "Life expectancy at birth")

ggplotly(time_serie_lifeexp_world)

time_serie_lifeexp_world <- data_timeseries %>%
  ggplot(aes(x = year, y = Life_expectancy_at_birth, color = continent)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", aes(color = "Global"), linetype = 2) +  # Ajouter une seule ligne de régression pour l'ensemble des données
  theme_bw() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  labs(x = "Time evolution", y = "Life expectancy at birth", color = "Continent") +
  guides(color = guide_legend(title = NULL))

time_serie_lifeexp_world <- time_serie_lifeexp_world + 
  scale_color_manual(values = "black", name = "Continent", labels = "Global")


ggplotly(time_serie_lifeexp, tooltip = c("country", "year", "Life_expectancy_at_birth"))
         