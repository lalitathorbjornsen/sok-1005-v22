#Mappeoppgave 2:
# Oppgave 1:

# Laster ned nødvendige pakker
library(tidyverse)
library(rjson)
library(ggplot2)
library(jsonlite)

data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json?fbclid=IwAR2tUvgmwMQ0pNjozAhI5_DXALFVBfw6o2syxGFiZSBJi7VM40rV3Z3FIXY")

#sjekke structure
summary(data)
sapply(data, class)

#velge riktige variabler ved hjelp av select
data <- data %>%
  select(name, deaths_per_100k, fully_vaccinated_pct_of_pop)


# plot
plot <- data %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y= deaths_per_100k, label=name)) + 
  geom_point(shape = 21, color = "grey", fill= "darkseagreen3", size = 3)  +
  geom_text(size=3, check_overlap= TRUE) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated ",
       y = "Avg. monthly death per 100 000", 
       caption = "Source: New York Times database") + 
  scale_x_continuous(breaks= scales::pretty_breaks(n=5), labels = scales::percent) +
  ylim(1, 20) +
  theme(text = element_text(family="Times New Roman", face="bold")) + 
  theme_bw()

plot

# Oppgave 2

# lage en lineær regresjon funksjon som viser sammenheng mellom x-aksen og y-aksen
regression <- lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data=data)
summary(regression)

# lm() funksjonen viser i console at mimnum avstanden fra datasettet og 
# den lineære regresjonen/linja er på -5.968 mens maks avstanden er på 7.635.
# Stigningstallet er negativ med -36.663 siden det er negativ sammenheng. 

# Plot med lineære regresjon
# Alternativ 1
plot2 <- data %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label=name)) + 
  geom_point(shape = 21, color = "grey", fill= "darkseagreen3", size = 3) +
  geom_text(size=3, check_overlap= TRUE) + 
  geom_smooth(method='lm', col="red", size=0.4) + 
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated ",
       y = "Avg. monthly death per 100 000", 
       caption = "Source: New York Times database") + 
  scale_x_continuous(breaks= scales::pretty_breaks(n=5), labels = scales::percent) +
  ylim(1, 20) +
  theme(text = element_text(family="Times New Roman", face="bold")) + 
  theme_bw()

plot2

# Alternativ 2:
plot +
  geom_smooth(method='lm', col="red", size=0.4)


