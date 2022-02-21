# Mappeoppgave 3: Skrape en html tabell fra en nettside

# Oppgave 1

#Nødvendige pakker
library(tidyverse)
library(rvest)
library(dplyr)
library(ggplot2)

#laster ned url
url <- ("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")
html <- read_html(url)
class(html)
structure(html)

#Finn html taggen og lese som tabell
html <- html_nodes(html,"table")
table <- html_table(html, fill = TRUE, header = TRUE)
print(table)

#Finner første tabell
table <- table[[1]]
table


table <- table %>%
  rename(wltp = `WLTP-tall`) %>%
  select(wltp, STOPP, `Modell (temp. varierte fra 0° til -10°)` )

# Fjerner det som ikke skal være med i variabelene
table$wltp <- substr(table$wltp,1,nchar(table$wltp)-11)
table$STOPP <- str_remove_all(table$STOPP, "km")

#Gjør om til numeric
table$wltp <- as.numeric(table$wltp)
table$STOPP <- na.omit(table$STOPP) 
table$STOPP <- as.numeric(table$STOPP)

#plot med 45 grader linje
plot <- table %>%
  ggplot(aes(x=wltp, y=STOPP)) + 
  geom_point() +
  labs(title="Oppgitt kjørelengde vs faktisk kjørelengde i km",
       x = "Oppgitt kjørelengde i km (wltp)", 
       y = "Faktisk kjørelengde i km (STOPP)", 
       caption = " Den røde linje representerer forventet kjørelengde.
       Source: motor.no
       Url: shorturl.at/mvGKU") + 
  theme(text = element_text(family="Times New Roman", face="bold")) + 
  ylim(200, 600)STOPP
xlim(200, 600) +
  geom_abline(intercept = 0, slope = 1, col="red") + 
  coord_fixed(ratio =1) +
  theme_bw()
plot


#Oppgave 2
# regression line with lm()
regression <- lm(STOPP ~ wltp, data=table)
summary(regression)

plot + geom_smooth(method='lm', col="orange", size=0.4)

# Kommentar:
# Etter å ha kjørt koden, blir det tolkes som det er avvik mellom de to variablene.
# De faktiske kjørelengdene er mindre enn de oppgitte kjørelengdene og det resulterer
# til at den oransje lineære regresjon linja er til høyre for den røde linja som 
# representerer forventet kjørelengde. 

