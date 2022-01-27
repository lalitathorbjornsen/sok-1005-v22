# Nødvendige pakker
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(zoo)
library(cowplot)
library(corrr)
library(scales)

# Først laster jeg ned datasettet med fread
data <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

# Deretter blir datasettet ryddes og unødvendig data blir fjernet
# Fjerner alt mellom land og AUST
# Velge vekk 1978, slik at rollmwan funker riktig og at nye tall blir tatt med 
# ved oppdateringer

data <- subset(data, select = -c(Land:AUST)) %>% 
  filter(Year != min(Year), Year != max(Year))

# Viser om det er numeric eller character
sapply(data, class)

# Gjør om til nummeric
data$Year <- as.numeric(data$Year)
data$Globe <- as.numeric(data$Globe)
data$Mo <- as.numeric(data$Mo)

#Sjekker igjen
sapply(data, class)

# 13 = starter å telle gjennomsnitt fra den 13 verdien
data = data %>% 
  mutate(rolltemp = rollmean(Globe, 13, fill=NA, align="right"), 
         Dato = as.yearmon(paste(data$Year, data$Mo), "%Y %m"))

# Gjør om dato som var numeric til Date for å vise fram alle årene på x-aksen
data$Dato <- as.Date(data$Dato)

#Lager graf

plot <- data %>%
  ggplot(aes(x=Dato, y=Globe)) + 
  geom_line(y=data$rolltemp, color="red" ) + geom_point(color="blue") + 
  labs(title = "Latest Global Average Tropospheric Temperatures", 
       x = "Year", y = "Change in Temperature, C") + theme_bw()

plot
# Endrer X-aksen til å få alle årene
# Setter blå strek på y aksen
plot + scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle=90, vjust =1, # endrer avstand på x-aksen
                                   hjust=1)) +
  coord_cartesian(ylim = c(-0.7, 0.9)) + geom_hline(yintercept = 0, colour="grey")


# Oppgave 2

#Først leser jeg alle datasettene og gir de et navn hvert.
#Deretter fjerner jeg unødvendige data
#Lager ny variabel til alle datasettene
Lower_Troposphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

Low_T <- Lower_Troposphere %>%
  select(Year, Mo, NoPol) %>% 
  mutate(Where = "Low_trop")

Mid_Troposphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")


Mid_T <- Mid_Troposphere %>%
  select(Year, Mo, NoPol) %>% 
  mutate(Where = "Mid_trop")

Tropopause <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")

Trop <- Tropopause %>%
  select(Year, Mo, NoPol) %>% 
  mutate(Where = "Tropo")  

Lower_Stratosphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")


Low_S <- Lower_Stratosphere %>%
  select(Year, Mo, NoPol) %>% 
  mutate(Where = "Low_strop")

sapply(Low_S, class)

# Gjør om alle variabelene til character 

Low_S$Year <- as.character(Low_S$Year)

Low_S$Mo <- as.character(Low_S$Mo)

Low_S$NoPol <- as.character(Low_S$NoPol)

Low_S$Where <- as.character(Low_S$Where)

# Sjekker
sapply(Low_S, class)



#Setter sammen datasett, to og to
H <- merge(Low_T, Mid_T, all = TRUE)
G <- merge(Trop, Low_S, all = TRUE)

#Gjør om til ent datasett
total <- merge(H, G, all = TRUE)

sapply(total, class)

total <- total %>%
  filter(Year != min(Year), Year != max(Year))


# Gjør År, Mo og NoPol til nummeric
total$Year <- as.numeric(total$Year)
total$NoPol <- as.numeric(total$NoPol)
total$Mo <- as.numeric(total$Mo)

sapply(total, class)


# plot med dato
total = total %>% 
  mutate(NoPol = rollmean(NoPol, 13, fill=NA, align="right"), 
         Dato = as.yearmon(paste(total$Year, total$Mo), "%Y %m"))

total$Dato <- as.Date(total$Dato)

#Graf

graf <- total %>%
  ggplot(aes(x = Dato, y = NoPol, colour=Where)) +
  geom_line(size=1) + labs(title="Latest Global Average Temperature",
                           x ="Year",
                           y = "Temperature in C from 60 til 90 degree North")
+ theme_bw() + geo_hline(yintercept = 0, colour="grey") +
  scale_y_continuous(breaks= scales::pretty_breaks(n=20), expand = expansion(add = 1))

graf

graf + scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  theme(axis.text.x = element_text(face="bold", angle=90, vjust =1,
                                   hjust=1)) 


