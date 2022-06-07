library(scales)
library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(dplyr)
library(ggthemes)

AppWichStoreAttributes <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/26afd5e7-90b1-4301-ac5e-5905b38c4ec2/file_downloaded") %>%
  janitor::clean_names() %>% rename(county_name = store_county) %>% rename(weather_station = store_weather_station)
AppWichStoreAttributes$annual_rent_estimate <- as.numeric(gsub(",","",AppWichStoreAttributes$annual_rent_estimate))


county_crime <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/3691994e-2874-4ee6-b215-12e130c96175/file_downloaded") %>% 
  janitor::clean_names()

county_demographic <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/527e7486-7233-460a-99e0-3529b7cd7d49/file_downloaded") %>% 
  janitor::clean_names() 

county_employment <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/846ac757-721e-4fd9-a414-5871f60de093/file_downloaded") %>% 
  janitor::clean_names() 
county_employment$county_labor_force <- as.numeric(gsub(",","",county_employment$county_labor_force))
county_employment$county_employed <- as.numeric(gsub(",","",county_employment$county_employed))
county_employment$county_unemployed <- as.numeric(gsub(",","",county_employment$county_unemployed))

weekly_sales <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b963fdd1-0df9-4257-bd62-65256ec9d57c/file_downloaded") %>% 
  janitor::clean_names() %>% mutate(date = as.Date(date, "%m/%d/%Y"))

weekly_weather <- read.csv("https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b8708b3c-c5e8-456e-83f4-9f23f472a840/file_downloaded") %>%
  janitor::clean_names() %>% 
  mutate(weather_date = as.Date(weather_date, "%d/%m/%Y")) %>% 
  rename(date = weather_date) %>% rename(week = weather_week)  

# Oppgave 1

# setter sammen

d1 <- merge(county_crime, county_demographic, by = "county_name")
d2 <- merge(d1, county_employment, by = "county_name")
d3 <- merge(AppWichStoreAttributes, d2, by = "county_name")
d4 <- merge(d3, weekly_sales, by = "store_num")
df <- merge(data.frame(d4, row.names = NULL),
            data.frame(weekly_weather, row.names = NULL), by = 0, all = TRUE)[-1]
df <- left_join(d4, weekly_weather)


##### Oppgave2 (Aggregere i description, snitt salg av de andre for å sammenligne seg selv 
# korsen man ligger i forhold til andre( snitt av alle i county/naboer
# forklare hvorfor valget vårres ved analysen er lerevant
# rapport skal være fra den ene butikken til lederen av hele bedriften ( fokus på meny)))
# mekke data for butikk 5 og 7
# totale salg 
#costnader 
# profitt 
# solgte produkter 
# produkter som ikke har fortjeneste (varer >= 0 og varere som er <= 0 )
# 

# uke nr 3 i i 4 mnd nettoprofitt for alle butikker
uke_r <- df %>%  filter(week == 16) %>% 
  group_by(store_name, annual_rent_estimate) %>% 
  summarise(profitt = sum(profit)) %>% 
  mutate(leie = annual_rent_estimate/52,
         netto_profitt = profitt - leie) %>% 
  select(store_name, profitt, netto_profitt)
uke_r

ggplot(uke_r, aes(x = "", y = netto_profitt, fill = store_name)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  labs(title = "Prosentandel av total profitten \n for alle butikker i uke 16", 
       x = "", y = "Netto Profitt ")

df %>%  filter(week == 16) %>% 
  group_by(store_name, annual_rent_estimate) %>% 
  summarise(profitt = sum(profit)) %>% 
  mutate(leie = annual_rent_estimate/52,
         netto_profitt = profitt - leie) %>% 
  select(store_name, profitt, netto_profitt) %>%  
  ggplot(., aes(x = store_name, y = netto_profitt, fill = store_name)) +
  geom_col() + guides(x = guide_axis(angle = -45)) +
  scale_fill_discrete(name = "Butikker") +
  labs(title = "Netto Profitt for hver butikk \n i uke 16", 
       x = "Butikker", y = "Profitt i USD ") + 
  theme_economist()

# alle som går i minus og resten i pluss
profitt_uke <- df %>% 
  filter(week == 16, store_name == "West Power StripMall",
         profit >= 0) %>% rename(Positiv_profitt = profit) %>% 
  group_by(Positiv_profitt) %>% 
  select(Positiv_profitt)

negativ_profitt_uke <- df %>% mutate(Negativ_profitt = profit) %>% 
  filter(week == 16, store_name == "West Power StripMall", 
         Negativ_profitt <= 0) %>% 
  group_by(Negativ_profitt) %>% 
  select(Negativ_profitt)

estimert_leie <- df %>% group_by(annual_rent_estimate) %>% 
  filter(week == 16, store_name == "West Power StripMall") %>% 
  summarise(sum(annual_rent_estimate)) %>% 
  mutate(Estimert_leie = annual_rent_estimate/52) %>% 
  select(Estimert_leie)

p1 <- rbind(profitt_uke, negativ_profitt_uke)
profitt_tot <- bind_rows(estimert_leie, p1)
profitt_tot[is.na(profitt_tot)] = 0


profitt_tot %>% mutate(Estimert_leie = Estimert_leie*(-1)) %>% 
  group_by(Positiv_profitt, Negativ_profitt, Estimert_leie) %>%
  colSums(1,) %>% enframe(name = "Profitt", value = "USD") %>%  
  ggplot(., aes(x = Profitt, y = USD, fill = Profitt)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste(format(USD, nsmall=0), "USD")), 
            position = position_stack(vjust= 0.5),
            colour = "black", size = 3) + 
  scale_fill_manual(values = c("red", "red", "green")) +
  labs(title = "Profitt vs \n faste og marginale kostnad", 
       x = " ", y = "USD") +
  theme_classic() 

# tabell ? 

# Lager snitt for året å sammneligne med uken vi reportere om

snitt <- df %>% select(profit, week, store_name) %>%
  filter(store_name == "West Power StripMall") %>% 
  group_by(week) %>% summarise(ukentlig_profitt = mean(sum(profit)))

ggplot(snitt) + 
  geom_col(aes(x = week, y = ukentlig_profitt),
           size = 0.8, color = "darkblue", fill = "white") +
  geom_line(aes(x = week, y = mean(ukentlig_profitt)),
            size = 1, color="red", group = 1) +
  geom_text(x = 42, y = 5300, label="Gjennomsnitlig ukentlig profitt",
            col = "red") +
  scale_color_manual(values = c("16" = "green")) +
  labs(title = "Ukentlig Profitt og \n Gjennomsnitts profitt for 53 uker", 
       x = "Uke nr", y = "USD") +
  theme_economist()


# Oppgave 3 
# ( sammenligne uka for uka i 1 mnd)
# Sammenligne hva som generer mest profitt ( antall varer eller pris )
# etterspørsel 

#Resultat:
# I en langsiktig salgsrapport for West power Stripmall, mnd nr 4, fra datoen 01.04.2012, 
# skal vi videre se på nøkkeltall som gjelder for en hel måned. Det første vi ser på er resultatet:

#  1.	(STOLPE)
# Nettoprofitten for West power Stripmall denne måneden er 21 644 dollar, medregnet leie av lokale.
# Sammenlignet med gjennomsnittet fra alle 12 månedene i dataene, ser vi at……

snitt_mnd <- df %>% 
  filter(store_name == "West Power StripMall") %>% 
  group_by(month, annual_rent_estimate) %>% 
  summarise(mnd_profitt = mean(sum(profit))) %>% 
  mutate(month = month.abb[as.numeric(month)]) %>% 
  mutate(mnd_leie = annual_rent_estimate/12, 
         netto_profitt = mnd_profitt - mnd_leie) %>% 
  select(month, netto_profitt, mnd_profitt)
snitt_mnd

ggplot(snitt_mnd,) + 
  geom_col(aes(x = month, y = mnd_profitt), 
           size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = month, y = mean(netto_profitt)), 
            size = 1.5, color="red", group = 1) +
  labs(title = "Måndtlig Profitt og \n Gjennomsnitts profitt for 12 månder", 
       x = "Måned", y = "USD") + 
  theme_economist()


# 2.	()
# Hvor stor andel utgjør de 5 varene som har høyest profitt av totalprofitten for den måneden?
#  Sammenlignet med andelen av de 5 varene som har høyest profitt gjennom hele året.



#Meny:
# 3.	()
# Hvordan produkter har en kostnadsprosent som er for høy?


# 4.	(SAMMENLIGNING AV BUTIKK 5 OG 7)
# Sammenlign brutto profittmargin med netto profittmargin. Hvor stor forskjell er det?

# Brutto profittmargin: (Brutto profitt/ omsetning) * 100
# (25 694/ 37 200) * 100 = 69. 07 %

# Netto profittmargin: (Netto profitt/ omsetning) * 100
# (21 644 / 37 200 ) * 100 = 58, 183 %



# Denne måneden har West power Stripmall en brutto profittmargin på 69, 07%, 
# mens de har en netto profittmargin på 58, 183 %. Dette viser at de faste kostnadene, 
# som innebærer leie, utgjør 69,07-58,183 = 10,887% av profittmarginen. 

# Sammenlignet med Power city Stripmall, som ligger i samme fylke, Power City, og også er på et kjøpesenter:

# Profitt: 42 053
# FK: 4500
# Netto profitt: 37 553
# Omsetning: 61 229

# Brutto profittmargin: (Brutto profitt/ omsetning) * 100
# (42053/ 61 229) * 100 = 68,682 %

# Netto profittmargin: (Netto profitt/ omsetning) * 100
# (37 553/ 61 229) * 100 = 61,332 %


# 68,682-61,332 = 7,35 %

#De faste kostnadene hos West power stripmall utgjør 10,887% av profittmarginen, 
# mens den hos Power city Stripmall er noen prosent lavere, og utgjør 7,35 % av profittmarginen. 
# West Power Stripmall har derfor høyere faste kostnader i prosent av egen profitt enn det Power City Stripmall har











# Oppgave 4 

#1
year_profit <- df  %>% group_by(store_name) %>%  select(store_name, profit) %>% 
  summarise(profitt = sum(profit)) 
year_profit
ggplot(year_profit, aes( x = store_name, y = profitt)) + geom_col() + 
  guides(x = guide_axis(angle = -60)) + theme_economist() +
  labs(title = "Årlig Profitt per butikk ", 
       x = "Butikk navn", y = "Profitt i USD") + 
  theme_economist()

# 2

df %>% 
  group_by(store_name, annual_rent_estimate, month) %>% 
  summarise(profitt = sum(profit)) %>%
  mutate(leie = annual_rent_estimate/12,
         netto_profitt = profitt - leie) %>% 
  select(store_name, netto_profitt, month) %>% 
  ggplot(., aes(x = month, y = netto_profitt, col = store_name)) + 
  geom_line() +
  labs(title = "Netto profitt over ett år ", 
       x = "Dato", y = "Netto Profitt ") +
  theme_economist()

# 3
pop_store <- df %>% 
  select(store_name, county_total_census_pop) %>%
  group_by(store_name) %>% 
  summarise(county_pop = median(county_total_census_pop))

netto_profitt <- df %>% 
  group_by(annual_rent_estimate, store_name) %>%
  select(profit, annual_rent_estimate) %>%  
  summarise(profitt = sum(profit)) %>% 
  mutate(leie = annual_rent_estimate,
         netto_profitt = profitt - leie) %>% 
  select(netto_profitt, store_name) 

ks9 <- df %>% 
  group_by(store_name, annual_rent_estimate, month) %>% 
  summarise(profitt = sum(profit)) %>%
  mutate(leie = annual_rent_estimate/12,
         netto_profitt = profitt - leie) %>% 
  select(store_name, netto_profitt, month) 

pf4 <- merge(ks9, pop_store, by = "store_name")

pf4 %>% mutate(pop_innt = netto_profitt/county_pop) %>% 
  group_by(pop_innt) %>% 
  ggplot(., aes(x = month, y = pop_innt, col = store_name)) + 
  geom_line() +
  labs(title = "Netto profitt over ett år ", 
       x = "Dato", y = "Netto Profitt ") +
  theme_economist()

# 4 
# Årlig nettoprofitt per inbygger

oppf <- merge(pop_store, netto_profitt, by = "store_name")

oppf %>% mutate(inntekt_per_p = netto_profitt/county_pop) %>%
  select(store_name, inntekt_per_p) %>% 
  ggplot(., aes(x = store_name, y = inntekt_per_p)) +
  geom_col() + guides(x = guide_axis(angle = -60)) +
  labs(title = "Årlig nettoprofitt per innbygger ", 
       x = "Butikk", y = "Netto Profitt ") +
  theme_economist()
# 5 
reise_klienter <- AppWichStoreAttributes %>%
  select(store_name, store_traveller_clients)
reise_klienter <- merge(reise_klienter, oppf, by = "store_name")

reise_klienter %>% group_by(store_traveller_clients) %>% 
  mutate(snitt_inntekt = mean(netto_profitt/county_pop)) %>%
  summarise(snitt_jn = median(snitt_inntekt)) %>%
  ggplot(., aes( x = store_traveller_clients, y = snitt_jn)) +
  geom_col() +
  labs(title = "Årlig gjennomsnitt nettoprofitt \n per innbygger: \n 
       Reisende klienter", 
       x = " ", y = "USD") +
  theme_economist()

# 6 

drive_through <- AppWichStoreAttributes %>%
  select(store_name, store_drive_through)
drive_through <- merge(drive_through, oppf, by = "store_name")

drive_through %>% group_by(store_drive_through) %>% 
  mutate(snitt_inntekt = mean(netto_profitt/county_pop)) %>%
  summarise(snitt_jn = median(snitt_inntekt)) %>%
  ggplot(., aes( x = store_drive_through, y = snitt_jn)) +
  geom_col() +
  labs(title = "Årlig gjennomsnitt nettoprofitt \n per innbygger: \n 
       Drive through", 
       x = " ", y = "USD") +
  theme_economist()

# 7
# lokaler

lokaler <- AppWichStoreAttributes %>%
  select(store_name, store_location)
lokaler <- merge(lokaler, oppf, by = "store_name")

lokaler %>% group_by(store_location) %>% 
  mutate(snitt_inntekt = mean(netto_profitt/county_pop)) %>%
  summarise(snitt_jn = median(snitt_inntekt)) %>%
  ggplot(., aes( x = store_location, y = snitt_jn)) +
  geom_col() +
  labs(title = "Årlig gjennomsnitt nettoprofitt \n per innbygger: \n 
       Drive through", 
       x = " ", y = "USD") +
  theme_economist()
# 8 
skole <- AppWichStoreAttributes %>%
  select(store_name, store_near_school)
skole <- merge(skole, oppf, by = "store_name")

skole %>% group_by(store_near_school) %>% 
  mutate(snitt_inntekt = mean(netto_profitt/county_pop)) %>%
  summarise(snitt_jn = median(snitt_inntekt)) %>%
  ggplot(., aes( x = store_near_school, y = snitt_jn)) +
  geom_col() +
  labs(title = "Årlig profitt per innbygger:  \n 
       Nært skole", 
       x = " ", y = "USD") +
  theme_economist()

# 9 
konkur <- AppWichStoreAttributes %>%
  select(store_name, store_competition_fastfood, store_competition_otherfood)
konkur <- merge(konkur, oppf, by = "store_name")

konkur %>% group_by(store_competition_fastfood, 
                    store_competition_otherfood) %>%
  pivot_longer(cols = 2:3) %>% 
  ggplot(., aes( x = store_name, y = value, fill = name )) +
  geom_col() +
  guides(x = guide_axis(angle = -60)) +
  labs(title = "Konkurrenter", 
       x = "", y = " ") +
  guides(fill=guide_legend(title=" ")) +
theme_economist()

# 10
df %>%
  select(store_name, county_unemployment_rate) %>%
  group_by(store_name, county_unemployment_rate) %>% 
  summarise(county_unemployment_rate = median(county_unemployment_rate)) %>% 
  ggplot(., aes(x = store_name, y = county_unemployment_rate)) +
  geom_col() + guides(x = guide_axis(angle = -60)) +
  labs(title = "Ledighetsrate", x = "", y = "") +
  theme_economist()








