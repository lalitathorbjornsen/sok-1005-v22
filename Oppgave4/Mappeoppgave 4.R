# Mappeoppgave 4 :  Scrape url

#Nødvendige pakker
library(rvest)
library(tidyverse)
library(rlist)

#lage liste med alle url til timeplanene, dvs SOK-1006, SOK-1005 og SOK-1016

timeplan <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&View=list",
                 "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list",
                 "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&View=list")

# Skrape en funksjon

scrape_fun <- function(url) {
  return(read_html(url) %>%
           html_nodes(.,'table') %>% 
           html_table(., fill = TRUE) %>% # Gjør det til en liste
           list.stack(.) %>% #Gjør alle listene til samlet datasett
           janitor::row_to_names(., 1) %>% #Første rad til header
           separate(Dato, 
                    into = c("Dag", "Dato"),
                    sep = "(?<=[A-Za-z])(?=[0-9])") %>% #Splitter dag og dato
           mutate(Dato = as.Date(Dato, format = "%d.%m.%Y"), #Formaterer år 
                  Uke = strftime(Dato, format = "%V")) %>%
           select(Dag, Dato, Uke, Tid, Rom, Lærer)) # Velger de ulike variablene 
}


# lager map for å lese av listene og funksjonen 
Kursplan <- map(timeplan, scrape_fun)

