library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

#----------Filter Data---------#

#look for an exact match: ==

penguins_biscoe <- penguins %>% filter(island == "Biscoe")

# to check
unique(penguins_biscoe$island)

#practice
penguins_2007 <- penguins %>% filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie", island == "Torgersen")

# gentoo penguins observed in 2008
gentoo_2008 <- penguins %>% filter(species == "Gentoo", year == 2008)

# Create subset that contains Gentoos and Adelies
gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

#Create a subset that contains observations where the island is dream or year is 2009
dream_2009 <- penguins %>% filter(island == "Dream" | year == 2009)

#Fiddler crabs 
ggplot(data = pie_crab, aes(x = water_temp, y = size)) +
  geom_point()

unique(pie_crab$site)

#keep observations for sites NIB, ZI, DB, JC


# does the value in our column match any of the values in this vector: %in%
pie_sites <- pie_crab %>% filter(site %in% c("NIB", "ZI", "DB", "JC"))

unique(pie_sites$site)

#

sites <- c("CC", "BB", "PIE")

pie_sites_2 <- pie_crab (site %in% sites)

# includes sites PIE, ZI, NIB, BB and CC
sites2 <- c("PIE", "ZI", "NIB", "BB", "CC")

pie_sites_3 <- pie_crab %>% filter(site %in% sites2)

# excluding filter statements: != is this not equal to
exclude_zi <- pie_crab %>% filter(site != "ZI")

# excluding filter statements multiple:
exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB", "CC", "PIE"))

# create a subset from pie crab that only contains obesrvations from sites nib, cc, and zi for crabs with size exceeding 13
crab_subset <- pie_crab %>% filter(site %in% c("NIB", "CC", "ZI"),
                                   size > 13)

#----------Selecting Columns---------#

# to subset columns use the select function

#Select individual columns by name use a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# check column names 
names(crabs_subset)

#select a range of columns
crabs_subset_2 <- pie_crab %>% select(site: air_temp)

# include separated ranges 
crabs_subset_3 <- pie_crab %>% select(date:water_temp, name)

#will show up in the order we call them
pie_crab %>% select(name, water_temp, size)


#----------Mutate---------#

#use dplyr::mutate() to add or update a column while keeping all existing columns 

#add a new column that has a new column with this calculation 
crabs_cm <- pie_crab %>% 
  mutate(size_cm = size / 10)

#what happens if I use mutate to add a new column containg the mean of the size column? - gave me the mean of all data aka 1 mean for the dataset
crabs_mean <- pie_crab %>% 
  mutate(size_mean = mean(size))

#replace old name column with values that say Teddy is awesome 
crabs_awesome <- pie_crab %>% 
  mutate(name = "Teddy is awesome")

# mutate with group_by to find mean crabsize by site
mean_size_by_site <- pie_crab %>% 
  group_by(site) %>% 
  summarize(mean_size = mean(size, na.rm = TRUE))

#what about a group by and then mutate
group_mutate <-pie_crab %>% 
  group_by(site) %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))

#what if i want to make a new column in pie_crab that contains "giant" if the size is gerater than 35, or not giant if the size is less than on equal to 35

#use dplyr::case_when() to write if-else statements more easily
crabs_bin <- pie_crab %>% 
  mutate(size_binned = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))

sites_binned <- pie_crab %>% 
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High"
  ))