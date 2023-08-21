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
