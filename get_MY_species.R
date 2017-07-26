#  Sara Williams
#  June 23, 2017
#  Collect data on all species on IUCN Red List from Malaysia

library(rredlist)

#  Set API token to environment variable
Sys.setenv(IUCN_REDLIST_KEY = "7b70de11d3fda3bb7ad7aa8714d04ac808198cf34fe37a91264e1d531d573106")

#  Generate data frame of all species found in Malaysia
my_species <- as.data.frame(rl_sp_country("MY")) %>%
my_species_rl <- filter(my_species, result.category != "LC")
