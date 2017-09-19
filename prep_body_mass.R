#  Sara Williams
#  September 19, 2017
#  Use panthera to add adult body mass to threathened mammals excel data.



library(dplyr)



path <- "C:/Users/saraw/Documents/SEARRP"
path_df_dat_raw <- paste(path, "raw_excel_data", sep = "/")
path_df_dat_proc <- paste(path, "processed_excel_data", sep = "/")
setwd(path)



panth_dat_tmp <- read.table(paste(path_df_dat, "PanTHERIA_1-0_WR05_Aug2008.txt", sep = "/"),
	sep="\t",
	header = TRUE)
my_mam_dat <- read.csv(paste(path_df_dat, "spp_data_w_constraint_info/sabah_mammals_threatened_all_spp.csv", sep = "/"))
my_bird_dat <- read.csv(paste(path_df_dat, "spp_data_w_constraint_info/sabah_birds_threatened.csv", sep = "/"))
my_amph_dat <- read.csv(paste(path_df_dat, "spp_data_w_constraint_info/sabah_amphibians_threatened_all_spp.csv", sep = "/"))


panth_mam_dat <- m_dat_tmp %>%
	dplyr::select(order = MSW05_Order, family = MSW05_Family, genus = MSW05_Genus, 
		species = MSW05_Species, binomial = MSW05_Binomial,  adult_body_mass_g = X5.1_AdultBodyMass_g, 
		home_range_km2 = X22.1_HomeRange_km2, trophic_level = X6.2_TrophicLevel)

mam_mass_dat <- my_mam_dat %>%
	left_join(panth_mam_dat, by = "binomial") %>%
	dplyr::select(binomial, adult_body_mass_g) %>%
	group_by(binomial) %>%
	slice(1) %>%
	as.data.frame()
mam_mass_dat[mam_mass_dat == -999] <- NA

bird_mass_dat <- my_bird_dat %>%
	dplyr::select(binomial) %>%
	mutate(adult_body_mass_g = "NA") %>%
	group_by(binomial) %>%
	slice(1) %>%
	as.data.frame()

amph_mass_dat <- my_amph_dat %>%
	dplyr::select(binomial) %>%
	mutate(adult_body_mass_g = "NA") %>%
	group_by(binomial) %>%
	slice(1) %>%
	as.data.frame()
	
	
	
write.csv(mam_mass_dat , file =  paste(path_df_dat_proc, "mammal_mass.csv", sep = "/"))
write.csv(bird_mass_dat , file =  paste(path_df_dat_proc, "bird_mass.csv", sep = "/"))
write.csv(amph_mass_dat , file =  paste(path_df_dat_proc, "amphibian_mass.csv", sep = "/"))