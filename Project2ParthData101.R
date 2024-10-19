install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)


# Load the dataset
data(penguins)

head(penguins)
View(penguins)

df <- penguins[!is.na(penguins$body_mass_g) & !is.na(penguins$species)& !is.na(penguins$sex), ]


ad <- df[df$species== "Chinstrap",]
mean(ad$body_mass_g)
#first rejected hypothesis
p_fail <- permutation_test(df, 'species', 'body_mass_g', 10000, 'Adelie', 'Chinstrap') 
cat(p_fail)


ade <- df[df$species == "Adelie",]
mean(ade$body_mass_g)

Chin <- df[df$species == "Chinstrap",]
mean(Chin$body_mass_g)

#Strong hypothesis
# Filter dataset for Gentoo and Chinstrap penguins
df_filtered_species_bill <- subset(df, species %in% c("Gentoo", "Chinstrap"))
p_strong_bill_length <- permutation_test(df_filtered_species_bill, 'species', 'bill_length_mm', 10000, 'Gentoo', 'Chinstrap')
cat(p_strong_bill_length)




# Close Call
df_filtered_year <- subset(df, sex == 'male' & island %in% c('Torgersen', 'Dream') & year == 2008)
p_year_flipper <- permutation_test(df_filtered_year, 'island', 'flipper_length_mm', 10000, 'Torgersen', 'Dream')
cat( p_year_flipper)



head(penguins_clean)



cs <- df_filtered_males[df_filtered_males$species == "Chinstrap",]
mean(cs$body_mass_g)
cs2 <- df_filtered_males[df_filtered_males$species == "Adelie",]
mean(cs2$body_mass_g)




cg <- df[df$species == "Adelie",]
femcg <- cg[cg$sex == "female",]

dg <- df[df$species == "Gentoo",]
mendg <- dg[dg$sex == "male",]
mean(mendg$body_mass_g)
mean(femcg$body_mass_g)
mean(df$body_mass_g)
mean(subset(df, df$species == "Gentoo")$body_mass_g)
mean(df$body_mass_g)

mean(subset(largest_gentoo_males)$body_mass_g)


maleGentoo <- df[df$species == "Gentoo" & df$sex == "male",]

largest_gentoo_males <- subset(maleGentoo, flipper_length_mm > 215 & bill_depth_mm > 14.8 & bill_length_mm > 48.5)

M <- mean(largest_gentoo_males$body_mass_g)
cat(M)

