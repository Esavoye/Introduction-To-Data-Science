# Unit 3.1.4 Basic Data Manipulation - Toy Dataset

library("dplyr")
library("tidyr")

# Read toy data CSV file
toy_data <- read.csv("refine_original.csv")

# Transform company name to all caps for consistency
toy_data[,1] <- toupper(toy_data[,1])
toy_data <- toy_data %>% mutate(company = if_else(company == 'PHILLIPS'| company == 'PHILLPS'| company == 'PHLIPS'| company == 'PHLLIPS'| company == 'FILLIPS', "PHILIPS",
                                               if_else(company == 'AKZ0'| company == 'AK ZO', "AKZO",
                                                                  if_else(company == "UNILVER", "UNILEVER", company))))

# Separate product code and product number
toy_data <- separate(toy_data, Product.code...number, into = c("product_code", "product_number"), sep = "-")

# Add product category to explain product code
toy_data <- toy_data %>% mutate("product_category" = if_else(product_code == "p", "Smartphone",
                                                  if_else(product_code == "v", "TV",
                                                          if_else(product_code == "x", "Laptop",
                                                                  if_else(product_code == "q", "Tablet", NA_character_)))))

# Create geocode location AKA full address
toy_data <- toy_data %>% mutate("full_address" = paste(address, city, country, sep = ",") )

# Dummy variables for company and product category
toy_data <- toy_data %>%  mutate("company_philips" = if_else(company == "PHILIPS", 1, 0)) %>% 
  mutate("company_azko" = if_else(company == "AZKO", 1, 0)) %>% 
  mutate("company_van_houten" = if_else(company == "VAN HOUTEN", 1, 0)) %>% 
  mutate("company_unilever" = if_else(company == "UNILEVER", 1, 0)) %>% 
  mutate("company_smartphone" = if_else(product_category == "Smartphone", 1, 0)) %>% 
  mutate("company_tv" = if_else(product_category == "TV", 1, 0)) %>% 
  mutate("company_van_laptop" = if_else(product_category == "Laptop", 1, 0)) %>% 
  mutate("company_tablet" = if_else(product_category == "Tablet", 1, 0)) 
  
tbl_df(toy_data)
View(toy_data)

write.csv(toy_data, "refine_clean.csv")
