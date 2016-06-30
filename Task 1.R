# My first R assignment (by Igor Gorlatov)
library(dplyr)
library(tidyr)
original.data <-read.csv("refine_original.csv")

clean.data <-original.data

#First objective: remove capital letters
task1.data <- lapply(original.data$company, function(x) {
  as.character(tolower(x))
})
glimpse(task1.data)

#Adding changed column to clean.data to build a fixed table
clean.data$company <- task1.data
head(clean.data$company, 10)

# Second objective: get the data into two columns
clean.data <- clean.data %>% separate(Product.code...number, c("Product_code", "Product_number"), sep = "-", convert = TRUE)

glimpse(clean.data)

#Third task - add a new column which defines the category of the product based on code

#Vectors that will serve for replacement of Product_code for Product_category
from <- c('p','v','x','q')
to <- c('Smartphone','TV','Laptop','Tablet')

#Function that does the actual replacement 
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

#Using mutate to create a new column Product_Category and our newly minted function does the substituion
clean.data <- clean.data %>%  
  mutate(Product_category = gsub2(from, to, Product_code))


#Task 3: Adding full address out of three columns
clean.Geodata <- clean.data %>% unite(fulladdress, address, city, country, sep = ",", remove = TRUE)
glimpse(clean.Geodata)


#Task 4: Create dummy variables for company and product category
# ================= company_phillips ===================
# listing all misspellings, so that all names are coded properly
cf <- c('phillips', 'philips','phillps', 'phllips', 'fillips')

# Trying to get all values that match our CF vector. Those that don't match are NA
iz.na <- factor (clean.Geodata$company, levels = cf)

# Turning values to zeros and ones.
company_phillips <- as.numeric(!is.na(iz.na))

# ================= company_akzo ===================
cf <- c('akzo', 'akz0','ak zo')

iz.na <- factor (clean.Geodata$company, levels = cf)

company_azko <- as.numeric(!is.na(iz.na))


# ================= company_van_houten ===================
cf <- 'van houten'

iz.na <- factor (clean.Geodata$company, levels = cf)

company_van_houten <- as.numeric(!is.na(iz.na))

# ================= company_unilever =====================
cf <- c('unilver', 'uniliever', 'unilever')

iz.na <- factor (clean.Geodata$company, levels = cf)

company_unilever <- as.numeric(!is.na(iz.na))

# Adding the new columns with mutate function to the clean.Geodata

clean.all <- clean.Geodata %>%  
  mutate(company_phillips = company_phillips, company_azko = company_azko,
         company_van_houten = company_van_houten, company_unilever = company_unilever)
clean.all$company <- as.character(clean.all$company)

glimpse(clean.all)
write.csv(clean.all, file="refine_clean.csv")
