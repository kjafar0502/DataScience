setwd("C:/Users/Khaled_Jafar/Desktop/DataScienceClass/DataWrangling")
getwd()

# import ccsv file and convert it to a data frame 
refine_original <- read.csv("refine_original.csv", header=TRUE, sep =",")
View(refine_original)

as.data.frame(refine_original)

#learn more about imported data set
head(refine_original)
tail(refine_original)

str(refine_original)
class(refine_original)
names(refine_original)




# cleaning up brand info and setting it to lower case

refine_original$company[grepl("p", refine_original$company, ignore.case = TRUE)] <- "phillips"
refine_original$company[grepl("k", refine_original$company, ignore.case = TRUE)] <- "akzo"
refine_original$company[grepl("t", refine_original$company, ignore.case = TRUE)] <- "van houten"
refine_original$company[grepl("r", refine_original$company, ignore.case = TRUE)] <- "unilever"


# load dplyr and tidyr libraries and then separate product code and number
library(dplyr)
library(tidyr)
refine_original <- refine_original %>% separate(Product.code...number, c("product_code", "product_number"), sep="-")


# now add product categories p = Smartphone, v = TV, x = Laptop, q = Tablet
product_categories <- c("smartphone","tv","laptop","tablet")
names(product_categories) <- c("p","v","x","q")
refine_original <- refine_original %>% mutate(product_category = product_categories[product_code])


# Add full address for geocoding separating address, city and country by commas
refine_original <- refine_original %>% unite(full_address, address, city, country, sep=",", remove = FALSE)

# Create dummy variables for company and product category

companies <- (refine_original %>% distinct(company))$company
for(eachCompany in companies) 
  refine_original[paste0("company_",eachCompany)] <- as.numeric(refine_original$company == eachCompany)


product_categories <- (refine_original %>% distinct(product_category))$product_category
for(eachCategory in product_categories)
  refine_original[paste0("category_",eachCategory)] <- as.numeric(refine_original$product_category == eachCategory)

# save cleaned up data
write.csv(refine_original, file = "refine_clean.csv")
