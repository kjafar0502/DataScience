
# import titanic csv file and inspect it
titanic_original <- read.csv("titanic_original.csv", header=TRUE, sep =",")

View(titanic_original)

str(titanic_original)
names(titanic_original)
summary(titanic_original)
class(titanic_original)


# Find the missing values for Embarked column and replace them with S

titanic_original$embarked[titanic_original$embarked==""] <- "S"

# Calculate the mean of the Age column and use that value to populate the missing values

titanic_original$age[is.na(titanic_original$age)] <- mean(titanic_original$age, na.rm = TRUE)

# Fill the empty Boat column  with a dummy value (e.g.the string 'NA')

titanic_original$boat[titanic_original$boat==""] <- "NA"

# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

titanic_original$has_cabin_number <- ifelse(titanic_original$cabin=="",0,1) 

# save cleaned up data
write.csv(titanic_original, file = "titanic_clean.csv")

