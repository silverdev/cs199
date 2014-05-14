# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
data <- read.csv('combinednodropna.csv', h=T) # Headers = True.

for (d in data){
    print(type.of(d)
    s = sd(data, na.rm = TRUE)
    if (s == 0){
        View(d)
    }
}


#remove user id's

#calulate Correlation matrix

#find how correlations compare.

#make graphs

