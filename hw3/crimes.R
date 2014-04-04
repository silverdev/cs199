#!/usr/bin/env Rscript

# Read the data.
data <- read.csv('communitiesH.data', h=T) # Headers = True.

# Remove non-predictive attributes (including state number).
clean_data <- data[, -c(1:5)]

# Remove the attributes with question marks (unknowns) on them.
unkw <- clean_data == '?'
unkw_per_attr <- apply(unkw, 2, sum) # Sum the columns (2).
attr_with_unkw <- which(unkw_per_attr > 0) # Equivalent to find in Matlab.
no_unkw_data <- clean_data[, -attr_with_unkw]

# Compute a regression.


# Evaluate the regression looking at the mean-squared error on the training data.


# Evaluate the regression splitting off some test data and looking at the 
# mean-squared error on that test data.



# EXTRA:
# Data visualization commands:
#  > View(data)
#  > summary(data)