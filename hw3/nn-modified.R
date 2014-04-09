# Compute the euclidean distance between two vectors that may contain question 
# mark elements in a vectorized way.
distance_vec <- function(v1, v2){
  # Change question mark elements of each vector by the same position elements
  # in the other vector, so that they get cancelled when computing the distance.
  unk_indices_1 <- which(v1 == '?')
  unk_indices_2 <- which(v2 == '?')

  v1[unk_indices_1] = v2[unk_indices_1]
  v2[unk_indices_2] = v1[unk_indices_2]

  # Change question marks elements with same position in both vectors by 0.
  unk_indices <- which(v1 == '?')
  v1[unk_indices] <- 0
  v2[unk_indices] <- 0

  # Ensure vectors type is numeric.
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)

  # Return the euclidian distance.
  return(sqrt(sum((v1 - v2) ^ 2)))
}

# Compute the euclidean distance between two vectors that may contain question 
# mark elements in a linear way.
distance_lin <- function(v1, v2){
  s <- 0
  for(i in 1:length(v1)){
    if(v1[i] != '?' & v2[i] != '?'){
      # Only elements that are not question marks contribute to the distance.
      s <- s + ((as.numeric(v1[i]) - as.numeric(v2[i])) ^ 2)
    }
  }
  return(sqrt(s))
}


# Compute the modified nearest neighbour of every element in the dataset.
nn_data <- clean_data
for(i in 1:nrow(clean_data)){
  min_distance <- -1
  min_index <- -1

  for(j in 1:nrow(clean_data)){
    if(i != j){
      d <- distance(clean_data[i,], clean_data[j,])
      if(min_distance == -1 | d < min_distance){
        min_distance <- d
        min_index <- j
      }
    }
  }
  
  nn_data[i,] <- clean_data[min_index,]
}