# This is the 4 Lads 4 Grads Long Term Model. This is built upon the concept of collaborative filtering 
# recommender systems. Basically, this code computes the similarity of a user's interest to find other users
# that are most similar to the user we want to generate recommendations for. We would then recommended 
# the shops where those similar users ended up buying their VVV from. 

# These are the packages requires for this code
library(readxl)
library(lsa)
library(dplyr)

# These are the data we need. Note that we do not require the shop datasets here. 
user_dat <- read.csv("Generated Artificial Data.csv")
# We take out rows that has no target attirbute. This is inevitable in real data.
user_dat <- na.omit(user_dat)

# This is the heart of this model. It only takes two inputs: id - which specifies which row (which user) 
# we will generate recommendations for and DistanceRange - which specifies how far from the user's location
# we want to search. The higher this value, the broader it is.
# Note that the long term model is not very different from the short term model. In fact, it works the 
# same way, just with different things. 
distance <- function(id, DistanceRange) {
  # We genearate a data table that exclude the user. We will compare the user to this data set
  OtherUser <- user_dat[-id,]
  # We turn the data tables into matrices and only with the columns we need 
  user <- as.matrix(user_dat[,5:37])
  other <- data.matrix(OtherUser[,5:37])
  # We generate the vectors where we will keep the shop name, similarity score, postal code, and distance
  ShopName <- c()
  SimilarityScore <- c()
  post <- c()
  PostalDifference <- c()
  # This loops compares the user to the other user so we can identify the other users most similar to the user
  # we want to generate recommendations for
  for (i in 1:nrow(OtherUser)){ 
    ShopName[i] <- as.character(OtherUser[i,48])
    post[i] <- as.character(OtherUser[i,49])
    PostalDifference[i] <- abs(user_dat[id,4]-OtherUser[i,49])
    SimilarityScore[i] <- cosine(user[id,],other[i,])
  }
  # We compile all the calculations into a data frame
  result <- data.frame(ShopName,post,SimilarityScore,PostalDifference)
  # We sort the data frame by similarity score
  result <- result[order(result$SimilarityScore,decreasing=TRUE),]
  # We filter out the rows with a similarity score of zero
  result <- filter(result,result$SimilarityScore>0)
  # We filter out results that are too far from the user (beyond the range specified)
  result <- filter(result,result$PostalDifference<=DistanceRange)
  # We filter out duplicate shop results
  result <- unique(result[,1:2])
  print("Based on your avatar and other users like you, you might like these shops!")
  return(result[1:10,1:2])
}

# Test Cases 
# For the sake of the continuity of the narrative, we will use the same test cases
# This user has the following notable details (we will only include categories this
# user has a score of more than 4): Male; Postal Code - 6287; Sport - 4, Outdoor - 4, Fashion - 4, Shoes - 4, 
# Beauty/wellness - 5, Beauty/drugstore - 5, Hairdresser - 5, baby store - 4, toy store - 4, into cycling, 
# has a child
distance(1,10)
# We can see that the recommended shops are closely related to the profile of the user (Kruidvat for beauty,
# De Unicaten for fashion, Tweewielercentrum Nix because he is into cycling, and so on). 
distance(1,20)
# As with a bigger range, there's more choices. The results are once again closesly related to the profile

# This user has the following notable details (we will only include categories this
# user has a score of more than 4): Female; Postal Code - 6255; electronics - 4, computer - 4, beauty - 5, 
# beauty/drugstores - 5, hairdresser - 5, baby store - 4, kid store - 4, fun store - 4, has a child, likes 
# fun
distance(444,10)
# Fletcher hotel seems to be recommended due to the fact that other user where she lives also goes to Fletcher
# hotel. 
distance(444,20)
# We can see that when we widen the distance range, more options are recommended. 

# Do feel free to play around the code! 
