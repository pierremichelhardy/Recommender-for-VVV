# This is the 4 Lads 4 Grads Short Term Model. This is built upon the concept of content based recommender
# systems. Basically, this code computes the similarity of a user's interest (harvested from the mini game)
# and the shop's characteristics and match them to those most similar

# These are the packages requires for this code to work
library(readxl)
library(lsa)
library(dplyr)

# This is the part of the code where we load our requires datasets. The ShopData holds the data about the 
# shops that accept VVV cards. This dataset is from the dataset that VVV gave us though we edited the formation
# to suit our needs. The UserData is the artificial data that was generated using the 
# ArtificialDataGenerator4Lads4Grads.R
ShopData <- read_excel("FULL SHOP DATASET ROW ORDERED WITH MINIGAME.xlsx")
UserData <- read.csv("Generated Artificial Data.csv")

# This is the heart of this model. It only takes two inputs: id - which specifies which row (which user) 
# we will generate recommendations for and DistanceRange - which specifies how far from the user's location
# we want to search. The higher this value, the broader it is. 
distance <- function(id,DistanceRange) {
  # In this part, we exract parts of the data that we need and put them in a matrix.
  # It is required to put it in a matrix since that's the format that the cosine function takes it as
  # user collects the data about the user
  user <- as.matrix(UserData[,4:37])
  # shop is a matrix with data about the shops
  shop <- data.matrix(ShopData[,2:35])
  # user2 is similar to user excluding the postal code so that it won't interefere with the cosine 
  # similarity calculation
  user2 <- as.matrix(UserData[,5:37])
  # shop2 is similar to to shop excluding the postal code for the same reasons
  shop2 <- data.matrix(ShopData[,3:35])
  # Here we declare vectors wherein we will put the shop names, similarity scores, postal codes, and 
  # how far a shop is from the user's location.
  ShopName <- c()
  SimilarityScore <- c()
  post <- c()
  PostalDifference <- c()
  # This loop then compares the user's details with all the shops in the database and collects the result. 
  # It also calculates the distance of the shop from the user but getting the difference of postal code
  # This logic is based on the assumption that postal code numbers near each other numerically is also near 
  # each other in real life. 
  for (i in 1:nrow(ShopData)){
    ShopName[i] <- as.character(ShopData[i,1])
    post[i] <- as.character(shop[i,1])
    PostalDifference[i] <- abs(user[id,1]-shop[i,1])
    SimilarityScore[i] <- cosine(user2[id,],shop2[i,])
  }
  # We collate all the information into a data frame
  result <- data.frame(ShopName,post,SimilarityScore,PostalDifference)
  # We sort the result by similarity score
  result <- result[order(result$SimilarityScore,decreasing=TRUE),]
  # We filter out results that has a similarity score of zero
  result <- filter(result,result$SimilarityScore>0)
  # We filter out results that are too far from the user (beyond the range specified)
  result <- filter(result,result$PostalDifference<=DistanceRange)
  print("Based on your avatar, you might like these shops!")
  # We then return the top ten recommended shops, its postal code, and similarity score.
  return(result[1:10,1:3])
}

# Test Cases 
# This user has the following notable details (we will only include categories this
# user has a score of more than 4): Male; Postal Code - 6287; Sport - 4, Outdoor - 4, Fashion - 4, Shoes - 4, 
# Beauty/wellness - 5, Beauty/drugstore - 5, Hairdresser - 5, baby store - 4, toy store - 4, into cycling, 
# has a child
distance(1,10)
# When ran, the top three result should be Kruidvat, Kapsalon, and De Unicaten. We can see that these shops
# were recommended to this user because the data mentioned that this person likes beauty products and going
# to the hairdresser. He also got recommended to De Unicaten, which is a clothes shop because he scores high 
# in fashion. The rest of the recommendations can also be connected to the other interests of the user
distance(1,20)
# After running this function on the same user but with a higher range, we can see that the recommendations have
# now increased since the scope of search also increased. Once again, the recommendations are closesly 
# related on the person's profile

# This user has the following notable details (we will only include categories this
# user has a score of more than 4): Female; Postal Code - 6255; electronics - 4, computer - 4, beauty - 5, 
# beauty/drugstores - 5, hairdresser - 5, baby store - 4, kid store - 4, fun store - 4, has a child, likes 
# fun
distance(444,10)
# Here is another test case on another user that is different from the first test case. The top 3 store 
# recommendations for her are flower shops since she scores high on liking florists. 
distance(444,20)
# Here, we widened the range again and we see that the recommendations increased to locations further 
# from the user. This time, the top recommendations are regarding beauty because this user is also very fond
# of beauty products. The rest of the recommendations can also be connected to the other interests of the user
