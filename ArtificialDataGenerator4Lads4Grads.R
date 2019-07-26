# This code is used to generate synthetic user data in order for us to  build and run the models on
# In general, this code has three main functions:
# 1 - Generate random input for the function that generates the mini game output, given the inputs
# 2 - The mini game output generating function
# 3 - A function that generates our target attribute: shops where the users ended up spending their VVV card
# The main output of this code will be a csv file that mimics the actual data we intend to capture in the 
# future. 
# NOTE: THIS CODE TAKES ABOUT A FULL MINUTE TO EXECUTE.

# These are the packages required to load 
library(readxl)
library(lsa)
library(dplyr)

# Generally, every time this function is ran, it produce another batch of dataset that different
# For now, we shall set the seed for reproducability reasons
set.seed(4)

# This part of the code is where we declare vectors of values that we will use to randomly pick values from
binary <- c(0,1)
OneToTen <- c(1,2,3,4,5,6,7,8,9,10)
Categories <- c(1,2,3,4,0,0,0)
sport <- c('hiking','cycling',0,0,0)
tri <- c(1,2,3)
OneToFive <- c(0,1,2,3,4,5)
DragDrop1 <- c(0,'glasses',0,0,0)
DragDrop2 <- c(0,'pet',0,0,0)
DragDrop3 <- c(0,'child',0,0,0)
DragDrop4 <- c(0,'fun',0,0)
ZipChoice <- c(6211,6212,6213,6214,6215,6216,6217,6218,6219,6221,6222,6224,6225,6226,6227,6228,6229,6231,6235,
               6241,6243,6245,6247,6255,6265,6267,6269,6271,6276,6277,6281,6285,6286,6287,6291,6294,6295)
# DataRows is where you can assign how many artificial data rows you wish to generate
DataRows<-1000 
# Here we create the matrix where we will put the randomly generate input for the mini game function
MiniGameInput <- matrix(nrow=DataRows,ncol=30)
colnames(MiniGameInput)<-c('row','gender','gf','sport','fashion','TravelCulture','FoodDrinks','HomeGarden',
                           'electronics','beauty','sport1','sport2','sport3','sport4','sport5','fashion1',
                           'fashion2','travel1','travel2','travel3','travel4','fd1','fd2','fd3','hg1','dd1',
                           'dd2','dd3','dd4','dd5')
ShopData <- read_excel("FULL SHOP DATASET ROW ORDERED WITH MINIGAME.xlsx")

# The RandomMiniGameInput is the function that produces random input for the mini game output generator
RandomMiniGameInput <- function(rowx){
  # Here we declare all the values and assign them the default values for now
  sport1x<- character()
  sport1x <- 0
  sport2x<- character()
  sport2x <- 0 
  sport3x<- character()
  sport3x <- 0
  sport4x<- character()
  sport4x <- 0
  sport5x<- character()
  sport5x <- 0
  fashion1x<- numeric()
  fashion1x<-0
  fashion2x<- numeric()
  fashion2x<-0
  travel1x<- numeric()
  travel1x<-0
  travel2x<- numeric()
  travel2x<-0
  travel3x<- numeric()
  travel3x<-0
  travel4x<- numeric()
  travel4x<-0
  fd1x<- numeric()
  fd1x<-0
  fd2x<- numeric()
  fd2x<-0
  fd3x<- numeric()
  fd3x<-0
  hg1x<- numeric()
  hg1x<-0
  dd1x<- character()
  dd1x <- 0
  dd2x<- character()
  dd2x <- 0
  dd3x<- character()
  dd3x <- 0
  dd4x<- character()
  dd4x <- 0
  dd5x<- character()
  dd5x <- 0
  # This part is where we randomly assign inputs for the values the mini game output function asks
  # Depending on the range of input it accepts, it gets from a vector of possible values
  # For 1 and 0 input, we get from the binary vector 
  # For 1 to 10 input, we get from the OneToTen
  # For ranked categories, the randomness comes from the order that samples are taken from the Categories 
  # vector. They would then be assigned to their respective variables. 
  # As for the dd variables, they each have their own assignment, as seen in their respective vectors above.
  genderx <- sample(binary,1)
  gfx <- sample(OneToTen,1)
  ranks <- sample(Categories,7)
  sportx <- ranks[1]
  fashionx <- ranks[2]
  TravelCultureX <- ranks[3]
  FoodDrinksx <- ranks[4]
  HomeGardenx <- ranks[5]
  electronicsx <- ranks[6]
  beautyx <- ranks[7]
  dd1x <- sample(DragDrop1,1)
  dd2x <- sample(DragDrop2,1)
  dd3x <- sample(DragDrop3,1)
  dd4x <- sample(DragDrop4,1)
  # In this part, we generate input for the sub-questions. Since it will depend on the answers above, this 
  # part requires some conditional statements. 
  if(sportx>0){
    sports <- sample(sport,2)
    sport1x <- sports[1]
    sport2x <- sports[2]
  }
  if(fashionx>0){
    fashion1x <- sample(tri,1)
    fashion2x <- sample(OneToFive,1)
  }
  if(TravelCultureX>0){
    travel1x <- sample(OneToFive,1)
    travel2x <- sample(OneToFive,1)
    travel3x <- sample(OneToFive,1)
    travel4x <- sample(OneToFive,1)
  }
  if(FoodDrinksx>0){
    fd1x <- sample(OneToFive,1)
    fd2x <- sample(OneToFive,1)
    fd3x <- sample(OneToFive,1)
  }
  if(HomeGardenx>0){
    hg1x <- sample(tri,1)
  }
  # We then gather all these random input into one vector and make this the product of this function
  RandomValues <- c(rowx,genderx,gfx,sportx,fashionx,TravelCultureX,FoodDrinksx,HomeGardenx,
                electronicsx,beautyx,sport1x,sport2x,sport3x,sport4x,sport5x,fashion1x,fashion2x,
                travel1x, travel2x,travel3x,travel4x,fd1x,fd2x,fd3x,hg1x,dd1x,dd2x,dd3x,dd4x,dd5x)
  return(RandomValues)
}

# This loop populates the matrix MiniGameInput full of random minigame input
for(i in 1:DataRows){
  MiniGameInput[i,]<-RandomMiniGameInput(i)
}

# Create a function to generate output for the minigame
# Our minigame of building an avatar is able to score 
# many variables by just a few clicks from the user!

# Create df with columns for output minigame
df <- as.data.frame(matrix(0, ncol = 46, nrow = DataRows))
# Name columns
colnames(df) <- c('Gender', 'Graduation field','Postal Code','Sports store', 'Outdoor store', 
                  'Bike store', 'Fashion & Clothing', 'Shoe store', 'Jewelry', 
                  'Lingerie', 'Hotel', 'Restaurant/bar/lunch', 'Day-activity', 
                  'Cultural/Museum', 'Art/Antique', 'Books/office supply', 
                  'Music', 'Photography/printing', 'Cooking/Delicatesse', 
                  'Liquor store', 'Construction', 'Florist/Garden', 
                  'Home deco & Household', 'Electronics',
                  'Computer/Telecom', 'Beauty/Wellness', 'Beauty/drug shop', 
                  'Hairdresser', 'Pet store', 'Baby/Kids', 'Toy store', 'Optician', 
                  'Fun stores', 'Grocery store', 'Department store', 'Souvenir', 'Sport1', 
                  'Sport2', 'Sport3','Sport4', 'Sport5', 'DragDrop1', 'DragDrop2', 
                  'DragDrop3', 'DragDrop4', 'DragDrop5')
# Create vector with graduation fields
GradField <- c('Business & Economics', 'Philosophy', 'Health', 'Law', 'Music', 
                'Technology', 'Architecture', 'Social studies', 
                'Language & culture', 'Agriculture')

# Generate a function for the output of the minigame
# Arguments are: 
# - the row (n)
# - gender
# - graduation field (gf)
# - the 7 different categories with their rank (1-4) in the minigame. 
# - answers to the subquestions in the chosen categories
# - input fields of sports
# - drag & drop objects chosen by the gift giver
# - default (if the gift giver doesn't know the interests of the graduate, he or she
# can choose a default recommendation, comprising department stores, grocery stores &
# souvenir stores)
# The input for these fields were the ones randomly generated by the RandomMiniGameInput function
# This function also includes some assumptions and correlations to make the resulting data more realistic
# Assumptions are mentioned by to mention all of them here, they are: 
# Assumption 1 - sport people like to go to outdoor stores anyways (minor weight automatically assigned to
# outdoor shops if the person answered that they're into sports)
# Assumption 2 - Active people like to go to cycling stores a bit (minor weight automatically assigned to 
# cycling if the person answered that they're into sports)
# Assumption 3 - females gets assigned some weight to jewelry and lingerie if they answered that they are into
# fashion and clothing.
# Assumption 4 - all home/household shop categories are equally liked by the graduate
MiniGame <- function(n, gender, gf, sport = NULL, fashion = NULL, 
                     TravelCulture = NULL, FoodDrinks = NULL, HomeGarden = NULL,
                     electronics = NULL, beauty = NULL, sport1 = 'NA', sport2 = 'NA', 
                     sport3 = 'NA', sport4 = 'NA', sport5 = 'NA', fashion1 = NULL,
                     fashion2 = NULL, travel1 = NULL, travel2 = NULL, travel3 = NULL,
                     travel4 = NULL, fd1 = NULL, fd2 = NULL, fd3 = NULL, hg1 = NULL,
                     dd1 = 'NA', dd2 = 'NA', dd3 = 'NA', dd4 = 'NA', dd5 = 'NA', 
                     default=FALSE) {
  # Specify gender variable (0 = female, 1 = male)
  if (gender == 0) {
    df[n, 'Gender'] <- "Female" 
  } else { 
    df[n, 'Gender'] <- "Male"
  }
  # Specify graduation field by input from the minigame
  df[n, 'Graduation field'] <- GradField[gf]
  # If default is TRUE --> gift giver does not know the hobbies/interests
  # from the gift giver --> standard recommendation of grocery stores, 
  # department stores and souvenir stores
  df[n, c('Grocery store', 'Department store', 'Souvenir')] <- 
    ifelse(default == TRUE, 5, 0)
  # If the sport category was chosen for the graduate, save the input
  # of the gift giver about which sports the graduate likes
  # Save input
  df[n,'Sport1'] <- sport1
  df[n,'Sport2'] <- sport2
  df[n,'Sport3'] <- sport3
  df[n,'Sport4'] <- sport4
  df[n,'Sport5'] <- sport5
  # Add correction to the output of the minigame for the ranking
  # of the category. The first rank has the highest scores and the 4th rank
  # has the lowest output scores.
  if (!is.null(sport)) { #  if sport ranking is not equal to NULL
    if (sport == 1) {
      a <- 1  
    } else if (sport == 2) {
      a <- 0.8
    } else if (sport == 3) {
      a <- 0.75
    } else if (sport == 4) {
      a <- 0.65
    } else {
      a <- 0
    }
    # max function is used because sports store is also in fashion category
    df[n, 'Sports store'] <- max(5 * a, df[n, 'Sports store'])
    df[n, 'Outdoor store'] <- 
      # for now just specified one outdoor sport as an example
      ifelse((sport1 == 'hiking'|sport2 == 'hiking'|sport3 == 'hiking'|
                sport4 == 'hiking'|sport5 == 'hiking'), 5 * a, 
             max(df[n, 'Outdoor store'], 4 * a)) 
    # max function because outdoor is also in fashion category
    # assumption: sport people like to go to outdoor stores anyways (= 4), 
    # also if they don't practice any outdoor sports
    df[n, 'Bike store'] <- 
      # for now just specified one cycling sport as an example
      ifelse((sport1 == "cycling"|sport2 == "cycling"|sport3 == "cycling"|
                sport4 == "cycling"|sport5 == "cycling"), 5 * a, 2 * a)
    # assumption: Active people like to go to cycling stores a bit 
    # less than outdoor stores (= 2)
  } else { #  if sport ranking is equal to NULL (is not specified in the function)
    df[n, c('Sports store', 'Outdoor store', 'Bike store')] <- 0
  }
  
  if (!is.null(fashion)) {
    if (fashion == 1) {
      b <- 1  
    } else if (fashion == 2) {
      b <- 0.8
    } else if (fashion == 3) {
      b <- 0.75
    } else if (fashion == 4) {
      b <- 0.65
    } else {
      b <- 0
    }
    df[n,'Fashion & Clothing'] <- ifelse((fashion1 == 2 | fashion1 == 3), 5*b, 0)
    df[n,'Sports store'] <- ifelse((fashion1 == 1 | fashion1 == 2), 
                                   max(5*b, df[n,'Sports store']), 
                                   max(0, df[n, 'Sports store']))
    df[n,'Outdoor store'] <- ifelse((fashion1 == 1 | fashion1 == 2), 
                                    max(5*b, df[n, 'Outdoor store']), 
                                    max(0, df[n, 'Outdoor store']))
    df[n, 'Shoe store'] <- fashion2 * b
    # assumption that graduating females are into jewelry and lingerie when the 
    # fashion & clothing category is chosen for them (= 4)
    if (gender == 0) {
      df[n, c('Jewelry', 'Lingerie')] <- 4 * a
    } else {
      df[n, c('Jewelry', 'Lingerie')] <- 0  
    }
  } else {
    df[n, c('Fashion & Clothing', 'Shoe store', 'Jewelry', 'Lingerie')] <- 0
    df[n,'Outdoor store'] <- max(0, df[n, 'Outdoor store'])
    df[n,'Sports store'] <- max(0, df[n, 'Sports store'])
  }
  
  if (!is.null(TravelCulture)) {
    if (TravelCulture == 1) {
      c <- 1  
    } else if (TravelCulture == 2) {
      c <- 0.8
    } else if (TravelCulture == 3) {
      c <- 0.75
    } else if (TravelCulture == 4) {
      c <- 0.65
    } else {
      c <- 0
    }
    df[n, c('Cultural/Museum', 'Day-activity', 'Hotel')] <- travel1 * c
    df[n, 'Restaurant/bar/lunch'] <- max(travel1 * c, df[n, 'Restaurant/bar/lunch'])
    df[n, 'Books/office supply'] <- travel2 * c
    df[n, 'Music'] <- travel3 * c
    df[n, 'Photography/printing'] <- travel4 * c
    df[n, 'Art/Antique'] <- max(travel4 * c, df[n, 'Art/Antique'])
  } else {
    df[n, c('Cultural/Museum', 'Day-activity', 'Hotel', 'Books/office supply',
            'Music', 'Photography/printing')] <- 0
    df[n, 'Restaurant/bar/lunch'] <- max(0, df[n, 'Restaurant/bar/lunch'])
    df[n, 'Art/Antique'] <- max(0, df[n, 'Art/Antique'])
  }
  
  if (!is.null(FoodDrinks)) {
    if (FoodDrinks == 1) {
      d <- 1  
    } else if (FoodDrinks == 2) {
      d <- 0.8
    } else if (FoodDrinks == 3) {
      d <- 0.75
    } else if (FoodDrinks == 4) {
      d <- 0.65
    } else {
      d <- 0
    }
    df[n, 'Cooking/Delicatesse'] <- max(fd1 * d, df[n, 'Cooking/Delicatesse'])
    df[n, 'Liquor store'] <- fd2 * d
    df[n, 'Restaurant/bar/lunch'] <- max(fd3 * d, df[n, 'Restaurant/bar/lunch'])
  } else {
    df[n, 'Cooking/Delicatesse'] <- max(0, df[n, 'Cooking/Delicatesse'])
    df[n, 'Liquor store'] <- 0
    df[n, 'Restaurant/bar/lunch'] <- max(0, df[n, 'Restaurant/bar/lunch'])
  }
  
  if (!is.null(HomeGarden)) {
    if (HomeGarden == 1) {
      e <- 1  
    } else if (HomeGarden == 2) {
      e <- 0.8
    } else if (HomeGarden == 3) {
      e <- 0.75
    } else if (HomeGarden == 4) {
      e <- 0.65
    } else {
      e <- 0
    }
    df[n,'Florist/Garden'] <- ifelse((hg1 == 1 | hg1 == 2), 5*e, 0)
    # Assumption: all home/household shop categories are equally liked by the graduate
    df[n, c('Construction', 'Home deco & Household')] <- 
      ifelse((hg1 == 2 | hg1 == 3), 5*e, 0)
    # max function because these shop categories exist also in other categories
    df[n, 'Art/Antique'] <- ifelse((hg1 == 2 | hg1 == 3), max(5*e, df[n, 'Art/Antique']),
                                   max(0, df[n, 'Art/Antique']))
    df[n, 'Cooking/Delicatesse'] <- ifelse((hg1 == 2 | hg1 == 3), 
                                           max(5*e, df[n, 'Cooking/Delicatesse']),
                                           max(0, df[n, 'Cooking/Delicatesse']))
  } else {
    df[n, c('Florist/Garden', 'Construction', 'Home deco & Household')] <- 0
    df[n, 'Art/Antique'] <- max(0, df[n, 'Art/Antique'])
    df[n, 'Cooking/Delicatesse'] <- max(0, df[n, 'Cooking/Delicatesse'])
  }
  
  if (!is.null(electronics)) {
    if (electronics == 1) {
      f <- 1  
    } else if (electronics == 2) {
      f <- 0.8
    } else if (electronics == 3) {
      f <- 0.75
    } else if (electronics == 4) {
      f <- 0.65
    } else {
      f <- 0
    }
    df[n, c('Electronics', 'Computer/Telecom')] <- 5 * f
  } else {
    df[n, c('Electronics', 'Computer/Telecom')] <- 0
  }  
  
  if (!is.null(beauty)) {
    if (beauty == 1) {
      g <- 1 
    } else if (beauty == 2) {
      g <- 0.8
    } else if (beauty == 3) {
      g <- 0.75
    } else if (beauty == 4) {
      g <- 0.65
    } else {
      g <- 0
    }
    df[n, c('Beauty/Wellness', 'Beauty/drug shop', 'Hairdresser')] <- 5 * g
  } else {
    df[n, c('Beauty/Wellness', 'Beauty/drug shop', 'Hairdresser')] <- 0
  }
  # We save the categories of objects that were chosen for this avatar.
  df[n, 'DragDrop1'] <- dd1
  df[n, 'DragDrop2'] <- dd2
  df[n, 'DragDrop3'] <- dd3
  df[n, 'DragDrop4'] <- dd4
  df[n, 'DragDrop5'] <- dd5
  # As an example we only chose higher level categories drag & drop categories: 
  # glasses, pets, child/baby, funny objects. You can model this down to the specific 
  # object level (e.g. sunglasses, cat, baby, tutu)
  # optician scored as 4 if person has glasses 
  df[n, 'Optician'] <- ifelse((dd1 == 'glasses' | dd2 == 'glasses' | dd3 == 'glasses' 
                               | dd4 == 'glasses' | dd5 == 'glasses'), 4, 0)
  # pet store scored as 4 if person has a pet
  df[n, 'Pet store'] <- ifelse((dd1 == 'pet' | dd2 == 'pet' | dd3 == 'pet' 
                                | dd4 == 'pet' | dd5 == 'pet'), 4, 0) 
  # toy store & baby store scored as 4 if person has a baby or child
  df[n, 'Toy store'] <- ifelse((dd1 == 'child' | dd2 == 'child' | 
                                  dd3 == 'child' | dd4 == 'child' | 
                                  dd5 == 'child'), 4, 0) 
  df[n, 'Baby/Kids'] <- ifelse((dd1 == 'child' | dd2 == 'child' | 
                                  dd3 == 'child' | dd4 == 'child' | 
                                  dd5 == 'child'), 4, 0)
  # To deal with the problem of people making funny/not realistic avatars
  # we added the possibility to drop funny items on the avatar. If the gift giver
  # adds funny objects to the avatar we will score the funny stores category as 4
  # so that fun stores will be in the recommendations.
  df[n, 'Fun stores'] <- ifelse((dd1 == 'fun' | dd2 == 'fun' | dd3 == 'fun' | 
                                   dd4 == 'fun' | dd5 == 'fun'), 4, 0)
  df[n, 'Postal Code'] <- sample(ZipChoice,1)
  return(df[n,])
}
# Futher explanation of the subquestions and specific arguments of the categories
# Sports
# If  the gift giver specificies that the graduate likes the Sports category 
# in the minigame Gift giver will be asked what Sport(s) the graduate likes/practices 
# Input to this function are the sports specified by the gift giver (sport1, sport2, 
# sport3, sport4, sport5)

# Fashion & Clothing category
# If the gift giver specifies that the graduate likes the Fashion & Clothing
# Category, then the gift giver will be asked to further specify 2 mandatory 
# subquestions by sliding a bar
# Subquestion 1: is he or she more into activewear (sportive/outdoor clothing = 1), 
# more into casual clothing (Fashion/clothing=3) or into both (=2)
# Subqueston 2: is he or she into shoes/sneakers? (scale 1-5)
# The underlying numbers of the slide bars are the input to this function 
# (fashion 1, fashion 2)

# Travel & Culture category
# If the gift giver specifies that the graduate likes the Travel & Culture
# Category, then the gift giver will be asked to specify 5 subquestions 
# By using a slide bar with the underlying numbers 0 to 5 
# Subquestion 1: is he or she into day-activities or a weekend getaway?
# Subquestion 2: is he or she into books?
# Subquestion 3: is he or she into music?
# Subqueston 4: is he or she into photography/art?
# The underlying numbers of the slide bars are the input to this function 
# (travel1, travel2, travel3, travel4)

# Food & Drinks category
# If the gift giver specifies that the graduate likes the Food & Drinks
# Category, then the gift giver will be asked to specify 3 subquestions 
# By using a slide bar with the underlying numbers 0 to 5 
# Subquestion 1: is he or she into cooking?
# Subqueston 2: is he or she into partying?
# Subquestion 3: Does he or she like to go out for diner/lunch?
# The underlying numbers of the slide bars are the input to this function
# (fd1, fd2, fd3)

# Home & Garden category
# If the gift giver specifies that the graduate is into the Home & Garden 
# category, the gift giver has two answer 1 more subquestion using a slider:
# Subquestion 1: Is the graduate more into flowers & garden (=1) or more into
#                home decoration/household (=3) or both (=2)
# The underlying numbers of the slide bar is the input to this function (hg1)

# Electronics category
# If the gift giver specifies that the graduate is into the Electronics 
# category, we assume that the graduate is equally liking the underlying 
# shop categories: Electronics, Computer/Telecom

# Beauty/Welness category
# If the gift giver specifies that the graduate is into the Beauty & 
# Welness category, we assume that the graduate is equally into 
# the underlying shop categories: beauty/welness, hairdresser, beauty & drug shop

# Drag & Drop category
# At the end of the minigame there is the possibility to further personalize the
# avatar by dragging and dropping items to the avatar.
# For now, we specified the possibility to drag & drop a pet to the avatar,
# to drag & drop a baby/child to the avatar, to drag & drop eyewear to the
# avatar and to drag and drop funny gadgets/outfits to the avatar.
# Underlying store categories correspond to pet stores, baby/child, 
# toy stores and fun stores. The objects that are chosen by the gift giver
# are saved in the dataset (dd1, dd2, dd3, dd4, dd5)

# This loop is the heart of the artificial data generation
# This populates the data frame with artificial data. This is made by feeding the random input 
# into the MiniGame function. The mini game function crunches the data and spit out data rows that looks like
# the final data we intend to work on. This function also applies some correlations between interests 
# to make the data more realistic
for (i in 1:DataRows){
  df[i,]<-MiniGame(n=as.numeric(MiniGameInput[i,1]), 
                   gender=as.numeric(MiniGameInput[i,2]), 
                   gf=as.numeric(MiniGameInput[i,3]), 
                   sport=as.numeric(MiniGameInput[i,4]), 
                   fashion=as.numeric(MiniGameInput[i,5]), 
                   TravelCulture=as.numeric(MiniGameInput[i,6]), 
                   FoodDrinks=as.numeric(MiniGameInput[i,7]), 
                   HomeGarden=as.numeric(MiniGameInput[i,8]),
                   electronics=as.numeric(MiniGameInput[i,9]), 
                   beauty=as.numeric(MiniGameInput[i,10]), 
                   sport1=MiniGameInput[i,11],
                   sport2=MiniGameInput[i,12],
                   sport3=MiniGameInput[i,13],
                   sport4=MiniGameInput[i,14],
                   sport5=MiniGameInput[i,15],
                   fashion1=as.numeric(MiniGameInput[i,16]),
                   fashion2=as.numeric(MiniGameInput[i,17]),
                   travel1=as.numeric(MiniGameInput[i,18]),
                   travel2=as.numeric(MiniGameInput[i,19]),
                   travel3=as.numeric(MiniGameInput[i,20]),
                   travel4=as.numeric(MiniGameInput[i,21]),
                   fd1=as.numeric(MiniGameInput[i,22]),
                   fd2=as.numeric(MiniGameInput[i,23]),
                   fd3=as.numeric(MiniGameInput[i,24]),
                   hg1=as.numeric(MiniGameInput[i,25]),
                   dd1=MiniGameInput[i,26],
                   dd2=MiniGameInput[i,27],
                   dd3=MiniGameInput[i,28],
                   dd4=MiniGameInput[i,29],
                   dd5=MiniGameInput[i,30],
                   default=FALSE)
}

# The distance function generates the target attribute of the model (the shops where people ended up 
# using their VVV cards on). In this part, we actually applied our short term model so that the target 
# attribute wuold also make sense depending on the user profile. The input of this function is the artificial
# mini game ouput we just produced above.
# The full commentary of this function will be on the Short Term Model.R since the code is quite identical
distance <- function(id,DistanceRange) {
  zip <- df[id,47]
  user <- as.matrix(df[,3:36])
  shop <- data.matrix(ShopData[,2:35])
  user2 <- as.matrix(df[,4:36])
  shop2 <- data.matrix(ShopData[,3:35])
  ShopName<- c()
  SimilarityScore <- c()
  PostCode <- c()
  PostCodeDifference <- c()
  for (i in 1:nrow(ShopData)){
    ShopName[i] <- as.character(ShopData[i,1])
    PostCode[i] <- as.character(shop[i,1])
    PostCodeDifference[i] <- abs(user[id,1]-shop[i,1])
    SimilarityScore[i] <- cosine(user2[id,],shop2[i,])
  }
  result <- data.frame(ShopName,PostCode,SimilarityScore,PostCodeDifference)
  result <- result[order(result$SimilarityScore,decreasing=TRUE),]
  result <- filter(result,result$SimilarityScore>0)
  result <- filter(result,result$PostCodeDifference<=DistanceRange)
  return(result[1:4,1:2])
}

# Here we populate vectors of the resulting target attribute and the postal code of the shop
TargetAttribute <- c()
TargetAttributePostCode <- c()
for (i in 1:DataRows){
  TargetAttribute[i] <- as.character(unlist(distance(i,15)[1]))
  TargetAttributePostCode[i] <- as.character(unlist(distance(i,15)[2]))
}

# Here we bind the vectors into new columns in the final data frame.
ArtificialData <- cbind(df,TargetAttribute)
ArtificialData <- cbind(ArtificialData,TargetAttributePostCode)

# Finally, we transport the aritificial data into a CSV. It will be ready for use by the BI Model.R, Short 
# Term Model.R, and Long Term Model.R
write.csv(ArtificialData,file="Generated Artificial Data.csv")
