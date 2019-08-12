# Recommender-for-VVV
This code is in partial fulfillment of the Smart Service Innovation Project I. This code generates artificial data and provides a proof of concept for a recommender system.

BUSINESS PROBLEM
A business problem was given to us by VVV. They tasked us on creating an innovative service for their gift card company. Our group focused on the idea of creating an app for VVV. The app would have a mini game in the form of a little dress up. Users who wants to give a card to their friends can create a customized avatar of the gift receiver. Based on the costumes we would create a profile of the gift receiver and recommend items for them to use the gift card on. 

DATA PROBLEM 
Since the data provided to us were lacking to create a proof-of-concept model, we had to create artificial data. The input would be the clothes put on the avatar and the output would be a row that represents the user's interests. The challenge in creating artificial data was to create an output that makes sense in context of the input. 

Afterwards, we decided to create two models with two different technniques. The one called the "short term model" uses the content based filtering technique. This is the short term model since it can work with few data but can be prone to the subjectivity of human categorization. 

The second model called "long term model" uses the collaborative filtering technique. This method requires more user data than content based filtering but would lead to better results. The best recommender systems like Spotify and Netflix uses collaborative filtering along with other techniques.

DATA SOLUTION
For the artificial data code, we created a not-so-elegant set of rules for transforming the truly random input into our desired output. 

For the two recommender models, both were based on using the cosine distance function to create recommendations. For the short term model, the distance function was applied to the user profile to another dataset of shop profiles. for the long term model, the cosine distance function was applied again to compare users similar users and recommend to them what the similar users have bought. 

BUSINESS SOLUTION
We came up with the app as the solution for VVV after conducting interviews with users. They found the VVV brand to be primitive. Having an app that recommends users is not an innovative idea relative to the other apps out there but for VVV, it just might be the key to reviving their brand. The aspect of the minigame would inject fun into the brand and at the same time, enable VVV to gain more data about their users. Gaining more data was another bonus for VVV because they mentioned that they know almost nothing about their customers. 
