setwd("D:\\יונתן\\תואר שני\\סמסטר ד\\כריית טקסט\\פרויקט")
library(ggplot2)
library(tidyverse)
library(tm)
library(ggthemes)
library(wordcloud)
library(plotrix)
library(sentimentr)
library(stringr)
library(topicmodels)
library(textstem)
library(SnowballC) 
library(ldatuning)
library(dplyr)
library(gridExtra)
library(ggmap)
library(NLP)
library(openNLP)
library(lexicon)
library("openNLPmodels.en")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_231')
library(qdap)

# read files
all1.df <- read.csv("Datafiniti_Hotel_Reviews.csv")
all2.df <- read.csv("Datafiniti_Hotel_Reviews_Jun19.csv")

# find most reviewed city
all1.table <- as.data.frame(table(all1.df$city))
all1.table <- all1.table[order(all1.table$Freq, decreasing = T),]

all2.table <- as.data.frame(table(all2.df$city))
all2.table <- all2.table[order(all2.table$Freq, decreasing = T),]
head(all1.table)
head(all2.table)

# narrow to San Diego reviews with relevant attributes
all1.df <- all1.df[all1.df$city == "San Diego",]
all1.df <- all1.df[,c("name","reviews.date","reviews.rating","reviews.text","reviews.title","reviews.userCity","reviews.userProvince")]

all2.df <- all2.df[all2.df$city == "San Diego",]
all2.df <- all2.df[,c("name","reviews.date","reviews.rating","reviews.text","reviews.title","reviews.userCity","reviews.userProvince")]

reviews.df <- rbind(all1.df,all2.df)

# remove hotels with less than 10 reviews
hotels.freq <- as.data.frame(table(reviews.df$name))
hotels.freq <- hotels.freq[hotels.freq[,2]>10,]
reviews.df <- reviews.df[is.element(reviews.df$name, hotels.freq[,1]),]

# hotel ratings
hotel.ratings <- aggregate(reviews.rating ~ name, data = reviews.df, FUN = "mean")
hotel.ratings$reviews.rating <- round(hotel.ratings$reviews.rating,2)

ggplot(hotel.ratings, aes(x = reorder(name, reviews.rating), y = reviews.rating, fill = reviews.rating)) + 
  geom_bar(stat = "identity", width=.8) +
  coord_flip() +
  ggtitle("Hotels' ratings") +
  xlab("") +
  ylab("avg. rating") +
  labs(fill = "avg. rating") +
  scale_fill_gradient(low = 'red', high = 'green') +
  theme_bw()

## compare between two hotels
# create cleaning function
clean.vec<-function(text.vec){
  custom.stopwords <- c(stopwords("english"),'san diego', 'hotel', 'stay','regards','regarding','always','told','stated','another','let','probably','call')
  text.vec <- tolower(text.vec)
  text.vec<- gsub("rooms","room",text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- removeNumbers(text.vec)
  text.vec <- stripWhitespace(text.vec)
  return(text.vec)
}
## clean text
# google translate
reviews.df$reviews.text[grepl("con mi familia",reviews.df$reviews.text, fixed = TRUE)] <- "Excellent 3 day stay with my family. Close to the metro service stations (Trolller). The first impression of the course is not pleasant if you arrive at night but in reality you can move safely in the area. Very friendly all the staff with whom we had dealings and a special attention from the people who served us well at breakfasts."
reviews.df$reviews.text[grepl("Ho soggiornato qui con la mia",reviews.df$reviews.text, fixed = TRUE)] <- "I stayed here with my family (3 children) for three nights and I enjoyed it very much. The room is spacious and well-kept, the heated outdoor swimming pool with Jacuzzi very good for children (open until 10pm), the professional staff (not particularly nice). There is free parking just below the hotel which facilitates loading and unloading of luggage. The location is good even if you still have to use the car to go to the city. The only note a little bit out of tune, the breakfast is not abundant and a bit scarce and the limited space dedicated to it. For the rest a recommended hotel."
reviews.df$reviews.text[grepl("calidad precio es ptima",reviews.df$reviews.text, fixed = TRUE)] <- "The price-quality ratio is optimal. In addition, the hotel is located in one of the best areas of San Diego (Little Italy) and very close to the airport. They have a free shuttle service to the airport."
reviews.df$reviews.text[grepl("Desayuno incluido habitaciones",reviews.df$reviews.text, fixed = TRUE)] <- "Breakfast included spacious rooms, everything you need, heated pool and jacuzzi, in front of the Marina is the location of the hotel and close to the airport. Parking already included in the price I only stayed one day but I liked that the beds have white blankets I want to express our thanks for completing the Guest Survey, about your recent stay at our hotel On behalf of our entire team, I would like to apologize for not meeting your expectations. Your satisfaction is important to us and we will use your feedback to implement improvements and ensure that in the future we will offer a better and more enjoyable experience to all of our guests. I hope you will consider staying with us again so that we have another opportunity to offer a superior experience and being able to greet you. If I personally can be of assistance to you with any matter related to the hotel, please do not hesitate to contact me at 619-224-3254 Sincerely, Travis R. Ford Front Office Manager BEST WESTERN Yacht Harbor Hotel"

# delete hotel's replys
reviews.df$reviews.text[grepl("hank you for investing your time in reviewing our hotel",reviews.df$reviews.text, fixed = TRUE)]

reviews.df$reviews.text[grepl("This hotel was paid through Expedia, however still charged the guests",reviews.df$reviews.text, fixed = TRUE)] <- "Our company books rooms for our techs that travel all across the US. This hotel was paid through Expedia, however still charged the guests personal credit card. When we called the hotel to discuss this error we were met with a stubborn, rude attitude not willing to make the situation right. Shame on you Manager Manu and shame on you Best Western. Last time I checked this is stealing. So beware when staying here and keep a close on on your credit card charges."
reviews.df$reviews.text[grepl("I am not quite sure why this is a Best Western Plus",reviews.df$reviews.text, fixed = TRUE)] <- "I am not quite sure why this is a Best Western Plus. A Best Western Oke is a better description. But it was better than the one we had in LA. The hotel is located outside San Diego, near a Naval airbase (with sometimes some noise, but not too bad).It had a decent size pool, large rooms, and if you are lucky, even working WIFI.The lady at reception had a bit of a problem helping her guests, as she was on the phone to her sister or taking other phone calls instead of finishing checking us in. And although we booked months in advance, it was too much trouble to reserve to adjoining rooms but she 'managed' to get us rooms on the same floor.Not great, not Plus, but a good night sleep"
reviews.df$reviews.text[grepl("The room was adequate but the breakfast was terrible. First the space is under",reviews.df$reviews.text, fixed = TRUE)] <- "The room was adequate but the breakfast was terrible. First the space is under construction. There is no room to sit. They did not keep the food stocked. So if you came late there wasn't much to eat. The hotel was overpriced for the condition it is in at the moment. Find another place to stay."

reviews.df$reviews.text[grepl("Please know that we take your feedback",reviews.df$reviews.text, fixed = TRUE)] <- "In the last few months, I've stayed at three different Hilton properties for work... and the Hilton Garden Inn in Rancho Bernardo, CA is a disappointment. Less well cared for than their sister brand Hampton Inn. And as a HiltonHonors member, they refused to give me a free bottle of water after the check-in for my 8 day stay. Wifi is sketchy. Parking and restaurant is substandard. Stay at a corporate rate at the nearby Rancho Bernardo Inn. So much more worth it!Also, light on mirror in bathroom has been out for a day and I just called down and the maintenance person on duty doesn't have access to the lightbulbs."

reviews.df$reviews.text[grepl("stains in the linnens, 1000 year old mattress",reviews.df$reviews.text, fixed = TRUE)] <- "Old motel style , recently remodelated but dirty, stinky , stains in the linnens, 1000 year old mattress , expensive ,just not worth it .Dear recent guest,I'm sorry to read your review and hear that you did not enjoy your stay in one of our poolside/putting green area rooms. Although these are only 2-story buildings with exterior access, most of our guests enjoy the patios and balconies these rooms have overlooking the golf putting green or the swimming pool. There are, however, 2 rooms in those buildings that do not have a balcony."

reviews.df$reviews.text[grepl("Do Not stay in room 1501. Although the room",reviews.df$reviews.text, fixed = TRUE)] <- "The issues started the first night. Do Not stay in room 1501. Although the room seems like it is a bit away from the elevators the bed room is right next to the elevator shaft. This would be fine if it was just elevator noise but every time it went up and down the banged like the someone was knocking on the door loudly. All night all day whenever someone was in the elevator. I complained the first night to the front desk and actually a maintenance guy, Jared. He said that Otis elevator was actually on site fixing the elevators. They actually came to room but could not figure out where the noise was coming from. I did mention it again on Wednesday but they did not do anything. They could have at least moved me to another room. On Friday I was supposed to meet someone for dinner. I did not have his cell though. I had stepped out of the room for a bit, unfortunately right when he called at 630. He tried again at 7. The staff did not leave a message on my room phone (or my cell/email which they had). At 9 I received a call from the front desk that I received 2 calls at 630 and 7. When I asked why I am just hearing about it at 9, the woman said the note got lost and they had just found it. Oh did I say the pool was closed. Lastly, 1501 is a joined room and on Friday night, at 11 a family decided to have a fight where someone was slapped and the mother and little daughter started crying. I called the office but as far as I know no one came up or called. Just not a good stay."

reviews.df$reviews.text[grepl(" one night. Everything was perfect, staff",reviews.df$reviews.text, fixed = TRUE)] <- "We were at the Club Pulse for just one night. Everything was perfect, staff at the front desk were very friendly and knowledgeable, everyone we ran into was professional and nice, the room was super comfy. We had a suite with a kitchenette (microwave, refrig with freezer unit, coffee maker, dishes, flatware, glasses for water and wine), 2 televisions - one in the bedroom the other in the living room. The seating was very comfortable and a table to eat. I highly recommend this property. We were in town to see the play Hamilton, we were easily able to walk to the Gas Lamp for dinner and then to the Civic Theater."
reviews.df$reviews.text[grepl("ego for a wedding and most peo",reviews.df$reviews.text, fixed = TRUE)] <- "We were in San Diego for a wedding and most people in the family stayed here, so we did too. The hotel itself is alright, there's nothing wrong with it, but nothing great. Rooms were decent. The location however is not the best. It's quite far from the city center, and with traffic the drive is long."
reviews.df$reviews.text[grepl("son's High School water polo",reviews.df$reviews.text, fixed = TRUE)] <- "I booked this hotel for my son's High School water polo tournament. The stay was wonderful the boys enjoyed breakfast and said this hotel had the best hotel eggs. I would definitely recommend this hotel for team travel. My husband and I enjoyed our stay as well I asked to be on a completely different floor from the boys. The room was a good size the bed was comfy this was the best sleep I had at a hotel! There was a small problem with the air conditioning but maintenance fixed the problem extremely quick! I would definitely stay here again! The staff was very friendly and accommodating."
reviews.df$reviews.text[grepl("website (because they would lose",reviews.df$reviews.text, fixed = TRUE)] <- "This hotel was having their pool renovated without mention on their website (because they would lose bookings) and the Front Office manager on duty Christine - was one of the least professional hotel managers I've ever talked to. In short, she was rude and inconsiderate when asked questions. As Marriott Platinum members they give you a free item from their market per room to use during stay which SHE turned around and charged for on the credit card. Essentially they committed fraud and acted like they couldn't do anything to make situations right when guests are unhappy. Marriott is losing control of quality or they are becoming a very difficult brand. Either way, avoid this hotel - ESPECIALLY if you are an elite Marriott member. Christine - really doesn't belong anywhere near guests or the hospitality industry."
reviews.df$reviews.text[grepl("website (because they would lose",reviews.df$reviews.text, fixed = TRUE)] <- "This hotel was having their pool renovated without mention on their website (because they would lose bookings) and the Front Office manager on duty Christine - was one of the least professional hotel managers I've ever talked to. In short, she was rude and inconsiderate when asked questions. As Marriott Platinum members they give you a free item from their market per room to use during stay which SHE turned around and charged for on the credit card. Essentially they committed fraud and acted like they couldn't do anything to make situations right when guests are unhappy. Marriott is losing control of quality or they are becoming a very difficult brand. Either way, avoid this hotel - ESPECIALLY if you are an elite Marriott member. Christine - really doesn't belong anywhere near guests or the hospitality industry."

reviews.df$reviews.text[grepl("WORST I have ever seen. Staring with parking you are paying 12.00 for a spot",reviews.df$reviews.text, fixed = TRUE)] <- "I basically live out of hotels and this is the WORST I have ever seen. Staring with parking you are paying 12.00 for a spot that you have to do a 3 point turn to get into and my Camry almost didnt fit into. The room was super small for being a family suite and everything was cramped. The was enough space between the foot of the bed and the tv table was wide enough for a vacuum to fit though as well as the space between the bed and the wall. The second room was divided by a French door only the fixed side of the French door had window covering so there is no privacy between rooms and the lights from either room shines into each other. The AC was weak and smelt like mold. The second room never got air and by the time you go to the bathroom forget it just open the window to let the air in. There is no circulation, except what was provided by a large fan in the wall between rooms that was extremely loud and reminded me of an attic fan. One good thing the bed was comfortable for a single night stay. This location does offer breakfast, but after making a waffle and getting a plate together and sitting down to eat each of us took a bite and ended up at Dennys down the street. I wasnt expecting a Hilton or the Ritz Carlton, but I was expecting something a little higher than what ended up looking like highway motel. Lucky for us we were there only one night before heading home to Los Angeles."
reviews.df$reviews.text[grepl("My first issue is the parking and there's not enough spaces. We had a suite tha",reviews.df$reviews.text, fixed = TRUE)] <- "We stayed for two nights and chose this location because it was close to things we wanted to do. My first issue is the parking and there's not enough spaces. We had a suite that had a closed off living room but the AC unit was in that room and so even with the door open the rest of the room was hot. There was fan on wall but it was so loud you couldn't talk. The room was very small, so much that only one person could fit between the tv and bed. The shower was horrible. The temp never stayed the same. The doors into the building and the elevators were barely functioning. The pool was dirty and the surrounding area. Breakfast was okay. The staff was not very friendly or accommodating. We have never been to CA and so we had a lot of questions and the staff acted like it was such a burden to answer them. Overall I wouldn't go back and I wouldn't let my friends or family go either."
reviews.df$reviews.text[grepl("husband's 60th birthday .Our kids and grandkids where all",reviews.df$reviews.text, fixed = TRUE)] <- "Staff was friendly and professional. Rooms were nice and clean. Pool and jacuzzi was great. Breakfast was delicious. We enjoyed use of the breackfast room for my husband's 60th birthday .Our kids and grandkids where all very comfortable in the suites. Thanks again!"
reviews.df$reviews.text[grepl(" ****PLEASE HIRE A SUPERVISOR FOR YOUR MAIDS",reviews.df$reviews.text, fixed = TRUE)] <- "We stayed at this hotel last year for 10 nights and while the rooms weren't completely up to date, it was clean, had extra room, breakfast was decent, and for the price we felt we got a great deal. Based on our past experience, we booked again for a family reunion and referred our family to also book this hotel. We arrived late at night and check in was fine. The lobby still looked nice and the elevator was still slow like we remembered. The first thing we noticed when we got to our room and turned the bed down were the yellow stains on the sheets. At this point I wasn't sure if the sheets had been laundered and they were permanent stains, or if the sheets had not been changed. I called the front desk and they had me come get a new set of sheets. By the time I got back up to the room, my husband had discovered the towels hanging in the bathroom had fresh lipstick/makeup all over them. I went back to talk to the front desk and the employee decided it was time to try a different room. Room 2 had a ant on top of the pillows on the bed and when we turned down the bed and pulled the pillows away from the headboard out ran 10 more. Back to the front desk we went and were now headed off to room 3. When opening the door to Room 3 we were immediately hit with what I can only describe as B.O. and it was potent. I called the front desk and told them about the smell and told them we would tough it out for the night as it was getting super late and we had a baby with us. I asked when the manager would be in the next day and was told 10 a.m. I ran the air vent on the airconditioner and also turned on the bathroom exhaust fan all night. The next morning the smell was as bad as ever. It literally was oozing out of everything in the room-the mattress, the couch. There were ants in this room also, but they were crawling on the wall instead of instead of in the bed. I headed down to visit with the manager of the hotel and found he was out due to an emergency. I expressed my concerns to the staff that was there (the housekeeping supervisor and a young gentleman) and asked to cancel the rest of my stay as well as the reservations for my family that would be arriving in town later in the week. The housekeeping supervisor came up to the room and acknowledged the smell and thought that it was possibly wet carpet-my opinion was that the room smelled due to body odor and not cleaning the room thoroughly. The only rooms the hotel had left were double rooms and she offered to let me walk through one before moving all of my belongings again, but I declined as we had already switched rooms 3 times and I also was very concerned about how many times my family coming into town would have to switch rooms to find an acceptable one.This hotel has drastically gone down hill in cleanliness since we stayed here last year. The linens in all of the rooms were stained and the towels were threadbare and grey. There were ants in 2 of the 3 rooms we checked into that night. I felt this hotel was along the lines of a motel 6 at best. Free breakfast cannot make up for a filthy hotel that is not being cleaned properly. We actually witnessed a maid RUNNING into and out of a room in less than 5 minutes. How do you clean a room that fast And we certainly didn't see a vacuum. We were wondering if the maids were paid by the room and not the hour. I do want to say the front desk staff was very accommodating and tried their hardest to find an acceptable room and I appreciate that they were willing to cancel our reservations when they realized their hotel had too many issues. ****PLEASE HIRE A SUPERVISOR FOR YOUR MAIDS AND INVEST IN REPL.ACING OLD/STAINED LINENS AND TOWELS***"
reviews.df$reviews.text[grepl("light during the day for my son's naps.3) Thin walls.",reviews.df$reviews.text, fixed = TRUE)] <- "Ok people, before the bashing begins just remember you are paying roughly 129/night and you get free WiFi and a hot breakfast to boot. All in an area where it usually runs 200+/night. You get what you want to pay for it. You will not get Hilton like amenities at this kind of deal.First the good:1) Great location, right off the freeway, 2 min from a huge shopping mall and plaza with everything under the sun to eat at. 10 min from Seaworld or Gaslamp or Qualcomm.2) Good breakfast, decent choices, some healthy items, coffee is good.3) Fridge and microwave in the rooms. AC works nicely. The bed was good, with a lot of pillows, which I believe they just switched out to new ones.4) Friendly staff at front desk. Now the not so good, or you get what you pay for:1) The pool is loud when active until 10pm. The rooms face the pool in an L shape, it echoes through the halls. If no one is there, then it is fine. During summer, it's brutal.2) The rooms do need updating. You can see it is a work in progress, but the walls in my rooms were all marked up, the decor was outdated. Hallways and elevator showed heavy signs of use. Pocket door for bathroom was very heavy and loud. One telephone didn't work, etc. Our blackout drapes were torn and pulling away from the window, so it did not stop much light during the day for my son's naps.3) Thin walls. We heard every footstep above us, every creak. We also heard everything in the halls, and with the outdoor floors you can hear everything rolling on it, or people walking through. Once asleep, not a bother.4) 8 parking fee. Whatever, it is stated on the website. Just add it to the room rate and call it free, makes the customer feel better and not nickel and dimed.5) Ant attack in our room, but they did remedy quickly when we notified. 6) The sofa bed was a joke. 7) Elevator would take us to level 3 when we pressed level 2. All day, until I told them about it.Look, this place is ok. You get good value for the rate you pay, which is cheap. It is obvious the manager is doing a lot of damage control here on TripAdvisor by trying to respond to all complaints, which reflects why the ranking for this hotel is so low, as there are a lot of complaints.Would I stay here again with family Probably not. But for a cheap room, and if I was by myself maybe. But not for my family, there were too many annoyances, but we walked out of there for under 300 total for two nights. Good luck people. But don't nail the resort for not being like Hilton when you are only paying 129/night ok!"
reviews.df$reviews.text[grepl("this property. We had never stayed at a Quality",reviews.df$reviews.text, fixed = TRUE)] <- "My wife and I recently spent a night at this property. We had never stayed at a Quality Suites property before. We came away very impressed. For our stay, at the end of May, it seemed the majority of the renovations had been completed. We received a complimentary room upgrade, thanks to Frederick Ross at the Front Desk. He was very helpful and very friendly.Our room was exceptionally clean and well appointed. The bed was firm and comfortable. Huge bathroom. There was a jacuzzi tub and even a gas fireplace.The Internet was fast and easy to hop on. The breakfast in the morning was also very good with a nice variety of hot items. It was easy to get to some very nice restaurants for a good meal as well.All in all we will now include Quality Suites in our list of choices for properties to stay at while traveling. An excellent value and"
reviews.df$reviews.text[grepl("October 9-12 and the entire weekend the ice machines and soda",reviews.df$reviews.text, fixed = TRUE)] <- "We stayed here on October 9-12 and the entire weekend the ice machines and soda machine on our floor was out of order. I did not check the other floors' soda machines. The parking was 12 a night and very tight for out Yukon XL. The breakfast was good and the beds were comfortable."
reviews.df$reviews.text[grepl("King suite with 2 double beds is great, although very tight space.",reviews.df$reviews.text, fixed = TRUE)] <- "The King suite with 2 double beds is great, although very tight space. The beds are bunkbeds in a separate room with door. Each room has a TV. There is a heated swimming pool with hot tub.The staff was very friendly and helpful. Our room was at the end of the corridor, so very quiet.The price is very reasonable, but there is a large fee for parking."
reviews.df$reviews.text[grepl("hopping mall. Free breakfast was great with fresh fruit, eggs",reviews.df$reviews.text, fixed = TRUE)] <- "The TV and refrigerator was not working. Excellent customer service to fix the problem. The location is close to restaurants and shopping mall. Free breakfast was great with fresh fruit, eggs, sausage , cereals,patties, yogurt, pastries,waffle maker. Parking fee12. The room was clean, room had 2 separate sleeping areas which was nice. Room had 1 bed and the other area had bunk beds. Good fir 4 people. Looking for a less expensive hotel this place will be fine in San Diego."
reviews.df$reviews.text[grepl("amazing time at this hotel def coming back for our next family",reviews.df$reviews.text, fixed = TRUE)] <- "we had an amazing time at this hotel def coming back for our next family vacation room was nice and cold and clean and for a decent price the staff was also very helpful and polite. Breakfast was good"
reviews.df$reviews.text[grepl("Great Stay-I was provided",reviews.df$reviews.text, fixed = TRUE)] <- "Great Stay-I was provided an upgraded suite room. Very nice. Room suite very spacious. All hotel amenities were readily available. Hotel staff was very courteous and helpful. I will definitively stay here again. I like the close proximity to freeway, shopping center and restaurants. Waffle house restaurant breakfast was fantastic and very delicious."
reviews.df$reviews.text[grepl("Very unique room configuration especially",reviews.df$reviews.text, fixed = TRUE)] <- "Very unique room configuration especially if you are a family. Doors open to the outside which all face inward to the open courtyard. First section or the room is your main bed, tv, mini fridge and microwave, then the second section separated by a siding interior door is a bunk bed, chest and tv then the back section of the room is the full bath. The bath area looks like it was updated at some time and is nice, staff has plenty of soap, shampoo, coffee and towels stocked. Staff was in the process of replacing all mattresses in the main beds of all rooms while I was there, in fact mine was being replaced as I checked in. Parking can be tough if the hotel is full especially if you drive a big car or truck. Exterior is in need of maintenance. Breakfast was plentiful and the normal Comfort Suites breakfast. Wifi was good. One thing that really disappointed me was the lack of many channels available on the TV. Had network channels, a couple of independent channels, did have Fox News but didn't work the last day I was there. No other news channels."
reviews.df$reviews.text[grepl("spa, as well as SeaWorld and LEGOLAND",reviews.df$reviews.text, fixed = TRUE)] <- "Spent the first three nights of a family trip here, using it as a base to see Legoland and Seaworld. The hotel itself is fine (we had a room with a bedroom separated by a sliding door for the children, complete with bunk beds), has a good outdoor pool and spa, and a decent free hot breakfast. All in all nothing sensational, but clean, tidy, reasonable value and - assuming you don't miss the turn off from Mission Valley Fwy! - in a convenient location, only 5-10 minutes from Seaworld and maybe half an hour from Legoland.I am very pleased you enjoyed your stay with us and you were able to enjoy our pool and spa, as well as SeaWorld and LEGOLAND."
reviews.df$reviews.text[grepl("rooms, the carpet, t.v. Is small the bathroom can use up grade tike",reviews.df$reviews.text, fixed = TRUE)] <- "Hotel is ok , but need upgrades in the rooms, the carpet, t.v. Is small the bathroom can use up grade tike in floor. When you call front desk and tell them someone right next to you is smoking and you are allergic to smoke. Accommodate me by offering another room, and not say ok and we will check it out."
reviews.df$reviews.text[grepl("appen soon. Sliding door to bathroom does not",reviews.df$reviews.text, fixed = TRUE)] <- "Arrived on a Sunday afternoon. Hotel Staff was nice upon greeting. Asked me to upgrade to a King with 2 bunk beds, but I kept the 2 double beds.The label of suite means an extra sitting room in the front with a sofa, small desk, TV, refrigerator, and microwave. The extra room made it nice for our little boy to play while we are resting in the bedroom.AC design is peculiar. Front room gets the main AC, then the room with beds is blocked by a glass sliding door. There is a fan which pulls the AC through to the bedroom. Bathroom was clean, but had a chemical smell. Carpet, wallpaper, and paint all look like they need remodel, which on the hotel website said was going to happen soon. Sliding door to bathroom does not have a lock. Nice to have 2 flat screen TVs. Bed was comfortable.We had a room in the back of the hotel, which made it quiet. The bathroom window faces a bunch of apartments, so you need to remember to close it for your privacy.Price was economical. I read the fine print about the 12 parking fee, I don't think is a big deal. However, they should create a line item in their billing before they quote the nightly price.Breakfast room was convenient and food was well stocked. I would consider going back to this hotel after the renovation. I hope they redo the AC system instead of relying on an old-fashioned fan to re-distribute the air."
reviews.df$reviews.text[grepl(" them excellent rating mostly because of the people",reviews.df$reviews.text, fixed = TRUE)] <- "I gave them excellent rating mostly because of the people that work there were so helpful. The place is nice and the room was clean. Bed was great and I slept well every night. Quiet too even though right next to freeway. Had a fridge, microwave and coffee maker in the room. There is a Trader Joe's right across the freeway and that helped with meals. I spent the week there while I visited Comic Con and a shuttle was a few steps away from there. Also full breakfast was included. Food was good. I could not eat the cheese omelette but they looked good. Free Wifi."
reviews.df$reviews.text[grepl("paid quite a bit to upgrade to a suite. Basically, it's",reviews.df$reviews.text, fixed = TRUE)] <- "We paid quite a bit to upgrade to a suite. Basically, it's a long narrow room with a divider between the window and the bed. The actual bedroom is very small and the sitting area is even smaller. The A/C is in the sitting area and you must keep the door open to allow cool air to flow into the bedroom. The additional daily parking fee exasperated the problem. The hotel sits next to the interstate but the noise did not seem to bother us once we were inside the room. The breakfast which was included was more than adequate."
reviews.df$reviews.text[grepl("hot or too cold -- the heating systems are terrible",reviews.df$reviews.text, fixed = TRUE)] <- "The only thing nice that we saw here was the pool, which looked clean and fresh (unfortunately, I didn't bring a suit). The place has very, very old and beat up furniture and was either too hot or too cold -- the heating systems are terrible, built under the window as in older cheap properties and then the beds are behind some sliding doors, so you have to leave those doors open if you want any cooling. The breakfast in the morning is really disgusting. They seem to have people making the breakfast who know nothing about what coffee is supposed to look like, for example, or what bacon strips look like. They had a whole container full of picnic bacon, which is just scraps of pork in no particular shape, generally used as bacon pieces for other foods. And here it is in their hot breakfast like you are supposed to dive in and just eat those bacon scraps as if they are slices of morning bacon. Not me!The coffee maker in the room saved the day for us, because it was actually much better than expected. In a general sense, this room was not the quality that we expected for what we paid, and we were generally disappointed. Complicating the situation, we asked for a wake up call at 10:06 PM one night, and they promised to call. We were never awakened and it caused some big problems. I hope the attached photos help people understand why they should avoid this property."
reviews.df$reviews.text[grepl("70/night this place was about half to a third of some",reviews.df$reviews.text, fixed = TRUE)] <- "If you need a pristine trendy hotel, with all the amenities, this is not the place for you. But if you are looking for clean, comfortable lodging, and can put up with a few items that are rough around the edges, this may be the place for you. At about 70/night this place was about half to a third of some of the more modern places that are literally next door. Our modest sized room was a true suite with a door between the bedroom and the sitting area. The bed was extremely comfortable and there was a big, clean bathroom. Staff were efficient and friendly. The breakfast was all we could ask for, and was generally kept well stocked. The breakfast rooms are large with lots of seating. As noted, there were some rough edges: the ventilation fans in our room were quite noisy the screen on our window was broken and the pool elevator was creaky, with a well outdated inspection report posted. The location in the valley is just off of I-8, with lots of restaurants and shopping nearby. We were able to get most anywhere we wanted to go to see the sights in 10-15 minutes."
reviews.df$reviews.text[grepl("making, breakfast etc... it has it all. The reception ",reviews.df$reviews.text, fixed = TRUE)] <- "This place was a breath of fresh air. After traveling for 6 weeks we needed a hotel that could offer laundry facilities, fridge, coffee making, breakfast etc... it has it all. The reception and housekeeping staff were all very friendly, even had fresh coffee and crisp red apples at reception. Frederick at front desk was happy to recommend local restaurants and attractions. Although the rooms are a bit dated it had a great comfy bed and a separate lounge. The daily breakfast was fabulous. It even had a hot dish to offer each day. I would highly recommend this hotel to any couple wanting a value for money stay."
reviews.df$reviews.text[grepl("room/sitting area (with a separate tv)",reviews.df$reviews.text, fixed = TRUE)] <- "My family and I (2 kids 2 adults) stayed at Quality Suites San Diego Sea World Area in early Oct 2018. We stayed in a king size room with a pull out sofa bed. The king bed was very comfy but the pull out sofa wasnt comfy for the kids and wouldnt be able to accommodate an adult. You would be better to get another bedding configuration. The pull out sofa bed was in an offset room/sitting area (with a separate tv) that can be closed with a glass sliding door. The full room is compact so if you have very big bags, they wouldnt be able to be left open all the time as you probably couldnt walk past them. The kids loved the pool, and we could here a little bit of noise from where we were but not much (we were on the 3rd floor in the far corner so it was the quietest area). The breakfast and breakfast area was good. The staff were friendly and the location was central to the zoo and the airport. We used Lyft to go places. You could go across the highway to the mission valley shopping area but we didnt do that. There is a Dennys, petrol station, benihana, Wendys and another food place within several minutes walk (and a Wendys further) on the hotel side of the highway. Overall we had a good stay as it provided everything we needed for our couple of nights."
reviews.df$reviews.text[grepl(" take FOREVER to replenish the hot foods. Overall,)",reviews.df$reviews.text, fixed = TRUE)] <- "Nice lush landscaping, nice pool. Close to freeways and gas station. Stayed in room 315. The kids liked having their own space. Hard to take a shower, no or low pressure. Air conditioner didn't seem to be working. I called front desk, girl said they hadn't had any complaints! Seriously I'm calling! Was so hot for 3 nights! There was a stand up fan so I assumed they knew the AC wasn't working. Breakfast was blah. Make sure you have time because the breakfast area was packed and they take FOREVER to replenish the hot foods. Overall, my stay was fine. Kids were happy."
reviews.df$reviews.text[grepl("stay. THANK YOU COMFORT SUITES!We)",reviews.df$reviews.text, fixed = TRUE)] <- "The hotel is nice, the bunk bed room is the best for a family. Our kids were so excited to have their own room and bunk beds. The pool and hottub were fun too. Breakfast is decent too. But what sold me is the customer service! I will go back. I left a very expensive toy realized it after a 10 hour drive home. They were SO HELPFUL, housekeeping, front desk, everyone. I just got it in the mail less than a week after our stay. THANK YOU COMFORT SUITES!"
reviews.df$reviews.text[grepl("mismatched, scuffed, phones are old,",reviews.df$reviews.text, fixed = TRUE)] <- "Towels are yellow and gray, wallpaper is dirty and marked, room is musty and smelly, carpet looks 30 years old, furniture is mismatched, scuffed, phones are old, the building is old and falling apart. The only nice area was the lobby."
reviews.df$reviews.text[grepl("refurbishing (which I think is happening soon).",reviews.df$reviews.text, fixed = TRUE)] <- "As with most Mission Valley area hotels, the location is good.The rooms are very aged. Definitely time for some refurbishing (which I think is happening soon). The morning breakfast is pretty nice with a good variety of foods for everyone.Parking is pretty tight if you get there late. Parking in the back is really hard with a larger vehicle. As with other Mission Valley hotels, not sure why they charge for parking other than adding on some extra bucks to their pockets. If you are family looking for included breakfast and don't mind roughing it just a bit, this is the place for you, if you can get a good rate."
reviews.df$reviews.text[grepl("up fixing it- because it was wired up weird.",reviews.df$reviews.text, fixed = TRUE)] <- "The room was not bad and it was a family room with two separate rooms. The refrigerator did not work and we had to call maintenance. He didn't fix it but we ended up fixing it- because it was wired up weird. We had fans in the room but there was no place to plug them in which was a little strange. There was an open granola bar left on the floor from the last guest that I guess housekeeping missed a couple of days in a row. The hotel was not a bad price but was still not really worth the price we paid. Breakfaste was average and very crowded every morning."
reviews.df$reviews.text[grepl("our plans. Housekeeping did a terrific job keeping the",reviews.df$reviews.text, fixed = TRUE)] <- "Comfort Suite Mission Valley was in a great location for our plans. Housekeeping did a terrific job keeping the room clean and comfortable. The building needed some work especially on flooring by the second story stair case. Parking was definitely limited and we were unaware of a parking fee per day until checking in, that really hit a nerve. Despite that we really enjoyed our stay and the comfort it provided."
reviews.df$reviews.text[grepl("r approximately 40 people and an additional outside patio",reviews.df$reviews.text, fixed = TRUE)] <- "Overall, a great place to stay. Area is east of Hotel Circle with less congestion easier freeway access. Stayed on the ground floor, steps from the pool and breakfast room. The room was a small suite that appeared to have been recently remodeled with 2 flat screen tv's, fridge, microwave, separate living area with a sofa bed, queen bed in the bedroom and a large bathroom. One of the best free hot breakfast buffets we've ever encountered. Huge serving area, separate dining area for approximately 40 people and an additional outside patio seating area. We stayed on a weekend and no lines for breakfast. Be aware that the hotel charges 12 a day for parking (lot is very large). At less than 75 a night on a weekend, this hotel is definitely a keeper!"
reviews.df$reviews.text[grepl("displeased. STAY AWAY FROM THIS HOTEL. It ran me 135",reviews.df$reviews.text, fixed = TRUE)] <- "My family and I checked in on Thursday 9/21 around 9pm. The parking lot was very small and hard to get a spot. When we finally got in the lobby, it looked clean and presentable. Our room was an entirely different story. It was facing outside by the pool. The light was already on in our room before we entered. Once we got in I was immediately disgusted. It smelled horrible (almost like a really bad sulfur), room looked very outdated and unappealing. Room was small, beds were old and I was so grossed out. I went to the front desk and explained how disappointed I was and they let me look at a few other rooms they offered--all of them terrible. We stayed put as there was no sense in moving to another awful room and quickly went to bed. When I woke up the next morning there were TWO cockroaches in the bathroom. YUCK! Southern California, I get it, bugs can happen. Every other hotel we stayed at for the rest of the week was so lovely. Very clean, up to date, and well kept. We had a few other family members that stayed in different rooms at this hotel and they were all equally displeased. STAY AWAY FROM THIS HOTEL. It ran me 135 after 12 for parking. We had 3 more rooms reserved at this hotel again for that Sunday but we canceled them immediately. Gross, outdated, smelly, and OVERPRICED. Never again will I stay at this hotel! BUYER BEWARE. Also, when I checked out of the hotel, the man at the desk asked me how everything was. I told him You don't want to know and the didn't have any response...not a sorry, to hear that, nothing.."
reviews.df$reviews.text[grepl("Just other GREAT STAY. ",reviews.df$reviews.text, fixed = TRUE)] <- "We always have stay at Comfort Inn for the last 8 years. We have stayed at this hotel for the last 5 years when we visit San Diego, CA. We have stayed at other hotel but in the end we choose Comfort inn. They have a Great Staff very good Breakfast and they listen to you. Just other GREAT STAY. Because the hotel will make the different in your time of stay."
reviews.df$reviews.text[grepl("breakfast was wonderful. Some noise heard through the walls from other tenants,",reviews.df$reviews.text, fixed = TRUE)] <- "Nice comfortable motel. Bed was comfortable, room was clean, breakfast was wonderful. Some noise heard through the walls from other tenants, but not a big problem. Costs we're reasonable and we would stay there again given the chance."
reviews.df$reviews.text[grepl("The outdoor heated pool/hot tub area was nice and well stocked with towels.",reviews.df$reviews.text, fixed = TRUE)] <- "I recently visited the area for Spring Break. This location was perfect. Close to all of the major attractions (Petco Park, Sea World, SD Zoo, etc). We were very close to the interstate but the location was very quiet. Shopping centers, movie theater, and numerous restaraunts nearby. We had issues checking in and the room type we booked was not available the first night. We were put in a smaller room but we did get a discounted rate for the inconvenience. Breakfast included hot items but they left a bit to be desired but my kids loved the make your own waffles (get there early or prepare to stand in line for quite a while). The outdoor heated pool/hot tub area was nice and well stocked with towels. The rooms were a bit tight (family suites) but they were very clean and the beds were comfortable. There are lots of families that stay here so there was constant noise from children running the corridors so if you are looking for peace and quiet this might not be for you. Overall a nice stay despite a few issues."
reviews.df$reviews.text[grepl("all charge for parking, and charge a lot more. This is a pretty",reviews.df$reviews.text, fixed = TRUE)] <- "We had two rooms for one night here, just staying over with some family after a college graduation ceremony. It was Memorial weekend, so understandably full of lots of families with younger children. The rooms were clean I appreciate the sheets covering the blanket and the fitted bottom sheet on the bed. Two areas in the room separated by glass doors. Two TV's, wi-fi, small frig and microwave:) Our internet rate was very reasonable for the area, especially considering it was a suite. They do charge 8 for parking most we have stayed at in the Hotel Circle area don't. The downtown hotels in San Diego almost all charge for parking, and charge a lot more. This is a pretty central location to get to all of San Diego's attractions. The breakfast was fairly standard, but is a good convenience to have to get something to eat in the AM before taking off to do touristy things."
reviews.df$reviews.text[grepl(" live in San Diego and I choose them because of the service and its very quite",reviews.df$reviews.text, fixed = TRUE)] <- "The service was great. I was there for the weekend to get away. This is my second time staying there. I love the spilt rooms. I would reccomend this hotel to all my friends. I live in San Diego and I choose them because of the service and its very quite."
reviews.df$reviews.text[grepl(" right,personnel very accommodating--a little hard",reviews.df$reviews.text, fixed = TRUE)] <- "good, clean and convenient, price was right,personnel very accommodating--a little hard to find some places due to no signage--would go again--food was very good at breakfast and service excellent. would ck out ref. before if needed..ours wasn't connected and didn't work when connected..but we had a cooler."
reviews.df$reviews.text[grepl(" a La Quinta property right next door.The breakfas",reviews.df$reviews.text, fixed = TRUE)] <- "This hotel is located in the Hotel circle area of San Diego. Please note being a San Diego resident, this hotel is not close to the downtown / gaslamp area or tourist attractions of San Diego. You are a good 15 / 20 minute cab ride to downtown. Most people stay at these hotels to be near the shopping malls. You will need a car to get around out here, the freeways around hotel circle are all over the place.The room itself is clean and big, but a little dated and the old wooden furniture could use a change, but understand that this hotel is more towards the budget end of hotels in San Diego, so its very decent for the price and surround by other hotels with a La Quinta property right next door.The breakfast is your average one with choices of bread, muffins and some scrambled eggs and sausage, coffee, or make yourself a waffle and some yogurts are available.The bed was pretty comfortable and i felt safe in my room and could not hear any noise from the neighbors. It's also nice you have a lounge area in your room to watch tv and relax and be away from the bedroom. The room also has a fridge and a microwave in them.Overall this is a good location if you need to be in Mission valley."
reviews.df$reviews.text[grepl("my family (we have two children of 4 and 5 years old). We chose a king suite",reviews.df$reviews.text, fixed = TRUE)] <- "We have just finished our trip here and all was perfect for my family (we have two children of 4 and 5 years old). We chose a king suite. The king bed was really comfortable. The room is tiny but there is so much to do in San Diego that we did not have to stay in the room other than to sleep. Perfect breakfast. Perfect pool and hot tub. Clean. The only negative point is that the hotel is not really eco friendly... plastic glass, plastic utensils....Well situated (need a car)."
reviews.df$reviews.text[grepl("l go from just average to very good.Overall, this hotel is a solid choice",reviews.df$reviews.text, fixed = TRUE)] <- "We stayed here for one night with our 2 young kids. As mentioned in other reviews, some of the suites are quite small, although some of that is probably just due to having a divider in the middle of the two parts. It was nice having a divider between us and the kids, even if it was just a clear door. The one suggestion I would make to improve the property would be to have either clouded glass or curtains over the glass part of the doors so that they're not completely transparent. That way you can still watch TV after the kids have gone to bed without worrying about waking them up.While the rooms show some use, the property has been kept up well. The pool was clean and heated and there are some nice areas for eating outside in the perfect San Diego weather. The hotel is about 10 minutes from Sea World, Mission Bay, and downtown,so its a quick jump to everywhere, although the downside is that nowhere is within walking distance. The best part of the hotel was the staff. They were very accommodating when we requested a crib for the room and, after we left one of our phone chargers, they went to our room and called us when they had found it. Service like that is what makes this hotel go from just average to very good.Overall, this hotel is a solid choice, as long as you don't mind driving to the local attractions. Also, just as a tip, there's a parking fee for parking in the lot, however, you can park on the steet for free, as long as you can find a spot."
reviews.df$reviews.text[grepl("to another hotel today! DISAPPOINTED",reviews.df$reviews.text, fixed = TRUE)] <- "It looks decent from the outside but the rooms are dingy and dirty. We were on the second floor and heard EVERY step the people took above us! We also woke up to water under the sink seeping onto the bathroom floor! We gave it a try but we are moving on to another hotel today! DISAPPOINTED"
reviews.df$reviews.text[grepl(" refrigerator, micro and pool. Parking was 12. per night.Nice place if y",reviews.df$reviews.text, fixed = TRUE)] <- "The staff was the other best thing about this hotel. They were very friendly and efficient, getting our keys replaced (twice), killing the ants eating my birthday cake and clearing the stopped up toilet.This is an older hotel, but the price is reasonable, it was clean and it has a great location near downtown and a mega shopping center. The rooms have some wear, but were roomy with a bedroom and sitting room. There was also free wi-fi, breakfast, computer room, refrigerator, micro and pool. Parking was 12. per night.Nice place if you're not too picky and can keep a sense of humor."


# isolate hotels' reviews
hampton.vec <- reviews.df$reviews.text[reviews.df$name == "Hampton Inn San Diego/Mission Valley"]
quality <- reviews.df[reviews.df$name == "Quality Suites San Diego SeaWorld Area",]
quality <- quality[order(quality$reviews.date, decreasing = T),]
quality.vec <- quality$reviews.text[1:35]

# clean text
hampton.vec <- clean.vec(hampton.vec)
quality.vec <- clean.vec(quality.vec)

# collapse the vector to a single document
# the object "all" is a corpus with two documents
hampton.vec <- paste(hampton.vec, collapse=" ")
quality.vec <- paste(quality.vec, collapse=" ")

all <- c(hampton.vec, quality.vec) #create the corpus (vector source) 

#create the corpus (vector source) with two documents
corpus <- VCorpus(VectorSource(all))

# create the Term Document Matrix
tdm <- TermDocumentMatrix(corpus, control=list(weighting=weightTf))
inspect(tdm[1:10,1:2])
tdm.m <- as.matrix(tdm) #convert to a matrix
#change column names
colnames(tdm.m) = c("Hampton Inn", "Quality Suites")
#choose color palette
display.brewer.all()
pal <- brewer.pal(9, "Purples")

#remove the 4 lightest colors
pal <- pal[-(1:4)] 
####construct a commonality cloud
set.seed(123) # for reproducibility
commonality.cloud(tdm.m, max.words = 200, random.order=FALSE,colors=pal) 

####construct a comparison cloud
comparison.cloud(tdm.m, max.words = 100, random.order=FALSE,title.size = 1,
                 colors=brewer.pal(ncol(tdm.m), "Dark2"))
####construct a Polarized Tag Plot - we would like to see the words with the highest difference
#only terms that appear in both documents
common.words <- subset(tdm.m, tdm.m[,1] > 0 & tdm.m[,2] > 0)
tail(common.words)
difference <- abs(common.words[,1] - common.words[,2]) #find the difference
# add a new column that contains the difference
common.words <- cbind(common.words, difference)
#order by the third column in decreasing order
common.words <- common.words[order(common.words[,3],decreasing = TRUE), ]
#select the first 25 term values
top25.df <- data.frame(x = common.words[1:25, 1],y = common.words[1:25, 2],
                       labels = rownames(common.words[1:25, ]))
head(top25.df)
#create the plot
# x contains the amazon frequency, y the delta frequency
pyramid.plot(top25.df$x, top25.df$y, labels = top25.df$labels,
             gap = 30, top.labels = c("Hampton Inn", "Words", "Quality Suites"),
             main = "Words in Common", unit = NULL) 

######################################### sentiment analysis
#Add polarity column
sentiment <- sentiment_by(reviews.df$reviews.text)
reviews.df$polarity <- sentiment$ave_sentiment
#Export polarity file
write.csv(reviews.df,"reviews.csv")

###### analyze chosen hotel
# only chosen hotel's reviews
quality <- reviews.df[reviews.df$name == "Quality Suites San Diego SeaWorld Area",]
sentiment <- sentiment_by(quality$reviews.text)
write.csv(quality,"quality.csv")

#create a histogram and a density plot to see the polarity distribution
ggplot(sentiment, aes(x=ave_sentiment, y=..density..))+ theme_gdocs() + xlab("polarity") +
  geom_histogram(binwidth=.1,fill="darkred",colour="grey60", size=.2) + geom_density(size=.75)

###Comparison cloud (positive and negative polarity)
#Split to positive and negative reviews (0.15 threshold)
positive.vec <- quality[quality$polarity >= 0.15, "reviews.text"]
negative.vec <- quality[quality$polarity < 0.15, "reviews.text"]

#Clean vectors
positive.vec <- clean.vec(positive.vec)
negative.vec <- clean.vec(negative.vec)

#collapse the vector to a single document
positive.vec <- paste(positive.vec, collapse=" ") 
negative.vec <- paste(negative.vec, collapse=" ") 

#Create one corpus with 2 documents
all <- c(positive.vec, negative.vec)
corpus <- VCorpus(VectorSource(all))

#create TDM
tdm <- TermDocumentMatrix(corpus, control=list(weighting=weightTfIdf))
tdm.m <- as.matrix(tdm)
inspect(tdm)[1:5,1:2]

# view top frequent terms
term.freq <- rowSums(tdm.m)
top.terms <- term.freq[order(term.freq, decreasing = T)][1:100]
top.terms
#change column names
colnames(tdm.m) <- c("Positive", "Negative")

####construct a comparison cloud
comparison.cloud(tdm.m, max.words = 50, random.order=FALSE,title.size = 1,
                 colors=brewer.pal(ncol(tdm.m), "Dark2"))

############# sentence tokenization
s <- quality$reviews.text
s <- as.String(s)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

sentences <- sent_token_annotator(s, a2)
## The actual entities in the string
s[sentences]

# build a corpus
x <- s[sentences]

myCorpus <- VCorpus(VectorSource(x))

myCorpus <- tm_map(myCorpus, removePunctuation)# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(tolower))# convert to lower case
custom.stopwords <- c(stopwords("english"),'san diego', 'hotel', 'stay','regards','regarding')
myCorpus <- tm_map(myCorpus, removeWords, custom.stopwords)
myCorpus <- tm_map(myCorpus, removeNumbers)# remove numbers
myCorpus <- tm_map(myCorpus, stripWhitespace)# remove extra whitespace

myCorpus <- tm_map(myCorpus, content_transformer(lemmatize_strings))

## topic modeling
# DTM, *without* tfXidf weighting
dtm <- DocumentTermMatrix(myCorpus)
dtmMatrix <- as.matrix(dtm)
write.csv(dtmMatrix, 'mydataDTM.csv')

# clean empty rows
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words
empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]]
myCorpus <- myCorpus[-as.numeric(empty.rows)]
sentences.df <- as.data.frame(myCorpus)

# number of topics
result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 5, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", #VEM/Gibbs
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

lda <- LDA(dtm.new, k = 9, control=list(seed=0,alpha=0.1),method = "Gibbs")
termsByTopic <- terms(lda, 10)
termsByTopic

head(lda@gamma,10) #topic distribution for each document (ten first documents)

topics <- topics(lda)
topics  #assignment of term to a topic
table(topics) #number of terms in each topic
#topics.labeled <- recode(topics, '1'='general experience','2'='staff service','3'='room utility', '4' = 'misc.',
#                         '5' = 'hotel utility', '6' = '???', '7' = 'room', '8' = 'location', '9' = 'hotel utility',
#                         '10' = 'parking charges', '11' = 'breakfast')
topics.labeled <- recode(topics, '1'='misc','2'='general','3'='staff services', '4' = 'breakfast',
                         '5' = 'room features', '6' = 'overall experience', '7' = 'location',
                         '8' = 'room utility', '9' = 'parking charges')

topics.df <- data.frame(topic=factor(topics.labeled))
sentences.with.topic <- cbind(text = sentences.df$text, topic = as.character(topics.df$topic))
sentences.with.topic <- as.data.frame(sentences.with.topic)

write.csv(sentences.with.topic,"sentenecs.csv")

## fine tuning
sentences.with.topic$topic[11] <- 'general'

sentences.with.topic$topic[98] <- 'general'

sentences.with.topic$topic[543] <- 'staff services'
sentences.with.topic$topic[546] <- 'misc'

sentences.with.topic$topic[364] <- 'room features'

sentences.with.topic$topic[524] <- 'breakfast'

sentences.with.topic$topic[370] <- 'parking charges'

sentences.with.topic$topic[456] <- 'staff services'
sentences.with.topic$topic[540] <- 'room features'
sentences.with.topic$topic[541] <- 'room features'

sentences.with.topic$topic[207] <- 'general'

# add new words to the dictionary
updated_hash_sentiment <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                             x = data.frame(
                                                               words = c('close','nearby','sea world','zoo',
                                                                         'shop center', 'movie theater', 'shop mall',
                                                                         'mission valley', 'mission bay','min away',
                                                                         'minute away','thin wall','footstep','small',
                                                                         'wall paper','hear every step','legoland',
                                                                         'charge','please hire','need update','joke',
                                                                         'outdate','like mold','loud',
                                                                         'need maintenance','chemical smell'),
                                                               polarity = c(0.1,0.75,0.75,0.75,0.75,0.75,0.75,0.75
                                                                            ,0.75,0.75,0.75,-0.75,-0.75,-0.75,
                                                                            -0.75,-0.75,0.75,-0.75,-0.75,-0.75
                                                                            ,-0.75,-0.75,-0.75,-0.75,-0.75,-0.75),
                                                               stringsAsFactors = FALSE
                                                             )
)

sentiment <- sentiment_by(sentences.with.topic$text,polarity_dt=updated_hash_sentiment)
sentences.with.topic$polarity <- sentiment$ave_sentiment

# Polarity Distribution by Topic
ggplot(sentences.with.topic, aes(x = as.factor(topic),
                      y = polarity,
                      fill = as.factor(topic))) +
  geom_boxplot() + 
  guides(fill = "none") +
  labs(title = "Polarity Distribution by Topic",
       x = "Topic",
       y = "Polarity")

