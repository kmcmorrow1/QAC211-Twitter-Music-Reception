## Kevin McMorrow

setwd('Desktop')
library(rtweet)
library(httr)
library(httpuv)
library(tm)
library(wordcloud)

appname = "kevdog"
key = "MBL0H3EkRae6B9pbKN88QOZmq"
secret = "aSUxvOgwVsHuWpaPkmrn9gdTsPGsoluvBxlsRqUm60JcyaHwB6"

twitter_token = create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

q = ('country music OR folk music OR americana music') #change this

dfLA = search_tweets(q, type="recent",geocode="34.029287,-118.262078,20mi",
                   token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000) #LA
dfLA$region = 'Southwest'
dfLA$num = 1
dfNY = search_tweets(q, type="recent",geocode="40.7128,-74.0059,20mi",
                   token=twitter_token, include_rts = FALSE, usr=TRUE, n=5000) #NY
dfNY$region = 'Northeast'
dfNY$num = 2
dfATL = search_tweets(q, type="recent",geocode="33.7490,-84.3880,20mi",
                   token=twitter_token, include_rts = FALSE, usr=TRUE, n=5000) #ATL
dfATL$region = 'Southeast'
dfATL$num = 3
dfSEAT = search_tweets(q, type="recent",geocode="47.6062,-122.3321,500mi",
                            token=twitter_token, include_rts = FALSE, usr=TRUE, n=5000) #SEAT
dfSEAT$region = 'Northwest'
dfSEAT$num = 4
#Merge the dfs... Might be unneccesary
n_df = Reduce(function(x, y) merge(x, y, all=TRUE), list(dfLA, dfNY, dfATL, dfSEAT))

country_df = n_df
rap_df = n_df
rock_df = n_df

#Wordcloud
x = n_df$text
x = gsub("[^A-Za-z0-9 ,.:;!?]", " ", x)
x = gsub("[ ]{2,}", " ", x)
x = gsub("https", " ", x)
x = gsub('music', " ", x)
x = gsub('country', " ", x)
x = gsub('rap', " ", x)
x = gsub('tco', " ", x)
doc = Corpus(VectorSource(x))
dtm = DocumentTermMatrix(x=doc, control=list(removePunctuation=T, removeNumbers=T, tolower=T,
                                             wordLengths=c(3,12), stopwords=T,
                                             weighting= function(x) weightBin(x)))
dtm_mat = as.matrix(dtm)
word_freq = colSums(dtm_mat)

s = colSums(dtm_mat)
k = order(s, decreasing=T)
w = colnames(dtm_mat)[k][1:500] #change
w_mat = dtm_mat[, w]
p = scale(w_mat)
k = n_df$num

opt = par(mfrow=c(2,2))
for (j in 1:4) {
  if (sum(k==j)< 4) {next}
  wordcloud(words=colnames(w_mat), freq=colSums(w_mat[k == j, ]), 
            max.words=50, main=paste("region:", n_df$region[n_df$num == j][1]))
  print(n_df$region[n_df$num == j][2])
}
#SW NE
#SE NW
#-------------------------------
#Facial recognition
nrow(country_df) #1444 rows
nrow(rock_df) #2033 rows
nrow(rap_df) #5149 rows

##creates a new df of 500 randomly chosen rows
#these will be used for the facial recognition portion
rand_countrydf = country_df[sample(nrow(country_df), 500), ] 
rand_rock_df = rock_df[sample(nrow(rock_df), 500), ]
rand_rapdf = rap_df[sample(nrow(rap_df), 500), ]

#-------------------------------
#Country facial recognition:

u_vec = unique(rand_countrydf$screen_name)
length(u_vec)
udf = lookup_users(users=u_vec, token=twitter_token, tw=FALSE)
nrow(udf)

endpoint = "https://api.kairos.com/detect"
app_id = "aa1cc858"
app_key = "f073ee6c5e0154294742ff1d666796a4"

image_url = gsub("_normal", "", udf$profile_image_url)
x = data.frame(id = seq(from=1, to=nrow(udf)), 
               screen_name = udf$screen_name,
               num_faces = rep(0, times=nrow(udf)),
               gender = rep("", times=nrow(udf)),
               age = rep(0, times=nrow(udf)),
               maleConfidence = rep(0, times=nrow(udf)),
               femaleConfidence = rep(0, times=nrow(udf)),
               asian = rep(0, times=nrow(udf)),
               hispanic = rep(0, times=nrow(udf)),
               black = rep(0, times=nrow(udf)),
               white = rep(0, times=nrow(udf)),
               other = rep(0, times=nrow(udf)),
               info = rep("", times=nrow(udf)),
               stringsAsFactors=F)

for (j in 1:nrow(udf)) {
  cat("j is", j, "\n")
  json_string = sub("xxx", image_url[j], '{ "image":"xxx"}' )
  
  m = regexpr("[A-Za-z]{3}$", image_url[j])
  ext = tolower(regmatches(image_url[j], m))
  
  ext_test = ext %in% c("jpg", "png")
  if (!ext_test) {
    x$info[j] = "Bad image"
    next
  }
  
  s = POST(url=endpoint, 
           add_headers("app_id"= app_id,
                       "app_key"=app_key),
           content_type="application/json",
           body=json_string)
  
  Sys.sleep(0.1)
  
  if (status_code(s) != 200) {
    x$info[j] = "Not_OK"
    next    
  }
  
  if (length(httr::content(s, as="raw")) < 300) {
    x$info[j] = "API error"
    next
  }
  
  w = httr::content(s, as="parsed")
  
  x$num_faces[j] = length(w$images[[1]]$faces)
  x$gender[j] = w$images[[1]]$faces[[1]]$attributes$gender$type
  x$age[j] = w$images[[1]]$faces[[1]]$attributes$age
  x$maleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$maleConfidence
  x$femaleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$femaleConfidence
  x$asian[j] = w$images[[1]]$faces[[1]]$attributes$asian
  x$hispanic[j] = w$images[[1]]$faces[[1]]$attributes$hispanic
  x$black[j] = w$images[[1]]$faces[[1]]$attributes$black
  x$white[j] = w$images[[1]]$faces[[1]]$attributes$white
  x$other[j] = w$images[[1]]$faces[[1]]$attributes$other
  
}

k = nchar(x$info) > 0
country_x2 = x[!k, ]


cmerge = merge(x=rand_countrydf[, c("screen_name", "text")], y=country_x2, by.x="screen_name", by.y="screen_name", all=FALSE)
write.csv(cmerge, "text_and_face.csv", row.names=F)

#----------------------------

#Rap facial recognition:

u_vec = unique(rand_rapdf$screen_name)
length(u_vec)
udf = lookup_users(users=u_vec, token=twitter_token, tw=FALSE)
nrow(udf)

endpoint = "https://api.kairos.com/detect"
app_id = "aa1cc858"
app_key = "f073ee6c5e0154294742ff1d666796a4"

image_url = gsub("_normal", "", udf$profile_image_url)
x = data.frame(id = seq(from=1, to=nrow(udf)), 
               screen_name = udf$screen_name,
               num_faces = rep(0, times=nrow(udf)),
               gender = rep("", times=nrow(udf)),
               age = rep(0, times=nrow(udf)),
               maleConfidence = rep(0, times=nrow(udf)),
               femaleConfidence = rep(0, times=nrow(udf)),
               asian = rep(0, times=nrow(udf)),
               hispanic = rep(0, times=nrow(udf)),
               black = rep(0, times=nrow(udf)),
               white = rep(0, times=nrow(udf)),
               other = rep(0, times=nrow(udf)),
               info = rep("", times=nrow(udf)),
               stringsAsFactors=F)

for (j in 1:nrow(udf)) {
  cat("j is", j, "\n")
  json_string = sub("xxx", image_url[j], '{ "image":"xxx"}' )
  
  m = regexpr("[A-Za-z]{3}$", image_url[j])
  ext = tolower(regmatches(image_url[j], m))
  
  ext_test = ext %in% c("jpg", "png")
  if (!ext_test) {
    x$info[j] = "Bad image"
    next
  }
  
  s = POST(url=endpoint, 
           add_headers("app_id"= app_id,
                       "app_key"=app_key),
           content_type="application/json",
           body=json_string)
  
  Sys.sleep(0.1)
  
  if (status_code(s) != 200) {
    x$info[j] = "Not_OK"
    next    
  }
  
  if (length(httr::content(s, as="raw")) < 300) {
    x$info[j] = "API error"
    next
  }
  
  w = httr::content(s, as="parsed")
  
  x$num_faces[j] = length(w$images[[1]]$faces)
  x$gender[j] = w$images[[1]]$faces[[1]]$attributes$gender$type
  x$age[j] = w$images[[1]]$faces[[1]]$attributes$age
  x$maleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$maleConfidence
  x$femaleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$femaleConfidence
  x$asian[j] = w$images[[1]]$faces[[1]]$attributes$asian
  x$hispanic[j] = w$images[[1]]$faces[[1]]$attributes$hispanic
  x$black[j] = w$images[[1]]$faces[[1]]$attributes$black
  x$white[j] = w$images[[1]]$faces[[1]]$attributes$white
  x$other[j] = w$images[[1]]$faces[[1]]$attributes$other
  
}

k = nchar(x$info) > 0
rap_x2 = x[!k, ]

rmerge = merge(x=rand_countrydf[, c("screen_name", "text")], y=rap_x2, by.x="screen_name", by.y="screen_name", all=FALSE)
write.csv(m2, "text_and_face.csv", row.names=F)


#-----------------------

#Rock facial recognition

u_vec = unique(rand_rock_df$screen_name)
length(u_vec)
udf = lookup_users(users=u_vec, token=twitter_token, tw=FALSE)
nrow(udf)

endpoint = "https://api.kairos.com/detect"
app_id = "aa1cc858"
app_key = "f073ee6c5e0154294742ff1d666796a4"

image_url = gsub("_normal", "", udf$profile_image_url)
x = data.frame(id = seq(from=1, to=nrow(udf)), 
               screen_name = udf$screen_name,
               num_faces = rep(0, times=nrow(udf)),
               gender = rep("", times=nrow(udf)),
               age = rep(0, times=nrow(udf)),
               maleConfidence = rep(0, times=nrow(udf)),
               femaleConfidence = rep(0, times=nrow(udf)),
               asian = rep(0, times=nrow(udf)),
               hispanic = rep(0, times=nrow(udf)),
               black = rep(0, times=nrow(udf)),
               white = rep(0, times=nrow(udf)),
               other = rep(0, times=nrow(udf)),
               info = rep("", times=nrow(udf)),
               stringsAsFactors=F)

for (j in 1:nrow(udf)) {
  cat("j is", j, "\n")
  json_string = sub("xxx", image_url[j], '{ "image":"xxx"}' )
  
  m = regexpr("[A-Za-z]{3}$", image_url[j])
  ext = tolower(regmatches(image_url[j], m))
  
  ext_test = ext %in% c("jpg", "png")
  if (!ext_test) {
    x$info[j] = "Bad image"
    next
  }
  
  s = POST(url=endpoint, 
           add_headers("app_id"= app_id,
                       "app_key"=app_key),
           content_type="application/json",
           body=json_string)
  
  Sys.sleep(0.1)
  
  if (status_code(s) != 200) {
    x$info[j] = "Not_OK"
    next    
  }
  
  if (length(httr::content(s, as="raw")) < 300) {
    x$info[j] = "API error"
    next
  }
  
  w = httr::content(s, as="parsed")
  
  x$num_faces[j] = length(w$images[[1]]$faces)
  x$gender[j] = w$images[[1]]$faces[[1]]$attributes$gender$type
  x$age[j] = w$images[[1]]$faces[[1]]$attributes$age
  x$maleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$maleConfidence
  x$femaleConfidence[j] = w$images[[1]]$faces[[1]]$attributes$gender$femaleConfidence
  x$asian[j] = w$images[[1]]$faces[[1]]$attributes$asian
  x$hispanic[j] = w$images[[1]]$faces[[1]]$attributes$hispanic
  x$black[j] = w$images[[1]]$faces[[1]]$attributes$black
  x$white[j] = w$images[[1]]$faces[[1]]$attributes$white
  x$other[j] = w$images[[1]]$faces[[1]]$attributes$other
  
}

k = nchar(x$info) > 0
rock_x2 = x[!k, ]

rrmerge = merge(x=rand_rock_df[, c("screen_name", "text")], y=rock_x2, by.x="screen_name", by.y="screen_name", all=FALSE)
write.csv(rrmerge, "text_and_face.csv", row.names=F)

#----------------------------

View(country_x2)
View(rap_x2)
View(rock_x2)

country_x2$male = (ifelse(country_x2$gender == 'M', 1, 0))
hist(x = country_x2$male, xlim=c(0,1), breaks =2, xlab = 'Gender', ylab = 'Frequency', main = 'Gender of Twitter Users (from Country Dataset)',
     col = c('red','blue'))
legend(legend = c('Female','Male'), x = 0.7, y =100, lty=c(1,1), lwd = c(5,5), col = c('red','blue'))


rap_x2$male = (ifelse(rap_x2$gender == 'M', 1, 0))
hist(x = rap_x2$male, xlim=c(0,1), breaks =2, xlab = 'Gender', ylab = 'Frequency', main = 'Gender of Twitter Users (from Rap/Hip-Hop Dataset)',
     col = c('red','blue'))
legend(legend = c('Female','Male'), x = 0.1, y =40, lty=c(1,1), lwd = c(5,5), col = c('red','blue'))

rock_x2$male = (ifelse(rock_x2$gender == 'M', 1, 0))
hist(x = rock_x2$male, xlim=c(0,1), breaks =2, xlab = 'Gender', ylab = 'Frequency', main = 'Gender of Twitter Users (from Rock Dataset)',
     col = c('red','blue'))
legend(legend = c('Female','Male'), x = 0.1, y =65, lty=c(1,1), lwd = c(5,5), col = c('red','blue'))


country_x2$young = (ifelse(country_x2$age <30, 1,0))
sum(country_x2$young)
l = hist(x = country_x2$age, xlab = 'User Age', main = 'Twitter User Ages (from Country Dataset)')
l$density = l$counts/sum(l$counts)*100
plot(l, freq = FALSE, main = 'Twitter User Ages (from Country Dataset)', ylab = 'Percentage', xlab='User Age')

rap_x2$young = (ifelse(rap_x2$age <30, 1,0))
sum(rap_x2$young)
z = hist(x = rap_x2$age, xlab = 'User Age', main = 'Twitter User Ages (from Rap Dataset)')
z$density = z$counts/sum(z$counts)*100
plot(z, freq = FALSE, main = 'Twitter User Ages (from Rap Dataset)', ylab = 'Percentage', xlab = 'User Age')

rock_x2$young = (ifelse(rock_x2$age <30, 1,0))
sum(rock_x2$young)
z = hist(x = rock_x2$age, xlab = 'User Age', main = 'Twitter User Ages (from Rock Dataset)')
z$density = z$counts/sum(z$counts)*100
plot(z, freq = FALSE, main = 'Twitter User Ages (from Rock Dataset)', ylab = 'Percentage', xlab = 'User Age')

country_x2$asian1 = ifelse(country_x2$asian > .5, 1, 0)
country_x2$hispanic1 = ifelse(country_x2$hispanic > .5, 1, 0)
country_x2$black1 = ifelse(country_x2$black > .5, 1, 0)
country_x2$white1 = ifelse(country_x2$white > .5, 1, 0)
country_x2$other1 = ifelse(country_x2$other > .5, 1, 0)

hist(x = c(country_x2$asian1,country_x2$hispanic1,country_x2$black1,country_x2$white1,country_x2$other1))
names = c('asian','hispanic','black', 'white','other')
sums = c(sum(country_x2$asian1), sum(country_x2$hispanic1), sum(country_x2$black1), sum(country_x2$white1), sum(country_x2$other1))

m = table(names,sums)
m
b = matrix(c('Asian','Hispanic','Black','White','Other',sum(country_x2$asian1),sum(country_x2$hispanic1),
             sum(country_x2$black1),sum(country_x2$white1), sum(country_x2$other1)), nrow = 2, ncol = 5)
b[,3] = 'Hispanic'
b[,4] = 'White'
b[,5]= 'Other'
b[2,] = 9
b[2,2] = 18
b[2,3] = 18
b[2,4] = 119
b[2,5]= 0
b
#-------------------------------------
rock_x2$asian1 = ifelse(rock_x2$asian > .5, 1, 0)
rock_x2$hispanic1 = ifelse(rock_x2$hispanic > .5, 1, 0)
rock_x2$black1 = ifelse(rock_x2$black > .5, 1, 0)
rock_x2$white1 = ifelse(rock_x2$white > .5, 1, 0)
rock_x2$other1 = ifelse(rock_x2$other > .5, 1, 0)

sums = c(sum(rock_x2$asian1), sum(rock_x2$hispanic1), sum(rock_x2$black1), sum(rock_x2$white1), sum(rock_x2$other1))
sums
names = c('Asian','Hispanic','Black','White','Other')


k = matrix(c('Asian','Hispanic','Black','White','Other',6, 10, 26, 76,2), ncol=5, nrow = 2)
k[,2]='Hispanic'
k[,3]='Black'
k[,4]='White'
k[,5]='Other'
k[2,1]= 6
k[2,2]= 10
k[2,3]=26
k[2,4]=76
k[2,5]=2
k

#-----------------------


rap_x2$asian1 = ifelse(rap_x2$asian > .5, 1, 0)
rap_x2$hispanic1 = ifelse(rap_x2$hispanic > .5, 1, 0)
rap_x2$black1 = ifelse(rap_x2$black > .5, 1, 0)
rap_x2$white1 = ifelse(rap_x2$white > .5, 1, 0)
rap_x2$other1 = ifelse(rap_x2$other > .5, 1, 0)
names = c('Asian','Hispanic','Black','White','Other')
sums = c(sum(rap_x2$asian1), sum(rap_x2$hispanic1), sum(rap_x2$black1), sum(rap_x2$white1), sum(rap_x2$other1))
k=matrix(names,ncol=5)
k
g = matrix(sums, ncol=5)
#**this is the proper way...**
k = rbind(k,g)
k

#-----------------------------------------
#Create a dataframe for each artist -- check out the public reception (use pitchfork/needledrop as a keyword??)


y = 'slowdive album'
s_df = search_tweets(y, type="recent",
                          token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000)
yy = 'logic album'

l_df = search_tweets(yy, type="recent",
                           token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000)
xy = 'gorillaz album'
g_df = search_tweets(xy, type="recent",
                     token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000)

zy = 'harry styles album'
h_df = search_tweets(zy, type="recent",
                     token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000)

ky = 'kendrick lamar album'
k_df = search_tweets(ky, type="recent",
                            token=twitter_token,include_rts = FALSE, usr=TRUE, n=5000)

#albums in play...
View(g_df) #gorillaz 
View(s_df) #slowdive
View(l_df) #logic
View(h_df) #harry styles





gwords = c('good','incredible','amazing','awesome','best','excellent','strong') #words w/ good connotation
bwords = c('bad','horrible','terrible','awful','worst', 'weak', 'bland') #words w/ bad conn.

counter = 0
#returns the the table of words, and total sum
for (s in gwords) {
  print(table(grepl(s, s_df$text, ignore.case=T)))
  counter = counter + sum((grepl(s, s_df$text, ignore.case=T)))
}

counter
w_percent = (counter/(nrow(s_df)))*100
w_percent










