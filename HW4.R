movies <- read.csv("Movies2022F-4.csv")

#General Plots

#Score vs Country
boxplot(movies$imdb_score~movies$country, main='Distribution of Score by Country ', xlab='Country', ylab='Score', col=colors) 
#did not include this on slides because there is a lot of data, and I could not capture it all in an image

#Score vs Genre
boxplot(movies$imdb_score~movies$genre, main='Distribution of Score by Genre ', xlab='Genre', ylab='Score', col=colors)

#Score vs Content
boxplot(movies$imdb_score~movies$content, main='Distribution of Score by Content ', xlab='Content', ylab='Score', col=colors)

#Score vs Budget
boxplot(movies$imdb_score~movies$Budget, main='Distribution of Score by Budget ', xlab='Budget', ylab='Score', col=colors)

#Score vs Gross
boxplot(movies$imdb_score~movies$Gross, main='Distribution of Score by Gross ', xlab='Gross', ylab='Score', col=colors)

tapply(movies$imdb_score, movies$country ,mean) 
tapply(movies$imdb_score, movies$genre ,mean)
tapply(movies$imdb_score, movies$content ,mean)
tapply(movies$imdb_score, movies$Budget ,mean)
tapply(movies$imdb_score, movies$Gross ,mean)

barplot


#PART A

#1
mexico_R <- movies[movies$country=='Mexico' & movies$content=='R',]
tapply(mexico_R$imdb_score, mexico_R$content, mean)
sd(mexico_R$imdb_score)

france_R <- movies[movies$country=='France' & movies$content=='R',]
tapply(france_R$imdb_score, france_R$content, mean)
sd(france_R$imdb_score)


#Plots to compare

f_movies <- subset(movies, country == "France" & content == "R")
boxplot(f_movies$imdb_score~f_movies$content, main='Distribution of Score by Content for France ', xlab='R rating', ylab='Score', col=colors)

m_movies <- subset(movies, country == "Mexico" & content == "R")
boxplot(m_movies$imdb_score~m_movies$content, main='Distribution of Score by Content for Mexico ', xlab='R rating', ylab='Score', col=colors)


#Z test to get p value and prove you will reject null hypothesis
mean1=7.440526
mean2= 6.822129
stdev1=1.043756
stdev2=0.8032415
size1=19
size2=202
round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)

#2
sk_budget <- movies[movies$country == "South Korea" & movies$Budget == "Medium",]
tapply(sk_budget$imdb_score, sk_budget$content, mean)
sd(sk_budget_score$imdb_score)

sa_budget <- movies[movies$country == "South Africa" & movies$Budget == "Medium",]
tapply(sa_budget$imdb_score, sa_budget$content, mean)
sd(sa_budget_score$imdb_score)


#Plots to compare

sk_movies <- subset(movies, country == "South Korea" & Budget == "Medium")
boxplot(sk_movies$imdb_score~sk_movies$Budget, main='Distribution of Score by Budget for South Korea ', xlab='Medium budget', ylab='Score', col=colors)

sa_movies <- subset(movies, country == "South Africa" & Budget == "Medium")
boxplot(sa_movies$imdb_score~sa_movies$Budget, main='Distribution of Score by Budget for South Africa ', xlab='Medium budget', ylab='Score', col=colors)


#Z test to get p value and prove you will reject null hypothesis
mean1=6.6
mean2=5.6585 
stdev1=0.8670285
stdev2= 2.137912 
size1=21
size2=33
round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)


#3
usa_scifi <- movies[movies$country == "USA" & movies$genre == "Sci-Fi",]
tapply(usa_scifi$imdb_score, usa_scifi$genre, mean)
sd(usa_scifi$imdb_score)

uk_scifi <- movies[movies$country == "UK" & movies$genre == "Sci-Fi",]
tapply(uk_scifi$imdb_score, uk_scifi$genre, mean)
sd(uk_scifi$imdb_score)


#Plots to compare

usa_movies <- subset(movies, country == "USA" & genre == "Sci-Fi")
boxplot(usa_movies$imdb_score~usa_movies$genre, main='Distribution of Score by Genre for USA ', xlab='Sci-Fi', ylab='Score', col=colors)

uk_movies <- subset(movies, country == "UK" & genre == "Sci-Fi")
boxplot(uk_movies$imdb_score~uk_movies$genre, main='Distribution of Score by Genre for UK ', xlab='Sci-Fi', ylab='Score', col=colors)


#Z test to get p value and prove you will reject null hypothesis
mean1=6.706818
mean2= 6.298237 
stdev1=1.134244
stdev2=1.14018
size1=44
size2=448
round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)



#PART B

#A: p close to but not 0, reject null hypothesis

mexico_R <- movies[movies$country=='Mexico' & movies$content=='R',]
tapply(mexico_R$imdb_score, mexico_R$content, mean)
sd(mexico_R$imdb_score)

france_R <- movies[movies$country=='France' & movies$content=='R',]
tapply(france_R$imdb_score, france_R$content, mean)
sd(france_R$imdb_score)


#Plots to compare --> already done above in Part A for this set of hypotheses


#Z test to get p value and prove you will reject null hypothesis
mean1=7.440526
mean2= 6.822129
stdev1=1.043756
stdev2=0.8032415
size1=19
size2=202
pvalue1 <- round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)
pvalue1

plot(x=seq(from = -1, to= 1, by=0.1),
     y=dnorm(seq(from = -1, to= 1,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=pvalue1, col='red')



#B: p close to significance level, but reject null hypothesis

india_PG <- movies[movies$country == "India" & movies$content == "PG",]
tapply(india_PG$imdb_score, india_PG$content, mean)
sd(india_PG$imdb_score)

hungary_movies <- movies[movies$country == "Hungary" & movies$content == "PG",]
tapply(hungary_movies$imdb_score, hungary_movies$content, mean)
sd(hungary_movies$imdb_score)


#Plots to compare

india_movies <- subset(movies, country == "India" & content == "PG")
boxplot(india_movies$imdb_score~india_movies$content, main='Distribution of Score by Content for India ', xlab='PG', ylab='Score', col=colors)

hungary_movies <- subset(movies, country == "Hungary" & content == "PG")
boxplot(hungary_movies$imdb_score~hungary_movies$content, main='Distribution of Score by Content for Hungary ', xlab='PG', ylab='Score', col=colors)



#Z test to get p value and prove you will reject null hypothesis
mean1=6.845
mean2= 5.9175
stdev1=1.255878
stdev2=0.03304038
size1= 6
size2=4
pvalue2 <- round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)

plot(x=seq(from = -1, to= 1, by=0.1),
     y=dnorm(seq(from = -1, to= 1,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=pvalue2, col='red')



#C: p greater than significance level, fail to reject null hypothesis

german_comedy <- movies[movies$country == "Germany" & movies$genre == "Comedy",]
tapply(german_comedy$imdb_score, german_comedy$content, mean)
sd(german_comedy$imdb_score)

russian_comedy <- movies[movies$country == "Russia" & movies$genre == "Comedy",]
tapply(russian_comedy$imdb_score, russian_comedy$content, mean)
sd(russian_comedy$imdb_score)


#Plots to compare

germany_movies <- subset(movies, country == "Germany" & genre == "Comedy")
boxplot(germany_movies$imdb_score~germany_movies$genre, main='Distribution of Score by Genre for Germany ', xlab='Comedy', ylab='Score', col=colors)

russia_movies <- subset(movies, country == "Russia" & genre == "Comedy")
boxplot(russia_movies$imdb_score~russia_movies$genre, main='Distribution of Score by Genre for Russia ', xlab='Comedy', ylab='Score', col=colors)


#Z test to get p value and prove you will fail to reject null hypothesis
mean1=6.091081
mean2=6.086667
stdev1=1.328741
stdev2=0.9988869
size1=74
size2=9
pvalue3 <- round(ztest(mean1,mean2,stdev1,stdev2,size1,size2),3)

plot(x=seq(from = -1, to= 1, by=0.1),
     y=dnorm(seq(from = -1, to= 1,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=pvalue3, col='red')