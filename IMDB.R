library("ggplot2")
library("dplyr")
library("plotly")
library("ggpubr")
library("car")
library("rstatix")
library("lattice")
movie <- read.csv("movie_data.csv")
movie <- movie[,c(7,10,13,14)]
movie <- na.omit(movie)
summary(movie)
ggplot(data=movie, aes(x=title_year, imdb_score)) +
  geom_point() +
  geom_rug(col="green",alpha=0.1, size=1.5)

ggplot(data=movie,aes(x=num_voted_users, imdb_score))
geom_point() +
  geom_rug(col="green",alpha=0.1, size=1.5)

ggplot(data=movie, aes(x=num_user_for_reviews, imdb_score)) 
geom_point() +
  geom_rug(col="green",alpha=0.1, size=1.5)
shapiro.test(movie$num_voted_users)
shapiro.test(movie$imdb_score)
cor.test(movie$num_voted_users,movie$imdb_score, method = 'spearman', exact=F)
ggplot(movie, aes(x=num_voted_users, y=imdb_score)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE,color="red")+ylim(0,10)+xlab("Number of voted users")+
  ylab("IMDB Score")+ggtitle("IMDB Score vs Number of voted users")
movie <- read.csv("movie_data.csv")
movie <- movie[-c(1,9)]
movie <- na.omit(movie)
ggplot(movie, aes(duration, imdb_score)) +geom_point(colour = "red", size = 0.5) +
  geom_smooth(method = "lm", se = F) + xlab("Duration of the Movie")+
  ylab("IMDB Scores")+ ggtitle("Relation Between Duration of a Movie and Its IMDB Score")
shapiro.test(movie$duration)
shapiro.test(movie$imdb_score)
cor.test(movie$duration,movie$imdb_score, method = 'spearman')
languages <- unique(movie$language)
vec_languages <- c()
for(i in 1:length(languages)){
  if(sum(movie$language==languages[i])>10){
    vec_languages <- c(vec_languages,languages[i])
  }
}
vec_languages <- vec_languages[-c(1,3,7,8)]
movie1 <- movie[movie$language %in% vec_languages,]
durbinWatsonTest(lm(imdb_score~language,data = movie1))
outliers <- movie1 %>% group_by(language) %>% identify_outliers(imdb_score)
data.frame(outliers$is.outlier,outliers$is.extreme)
dotplot(imdb_score~language,data=movie1,xlab="IMDB Score",ylab="Language",
        main="IMDB Score vs Language")
bartlett.test(imdb_score~language,data=movie1)

res_aov <- aov(imdb_score ~ language,data = movie1)
shapiro.test(res_aov$residuals)
ggdensity(res_aov$residuals,color = "red",xlab="Residual",ylab = "Density",
          size=1,title = "Normality Check")
kruskal.test(imdb_score~language,data=movie1)
ggplot(movie1, aes(x=language, y=imdb_score,fill=language)) + 
  geom_boxplot(aes(color = language))+theme(legend.position="none")+
  scale_fill_brewer(palette="Dark2")+xlab("Language")+ylab("IMDB Score")+
  ggtitle("IMDB Score vs Language")

