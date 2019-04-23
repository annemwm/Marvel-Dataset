### Marvel Summaries ### 
setwd("C:/Users/Anne/Desktop/all_edges_nodes/marvel")
marvel_chars <- data.frame()
tmp = list.files(pattern="*.csv")
marvel_df = lapply(tmp, read.csv)

titles <- c("Ant Man", "Ant Man and the Wasp", "Avengers: Age of Ultron", "Avengers: Infinity War", "Black Panther", "Captain America: Civil War", "Captain America: the First Avenger", "Captain America: the Winter Soldier", "Doctor Strange", "Guardians of the Galaxy","Guardians of the Galaxy 2","Iron Man","Iron Man 2", "Iron Man 3", "Spider-Man: Homecoming", "The Avengers", "The Incredible Hulk", "Thor", "Thor: Ragnarok", "Thor: The Dark World")

get_stats <- function(movie){
  nchars <- nrow(movie)
  
  nmale <- nrow(movie[movie$GENDER=="male",])
  nfemale <- nrow(movie[movie$GENDER=="female",])
  
  pmale <- nmale/nchars
  pfemale <- nfemale/nchars
  
  # good %? power %?
  pgood <- nrow(movie[movie$ALIGNMENT=="good",])/nchars
  nhero <- nrow(movie[movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero",])
  phero <- nrow(movie[movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero",])/nchars
  
  pbeard <- nrow(movie[movie$FACIAL == "y",])/nmale # ignore women
  pbald <- nrow(movie[movie$HEAD == "bald",])/nmale # ignore women here
  
  # given bald and beard, bad?
  pbadgbald <- nrow(movie[movie$HEAD == "bald" & movie$ALIGNMENT == "bad",])/nrow(movie[movie$HEAD=="bald",])
  pbadgbeard <- nrow(movie[movie$FACIAL == "y" & movie$ALIGNMENT == "bad",])/nrow(movie[movie$FACIAL=="y",])
  
  #given bad, bald and beard?
  pbaldgbad <- nrow(movie[movie$HEAD == "bald" & movie$ALIGNMENT == "bad",])/nrow(movie[movie$ALIGNMENT=="bad" & movie$GENDER == "male",])
  pbeardgbad <- nrow(movie[movie$FACIAL == "y" & movie$ALIGNMENT == "bad",])/nrow(movie[movie$ALIGNMENT=="bad" & movie$GENDER == "male",])
  
  pbeardbaldgbad <- nrow(movie[movie$HEAD == "bald" & movie$FACIAL == "y" & movie$ALIGNMENT=="bad",])/nrow(movie[movie$ALIGNMENT=="bad",])
  
  # gender and badness
  pmangbad <- nrow(movie[movie$ALIGNMENT =="bad" & movie$GENDER=="male",])/nrow(movie[movie$ALIGNMENT=="bad",])
  pwomangbad <- nrow(movie[movie$ALIGNMENT =="bad" & movie$GENDER=="female",])/nrow(movie[movie$ALIGNMENT=="bad",])
  
  pbadgman <- nrow(movie[movie$ALIGNMENT =="bad" & movie$GENDER=="male",])/nmale
  pbadgwoman <- nrow(movie[movie$ALIGNMENT =="bad" & movie$GENDER=="female",])/nfemale
  
  
  # power & gender? 
  pmangpower <- nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "male",])/ nhero
  pwomangpower <- nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female",])/ nhero
  
  ppowergman <- nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "male",])/ nmale
  ppowergwoman <- nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female",])/ nfemale
  
  pgoodgpowf <- nrow(movie[movie$ALIGNMENT=="good" & (movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female",])/nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female",])
  pgoodgpowm <- nrow(movie[movie$ALIGNMENT=="good" & (movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "male",])/nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "male",])
  
  return(data.frame("nchars"=nrow(movie), "percent_men"=pmale, "percent_women"=pfemale, "percent_good"=pgood, "percent_hero"=phero, "percent_beard"=pbeard, "percent_bald"=pbald ,"percent_badgivenbald"=pbadgbald,
           "percent_badgbeard"=pbadgbeard, "percent_baldgbad"=pbaldgbad, "percent_beardgbad"=pbeardgbad, "percent_beardbaldgbad"=pbeardbaldgbad ,"percent_mengbad"=pmangbad,
           "percent_womengbad"=pwomangbad, "percent_badgman"=pbadgman, "percent_badgwoman"=pbadgwoman, "percent_manghero"=pmangpower,
           "percent_womanghero"=pwomangpower, "percent_herogman"=ppowergman, "percent_herogwoman"=ppowergwoman, "per_goodgherow"=pgoodgpowf, "per_goodgherom"=pgoodgpowm))
}


marvel_summary <- data.frame()
for (i in (1: length(marvel_df))){
  movie <- marvel_df[[i]]
  movie_summary <- get_stats(movie)
  marvel_summary <- rbind(marvel_summary, movie_summary)
}
marvel_summary <- cbind(titles, marvel_summary)
write.csv(marvel_summary, "marvel_summary.csv")

mega_data2 <- mega_data
mega_data <- data.frame()
for (i in (1: length(marvel_df))){
  mega_data <- rbind(mega_data, marvel_df[[i]])
}
dups <- mega_data[duplicated(mega_data$CHARACTER),]
mega_data<-mega_data[!duplicated(mega_data$CHARACTER),]

mega_sum <- get_stats(mega_data)
write.csv(mega_sum, "mega_summary.csv")

double <- data.frame(table(as.character(dups$CHARACTER)))
double$Freq <- as.vector(double$Freq)+1

mega_data <- cbind(mega_data, "OCCUR"=1)
for (i in 1:nrow(double)){
  char <- as.character(double$Var1[i])
  ct <- double$Freq[i]
  mega_data[mega_data$CHARACTER==char,]$OCCUR <- ct + 1
}
write.csv(mega_data, "mega_data.csv")

mega_ladies <- mega_data[mega_data$GENDER=="female" & (mega_data$STATUS == "human hero" | mega_data$STATUS == "superhero" | mega_data$STATUS =="alien hero"),]
mega_guys <- mega_data[mega_data$GENDER =="male" & (mega_data$STATUS == "human hero" | mega_data$STATUS == "superhero" | mega_data$STATUS =="alien hero"),]
summary(mega_ladies$OCCUR)
hist(mega_ladies$OCCUR)
summary(mega_guys$OCCUR)
hist(mega_guys$OCCUR)

mega_data_36 <- mega_data[mega_data$CHARACTER %in% deg.top36,]
nrow(mega_data_36[mega_data_36$GENDER=="male",])/36

movie<-mega_data
fisher.test(cbind(c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female",]), 
                    nrow(movie[movie$GENDER=="female",])), c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero")
                                                                        & movie$GENDER == "male",]), nrow(movie[movie$GENDER=="male",]))))

fisher.test(cbind(c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female" 
        
                                                      & movie$ALIGNMENT=="good",]), nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") 
                                                                     & movie$GENDER=="female",])), 
                  c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") 
                               & movie$GENDER == "male" & movie$ALIGNMENT =="good",]), 
                    nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER=="male",]))))

# badness test
fisher.test(cbind(c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER == "female" 
                               & movie$ALIGNMENT=="bad",]), nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") 
                                                                        & movie$GENDER=="female",])), 
                  c(nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") 
                               & movie$GENDER == "male" & movie$ALIGNMENT =="bad",]), 
                    nrow(movie[(movie$STATUS == "human hero" | movie$STATUS == "superhero" | movie$STATUS =="alien hero") & movie$GENDER=="male",]))))

# bald and beard and good overall good to b&b&b over all bad
fisher.test(cbind(c(nrow(movie[movie$HEAD == "bald" & movie$FACIAL == "y" & movie$ALIGNMENT=="bad" & movie$GENDER=="male",]),nrow(movie[movie$ALIGNMENT=="bad" & movie$GENDER=="male",])), 
                  c(nrow(movie[movie$HEAD == "bald" & movie$FACIAL == "y" & movie$ALIGNMENT=="good" & movie$GENDER=="male",]), nrow(movie[movie$ALIGNMENT=="good" & movie$GENDER=="male",]))))

# bald  and good overall good to b&b over all bad
fisher.test(cbind(c(nrow(movie[movie$HEAD == "bald" & movie$ALIGNMENT=="bad" & movie$GENDER=="male",]),nrow(movie[movie$ALIGNMENT=="bad" & movie$GENDER=="male",])), 
                  c(nrow(movie[movie$HEAD == "bald" & movie$ALIGNMENT=="good" & movie$GENDER=="male",]), nrow(movie[movie$ALIGNMENT=="good" & movie$GENDER=="male",]))))

# beard  and good overall good to b&b over all bad
fisher.test(cbind(c(nrow(movie[movie$FACIAL=="y" & movie$ALIGNMENT=="bad" & movie$GENDER=="male",]),nrow(movie[movie$ALIGNMENT=="bad" & movie$GENDER=="male",])), 
                  c(nrow(movie[movie$FACIAL=="y" & movie$ALIGNMENT=="good" & movie$GENDER=="male",]), nrow(movie[movie$ALIGNMENT=="good" & movie$GENDER=="male",]))))


