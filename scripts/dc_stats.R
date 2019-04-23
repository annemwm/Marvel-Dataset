### Marvel Summaries ### 
setwd("C:/Users/Anne/Desktop/all_edges_nodes/dc")
marvel_chars <- data.frame()
tmp = list.files(pattern="*.csv")
marvel_df = lapply(tmp, read.csv)

titles <- c("Batman Begins", "Batman V Superman", "Green Lantern", "Justice League", "Man of Steel", "Suicide Squad", "The Dark Knight", "The Dark Knight Rises", "Wonder Woman")
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


dc_summary <- data.frame()
for (i in (1: length(marvel_df))){
  movie <- marvel_df[[i]]
  movie_summary <- get_stats(movie)
  dc_summary <- rbind(dc_summary, movie_summary)
}
dc_summary <- cbind(titles, dc_summary)
write.csv(marvel_summary, "marvel_summary.csv")


mega_data_dc <- data.frame()
for (i in (1: length(dc_df))){
  mega_data_dc <- rbind(mega_data_dc, dc_df[[i]])
}
dups <- mega_data_dc[duplicated(mega_data_dc$CHARACTER),]
mega_data_dc<-mega_data_dc[!duplicated(mega_data_dc$CHARACTER),]

mega_sum_dc <- get_stats(mega_data_dc)
write.csv(mega_sum, "mega_summary.csv")

double <- data.frame(table(as.character(dups$CHARACTER)))
double$Freq <- as.vector(double$Freq)+1

mega_data_dc <- cbind(mega_data_dc, "OCCUR"=1)
for (i in 1:nrow(double)){
  char <- as.character(double$Var1[i])
  ct <- double$Freq[i]
  mega_data_dc[mega_data_dc$CHARACTER==char,]$OCCUR <- ct 
}
write.csv(mega_data, "mega_data.csv")

mega_ladies_dc <- mega_data_dc[mega_data_dc$GENDER=="female" & (mega_data_dc$STATUS == "human hero" | mega_data_dc$STATUS == "superhero" | mega_data_dc$STATUS =="alien hero"),]
mega_guys_dc <- mega_data_dc[mega_data_dc$GENDER =="male" & (mega_data_dc$STATUS == "human hero" | mega_data_dc$STATUS == "superhero" | mega_data_dc$STATUS =="alien hero"),]
summary(mega_ladies_dc$OCCUR)
hist(mega_ladies_dc$OCCUR)
summary(mega_guys_dc$OCCUR)
hist(mega_guys_dc$OCCUR)

mega_data_dc_top <- mega_data_dc[mega_data_dc$CHARACTER %in% deg[1:10],]
nrow(mega_data_dc_top[mega_data_dc_top$GENDER=="male",])/10

movie<-mega_data_dc
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


dates <- mega_data_dc$RELEASE_DATE#levels(mega_data_dc$RELEASE_DATE)
dates <- as.integer(str_sub(dates, -2,-1))
hist(dates, breaks=12)

