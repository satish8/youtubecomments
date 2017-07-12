library(tuber)
library(stringr)
library(text2vec)
library(textreuse)
library(qdap)
library(dplyr)
library(stringi)

#getting  youtube oath
yt_oauth(app_id = NULL, app_secret = NULL, scope = "ssl",
         token = ".httr-oauth")

#Reading all subtitles files from directory
subtitle_all<-list.files(path ="/home/satish/test/subtitles" ,pattern="*.vtt")

# function to fetch and clean youtube comment
process_comment<-function(video_id,f_name){
  comment<-get_comment_threads(filter = c(video_id = video_id))
  mymat<-as.data.frame(comment$textOriginal)
  y <- gsub("[[:punct:]]","",mymat$`comment$textOriginal`)
  y<-gsub("\n", " ",y)
  y<-str_trim((y))
  y<-gsub("\\s+"," ",y)
  y<-tolower(y)
  mymat<-as.data.frame(y)
  for(i in 1:nrow(mymat)) {
    myfile <- file.path("/home/satish/test/nlp", paste0(f_name,"_comment", i, ".txt"))
    # filename = paste("/home/satish/test", i, ".txt", sep="")
    write.table(mymat[i,], myfile ,sep = "", row.names = FALSE, col.names = FALSE,
                quote = FALSE, append = FALSE)
  }
  
}

#function to clean you tube subtitles
subtitle_clean<-function(subtitle_name){
  d = read.table(paste("/home/satish/test/subtitles/",subtitle_name,sep = ""), sep="\n")
  y <- gsub("[[:punct:]]","",d$textOriginal)
  y<-gsub("\n", " ",y)
  dd<-d[!grepl('-->', d$V1),]
  dd<-as.data.frame(dd)
  dd<-paste(unlist(dd), collapse =" ")
  dd<-tolower(dd)
  dd<-gsub("\\s+"," ",dd)
  write.table(dd,paste("/home/satish/test/nlp/",subtitle_name,".txt",sep = ""), sep="\n" ,row.names = FALSE, col.names = FALSE,
              quote = FALSE)
}



#calling functions to get and clean comments 
process_comment("qdJvJH84R3U","hanuman_chalisa")
process_comment("o2fb6FiaL9g","sare_janha se")
process_comment("HL0oVX14JMs","love_my _india")
process_comment("51dbuDzTxp8","maha_mrityunjaya")
process_comment("AWr9GARBeH8","jan_gan_man")


#getting cleaned subtitles
subtitle_clean(subtitle_all[1])
subtitle_clean(subtitle_all[2])
subtitle_clean(subtitle_all[3])
subtitle_clean(subtitle_all[4])
subtitle_clean(subtitle_all[5])


#creating tokens to get similarity score
my_corpus <- TextReuseCorpus(dir = "/home/satish/test/nlp",
                             tokenizer = tokenize_ngrams, n = 3,
                             minhash_func = minhash, keep_tokens = TRUE,
                             progress = FALSE)

# getting result  of matched comments
result<-pairwise_compare(my_corpus, jaccard_similarity) %>% 
  round(3) %>% 
  pairwise_candidates() %>% 
  arrange(desc(score))%>%
  filter(score>0 )%>%
  filter((b%in%subtitle_all))%>%
  filter(!(a%in%subtitle_all))


