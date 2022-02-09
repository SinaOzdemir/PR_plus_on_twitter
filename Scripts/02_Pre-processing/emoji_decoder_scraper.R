
# directories -------------------------------------------------------------

dataDIR<- paste0(getwd(),"/Data/")


# libraries ---------------------------------------------------------------
devtools::install_github("https://github.com/hadley/emo")

packs<- c("tidyverse","emo")

lapply(packs, library,character.only =T)



# The real decoder --------------------------------------------------------

emoji_decoder_2 <- data.frame(emoji_text = names(ji_name),
                              emoji_code = as.character(ji_name))

emoji_decoder_2$emoji_code<-as.character(emoji_decoder_2$emoji_code)

emoji_decoder_2$emoji_text <- paste0(" [",emoji_decoder_2$emoji_text,"] ")

emoji_decoder_a<-grep(pattern = "*asteri|keycap_star",x = emoji_decoder_2$emoji_text,ignore.case = T,value = T)

emoji_decoder_2 <- emoji_decoder_2[-which(emoji_decoder_2$emoji_text%in%emoji_decoder_a),]

# replace the emojis in the text ------------------------------------------


#there is a special character somewhere that breaks the function, need to find that one

twt_sample <- readRDS(file = paste0(dataDIR,"raw_coding_sample.RDS"))

for (i in 1:nrow(emoji_decoder_2)) {
  
  print(paste0("replacing ",emoji_decoder_2$emoji_text[i],"regex is ", emoji_decoder_2$emoji_code[i]))
  
  twt_sample$text<- str_replace_all(string = twt_sample$text,
                                  pattern = emoji_decoder_2$emoji_code[i],
                                  replacement = emoji_decoder_2$emoji_text[i])
}

twt_sample$text<-gsub("&","and",twt_sample$text)

twt_sample$status_id<-paste0("x",twt_sample$status_id)

twt_sample$text<-gsub(";",",",twt_sample$text)

twt_sample$text<-gsub("\r?\n|\r"," ",twt_sample$text)

a<-twt_sample$text[grepl(pattern = "We must find ways, together, to mitigate a bigger social crisis after the one brought on by the #coronavirus.",x = twt_sample$text,fixed = T)]

twt_sample %>%
  select(screen_name,created_at,status_id,text,lang) %>%
  saveRDS(object = .,file = paste0(dataDIR,"coding_sample.RDS"))

twt_sample %>%
  select(screen_name,created_at,status_id,text,lang) %>%
  write.table(x = .,file = paste0(dataDIR,"coding_sample.txt"),
              sep = ";",col.names = T,row.names = T,fileEncoding = "UTF-8",qmethod = "double")

twt_sample %>%
  select(screen_name,created_at,status_id,text,lang) %>%
  write.table(x = .,file = paste0(dataDIR,"coding_sample.csv"),
              sep = ";",col.names = T,row.names = T,fileEncoding = "UTF-8",qmethod = "double")


#Deprecated code-chunks

#(D) Scrape unicodes of emojies (it is pretty useless tho) ------------------------------------------------------


emoji_dictionary<- read_html(x = "http://www.unicode.org/emoji/charts/full-emoji-list.html")

emoji_no <- emoji_dictionary %>% html_nodes(x = .,css = "td.rchars") %>% html_text() %>% as.numeric()

emoji_utf_codes <- emoji_dictionary %>% rvest::html_nodes(x = .,css = ".code a") %>% html_text(trim = T)

emoji_descriptions<- emoji_dictionary %>% rvest::html_nodes(x = .,css = ".name") %>% html_text(trim = T) %>% gsub(pattern = " ",replacement = "-",x = .)

#emoji skin color:

emoji_skin <- read_html("https://www.unicode.org/emoji/charts/full-emoji-modifiers.html#1f468_1f3fe_200d_1f91d_200d_1f468_1f3fd")

emoji_skin_no<- emoji_skin %>% html_nodes(css="td.rchars") %>% html_text() %>% as.numeric()

emoji_skin_code <- emoji_skin %>% html_nodes(css = ".code") %>% html_text()

emoji_skin_description <- emoji_skin %>% html_nodes(css = ".name") %>% html_text()

emoji_skin_description_a <- str_split(string = emoji_skin_description,pattern = ":",simplify = T)

emoji_skin_description_a[,1]<-gsub(pattern = " ",replacement = "-",emoji_skin_description_a[,1])

emoji_skin_description_a[,2]<-emoji_skin_description_a[,2] %>% trimws() %>% paste0("[",.,"]")

emoji_skin_description_b <- paste0(emoji_skin_description_a[,1],emoji_skin_description_a[,2])

#bind them all in darkness

emoji_skin_df <- data.frame(emoji_no = emoji_skin_no,
                            emoji_utf_codes = emoji_skin_code,
                            emoji_descriptions = emoji_skin_description_b)


emoji_df <- data.frame(emoji_no = emoji_no,
                       emoji_utf_codes = emoji_utf_codes,
                       emoji_descriptions = emoji_descriptions)

emoji_decoder <- rbind(emoji_df,emoji_skin_df)

saveRDS(emoji_decoder)


