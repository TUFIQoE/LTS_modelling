library(jsonlite)
library(tidyverse)
library(purrr)
library(ggplot2)
library(plyr)
library(gtools)
library(tidyr)
library(tidyverse)

temp = list.files(pattern="*.json")
df_s <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_s) <- c('external_id', 'date', 'video', 'q2', 'q4', 'q3')
var = length(temp)
for (y in 1: var) {
  json_data <- read_json(path=temp[y])
  l = length(json_data)
  for (x in 1: l) {
    my_list <-json_data[x]
    for (z in 1:7){
      my_list <- unlist(my_list, recursive = FALSE)}
    if ("result.reply_form.content1.answer.value" %in% names(my_list) == TRUE & "result.reply_form.content2.answer.value" %in% names(my_list) == TRUE) {
      new_list <- my_list[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value", "result.reply_form.content2.answer.value")]
      new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
      new_list$result.reply_form.content2.answer.value[is_null(new_list$result.reply_form.content2.answer.value)] = NA
      new_list$result.reply_form.content.answer.value[is_null(new_list$result.reply_form.content.answer.value)] = NA
      df_s[nrow(df_s) + 1,] <- c(new_list)
    }
    else if ("result.reply_form.content1.answer.value" %in% names(my_list) == TRUE) {
      new_list <- my_list[c("external_id", "date", "result.video", "result.reply_form.content1.answer.value")]
      new_list$result.reply_form.content1.answer.value[is_null(new_list$result.reply_form.content1.answer.value)] = NA
      new_list$result.reply_form.content2.answer.value =  NA
      new_list$result.reply_form.content.answer.value =  NA
      df_s[nrow(df_s) + 1,] <- c(new_list)
    }
    else if ("result.reply_form.content.answer.value" %in% names(my_list) == TRUE) {
      new_list <- my_list[c("external_id", "date", "result.video")] 
      new_list$result.reply_form.content1.answer.value =  NA
      new_list$result.reply_form.content2.answer.value =  NA
      new_list$result.reply_form.content.answer.value = as.character(my_list[c("result.reply_form.content.answer.value")])
      #new_list$result.reply_form.content.answer.value[is_null(new_list$result.reply_form.content.answer.value)] = 0
      df_s[nrow(df_s) + 1,] <- c(new_list)
    }
    else {
      new_list <- my_list[c("external_id", "date", "result.video")]
      new_list$result.reply_form.content1.answer.value =  NA
      new_list$result.reply_form.content2.answer.value =  NA
      new_list$result.reply_form.content.answer.value =  NA
      df_s[nrow(df_s) + 1,] <- c(new_list)
    }
    
  }
  
}

df_s2 <- df_s[!grepl("calib", df_s$video),]
df_s3 <- df_s2[!grepl("nature", df_s2$video),]
df_s3 <- df_s3[,c(1,2,3,4,6,5)]

write.csv(df_s3,"phase4_update_lts.csv", row.names = FALSE)





