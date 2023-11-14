library(tidyverse)
library(dplyr)
library(ggplot2)
scores_ph4 <- read_csv("phase4_update_lts.csv", col_types = "fTcnlc")
scores_ph4 <- scores_ph4[!grepl("video", scores_ph4$video),]
scores_ph4$video_n <- substr(scores_ph4$video, 1, 3)
scores_ph4$quality <- as.numeric(substr(scores_ph4$video, 11, 11))
scores_ph4$external_id <- substr(scores_ph4$external_id, 1, 5)
scores_ph4$video_n <- as.numeric(scores_ph4$video_n)
#there is no video of number 36, so this part fixes the week division
scores_ph4$video_n[scores_ph4$video_n >35] <-  scores_ph4$video_n[scores_ph4$video_n >35] - 1
#there is no video of number 66, so this part fixes the week division
scores_ph4$video_n[scores_ph4$video_n >65] <-  scores_ph4$video_n[scores_ph4$video_n >65] - 1
scores_ph4$week <-  as.integer(((scores_ph4$video_n - 1)/7) + 1)
scores_ph4$day <- scores_ph4$video_n %% 7

#fix names of 4 videofiles
scores_ph4$video[scores_ph4$video == "001_p1204_5.0_4phreal_widoki.mp4"] <- "001_p1204_5.0_4phreal_widoki_y.mp4"
scores_ph4$video[scores_ph4$video == "003_p1204_2.0_4phreal_widoki.mp4"] <- "003_p1204_2.0_4phreal_widoki_n.mp4"
scores_ph4$video[scores_ph4$video == "009_p1204_5.0_4phreal_widoki.mp4"] <- "009_p1204_5.0_4phreal_widoki_y.mp4"
scores_ph4$video[scores_ph4$video == "011_p1204_2.0_4phreal_widoki.mp4"] <- "011_p1204_2.0_4phreal_widoki_y.mp4"

scores_ph4 <- scores_ph4 %>%
  add_column(Monday = NA,
             Tuesday = NA,
             Wednesday = NA,
             Thursday = NA,
             Friday = NA,
             Saturday = NA)
scores_ph4$Monday[scores_ph4$day == 1] <- scores_ph4$quality[scores_ph4$day == 1]
scores_ph4$Tuesday[scores_ph4$day == 2] <- scores_ph4$quality[scores_ph4$day == 2]
scores_ph4$Wednesday[scores_ph4$day == 3] <- scores_ph4$quality[scores_ph4$day == 3]
scores_ph4$Thursday[scores_ph4$day == 4] <- scores_ph4$quality[scores_ph4$day == 4]
scores_ph4$Friday[scores_ph4$day == 5] <- scores_ph4$quality[scores_ph4$day == 5]
scores_ph4$Saturday[scores_ph4$day == 6] <- scores_ph4$quality[scores_ph4$day == 6]

#delate the unfull week
scores_ph4 <- scores_ph4[!grepl("14", scores_ph4$week),]



df_s <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(df_s) <- c("external_id", "week", "mo", "tu", "we", "th", "fr", "sa", "n", "n_lowq", "q2", "n_full", "n_obs", "q4")

w <- scores_ph4 %>% group_by(week)
z <- group_split(w)
n_watched_testers <- scores_ph4 %>%
  group_by(external_id) %>%
  dplyr::summarize(perc_videos = n()/(7*length(z)))

for (p in 1:length(z)){
  my_list <- unlist(z[p], recursive = FALSE)
  d <- as.data.frame(my_list)
  db <- d %>% filter(quality == 2)
  da <- db %>%
    group_by(external_id) %>%
    dplyr::summarize(n_lowq = n()) 
  d <- d[!duplicated(d[c("external_id","video")]),]
  dx <- d %>%
    group_by(external_id) %>%
    dplyr::summarize(week = week, n = n(), full = n(), q2 = q2, q4 = q4, mo = sum(Monday, na.rm = TRUE), tu = sum(Tuesday, na.rm = TRUE), we = sum(Wednesday, na.rm = TRUE), th = sum(Thursday, na.rm = TRUE), fr = sum(Friday, na.rm = TRUE), sa = sum(Saturday, na.rm = TRUE)) %>%
    dplyr::mutate(qq = sum(q2, na.rm = TRUE))
  dx$full <-ifelse(dx$n==7,1,0)
  dx$n_full <- sum(dx$full)/7
  d1 <- left_join(dx, da)
  d1 <- d1[complete.cases(d1$q2),]
  d1$n_obs <- nrow(d1)
  d1$mo[d1$mo == 0] = NA
  d1$tu[d1$tu == 0] = NA
  d1$we[d1$we == 0] = NA
  d1$th[d1$th == 0] = NA
  d1$fr[d1$fr == 0] = NA
  d1$sa[d1$sa == 0] = NA
  d0 <- d1[,c("external_id", "week", "mo", "tu", "we", "th", "fr", "sa", "n", "n_lowq", "q2", "n_full", "n_obs", "q4")]
  df_s <- rbind(df_s, d0)
}

df_s$week_q <- sprintf("%d,%d,%d,%d,%d,%d", df_s$mo, df_s$tu, df_s$we, df_s$th, df_s$fr, df_s$sa)

#mos for all observations
  d_mos <- df_s %>%
  group_by(week) %>%
    dplyr::summarize(n_votes = n(), mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE))

  
#mos for only full observations
  d_mos_full <- df_s %>% filter(n == 7) %>%
    group_by(week) %>%
    dplyr::summarize(n_votes = n(), week_q = substr(paste(week_q, collapse = ""), 1, 11), mos = mean(q2, na.rm = TRUE), sd = sd(q2, na.rm = TRUE), mean_q = mean((mo+tu+we+th+fr+sa)/6, na.rm = TRUE))
  
d_mos_full %>%
  ggplot(aes(week, mos)) + geom_errorbar(aes(ymin = mos - 1.96*sd/sqrt(n_votes), 
                                             ymax = mos + 1.96*sd/sqrt(n_votes)))

#check answers for content questions
scores_ph4$question <- ifelse(grepl("n.mp4", scores_ph4$video), FALSE, ifelse(grepl("y.mp4", scores_ph4$video), TRUE, NA))
scores_ph4$content_q <- ifelse(scores_ph4$question == 'FALSE' & scores_ph4$q3 == 'FALSE', 1,
                    ifelse(scores_ph4$question == 'TRUE' & scores_ph4$q3 == 'TRUE', 1, 0))

content <- scores_ph4 %>% filter(!is.na(content_q)) %>%
  group_by(external_id) %>%
  dplyr::summarize(count(content_q))

#save as csv
  write.csv(df_s,"phase4_current_results.csv", row.names = FALSE)
  write.csv(content,"phase4_content_quest.csv", row.names = FALSE)
  write.csv(n_watched_testers,"phase4_percantage_watched.csv", row.names = FALSE)
  write.csv(d_mos,"phase4_mos.csv", row.names = FALSE)
  write.csv(d_mos_full,"phase4_mos_onlyfullobs.csv", row.names = FALSE)
  write.csv(scores_ph4,"data_for_plots.csv", row.names = FALSE)
  