library(tidyverse)
library(ggpubr)
library(ordinal)
library(optimx)
library(stats)
library(data.table)
library(boot)

#Here choose the csv, it can be with real p1204 or desired only
all_data <- read_csv("phase4_current_results_p1204.csv", col_types = "fnnnnnnnnnnnncc")
#Here choose for which data you want the model: 
#n<8 for all data, n==7 for only full weeks and n<7 for only unfull weeks
data_by_weeks_n <- all_data  %>% filter(n == 7)
#Here choose what content you want: 
#n < 26 for all content, n < 14 for nature only and n > 13 for slowmo only
data_by_weeks <- data_by_weeks_n %>% filter(week < 26)
#if you want to exclude some testers you can do this after this line



# Different classes of weighting functions ============================
kumaraswamy_weights <- function(steps, a, b){
  kum_weights <- rep(0, length(steps) - 1)
  for (i in 2:length(steps)){
    kum_weights[i-1] = (1 - steps[i-1]^a)^b - (1 - steps[i]^a)^b
  }
  return(kum_weights)
}

kum_par <- c(1,1, 0.6,1, 0.9,1, 1,0.5, 5,0.9, 0.5,0.5, 0.9,0.9, 5,5, 3,6, 2,6)
dim(kum_par) <- c(2, 10)
kum_par <- t(kum_par)
colnames(kum_par) <- c("a", "b")

f_mean <- function(x) {
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- 0
  norm <- rep(1, nrow(x))
  norm[is.na(x[,1])] <- 0
  for (i in c(2:6)){
    tmp_norm <- rep(1, nrow(x))
    tmp_norm[is.na(x[,i])] <- 0
    norm <- norm + tmp_norm
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- 0
    tmp <- tmp + tmp_na
  }
  return(tmp / norm)
}

f_min <- function(x) {
  tmp <- x[,1]
  tmp[is.na(x[,1])] <- Inf  # Initialize with positive infinity
  for (i in c(2:6)){
    tmp_na <- x[,i]
    tmp_na[is.na(tmp_na)] <- Inf  # Treat missing values as positive infinity
    tmp <- pmin(tmp, tmp_na)  # Calculate the element-wise minimum
  }
  tmp[tmp == Inf] <- NA  # Replace positive infinity with NA
  return(tmp)
}

predict_and_update <- function(model, data, weights, model_column) {
  predicted_probs <- predict(model, newdata = data, type = "prob")
  predicted_probs_matrix <- do.call(rbind, predicted_probs)
  weights <- matrix(rep(c(1,2,3,4,5), 25), nrow = 25, ncol = 5, byrow = TRUE)
  data[[model_column]] <- rowSums(predicted_probs_matrix * weights)
  return(data)
}

normalize_beta <- function(model) {
  beta_values <- model$beta
  sum_of_coefficients <- sum(beta_values)
  normalized_beta <- beta_values / sum_of_coefficients
  return(normalized_beta)
}

data_by_weeks$f_mean = f_mean(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks$f_min = f_min(as.matrix(data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]))
data_by_weeks$os <- factor(data_by_weeks$q2, ordered = TRUE, 
                           levels = c(1, 2, 3, 4, 5))

steps <- c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 6/6)

f_kum <- function(x, steps, a, b) {
  norm_vec <- t(replicate(nrow(x), kumaraswamy_weights(steps, a, b)))
  norm_vec[is.na(x)] <- 0
  rowSums(x * norm_vec, na.rm = TRUE) / rowSums(norm_vec)
}

objective_fun <- function(params, df, steps) {
  a <- params[1]
  b <- params[2]
  
  df$tmp <- f_kum(as.matrix(df[,c("mo", "tu", "we", "th", "fr", "sa")]), 
                  steps, a, b)
  
  model <- clm(os ~ tmp, data = df)  
  
  return(AIC(model))  
}

best_AIC <- 10^9
opt_param <- 0
for (i in 1:nrow(kum_par)){
  init_params <- kum_par[i,]
  lower_bounds <- c(0.00001, 0.00001)
  opt_data <- data_by_weeks 
  
  opt_result <- optimx(init_params, objective_fun, method="L-BFGS-B", 
                       lower=lower_bounds, df = opt_data, steps = steps)
  
  best_params <- opt_result
  print(best_params)
  if (best_params$value < best_AIC)
  {
    best_AIC <- best_params$value
    opt_param <- best_params
  }
}

data_by_weeks$f_best_fun <- f_kum(as.matrix(
  data_by_weeks[,c("mo", "tu", "we", "th", "fr", "sa")]), 
  steps, opt_param$a, opt_param$b)
opt_model <- clm(os ~ f_best_fun, data = data_by_weeks)
summary(opt_model)

cor_plot_data <- data_by_weeks %>%
  group_by(week) %>%
  summarize(f_best_fun = first(f_best_fun), mos = mean(q2))

cor_plot_data <- predict_and_update(opt_model, cor_plot_data, weights, "opt_model")

# Model 1
model_glz1 <- clm(os ~ mo + tu + we + th + fr + sa, data = data_by_weeks)
cor_plot_data_glz1 <- data_by_weeks %>%
  group_by(week) %>%
  summarize(mo = first(mo), tu = first(tu), we = first(we), th = first(th), fr = first(fr), sa = first(sa), mos = mean(q2))

cor_plot_data_glz1 <- predict_and_update(model_glz1, cor_plot_data_glz1, weights, "model1")
normalized_beta1 <- normalize_beta(model_glz1)

# Model 2
glz_data <- data_by_weeks %>%
  mutate(motu = (mo + tu)/2) %>%
  mutate(weth = (we + th)/2) %>%
  mutate(frsa = (fr + sa)/2)

model_glz2 <- clm(os ~ motu + weth + frsa, data = glz_data)
cor_plot_data_glz2 <- glz_data %>%
  group_by(week) %>%
  summarize(motu = first(motu), weth = first(weth), frsa = first(frsa), mos = mean(q2))

cor_plot_data_glz2 <- predict_and_update(model_glz2, cor_plot_data_glz2, weights, "model2")
normalized_beta2 <- normalize_beta(model_glz2)

# Model 3
model_glz3 <- clm(os ~ mo + we + fr, data = glz_data)  
cor_plot_data_glz3 <- data_by_weeks %>%
  group_by(week) %>%
  summarize(mo = first(mo), we = first(we), fr = first(fr), mos = mean(q2))

cor_plot_data_glz3 <- predict_and_update(model_glz3, cor_plot_data_glz3, weights, "model3")
normalized_beta3 <- normalize_beta(model_glz3)

# Model 4
model_glz4 <- clm(os ~ tu + th + sa, data = glz_data) 
cor_plot_data_glz4 <- data_by_weeks %>%
  group_by(week) %>%
  summarize(tu = first(tu), th = first(th), sa = first(sa), mos = mean(q2))


cor_plot_data_glz4 <- predict_and_update(model_glz4, cor_plot_data_glz4, weights, "model4")
normalized_beta4 <- normalize_beta(model_glz4)
data_by_weeks <- data_by_weeks %>%
  left_join(cor_plot_data, by = "week") %>%
  left_join(cor_plot_data_glz1, by = "week") %>%
  left_join(cor_plot_data_glz2, by = "week") %>%
  left_join(cor_plot_data_glz3, by = "week") %>%
  left_join(cor_plot_data_glz4, by = "week")

data_by_weeks %>%
  group_by(week) %>% 
  dplyr::summarize(n_votes = n(), mos_real = mean(q2, na.rm = TRUE), 
                   mos_k = mean(opt_model, na.rm = TRUE), 
                   mos_glz1 = mean(model1, na.rm = TRUE),
                   mos_glz2 = mean(model2, na.rm = TRUE),
                   mos_glz3 = mean(model3, na.rm = TRUE),
                   mos_glz4 = mean(model4, na.rm = TRUE),
                   mos_mean = mean(f_mean, na.rm = TRUE),
                   sd_real = sd(q2, na.rm = TRUE), 
                   sd_k = sd(opt_model, na.rm = TRUE)) %>%
  ggplot(aes(week, mos_real)) + 
  geom_errorbar(aes(ymin = mos_real - 1.96*sd_real/sqrt(n_votes), 
                    ymax = mos_real + 1.96*sd_real/sqrt(n_votes))) + 
  geom_point(aes(week, mos_k, color = "mos_k", shape = "mos_k")) + geom_point(aes(week, mos_glz3, color = "mos_glz3", shape = "mos_glz3")) + 
  geom_point(aes(week, mos_glz4, color = "mos_glz4", shape = "mos_glz4")) + geom_point(aes(week, mos_glz1, color = "mos_glz1", shape = "mos_glz1"), alpha = 0.5) +
  geom_point(aes(week, mos_glz2, color = "mos_glz2", shape = "mos_glz2"), alpha = 0.5)  +
  ylim(2,5) + labs(
    x = "Week number",
    y = "MOS",
    color = "Model name"
  ) + scale_color_manual(
    values = c("mos_k" = "red", "mos_glz3" = "chartreuse", "mos_glz4" = "orange", "mos_glz1" = "cyan", "mos_glz2" = "blue"),
    labels = c(bquote(italic("opt_k")), bquote(italic("odd")), bquote(italic("even")), bquote(italic("all")), bquote(italic("agg")))
  ) + scale_shape_manual(
    values = c("mos_k" = 0, "mos_glz3" = 1, "mos_glz4" = 2, "mos_glz1" = 3, "mos_glz2" = 4),
    labels = c(bquote(italic("opt_k")), bquote(italic("odd")), bquote(italic("even")), bquote(italic("all")), bquote(italic("agg")))
  ) +
  guides(title = "Model name",
         color = guide_legend(override.aes = list(shape = c(0,1,2,3,4))),
         shape = guide_none()
  ) + 
  theme(legend.text.align = 0)+ theme_bw()



days_of_week_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
axis_labels_df <- data.frame(steps = steps[2:7], days = factor(days_of_week_labels, levels = days_of_week_labels))
axis_labels_df2 <- data.frame(steps = steps[c(2, 4, 6)], days = factor(c("Monday", "Wednesday", "Friday")))
axis_labels_df4 <- data.frame(steps = steps[c(2, 5, 7)], days = factor(c("Tuesday", "Thursday", "Saturday")))


ggplot() +
  geom_point(aes(x = axis_labels_df2$days, y = normalized_beta3, color = "odd")) +
  geom_point(aes(x = axis_labels_df$days, y = kumaraswamy_weights(steps, opt_param$a, opt_param$b), color = "opt_k")) +
  geom_segment(aes(x =1, xend = 2, y = normalized_beta2[1], yend = normalized_beta2[1], color = "agg")) + 
  geom_segment(aes(x =3, xend = 4, y = normalized_beta2[2], yend = normalized_beta2[2], color = "agg")) + 
  geom_segment(aes(x =5, xend = 6, y = normalized_beta2[3], yend = normalized_beta2[3], color = "agg")) +
  geom_point(aes(x = axis_labels_df4$days, y = normalized_beta4, color = "even")) +
  labs(
    x = "Days of the week",
    y = "Mean weights",
    color = "Model name"
  ) +
  scale_color_manual(values = c("odd" = "blue", "opt_k" = "red", "agg" = "green", "even" = "orange"),  labels = c(bquote(italic("odd")), bquote(italic("opt_k")), bquote(italic("agg")), bquote(italic("even"))))  +
  scale_x_discrete(limits = days_of_week_labels) +
  theme_bw() +
  guides(color = guide_legend(title = "Model name"))

