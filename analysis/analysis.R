library(tidyverse)
require(haven)
require(EdSurvey)
require(intsvy)
require(mirtCAT)
require(mirt)
require(stringr)
require(pbapply)
require(ggpubr)


#### Hyper-parameters ####
timss_data_path <- ".../TIMSS2011_IDB_SPSS_G8/Data"
subject <- "m"
grade <- "8"
country_codes_df <- intsvy:::iea.country
country_means_file_name <- paste0("country_means_", subject, grade, ".csv")
country_percentiles_file_name <- paste0("country_percentiles_", subject, 
                                        grade, ".csv")
llm_response_path <- ".../TIMSS_2011_Math_G8_Responses.csv"


#### Functions ####
## function to convert irt parameters to mirt parameters
convert_irt_to_mirt <- function(pars, model_type, n_cat) {
  n_rows <- nrow(pars)
  for (i in seq_len(n_rows)) {
    par_vec <- unlist(pars[i,])
    par_vec_new <- mirt::traditional2mirt(par_vec, model_type, ncat=n_cat)
    if (i == 1) {
      new_pars <- data.frame(matrix(ncol = length(par_vec_new), nrow = n_rows))
      colnames(new_pars) <- names(par_vec_new)
    }
    new_pars[i,] <- par_vec_new
  }
  return(new_pars)
}

## function to convert irt scale scores to timss scale
convert_to_timss_scale <- function(thetas, grade) {
  if (grade == 4) {
    a <- mean(c(516.32438, 516.41272, 516.60847, 516.19085, 515.99456))
    b <- mean(c(94.09515, 94.32281, 93.17783, 94.03151, 94.10840))
  }
  else if (grade == 8) {
    a <- mean(c(477.07673, 477.20509, 477.16649, 476.78226, 477.44342))
    b <- mean(c(111.73350, 112.92135, 113.23526, 113.35697, 113.05174))
  }
  
  thetas_lower <- thetas[1] - 1.96*thetas[2]
  thetas_upper <- thetas[1] + 1.96*thetas[2]
  achievements <- sapply(thetas[1], FUN=function(x) a + x*b)
  achievements_ci_lower <- sapply(thetas_lower, FUN=function(x) a + x*b)
  achievements_ci_upper <- sapply(thetas_upper, FUN=function(x) a + x*b)
  
  return(as.numeric(c(achievements, achievements_ci_lower, achievements_ci_upper)))
}

## function to compute highest possible score per respondent
compute_highest_possible_score <- function(score_vec, all_item_ids, all_poly_param_ids) {
  item_ids <- all_item_ids[!is.na(score_vec)]
  score <- 0
  
  if (is.logical(item_ids)) return(NA)
  for (item_id in item_ids) {
    if (item_id %in% all_poly_param_ids) score <- score + 2
    else score <- score + 1
  }
  
  return(score)
}

## function to get country-level statistics from student item responses
compute_country_stats <- function(data_path, subject, dichot_item_ids, poly_item_ids) {
  country_code <- toupper(str_extract(data_path, "(?<=bsa).*(?=m5)"))
  country_name <- country_codes_df[country_codes_df["ISO"] == country_code, ][["Country"]]
  
  data <- read_sav(data_path) %>% 
    select(starts_with("M")) %>% 
    haven::as_factor()
  
  dichot_item_ids <- toupper(dichot_item_ids)
  poly_item_ids <- toupper(poly_item_ids)
  dichot_item_ids_df <- data.frame(ItemID = dichot_item_ids)
  poly_item_ids_df <- data.frame(ItemID = poly_item_ids)
  all_item_ids <- c(dichot_item_ids, poly_item_ids)
  
  scores <- scoreTIMSS(data, 
                       polyParamTab = poly_item_ids_df, 
                       dichotParamTab = dichot_item_ids_df) %>% 
    select(tidyselect::all_of(all_item_ids))

  standardized_scores <- apply(scores, 1, function(x) sum(x, na.rm = TRUE)) /
    apply(scores, 1, compute_highest_possible_score, all_item_ids=all_item_ids, all_poly_param_ids=poly_item_ids)
  standardized_scores <- standardized_scores[!is.na(standardized_scores)]
  
  quantile_thresholds <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  quantile_scores <- quantile(standardized_scores, quantile_thresholds, na.rm=TRUE)
  mean_score <- mean(standardized_scores, na.rm = TRUE)
  se <- sd(standardized_scores, na.rm=TRUE) / sqrt(length(standardized_scores))
  
  stats_matrix <- matrix(data = c(country_name, mean_score, se, quantile_scores), ncol = 8)
  colnames(stats_matrix) <- c("IDCNTRYL", "Mean", "s.e.", "k_5", "k_25", "k_50", "k_75", "k_95")

  return(as.data.frame(stats_matrix))  
}


#### Data Preparation ####

## uncomment the following code to generate country_means_df and
## country_percentiles_df
# timss8g <- intsvy.select.merge(
#              folder = timss_data_path,
#              student =c("ITSEX"),
#              # countries = countries,
#              config = timss8_conf)

# country_means_df <- intsvy.mean.pv(pvnames = paste0("BSMMAT0", 1:5),
#                                    by = "IDCNTRYL",
#                                    data = timss8g,
#                                    config = timss8_conf)
#
# country_means_df %>%
#   write_csv2(country_means_file_name)

# country_percentiles_df <- intsvy.per.pv(pvnames = paste0("BSMMAT0", 1:5),
#                                         per = c(5, 25, 50, 75, 95),
#                                         by = "IDCNTRYL",
#                                         data = timss8g,
#                                         config = timss8_conf)
#
#
# country_percentiles_df %>%
#   write_csv2(country_percentiles_file_name)


## read country level data (means and percentiles)
country_means_df <- read_csv2(country_means_file_name)
country_percentiles_df <- read_csv2(country_percentiles_file_name)

country_percentiles_df <- country_percentiles_df %>% 
  select(-`Std. err.`) %>% 
  pivot_wider(names_from = Percentiles, values_from = Score, names_prefix = "k_")

## load timss data (with usa as an example)
# downloadTIMSS("~/", year=2011)
timss_usa <- readTIMSS("~/TIMSS/2011", countries="usa", gradeLvl = grade)
timss_usa$dichotParamTab$ItemID

## find all student response datasets
files <- list.files(path = timss_data_path, pattern = "^bsa.*m5\\.sav$", full.names = TRUE)

## obtain country-level statistics for each dataset
dichot_item_ids <- timss_usa$dichotParamTab$ItemID
dichot_item_ids <- dichot_item_ids[grep(paste0("^", subject), dichot_item_ids)]
poly_item_ids <- timss_usa$polyParamTab$ItemID
poly_item_ids <- poly_item_ids[grep(paste0("^", subject), poly_item_ids)]

country_stats <- pblapply(files, 
                          compute_country_stats, subject = subject,
                          dichot_item_ids = dichot_item_ids,
                          poly_item_ids = poly_item_ids)

country_stats_df <- bind_rows(country_stats)

## read llm response csv
llm_responses <- read.csv(llm_response_path) %>% 
  select(Item, starts_with(c("gpt4", "gemini")))
llm_responses["Item"] <- tolower(llm_responses[, "Item"])
released_item_ids <- llm_responses %>% 
  pull("Item")

## get parameters for dichotomous items
all_dichot_item_params <- timss_usa$dichotParamTab[c("ItemID", "slope", "difficulty", "guessing")]
released_dichot_item_ids <- all_dichot_item_params$ItemID[all_dichot_item_params$ItemID %in% released_item_ids]
released_dichot_item_params <- all_dichot_item_params[all_dichot_item_params$ItemID %in% released_item_ids, 
                                                      c("slope", "difficulty", "guessing")]
colnames(released_dichot_item_params) <- c("a", "b", "g")
released_dichot_item_params["a"] <- 1.7*released_dichot_item_params["a"]
released_dichot_item_params["u"] <- 1
released_dichot_item_params_for_mirt <- convert_irt_to_mirt(released_dichot_item_params, 
                                                            model_type = "3PL", 
                                                            n_cat = 2)
names(released_dichot_item_params_for_mirt)

## get parameters for poly items
all_poly_item_params <- timss_usa$polyParamTab[c("ItemID", "slope", "itemLocation", "d1", "d2")]
released_poly_item_ids <- all_poly_item_params$ItemID[all_poly_item_params$ItemID %in% released_item_ids]
released_poly_item_params <- all_poly_item_params[all_poly_item_params$ItemID %in% released_item_ids, 
                                                  c("slope", "itemLocation", "d1", "d2")]
released_poly_item_params["slope"] <- released_poly_item_params["slope"]*1.7
released_poly_item_params["b1"] <- released_poly_item_params["itemLocation"] - released_poly_item_params["d1"]
released_poly_item_params["b2"] <- released_poly_item_params["itemLocation"] - released_poly_item_params["d2"]
released_poly_item_params <- released_poly_item_params[c("slope", "b1", "b2")]
colnames(released_poly_item_params) <- c("a", "b1", "b2")
released_poly_item_params["c"] <- 0
released_poly_item_params_for_mirt <- convert_irt_to_mirt(released_poly_item_params, "gpcmIRT", 3)
names(released_poly_item_params_for_mirt)

## get all mirt parameters
released_item_params_for_mirt <- dplyr::bind_rows(released_dichot_item_params_for_mirt, released_poly_item_params_for_mirt)

## order llm responses by the order in dichotItemIds and polyItemIds
llm_responses_ordered <- llm_responses[match(c(released_dichot_item_ids, 
                                              released_poly_item_ids), 
                                              llm_responses$Item),]


#### Model Estimation ####
## irt model to estimate abilities
mod <- generate.mirt_object(released_item_params_for_mirt, itemtype = c(rep("3PL",  nrow(released_dichot_item_params_for_mirt)), 
                                                                        rep("gpcm", nrow(released_poly_item_params_for_mirt))))
coef(mod)

## estimate ability score on timss scale
test_takers <- colnames(llm_responses[,-1])
thetas_list <- list()
for (test_taker in test_takers) {
  thetas_list[[test_taker]] <- fscores(mod, response.pattern = llm_responses_ordered[, test_taker])
}

thetas <- do.call(rbind.data.frame, thetas_list)
thetas_timss_scale <- apply(thetas, 1, convert_to_timss_scale, grade=8)

highest_score_llm <- compute_highest_possible_score(llm_responses[[test_takers[1]]], llm_responses$Item, released_poly_item_ids)
standardized_scores_llm <- apply(llm_responses[,-1], 2, sum, na.rm=TRUE)/highest_score_llm
standardized_scores_se_llm <- sqrt(standardized_scores_llm*(1-standardized_scores_llm)/nrow(llm_responses))


inverse_variance_weighting <- function(estimates, variances) {
  # Compute the weights using the inverse of variances
  weights <- 1 / variances
  
  # Normalize the weights to sum up to 1
  weights <- weights / sum(weights)
  
  # Calculate the weighted estimate
  weighted_estimate <- sum(estimates * weights)
  
  # Calculate the weighted variance
  weighted_se <- sqrt(1/sum(1/variances))
  
  return(list(weighted_estimate = weighted_estimate, weighted_se = weighted_se))
}

standardized_weighted_gpt4_performance <- inverse_variance_weighting(standardized_scores_llm[1:11], standardized_scores_se_llm[1:11]^2)
standardized_weighted_gpt4_mean <- standardized_weighted_gpt4_performance[[1]]
standardized_weighted_gpt4_se <- standardized_weighted_gpt4_performance[[2]]
standardized_scores_upper_gpt4 <- standardized_weighted_gpt4_mean + 1.96*standardized_weighted_gpt4_se
standardized_scores_lower_gpt4 <- standardized_weighted_gpt4_mean - 1.96*standardized_weighted_gpt4_se

var(standardized_scores_llm[12:22])
var(standardized_scores_llm[1:11])

standardized_weighted_gemini_performance <- inverse_variance_weighting(standardized_scores_llm[12:22], standardized_scores_se_llm[12:22]^2)
standardized_weighted_gemini_mean <- standardized_weighted_gemini_performance[[1]]
standardized_weighted_gemini_se <- standardized_weighted_gemini_performance[[2]]
standardized_scores_upper_gemini <- standardized_weighted_gemini_mean + 1.96*standardized_weighted_gemini_se
standardized_scores_lower_gemini <- standardized_weighted_gemini_mean - 1.96*standardized_weighted_gemini_se

quantile_thresholds <- c(0.05, 0.25, 0.75, 0.95)
standardized_quantile_scores_gpt4 <- quantile(standardized_scores_llm[1:11], quantile_thresholds, na.rm=TRUE)
standardized_quantile_scores_gemini <- quantile(standardized_scores_llm[12:22], quantile_thresholds, na.rm=TRUE)


llm_stats_df <- tibble("IDCNTRYL" = c("GPT-4V", "Gemini-Pro-Vision"),
                       "Mean" = c(standardized_weighted_gpt4_mean, standardized_weighted_gemini_mean),
                       "ci_upper" = c(standardized_scores_upper_gpt4, standardized_scores_upper_gemini),
                       "ci_lower" = c(standardized_scores_lower_gpt4, standardized_scores_lower_gemini),
                       "k_5"  = c(standardized_quantile_scores_gpt4[[1]],standardized_quantile_scores_gemini[[1]]), 
                       "k_25" = c(standardized_quantile_scores_gpt4[[2]],standardized_quantile_scores_gemini[[2]]), 
                       "k_75" = c(standardized_quantile_scores_gpt4[[3]],standardized_quantile_scores_gemini[[3]]), 
                       "k_95" = c(standardized_quantile_scores_gpt4[[4]],standardized_quantile_scores_gemini[[4]]))


irt_weighted_gpt4_performance <- inverse_variance_weighting(thetas$F1[1:11], thetas$SE_F1[1:11]^2)
irt_weighted_gpt4_mean <- irt_weighted_gpt4_performance[[1]]
irt_weighted_gpt4_se <- irt_weighted_gpt4_performance[[2]]
irt_gpt4_performance_timss_scale <- convert_to_timss_scale(c(irt_weighted_gpt4_mean,irt_weighted_gpt4_se), grade=8)

irt_weighted_gemini_performance <- inverse_variance_weighting(thetas$F1[12:22], thetas$SE_F1[12:22]^2)
irt_weighted_gemini_mean <- irt_weighted_gemini_performance[[1]]
irt_weighted_gemini_se <- irt_weighted_gemini_performance[[2]]
irt_gemini_performance_timss_scale <- convert_to_timss_scale(c(irt_weighted_gemini_mean,irt_weighted_gemini_se), grade=8)


irt_quantile_scores_gpt4 <- quantile(thetas_timss_scale[1,1:11], quantile_thresholds, na.rm=TRUE)
irt_quantile_scores_gemini <- quantile(thetas_timss_scale[1,12:22], quantile_thresholds, na.rm=TRUE)


llm_means_df <- tibble("IDCNTRYL" = c("GPT-4V", "Gemini-Pro-Vision"),
                       "Mean" = c(irt_gpt4_performance_timss_scale[1],     irt_gemini_performance_timss_scale[1]),
                       "ci_upper" = c(irt_gpt4_performance_timss_scale[3], irt_gemini_performance_timss_scale[3]),
                       "ci_lower" = c(irt_gpt4_performance_timss_scale[2], irt_gemini_performance_timss_scale[2]),
                       "k_5"  = c(irt_quantile_scores_gpt4[[1]],irt_quantile_scores_gemini[[1]]), 
                       "k_25" = c(irt_quantile_scores_gpt4[[2]],irt_quantile_scores_gemini[[2]]), 
                       "k_75" = c(irt_quantile_scores_gpt4[[3]],irt_quantile_scores_gemini[[3]]), 
                       "k_95" = c(irt_quantile_scores_gpt4[[4]],irt_quantile_scores_gemini[[4]]))

selected_countries <- country_means_df %>% 
  arrange(desc(Mean)) %>% 
  pull(IDCNTRYL) %>% 
  .[1:15]

colnames(country_stats_df)

#### Plot LLM Performance against Uncalibrated Achievement Scores by Selected Countries ####
(uncalibrated <- country_stats_df %>% 
  filter(IDCNTRYL %in% selected_countries) %>% 
  mutate_at(vars(-IDCNTRYL), as.numeric) %>% 
  mutate(ci_upper = Mean-1.96*`s.e.`,
         ci_lower = Mean+1.96*`s.e.`) %>% 
  bind_rows(llm_stats_df) %>% 
  ggplot(aes(x = reorder(IDCNTRYL, Mean), 
             ymin = k_5, 
             lower = k_25,
             middle = Mean,
             upper = k_75,
             ymax = k_95)) +
  geom_boxplot(stat="identity", alpha=0.5) +
  geom_errorbar(aes(ymin = ci_lower, 
                    ymax = ci_upper), width = 0.5) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  coord_flip() +
  theme_bw(base_size = 24) +
  theme(axis.title.y=element_blank()))



#### Plot LLM Performance against Calibrated Achievement Scores by Selected Countries ####
(calibrated <- country_means_df %>% 
  left_join(country_percentiles_df) %>% 
  filter(IDCNTRYL %in% selected_countries) %>% 
  mutate(ci_upper = Mean-1.96*`s.e.`,
         ci_lower = Mean+1.96*`s.e.`) %>% 
  bind_rows(llm_means_df) %>% 
  ggplot(aes(x = reorder(IDCNTRYL, Mean), 
             ymin = k_5, 
             lower = k_25,
             middle = Mean,
             upper = k_75,
             ymax = k_95)) +
  geom_boxplot(stat="identity", alpha=0.5) +
  geom_errorbar(aes(ymin = ci_lower, 
                    ymax = ci_upper), width = 0.5) + 
  scale_y_continuous(limits = c(200,800), breaks = seq(200,800,100)) +
  coord_flip() +
  theme_bw(base_size = 24) +
  theme(axis.title.y=element_blank()))


(combined_plot <- ggarrange(uncalibrated, calibrated, 
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1))

ggexport(combined_plot, width = 16, height = 8, filename = "proficiency.pdf")


#### Plot LLM Performance against Uncalibrated Achievement Scores by All Countries ####
(uncalibrated_full <- country_stats_df %>% 
    mutate_at(vars(-IDCNTRYL), as.numeric) %>% 
    mutate(ci_upper = Mean-1.96*`s.e.`,
           ci_lower = Mean+1.96*`s.e.`) %>% 
    bind_rows(llm_stats_df) %>% 
    ggplot(aes(x = reorder(IDCNTRYL, Mean), 
               ymin = k_5, 
               lower = k_25,
               middle = Mean,
               upper = k_75,
               ymax = k_95)) +
    geom_boxplot(stat="identity", alpha=0.5) +
    geom_errorbar(aes(ymin = ci_lower, 
                      ymax = ci_upper), width = 0.3) + 
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
    coord_flip() +
    theme_bw(base_size = 18) +
    theme(axis.title.y=element_blank()))



#### Plot LLM Performance against Calibrated Achievement Scores by All Countries ####
(calibrated_full <- country_means_df %>% 
   left_join(country_percentiles_df) %>% 
   mutate(ci_upper = Mean-1.96*`s.e.`,
          ci_lower = Mean+1.96*`s.e.`) %>% 
   bind_rows(llm_means_df) %>% 
   ggplot(aes(x = reorder(IDCNTRYL, Mean), 
              ymin = k_5, 
              lower = k_25,
              middle = Mean,
              upper = k_75,
              ymax = k_95)) +
   geom_boxplot(stat="identity", alpha=0.5) +
   geom_errorbar(aes(ymin = ci_lower, 
                     ymax = ci_upper), width = 0.3) + 
   scale_y_continuous(limits = c(200,800), breaks = seq(200,800,100)) +
   coord_flip() +
   theme_bw(base_size = 18) +
   theme(axis.title.y=element_blank())) +
   scale_x_discrete(labels=c("GPT-4V"=expression(bold(GPT4V))), parse=TRUE)


(combined_plot <- ggarrange(uncalibrated_full, calibrated_full, 
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1))

ggexport(combined_plot, width = 12, height = 16, filename = "proficiency_full.pdf")




#### analysis of difficult test items
difficult_item_ids <- released_item_params_for_mirt %>% 
  mutate(item_id = c(released_dichot_item_ids, 
              released_poly_item_ids)) %>% 
  mutate(d = ifelse(is.na(d), d1+d2, d)) %>% 
  select(item_id, d) %>% 
  arrange(d) %>% 
  filter(d > -0.38108960) %>% 
  # filter(d < -1.93488963) %>% 
  pull(item_id)


compute_highest_possible_score(difficult_item_ids, 
                               difficult_item_ids, 
                               released_poly_item_ids)

llm_responses_ordered %>% 
  filter(Item %in% difficult_item_ids) %>% 
  pivot_longer(cols = matches("^(gpt|gemini)"), 
               names_to = "model", 
               values_to = "score") %>% 
  group_by(model) %>% 
  summarise(score = sum(score)/28) %>% 
  mutate(model = substr(model, 1, 4)) %>% 
  group_by(model) %>% 
  summarise(mean_score = mean(score))


  