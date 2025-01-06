library(tidyverse)
library(readxl)
library(purrr)

df <- read_excel("Aggregated_Data.xlsx")

View(df)

# turning caategorical variables into numerical
df <- df %>%
  mutate(gender_numeric = ifelse(gender == "male", 1, 0)) %>% 
  mutate(education_numeric = case_when(
    education == "diploma" ~ 0,
    education == "bachelor" ~ 1,
    education == "master" ~ 2,
    education == "PhD" ~ 3
  )) %>% 
  mutate(
    cinnomer = ifelse(previous_medication == "cinnomer", 1, 0),
    cinnovex = ifelse(previous_medication == "cinnovex", 1, 0),
    DMF = ifelse(previous_medication == "DMF", 1, 0),
    fingolimod = ifelse(previous_medication == "fingolimod", 1, 0),
    recigen = ifelse(previous_medication == "recigen", 1, 0)
  )

# value counts
lapply(df, table)

results_1 <- df %>%
  summarise(across(c(Age, Years_Diagnosis, EDSS1, EDSS2, EDSS3), 
                   list(Median = ~median(.x, na.rm = TRUE),
                   IQR = ~IQR(.x, na.rm = TRUE))))

print(results_1)

# Mean and IQR for Cognitive Function and QoL
results_2 <- df %>%
  summarise(across(c(CVLT_1, CVLT_2, CVLT_3,
                     PASAT_1, PASAT_2, PASAT_3,
                     SDMT_1, SDMT_2, SDMT_3,
                     CVLT_delay_1, CVLT_delay_2, CVLT_delay_3,
                     BVMT_total_1, BVMT_total_2, BVMT_total_3,
                     BVMT_delay_1, BVMT_delay_2, BVMT_delay_3,
                     COWAT_1, COWAT_2, COWAT_3,
                     D_kefs_des_1, D_kefs_des_2, D_kefs_des_3,
                     D_kef_sort_1, D_kef_sort_2, D_kef_sort_3,
                     JLO_1, JLO_2, JLO_3,
                     Phys_health_1, Phys_health_2, Phys_health_3,
                     Ment_health_1, Ment_health_2, Ment_health_3), 
                      list(Median = ~round(median(.x, na.rm = TRUE), 2),
                      IQR = ~round(IQR(.x, na.rm = TRUE), 2),
                      Mean = ~round(mean(.x, na.rm = TRUE), 2),
                      STD = ~round(sd(.x, na.rm = TRUE), 2))
                   ))

print(results_2)

# converting data into long format
df_long <- df %>%
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Column", 
               values_to = "Value")

# Draw QQ plots
ggplot(df_long, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  facet_wrap(~Column, scales = "free") +
  labs(title = "QQ Plots for Columns", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Draw Histograms
ggplot(df_long, aes(x = Value)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  facet_wrap(~Column, scales = "free") +
  labs(title = "Histograms for Numeric Columns", 
       x = "Value", 
       y = "Frequency") +
  theme_minimal()


# Comparing scores of Cognitive Function and QoL
groups <- list(
  c("CVLT_1", "CVLT_2", "CVLT_3"),
  c("PASAT_1", "PASAT_2", "PASAT_3"),
  c("SDMT_1", "SDMT_2", "SDMT_3"),
  c("CVLT_delay_1", "CVLT_delay_2", "CVLT_delay_3"),
  c("BVMT_total_1", "BVMT_total_2", "BVMT_total_3"),
  c("BVMT_delay_1", "BVMT_delay_2", "BVMT_delay_3"),
  c("COWAT_1", "COWAT_2", "COWAT_3"),
  c("D_kefs_des_1", "D_kefs_des_2", "D_kefs_des_3"),
  c("D_kef_sort_1", "D_kef_sort_2", "D_kef_sort_3"),
  c("JLO_1", "JLO_2", "JLO_3"),
  c("Phys_health_1", "Phys_health_2", "Phys_health_3"),
  c("Ment_health_1", "Ment_health_2", "Ment_health_3")
)

process_group <- function(cols, data) {
  
  group_data <- data[, cols]
  friedman_result <- friedman.test(as.matrix(group_data))
  
  group_long <- group_data %>%
    pivot_longer(cols = everything(), names_to = "Time", values_to = "Score")
  
  pairwise_result <- pairwise.wilcox.test(
    x = group_long$Score,
    g = group_long$Time,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )
  
  # Return results as a list
  list(
    Friedman = friedman_result,
    Pairwise = pairwise_result
  )
}

results <- map(groups, ~process_group(.x, df))

for (i in seq_along(results)) {
  cat("\n### Group", i, "###\n")
  print(results[[i]]$Friedman)
  print(results[[i]]$Pairwise)
}

