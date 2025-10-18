


# FURTHER EDA FOR ANALYSIS 
rel_data <- deprivation_tg2_total_pop %>% 
  select(average_deprivation, district, therapeutic_grp2, year_disp,
         utilisation, pasifika_pop_pct, other_pop_pct, maori_pop_pct,
         asian_pop_pct, child_pop_pct, adult_pop_pct, old_adult_pop_pct)


# 2020 snap shot 
rel_data %>% 
  filter(year_disp == 2020) %>%
  select(where(is.numeric)) %>%  
  pairs.panels(method = "spearman", 
               hist.col = "lightgreen", 
               density = TRUE,  
               ellipses = FALSE 
  )

rel_data %>% 
  filter(year_disp == 2021) %>%
  select(where(is.numeric)) %>%  
  pairs.panels(method = "spearman", 
               hist.col = "lightgreen", 
               density = TRUE,  
               ellipses = FALSE 
  )

rel_data %>% 
  filter(year_disp == 2022) %>%
  select(where(is.numeric)) %>%  
  pairs.panels(method = "spearman", 
               hist.col = "lightgreen", 
               density = TRUE,  
               ellipses = FALSE 
  )

rel_data %>% 
  filter(year_disp == 2023) %>%
  select(where(is.numeric)) %>%  
  pairs.panels(method = "spearman", 
               hist.col = "lightgreen", 
               density = TRUE,  
               ellipses = FALSE 
  )

rel_data %>% 
  filter(year_disp == 2024) %>%
  select(where(is.numeric)) %>%  
  pairs.panels(method = "spearman", 
               hist.col = "lightgreen", 
               density = TRUE,  
               ellipses = FALSE 
  )



###############################################################################
# YEAR AND MEDICATION TYPE 
###############################################################################


corr_plot_year_med(rel_data, 2020, "Antidepressants")
corr_plot_year_med(rel_data, 2020, "Antipsychotics")
corr_plot_year_med(rel_data, 2020, "Anxiolytics")
corr_plot_year_med(rel_data, 2020, "Sedatives and Hypnotics")
corr_plot_year_med(rel_data, 2020, "Stimulants/ADHD Treatments")
corr_plot_year_med(rel_data, 2020, "Treatments for Substance Dependence")

corr_plot_year_med(rel_data, 2024, "Antidepressants")
corr_plot_year_med(rel_data, 2024, "Antipsychotics")
corr_plot_year_med(rel_data, 2024, "Anxiolytics")
corr_plot_year_med(rel_data, 2024, "Sedatives and Hypnotics")
corr_plot_year_med(rel_data, 2024, "Stimulants/ADHD Treatments")
corr_plot_year_med(rel_data, 2024, "Treatments for Substance Dependence")

# doesn't look like there's any correlation between deprivation and utilisation
# however, there does look to be one with old age and other ethnicity 

# other pop is made up of NZ Europeans, known to have older age demographics
# compared to other ethnic groups.. 
# deprivation does not seem that important for drug utilisation 







m <- lmer(util_rate ~ year + deprivation + ethnicity_pct + (year | DHB),
          data = df, weights = population)







m_fe <- lm(utilisation ~ district + year_disp, data = deprivation_tg2)

#m1 <- lmer(utilisation ~ year_disp + average_deprivation + (1 | district), data = deprivation_tg2)


test <- m_fe$coefficients



# do ANOVAs and tukey tests of each district, for each medication type  
for(medication in unique(deprivation_tg2$therapeutic_grp2)){
  print(medication)
  anova_tukey_med(deprivation_tg2, medication)
}



# 
# antidepressants <- deprivation_tg2 %>% filter(therapeutic_grp2 == "Antidepressants")
# 
# anova_district <- aov(utilisation ~ district, data = antidepressants)
# 
# tukey_district <- TukeyHSD(anova_district)
# 
# tukey_results_df <- as.data.frame(tukey_district$district)
# 
# plot(tukey_district)