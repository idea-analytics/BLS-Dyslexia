library(ideadata)
library(dplyr)
library(janitor)

dibels <- get_table("DIBELS_MCLASS") %>%
  filter(`School Year` %in% c("2022-2023", "2023-2024")) %>%
  collect() %>%
  janitor::clean_names() %>%
  select(school_year,
         assessment_measure_composite_score_levels,
         student_primary_id,
         benchmark_period) %>%
  rename(academic_year = school_year,
         student_number = student_primary_id) %>%
  mutate(assessment_rating = case_when( assessment_measure_composite_score_levels == "Well Below Benchmark" ~ 0,
                                       assessment_measure_composite_score_levels == "Below Benchmark" ~ 1,
                                       assessment_measure_composite_score_levels == "At Benchmark" ~ 2,
                                       assessment_measure_composite_score_levels == "Above Benchmark" ~ 3,
                                       TRUE ~ NA)) %>%

  filter(!is.na(assessment_rating)) %>%
  group_by(academic_year,
           student_number,
           benchmark_period) %>%
  summarise(assessment_rating = max(assessment_rating)) %>%
  ungroup() %>%
  mutate(assessment_measure_composite_score_levels = case_when( assessment_rating == 0 ~ "Well Below Benchmark",
                                                                  assessment_rating == 1 ~ "Below Benchmark",
                                                                  assessment_rating == 2 ~ "At Benchmark",
                                                                  assessment_rating == 3 ~ "Above Benchmark",
                                                                  TRUE ~ NA)) %>%
  pivot_wider(names_from = benchmark_period,
              values_from = c(assessment_rating, assessment_measure_composite_score_levels),
              names_prefix = "dibels_") %>%
  janitor::clean_names()