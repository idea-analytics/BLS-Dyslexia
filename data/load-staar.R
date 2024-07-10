library(ideadata)
library(dplyr)
library(janitor)
library(tidyverse)

staar <- get_table(.table_name = "STAAR", 
                   .database_name = "Dashboard", 
                   .schema = "dbo", 
                   .server_name = "RGVPDRA-DASQL") %>%
  filter(SchoolYear %in% c("2022-2023", "2023-2024"),
         SubjectCode %in% c("Reading")) %>% #"Reading","English I", "English II"
  select(LocalStudentID,
         LevelII,
         LevelIII,
         LevelIIFinal,
         ProgressMeasure,
         SubjectCode,
         SchoolYear) %>%
  collect() %>%
  janitor::clean_names() %>%
  mutate(staar_score = as.numeric(level_ii) + as.numeric(level_iii) + as.numeric(level_ii_final),
         progress_measure = as.numeric(progress_measure)) %>%
  select(-level_ii, -level_iii, -level_ii_final) %>%
  rename(student_number = local_student_id,
         academic_year = school_year) %>%
  group_by(student_number,
           academic_year,
           subject_code) %>%
  summarise(progress_measure = max(progress_measure),
            staar_score = max(staar_score)) %>%
  ungroup() %>%
  pivot_wider(names_from = subject_code, values_from = c(progress_measure, staar_score), names_prefix = "staar_") %>%
  janitor::clean_names()