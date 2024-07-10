library(ideadata)
library(dplyr)
library(janitor)

schools <- get_schools() %>%
  collect()

regions <- get_regions() %>%
  collect()

schools_regions <- schools %>%
  inner_join(regions, join_by(RegionID)) %>%
  select(school_name = SchoolShortName,
         region_name = RegionDescription,
         school_number = SchoolNumber)

students <- get_table(.table_name = "Students", 
                      .database_name = "PROD1", 
                      .schema = "Schools", 
                      .server_name = "RGVPDSD-DWPRD1") %>%
  filter(AcademicYear %in% c("2022-2023", "2023-2024")) %>%
  select(AcademicYear,
         StudentNumber,
         FirstName,
         LastName,
         SPED,
         GradeLevelID,
         SchoolNumber,
         ExitDate) %>%
  inner_join(get_table(.table_name = "Students", 
                       .database_name = "PROD1", 
                       .schema = "Schools", 
                       .server_name = "RGVPDSD-DWPRD1") %>%
               filter(AcademicYear %in% c("2022-2023", "2023-2024")) %>% 
               select(AcademicYear, StudentNumber, ExitDate) %>% 
               group_by(AcademicYear, StudentNumber) %>%
               summarise(ExitDate = max(ExitDate)) %>%
               ungroup(), join_by(AcademicYear, StudentNumber, ExitDate)) %>%
  collect() %>%
  select(-ExitDate) %>%
  janitor::clean_names() %>%
  mutate(student_number = as.character(student_number),
         grade_level_display = case_when(grade_level_id >= 1 ~ as.character(grade_level_id),
                                         grade_level_id == 0 ~ "K",
                                         grade_level_id == 0 ~ "PK")) %>%
  left_join(schools_regions, join_by(school_number))
  
