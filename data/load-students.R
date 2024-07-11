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


students_csi_22_23 <-  get_table(.table_name = "StudentCSI", 
                                 .database_name = "PROD1", 
                                 .schema = "Schools", 
                                 .server_name = "RGVPDSD-DWPRD1") %>%
  filter(InterventionType %in% c('Comprehension',
                                  'Content Mastery',
                                  'Decoding',
                                  'DISE',
                                  'E to E',
                                  'Expressive Writing',
                                  'Imagine Learning',
                                  'Lexercise',
                                  'LLI',
                                  'Off model-TEKS based',
                                  'Reading Foundations',
                                  'Reading Intervention',
                                  'Reading Success',
                                  'RMSE'),
         AcademicYear == '2022-2023') %>%
  select(StudentNumber, AcademicYear) %>%
  mutate(is_csi = 1) %>%
  distinct() %>%
  collect() %>%
  janitor::clean_names()


username <- "SSISUser"
password <- "$SSISUser"

server <- "RGVPDSD-SQLCSI.ips.org"

connection_string <- glue::glue(
  "Driver={{ODBC Driver 17 for SQL Server}};", #literal brace requires {{ }}
  "Server={server};",
  "UID={username};",
  "PWD={password};",
  "database=CSIInstruction"
)

conn <- DBI::dbConnect(odbc::odbc() 
                       ,timeout = 10 , 
                       .connection_string = connection_string
)

students_csi_23_24_table <- DBI::dbGetQuery(conn,paste("SELECT DISTINCT  [STUDENT_NUMBER]
FROM [RGVPDSD-SQLCSI].[CSIInstruction].[dbo].[StudentCSIDetails]
WHERE [SCHOOLTERMIDS] = '3400' AND [READINGINTERVENTION] = 1")) 


students_csi_23_24 <- students_csi_23_24_table %>%
  mutate(academic_year = '2023-2024',
         is_csi = 1) %>%
  janitor::clean_names()
  
students_csi <- students_csi_22_23 %>%
  union_all(students_csi_23_24)

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
  left_join(students_csi, join_by(student_number, academic_year)) %>%
  mutate(is_csi = if_else(is.na(is_csi) , 0, 1)) %>%
  mutate(sped = if_else(sped == TRUE, 1, 0)) %>%
  mutate(student_number = as.character(student_number),
         grade_level_display = case_when(grade_level_id >= 1 ~ as.character(grade_level_id),
                                         grade_level_id == 0 ~ "K",
                                         grade_level_id == 0 ~ "PK")) %>%
  left_join(schools_regions, join_by(school_number))
  
