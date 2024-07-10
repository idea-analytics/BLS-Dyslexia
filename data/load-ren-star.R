library(ideadata)
library(dplyr)
library(janitor)


username <- Sys.getenv('IDEA_RNA_DB_UID') 
password <- Sys.getenv('IDEA_RNA_DB_PWD') 

server <- "RGVPDSD-DWSRC4.ips.org"

connection_string <- glue::glue(
  "Driver={{ODBC Driver 17 for SQL Server}};", #literal brace requires {{ }}
  "Server={server};",
  "trusted_connection=yes;",
  "UID={username};",
  "PWD={password};",
  "database=SRC_AR"
)

conn <- DBI::dbConnect(odbc::odbc() 
                       ,timeout = 10 , 
                       .connection_string = connection_string
)


ren_star_reading_table <- DBI::dbGetQuery(conn,paste("SELECT 
         StudentIdentifier,
         GradeEquivalent,
         Lexile,
         ScreeningPeriodWindowName,
         AcademicYear
         FROM [SRC_AR].[dbo].[StarReadingV2]
                                                     WHERE AcademicYear IN ('2022-2023', '2023-2024') "))

ren_star_reading <- ren_star_reading_table %>%  #get_table("StarReadingV2") %>%
  #filter(AcademicYear %in% c("2022-2023", "2023-2024")) %>%
  select(StudentIdentifier,
         GradeEquivalent,
         Lexile,
         ScreeningPeriodWindowName,
         AcademicYear) %>%
  collect() %>%
  filter(ScreeningPeriodWindowName %in% c("BOY Testing Window",
                                          "EOY Testing Window",
                                          "MOY Testing Window"))%>%
  mutate(ScreeningPeriodWindowName = case_when(ScreeningPeriodWindowName ==  "BOY Testing Window" ~ "BOY",
                                               ScreeningPeriodWindowName ==  "MOY Testing Window" ~ "MOY",
                                               ScreeningPeriodWindowName ==  "EOY Testing Window" ~ "EOY")) %>%
  janitor::clean_names() %>%
  mutate(lexile_score = gsub("L", "",lexile),
         lexile_score = gsub("BR", "-", lexile_score),
         lexile_score = as.numeric(lexile_score)) %>%
  select(-lexile) %>%
  group_by(student_identifier,
           screening_period_window_name,
           academic_year) %>%
  summarise(grade_equivalent = max(grade_equivalent),
            lexile_score= max(lexile_score)) %>%
  ungroup() %>%
  rename(student_number = student_identifier) %>%
  pivot_wider(names_from = screening_period_window_name, 
              values_from = c(grade_equivalent,lexile_score), 
              names_prefix = "renstar_") %>%
  janitor::clean_names()
