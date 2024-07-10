
source("data/load-students.R")
source("data/load-compiled-student-list.R")
source("data/load-staar.R")
source("data/load-ren-star.R")
source("data/load-dibels.R")


project_data <- students %>%
  full_join(compiled_student_list %>% select(student_number, teacher_email),  
            join_by(student_number)) %>%
  mutate(in_program = if_else(is.na(teacher_email), 0, 1)) %>%
  left_join(staar, join_by(student_number, academic_year)) %>%
  left_join(ren_star_reading, join_by(student_number, academic_year)) %>%
  left_join(dibels, join_by(student_number, academic_year))
  

write.csv( x= project_data, file = "data/project_data.csv", row.names = FALSE)