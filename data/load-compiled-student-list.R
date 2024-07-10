compiled_student_list <- read.csv("data/compiled_student_list.csv") %>%
  mutate(student_number = as.character(student_number))
  