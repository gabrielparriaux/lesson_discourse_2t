# import from Trint ----

## import transcript and comments ----
first_teacher_transcript <- read.csv(paste0(data_dir, "/import/", first_teacher, "_transcript.csv"))
second_teacher_transcript <- read.csv(paste0(data_dir, "/import/", second_teacher, "_transcript.csv"))
first_teacher_comments <- read.csv(paste0(data_dir, "/import/", first_teacher, "_comments.csv"))
second_teacher_comments <- read.csv(paste0(data_dir, "/import/", second_teacher, "_comments.csv"))

# check errors ----

## check errors first_teacher ----
check_1 <- subset(first_teacher_transcript, xor(Speaker == first_teacher_name, Status == "verified"))

## check errors second_teacher ----
check_2 <- subset(second_teacher_transcript, xor(Speaker == second_teacher_name, Status == "verified"))