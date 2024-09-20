# export corpus and tokens objects for global analysis ----

## full corpus and tokens

# Save corpus_segm_unlemm_full to a file for export to global analysis
saveRDS(corpus_segm_unlemm_full, file = paste0(data_dir, "/export/", this_lesson_id, "_corpus_segm_unlemm_full.rds"))

# Save tok_segm_unlemm_w_pos_full to a file for export to global analysis
saveRDS(tok_segm_unlemm_w_pos_full, file = paste0(data_dir, "/export/", this_lesson_id, "_tok_segm_unlemm_w_pos_full.rds"))

# Save corpus_segm_unlemm_full to a file for export to global analysis
saveRDS(tok_segm_lemm_w_pos_full, file = paste0(data_dir, "/export/", this_lesson_id, "_tok_segm_lemm_w_pos_full.rds"))

## partial corpus and tokens

# Save corpus_segm_unlemm_part to a file for export to global analysis
saveRDS(corpus_segm_unlemm_part, file = paste0(data_dir, "/export/", this_lesson_id, "_corpus_segm_unlemm_part.rds"))

# Save tok_segm_lemm_part to a file for export to global analysis
saveRDS(tok_segm_lemm_part, file = paste0(data_dir, "/export/", this_lesson_id, "_tok_segm_lemm_part.rds"))

# constitute data.frames for export ----

# create a dataframe to collect all the variables related to the lesson and computed during the analysis
lesson_df <- data.frame(lesson_full_duration = lesson_full_duration,
                        lesson_disc_number_of_statements = lesson_disc_number_of_statements,
                        lesson_to_class_number_of_statements = lesson_to_class_number_of_statements,
                        lesson_to_pupil_number_of_statements = lesson_to_pupil_number_of_statements,
                        lesson_to_teacher_number_of_statements = lesson_to_teacher_number_of_statements,
                        lesson_disc_text_length = lesson_disc_text_length,
                        lesson_to_class_text_length = lesson_to_class_text_length,
                        lesson_to_pupil_text_length = lesson_to_pupil_text_length,
                        lesson_to_teacher_text_length = lesson_to_teacher_text_length,
                        lesson_disc_vocab_size = lesson_disc_vocab_size,
                        lesson_to_class_vocab_size = lesson_to_class_vocab_size,
                        lesson_to_pupil_vocab_size = lesson_to_pupil_vocab_size,
                        lesson_to_teacher_vocab_size = lesson_to_teacher_vocab_size,
                        lesson_disc_duration = lesson_disc_duration,
                        lesson_to_class_duration = lesson_to_class_duration,
                        lesson_to_pupil_duration = lesson_to_pupil_duration,
                        lesson_to_teacher_duration = lesson_to_teacher_duration,
                        lesson_disc_lexical_diversity = lesson_disc_lexical_diversity,
                        lesson_to_class_lexical_diversity = lesson_to_class_lexical_diversity,
                        lesson_to_pupil_lexical_diversity = lesson_to_pupil_lexical_diversity,
                        lesson_to_teacher_lexical_diversity = lesson_to_teacher_lexical_diversity,
                        number_of_lesson_clusters = number_of_lesson_clusters
)

# bind the initial lesson_lesson dataframe with the lesson_df dataframe
lesson_lesson <- cbind(lesson_lesson, lesson_df)

# create a vector with the variables names related to the discourse
discourses_variable_names <- c("teacher_id",
                               "disc_number_of_statements",
                               "disc_to_class_number_of_statements",
                               "disc_to_pupil_number_of_statements",
                               "disc_to_teacher_number_of_statements",
                               "disc_text_length",
                               "disc_to_class_text_length",
                               "disc_to_pupil_text_length",
                               "disc_to_teacher_text_length",
                               "disc_vocab_size",
                               "disc_to_class_vocab_size",
                               "disc_to_pupil_vocab_size",
                               "disc_to_teacher_vocab_size",
                               "disc_duration",
                               "disc_to_class_duration",
                               "disc_to_pupil_duration",
                               "disc_to_teacher_duration",
                               "disc_lexical_diversity",
                               "disc_to_class_lexical_diversity",
                               "disc_to_pupil_lexical_diversity",
                               "disc_to_teacher_lexical_diversity"
)

# create a vector with the values of the variables related to the discourse of the first_teacher
discourses_first_teacher <- c(first_teacher,
                              first_teacher_disc_number_of_statements,
                              first_teacher_disc_to_class_number_of_statements,
                              first_teacher_disc_to_pupil_number_of_statements,
                              first_teacher_disc_to_teacher_number_of_statements,
                              first_teacher_disc_text_length,
                              first_teacher_disc_to_class_text_length,
                              first_teacher_disc_to_pupil_text_length,
                              first_teacher_disc_to_teacher_text_length,
                              first_teacher_disc_vocab_size,
                              first_teacher_disc_to_class_vocab_size,
                              first_teacher_disc_to_pupil_vocab_size,
                              first_teacher_disc_to_teacher_vocab_size,
                              first_teacher_disc_duration,
                              first_teacher_disc_to_class_duration,
                              first_teacher_disc_to_pupil_duration,
                              first_teacher_disc_to_teacher_duration,
                              first_teacher_disc_lexical_diversity,
                              first_teacher_disc_to_class_lexical_diversity,
                              first_teacher_disc_to_pupil_lexical_diversity,
                              first_teacher_disc_to_teacher_lexical_diversity
)

# create a vector with the values of the variables related to the discourse of the second_teacher
discourses_second_teacher <- c(second_teacher,
                               second_teacher_disc_number_of_statements,
                               second_teacher_disc_to_class_number_of_statements,
                               second_teacher_disc_to_pupil_number_of_statements,
                               second_teacher_disc_to_teacher_number_of_statements,
                               second_teacher_disc_text_length,
                               second_teacher_disc_to_class_text_length,
                               second_teacher_disc_to_pupil_text_length,
                               second_teacher_disc_to_teacher_text_length,
                               second_teacher_disc_vocab_size,
                               second_teacher_disc_to_class_vocab_size,
                               second_teacher_disc_to_pupil_vocab_size,
                               second_teacher_disc_to_teacher_vocab_size,
                               second_teacher_disc_duration,
                               second_teacher_disc_to_class_duration,
                               second_teacher_disc_to_pupil_duration,
                               second_teacher_disc_to_teacher_duration,
                               second_teacher_disc_lexical_diversity,
                               second_teacher_disc_to_class_lexical_diversity,
                               second_teacher_disc_to_pupil_lexical_diversity,
                               second_teacher_disc_to_teacher_lexical_diversity
)

# create a dataframe constituted by the three preceeding vectors
discourses_df <- data.frame(discourses_first_teacher, discourses_second_teacher)
discourses_df <- t(discourses_df)
colnames(discourses_df) <- discourses_variable_names

# merge discourses_df and lesson_discourses by teacher_id
lesson_discourses <- merge(lesson_discourses, discourses_df, by = "teacher_id")

# export dataframes for global analysis ----

# Save lesson_lesson to a file for export to global analysis
saveRDS(lesson_lesson, file = paste0(data_dir, "/export/", this_lesson_id, "_lesson_lesson.rds"))

# Save lesson_discourses to a file for export to global analysis
saveRDS(lesson_discourses, file = paste0(data_dir, "/export/", this_lesson_id, "_lesson_discourses.rds"))