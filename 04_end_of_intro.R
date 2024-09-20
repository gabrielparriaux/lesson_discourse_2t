# second-teacher-columns-management ----

# delete unnecessary columns
drops2 <- c("Out","Duration.y", "Commented_Text", "Thread_Resolved")
second_teacher_disc_df <- second_teacher_disc_df[ , !(names(second_teacher_disc_df) %in% drops2)]

# rename columns after merge
names(second_teacher_disc_df)[names(second_teacher_disc_df) == 'Duration.x'] <- 'statement_duration'
names(second_teacher_disc_df)[names(second_teacher_disc_df) == 'Comment_1'] <- 'recipient'

# remove unnecessary email in recipient column
second_teacher_disc_df$recipient<-gsub("name.surname@domain.org: ","",as.character(second_teacher_disc_df$recipient))

# triplicate column to separate comments
second_teacher_disc_df$comment_didactic <- second_teacher_disc_df$recipient
second_teacher_disc_df$comment_bugs <- second_teacher_disc_df$recipient

## recipient column ----

# delete all comments that do not contain adresse: in recipient column and replace them with NA values
second_teacher_disc_df$recipient <- with(second_teacher_disc_df, ifelse(!grepl("*ADRESSE:*", second_teacher_disc_df$recipient), NA, second_teacher_disc_df$recipient))

# remove other comments about didactic or bugs from the recipient column
# ADRESSE: comes always first in the comments, so we need to remove what comes after
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
second_teacher_disc_df$recipient <- gsub("BUG ENREGISTREMENT.*","",as.character(second_teacher_disc_df$recipient))
second_teacher_disc_df$recipient <- gsub("DIDACTIQUE.*","",as.character(second_teacher_disc_df$recipient))

# remove possible \n introduced by a line break when introducing the comment in Trint
second_teacher_disc_df$recipient <- gsub("\n"," ",as.character(second_teacher_disc_df$recipient))

## comment_didactic column ----

# delete all comments that do not contain DIDACTIQUE: in comment_didactic column
second_teacher_disc_df$comment_didactic <- with(second_teacher_disc_df, ifelse(!grepl("*DIDACTIQUE:*", second_teacher_disc_df$comment_didactic), "", second_teacher_disc_df$comment_didactic))

# remove other comments about adresse: or bugs from the comment_didactic column
# DIDACTIQUE: comes in the middle in the comments, so there are things to remove before and after…
# .*DIDACTIQUE selects all the text that comes before and up to DIDACTIQUE
second_teacher_disc_df$comment_didactic <-gsub(".*DIDACTIQUE","DIDACTIQUE",as.character(second_teacher_disc_df$comment_didactic))
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
second_teacher_disc_df$comment_didactic <- gsub("BUG ENREGISTREMENT.*","",as.character(second_teacher_disc_df$comment_didactic))

# remove possible \n introduced by a line break when introducing the comment in Trint
second_teacher_disc_df$comment_didactic <- gsub("\n"," ",as.character(second_teacher_disc_df$comment_didactic))

## comment_bugs column ----

# delete all comments that do not contain BUG in comment_bugs column
second_teacher_disc_df$comment_bugs <- with(second_teacher_disc_df, ifelse(!grepl("*BUG*", second_teacher_disc_df$comment_bugs), "", second_teacher_disc_df$comment_bugs))

# remove other comments about adresse: or didactic from the comment_bugs column
# BUG ENREGISTREMENT always comes last in the comments, so we need to remove all that comes before
# .*BUG selects all the text that comes before and up to BUG
second_teacher_disc_df$comment_bugs <-gsub(".*BUG","BUG",as.character(second_teacher_disc_df$comment_bugs))

# remove possible \n introduced by a line break when introducing the comment in Trint
second_teacher_disc_df$comment_bugs <- gsub("\n"," ",as.character(second_teacher_disc_df$comment_bugs))

# copy ADRESSE: values in recipient column when value is NA, direction down (default)
second_teacher_disc_df <- second_teacher_disc_df %>% fill(recipient)

# replace values into column to remove ADDRESS and to put in English
second_teacher_disc_df$recipient <- with(second_teacher_disc_df, ifelse(grepl("ADRESSE: classe", second_teacher_disc_df$recipient), "class", second_teacher_disc_df$recipient))
second_teacher_disc_df$recipient <- with(second_teacher_disc_df, ifelse(grepl("ADRESSE: élève", second_teacher_disc_df$recipient), "pupil", second_teacher_disc_df$recipient))
second_teacher_disc_df$recipient <- with(second_teacher_disc_df, ifelse(grepl("ADRESSE: enseignant", second_teacher_disc_df$recipient), "teacher", second_teacher_disc_df$recipient))

# merging two teachers discourses into lesson discourse ----

# rbind the two tables
lesson_disc_df <- rbind(first_teacher_disc_df, second_teacher_disc_df)

# sort by timestamp column
lesson_disc_df <- lesson_disc_df[order(lesson_disc_df$timestamp, decreasing = FALSE), ]

# time-management ----

# remove digits for seconds
lesson_disc_df$timestamp = substr(lesson_disc_df$timestamp, 1, nchar(lesson_disc_df$timestamp)-4)

# store first statement starting time in a variable
lesson_start_time <- period_to_seconds(hms(lesson_disc_df$timestamp[1]))

# remove pause duration if needed (only for statements after the pause)
if (pause_yes) {
  # loop through lesson_disc_df$timestamp and remove pause duration for all values after pause_start_time
  lesson_disc_df <- mutate(lesson_disc_df, timestamp = case_when(
    period_to_seconds(hms(lesson_disc_df$timestamp)) > pause_start_time ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(lesson_disc_df$timestamp)) - pause_duration_in_s)), format="%H:%M:%S"), 
    TRUE   ~ timestamp 
  ))
}

# adjust starting time to zero and move all the timing backward accordingly
lesson_disc_df <- mutate(lesson_disc_df, timestamp = case_when(
  TRUE   ~ strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + (seconds_to_period(period_to_seconds(hms(lesson_disc_df$timestamp)) - lesson_start_time)), format="%H:%M:%S")
))

# store last statement starting time in a variable
lesson_end_time <- period_to_seconds(hms(lesson_disc_df$timestamp[nrow(lesson_disc_df)]))

# total duration of the lesson in seconds
lesson_full_duration <- lesson_end_time + lesson_disc_df$statement_duration[nrow(lesson_disc_df)]

## lesson_disc_comments_dida_df ----

# remove rows where comment_didactic is empty
lesson_disc_comments_dida_df <- subset(lesson_disc_df, comment_didactic!="")

# delete unnecessary columns
drops8 <- c("statement_duration", "recipient", "comment_bugs")
lesson_disc_comments_dida_df <- lesson_disc_comments_dida_df[ , !(names(lesson_disc_comments_dida_df) %in% drops8)]

# compute-time-slices ----

# intervals in seconds
time_interval <- 600

# number of intervals
number_of_intervals <- ceiling(lesson_full_duration / time_interval)

# for loop to create the time slices
time_slices <- data.frame()
for (i in 1:number_of_intervals) {
  time_slices <- rbind(time_slices, data.frame(start = (i-1)*time_interval, end = i*time_interval))
}

# for loop to rename rows
for (i in 1:nrow(time_slices)) {
  rownames(time_slices)[i] <- paste0("time[", (i-1)*10, "-", i*10, "]")
}

# assign-statements-to-time-slices ----

# for lesson_disc_df 

# compute a new column with timestamp value converted in seconds in lesson_disc_df
lesson_disc_df$timestamp_sec <- period_to_seconds(hms(lesson_disc_df$timestamp))

# create an empty column for time_slice
lesson_disc_df$time_slice = NA_character_

# loop through the dataframe to fill in the time_slice column until number_of_intervals-1
for(i in 1:(number_of_intervals-1)){
  lesson_disc_df <- lesson_disc_df %>%
    mutate(time_slice = case_when(
      timestamp_sec >= time_slices[i, 1] & timestamp_sec < time_slices[i+1, 1] ~ rownames(time_slices)[i], 
      TRUE ~ time_slice
    ))
}

# fill in the time_slice column for the last interval 
lesson_disc_df <- lesson_disc_df %>%
  mutate(time_slice = case_when(
    timestamp_sec >= time_slices[number_of_intervals, 1] ~ rownames(time_slices)[number_of_intervals],
    TRUE ~ time_slice
  ))

# remove timestamp_sec column
lesson_disc_df <- subset(lesson_disc_df, select = -c(timestamp_sec))

# Here we have a complete corpus (in the form of a dataframe) with all the words in their original form

## create a copy just for display because we will remove rownames so that they are not displayed in output
lesson_disc_for_display <- lesson_disc_df

## remove rownames
rownames(lesson_disc_for_display) <- NULL

# apostrophe replacement for quanteda ----

## in the lemmatisation process, ’ character (French apostrophe or typographic apostrophe) is not recognised by quanteda, so we need to replace it with ' (straight apostrophe) for the corpus creation. We keep the original text with the curved apostrophe for the display, which is nicer. 

## replace ’ character with '
lesson_disc_df$statement_text <- gsub("’","'",as.character(lesson_disc_df$statement_text))

# main-corpus-creation ----

## create a corpus object with quanteda from full discourse of the lesson
corpus_lesson_disc <- corpus(lesson_disc_df, text_field = "statement_text")

## for the record, display one full text entry of the object
## as.character(corpus_lesson_disc)[1]

## save summary of the corpus in a table
corpus_lesson_disc.stats <- summary(corpus_lesson_disc, n = 1000000)

## create a tokens object with the full corpus unlemmatised (used just after for statistics, and later for dispersion and word in context)
tok_lesson_disc <- tokens(corpus_lesson_disc, remove_punct = TRUE, split_hyphens = FALSE)
tok_lesson_disc <- tokens_split(tok_lesson_disc,"'") # to split forms like "l'ordinateur" into "l" and "ordinateur"
tok_lesson_disc <- tokens_tolower(tok_lesson_disc)

# compute-general-stats-for-lesson ----

# as the corpus has not been lemmatised and is complete, the stats we get here are for the original text complete without lemmatisation. Punctuation is removed.

## text_length

## compute values for text_length of first_teacher (= discourse)
first_teacher_disc_to_class_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")) %>% sum()
first_teacher_disc_to_pupil_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")) %>% sum()
first_teacher_disc_to_teacher_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")) %>% sum()
first_teacher_disc_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == first_teacher)) %>% sum()

## compute values for text_length of second_teacher (= discourse)
second_teacher_disc_to_class_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "class")) %>% sum()
second_teacher_disc_to_pupil_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "pupil")) %>% sum()
second_teacher_disc_to_teacher_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "teacher")) %>% sum()
second_teacher_disc_text_length <- ntoken(tokens_subset(tok_lesson_disc, teacher_id == second_teacher)) %>% sum()

## compute values for text length of merged_teachers (= lesson)
lesson_to_class_text_length <- ntoken(tokens_subset(tok_lesson_disc, recipient == "class")) %>% sum()
lesson_to_pupil_text_length <- ntoken(tokens_subset(tok_lesson_disc, recipient == "pupil")) %>% sum()
lesson_to_teacher_text_length <- ntoken(tokens_subset(tok_lesson_disc, recipient == "teacher")) %>% sum()
lesson_disc_text_length <- ntoken(tokens_subset(tok_lesson_disc)) %>% sum()

## vocab_size

## compute values for vocabulary size of first_teacher (= discourse)
first_teacher_disc_to_class_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")))
first_teacher_disc_to_pupil_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")))
first_teacher_disc_to_teacher_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")))
first_teacher_disc_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == first_teacher)))

## compute values for vocabulary size of second_teacher (= discourse)
second_teacher_disc_to_class_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "class")))
second_teacher_disc_to_pupil_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "pupil")))
second_teacher_disc_to_teacher_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "teacher")))
second_teacher_disc_vocab_size <- length(types(tokens_subset(tok_lesson_disc, teacher_id == second_teacher)))

## compute values for vocabulary size of merged_teachers (= lesson)
lesson_to_class_vocab_size <- length(types(tokens_subset(tok_lesson_disc, recipient == "class")))
lesson_to_pupil_vocab_size <- length(types(tokens_subset(tok_lesson_disc, recipient == "pupil")))
lesson_to_teacher_vocab_size <- length(types(tokens_subset(tok_lesson_disc, recipient == "teacher")))
lesson_disc_vocab_size <- length(types(tok_lesson_disc))

## duration

## compute values for duration of first_teacher (= discourse)
first_teacher_disc_to_class_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
first_teacher_disc_to_pupil_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
first_teacher_disc_to_teacher_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
first_teacher_disc_duration <- tokens_subset(tok_lesson_disc, teacher_id == first_teacher)$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

## compute values for duration of second_teacher (= discourse)
second_teacher_disc_to_class_duration <- tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "class")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
second_teacher_disc_to_pupil_duration <- tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "pupil")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
second_teacher_disc_to_teacher_duration <- tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "teacher")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
second_teacher_disc_duration <- tokens_subset(tok_lesson_disc, teacher_id == second_teacher)$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

## compute values for duration of merged_teachers (= lesson)
lesson_to_class_duration <- tokens_subset(tok_lesson_disc, recipient == "class")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
lesson_to_pupil_duration <- tokens_subset(tok_lesson_disc, recipient == "pupil")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
lesson_to_teacher_duration <- tokens_subset(tok_lesson_disc, recipient == "teacher")$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }
lesson_disc_duration <- tok_lesson_disc$statement_duration %>% sum() %>%
  round(., 0) %>% { paste0(((. - (. %% 60)) / 60), " min ", . %% 60, " sec") }

## number_of_statements

## count number of statements for first_teacher discourse
first_teacher_disc_to_class_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "class"))
first_teacher_disc_to_pupil_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "pupil"))
first_teacher_disc_to_teacher_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher & recipient == "teacher"))
first_teacher_disc_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == first_teacher))

## count number of statements for second_teacher discourse
second_teacher_disc_to_class_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "class"))
second_teacher_disc_to_pupil_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "pupil"))
second_teacher_disc_to_teacher_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == second_teacher & recipient == "teacher"))
second_teacher_disc_number_of_statements <- length(tokens_subset(tok_lesson_disc, teacher_id == second_teacher))

## count number of statements for lesson discourse
lesson_to_class_number_of_statements <- length(tokens_subset(tok_lesson_disc, recipient == "class"))
lesson_to_pupil_number_of_statements <- length(tokens_subset(tok_lesson_disc, recipient == "pupil"))
lesson_to_teacher_number_of_statements <- length(tokens_subset(tok_lesson_disc, recipient == "teacher"))
lesson_disc_number_of_statements <- length(tok_lesson_disc)

## lexical_diversity

## create df with lexical_diversity values by teacher
teacher_recipient_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = interaction(teacher_id, recipient)) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)
teacher_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = teacher_id) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)

## create df with lexical_diversity value for the lesson, by recipient
recipient_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = recipient) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)
## artificially add all_docs docvar to be able to group all docs in tokens object
tok_lesson_disc$all_docs <- "all_docs"
## create df with lexical_diversity value for the lesson
lesson_lexical_diversity_df <- tokens_group(tok_lesson_disc, groups = all_docs) %>% textstat_lexdiv(c("MATTR"), MATTR_window = 50)

## compute values for lexical diversity of first_teacher (= discourse)
first_teacher_disc_to_class_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "class")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
first_teacher_disc_to_pupil_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "pupil")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
first_teacher_disc_to_teacher_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(first_teacher, ".", "teacher")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
first_teacher_disc_lexical_diversity <- teacher_lexical_diversity_df %>% filter(document == first_teacher) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)

## compute values for lexical diversity of second_teacher (= discourse)
second_teacher_disc_to_class_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(second_teacher, ".", "class")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
second_teacher_disc_to_pupil_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(second_teacher, ".", "pupil")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
second_teacher_disc_to_teacher_lexical_diversity <- teacher_recipient_lexical_diversity_df %>% filter(document == paste0(second_teacher, ".", "teacher")) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
second_teacher_disc_lexical_diversity <- teacher_lexical_diversity_df %>% filter(document == second_teacher) %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)

## compute values for lexical_diversity for the lesson
lesson_to_class_lexical_diversity <- recipient_lexical_diversity_df %>% filter(document == "class") %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
lesson_to_pupil_lexical_diversity <- recipient_lexical_diversity_df %>% filter(document == "pupil") %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
lesson_to_teacher_lexical_diversity <- recipient_lexical_diversity_df %>% filter(document == "teacher") %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)
lesson_disc_lexical_diversity <- lesson_lexical_diversity_df %>% select(MATTR) %>% as.numeric() %>% round(digits = 4)

# first-teacher-discourse-stats-table-creation ----

# create one vector for each column of the df
ft_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Full discourse")
ft_number_of_statements <- c(first_teacher_disc_to_class_number_of_statements, first_teacher_disc_to_pupil_number_of_statements, first_teacher_disc_to_teacher_number_of_statements, first_teacher_disc_number_of_statements)
ft_duration <- c(first_teacher_disc_to_class_duration, first_teacher_disc_to_pupil_duration, first_teacher_disc_to_teacher_duration, first_teacher_disc_duration)
ft_text_length <- c(first_teacher_disc_to_class_text_length, first_teacher_disc_to_pupil_text_length, first_teacher_disc_to_teacher_text_length, first_teacher_disc_text_length)
ft_vocab_size <- c(first_teacher_disc_to_class_vocab_size, first_teacher_disc_to_pupil_vocab_size, first_teacher_disc_to_teacher_vocab_size, first_teacher_disc_vocab_size)
ft_lexical_diversity <- c(first_teacher_disc_to_class_lexical_diversity, first_teacher_disc_to_pupil_lexical_diversity, first_teacher_disc_to_teacher_lexical_diversity, first_teacher_disc_lexical_diversity)

# create dataframe with those vectors
first_teacher_stats_table <- data.frame(ft_recipients, ft_number_of_statements, ft_duration, ft_text_length, ft_vocab_size, ft_lexical_diversity)

# change column names
colnames(first_teacher_stats_table) <- c("Recipient", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)", "Lexical diversity (MATTR)")

first_teacher_stats_table_caption <- paste("Statistics on classroom discourse for", first_teacher, ", discourse_id:", first_discourse_id)

# second-teacher-discourse-stats-table-creation ----

# create one vector for each column of the df
st_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Full discourse")
st_number_of_statements <- c(second_teacher_disc_to_class_number_of_statements, second_teacher_disc_to_pupil_number_of_statements, second_teacher_disc_to_teacher_number_of_statements, second_teacher_disc_number_of_statements)
st_duration <- c(second_teacher_disc_to_class_duration, second_teacher_disc_to_pupil_duration, second_teacher_disc_to_teacher_duration, second_teacher_disc_duration)
st_text_length <- c(second_teacher_disc_to_class_text_length, second_teacher_disc_to_pupil_text_length, second_teacher_disc_to_teacher_text_length, second_teacher_disc_text_length)
st_vocab_size <- c(second_teacher_disc_to_class_vocab_size, second_teacher_disc_to_pupil_vocab_size, second_teacher_disc_to_teacher_vocab_size, second_teacher_disc_vocab_size)
st_lexical_diversity <- c(second_teacher_disc_to_class_lexical_diversity, second_teacher_disc_to_pupil_lexical_diversity, second_teacher_disc_to_teacher_lexical_diversity, second_teacher_disc_lexical_diversity)

# create dataframe with those vectors
second_teacher_stats_table <- data.frame(st_recipients, st_number_of_statements, st_duration, st_text_length, st_vocab_size, st_lexical_diversity)

# change column names
colnames(second_teacher_stats_table) <- c("Recipient", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)", "Lexical diversity (MATTR)")

second_teacher_stats_table_caption <- paste("Statistics on classroom discourse for", second_teacher, ", discourse_id:", second_discourse_id)

# lesson-stats-table-creation ----

# create one vector for each column of the df
lesson_recipients <- c("Whole class", "Individual pupil", "Other teacher", "Full discourse")
lesson_number_of_statements <- c(lesson_to_class_number_of_statements, lesson_to_pupil_number_of_statements, lesson_to_teacher_number_of_statements, lesson_disc_number_of_statements)
lesson_duration <- c(lesson_to_class_duration, lesson_to_pupil_duration, lesson_to_teacher_duration, lesson_disc_duration)
lesson_text_length <- c(lesson_to_class_text_length, lesson_to_pupil_text_length, lesson_to_teacher_text_length, lesson_disc_text_length)
lesson_vocab_size <- c(lesson_to_class_vocab_size, lesson_to_pupil_vocab_size, lesson_to_teacher_vocab_size, lesson_disc_vocab_size)
lesson_lexical_diversity <- c(lesson_to_class_lexical_diversity, lesson_to_pupil_lexical_diversity, lesson_to_teacher_lexical_diversity, lesson_disc_lexical_diversity)

# create dataframe with those vectors
lesson_stats_table <- data.frame(lesson_recipients, lesson_number_of_statements, lesson_duration, lesson_text_length, lesson_vocab_size, lesson_lexical_diversity)

# change column names
colnames(lesson_stats_table) <- c("Recipient", "No. of statements","Duration", "Text length (in words)", "Vocabulary size (in words)", "Lexical diversity (MATTR)")

lesson_stats_table_caption <- paste("Statistics on classroom discourse for the lesson_id:", this_lesson_id)

# compute-number-of-tokens-by-statement ----

# test --- as presented in https://api.rpubs.com/cbpuschmann/textmining

# plot the number of tokens per statement, with colors for recipient
tokens_per_statement_by_recipient <- ggplot(
  corpus_lesson_disc.stats, 
  aes(x = fct_inorder(Text), y = Tokens, group=1, color=recipient)) + 
  # geom_line(linewidth = .3) + 
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  geom_point(size = .5) +
  labs(title = "", x = "Statements", y = "Words") +
  # dark_mode(theme_bw()) + # if we want to use dark mode
  theme_bw() +
  theme(legend.title=element_text(size=13), legend.text=element_text(size=12)) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 0, vjust = 0.8, hjust=1)) + 
  # limit the number of x labels
  scale_x_discrete(breaks = c(corpus_lesson_disc.stats$Text[1], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/2], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4*3], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)]))

# smoothing intents

# same plot than before, but with smoothing function integrated in ggplot with library tidyquant (instead of geom_line())
# https://search.r-project.org/CRAN/refmans/tidyquant/html/geom_ma.html
# library(tidyquant)
# ggplot(corpus_merged_teachers.stats, aes(statement_text, Tokens, group=1, color=To)) + geom_point(size = 1) + ggtitle("Tokens per statement") + geom_ma(ma_fun = DEMA, n = 3, linetype = "solid", size = 2)

# smoothing with rollmean and ksmooth, creating a temporary df
# https://boostedml.com/2020/05/an-introduction-to-time-series-smoothing-in-r.html
# temp <- corpus_merged_teachers.stats %>% select(statement_text, Tokens)
# plot(temp$Tokens)
# Simple Moving Average (SMA)
# lines(rollmean(temp$Tokens,3),col='blue')
# Triangular Moving Average (TMA)
# lines(rollmean(rollmean(temp$Tokens,2),2),col='blue')
# this one doesn’t work because I don’t know how to plot a serie of individual values (it requires two numeric axes)
# lines(ksmooth(temp$statement_text, temp$Tokens, "normal", bandwidth = 2), col = 2)

# plot the number of tokens per statement, with colors for teacher_id
tokens_per_statement_by_teacher <- ggplot(
  corpus_lesson_disc.stats, 
  aes(x = fct_inorder(Text), y = Tokens, group=1, color=teacher_id)) + 
  # geom_line(linewidth = .3) + 
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  geom_point(size = .3) + 
  labs(title = "", x = "Statements", y = "Words") +
  # dark_mode(theme_bw()) + # if we want to use dark mode
  theme_bw() +
  theme(legend.title=element_text(size=13), legend.text=element_text(size=12)) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 0, vjust = 0.8, hjust=1)) + 
  scale_x_discrete(breaks = c(corpus_lesson_disc.stats$Text[1], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/2], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)/4*3], corpus_lesson_disc.stats$Text[nrow(corpus_lesson_disc.stats)]))

# plot the number of types per statement
ggplot(corpus_lesson_disc.stats, aes(Text, Types, group=1)) + geom_line() + geom_point() + ggtitle("Types per statement")

# plot the number of sentences per statement
ggplot(corpus_lesson_disc.stats, aes(Text, Sentences, group=1)) + geom_line() + geom_point() + ggtitle("Sentences per statement")

# plot the three together
ggplot(corpus_lesson_disc.stats %>% gather(Types, Tokens, Sentences, key = "Unit", value = "Number"), aes(Text, Number, group = Unit, col = Unit)) + geom_line(linewidth = 1) + ggtitle("Tokens, Types and Sentences per statement")

# plot the type-token ratio per statement
ggplot(corpus_lesson_disc.stats, aes(Tokens, Types, group=1, label = Text)) + geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + ggtitle("Type-Token ratio per statement")

# compute-tokens-in-context-and-dispersion ----

# find a word in context
matching <- kwic(tok_lesson_disc, "program*")
head(matching, 20)

# find two words in context
matching <- kwic(tok_lesson_disc, c("avance*", "tourne*"), window = 10, case_insensitive = FALSE)
head(matching, 30)

# dispersion of a word in the text
textplot_xray(kwic(tok_lesson_disc, "commande*", valuetype = "regex", case_insensitive = FALSE), kwic(tok_lesson_disc, "program*", valuetype = "regex", case_insensitive = FALSE))