# first-teacher-columns-management ----

# delete unnecessary columns
drops <- c("Out","Duration.y", "Commented_Text", "Thread_Resolved", "Comment_2")
first_teacher_disc_df <- first_teacher_disc_df[ , !(names(first_teacher_disc_df) %in% drops)]

# rename columns after merge
names(first_teacher_disc_df)[names(first_teacher_disc_df) == 'Duration.x'] <- 'statement_duration'
names(first_teacher_disc_df)[names(first_teacher_disc_df) == 'Comment_1'] <- 'recipient'

# remove unnecessary email in recipient column
first_teacher_disc_df$recipient<-gsub("name.surname@domain.org: ","",as.character(first_teacher_disc_df$recipient))

# triplicate column to separate comments
first_teacher_disc_df$comment_didactic <- first_teacher_disc_df$recipient
first_teacher_disc_df$comment_bugs <- first_teacher_disc_df$recipient

## recipient column ----

# delete all comments that do not contain adresse: in recipient column and replace them with NA values
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(!grepl("*ADRESSE:*", first_teacher_disc_df$recipient), NA, first_teacher_disc_df$recipient))

# remove other comments about didactic or bugs from the recipient column
# ADRESSE: comes always first in the comments, so we need to remove what comes after
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_disc_df$recipient <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- gsub("DIDACTIQUE.*","",as.character(first_teacher_disc_df$recipient))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$recipient <- gsub("\n"," ",as.character(first_teacher_disc_df$recipient))

## comment_didactic column ----

# delete all comments that do not contain DIDACTIQUE: in comment_didactic column
first_teacher_disc_df$comment_didactic <- with(first_teacher_disc_df, ifelse(!grepl("*DIDACTIQUE:*", first_teacher_disc_df$comment_didactic), "", first_teacher_disc_df$comment_didactic))

# remove other comments about adresse: or bugs from the comment_didactic column
# DIDACTIQUE: comes in the middle in the comments, so there are things to remove before and after…
# .*DIDACTIQUE selects all the text that comes before and up to DIDACTIQUE
first_teacher_disc_df$comment_didactic <-gsub(".*DIDACTIQUE","DIDACTIQUE",as.character(first_teacher_disc_df$comment_didactic))
# BUG ENREGISTREMENT.* selects all the text starting from BUG ENREGISTREMENT and everything that follows
first_teacher_disc_df$comment_didactic <- gsub("BUG ENREGISTREMENT.*","",as.character(first_teacher_disc_df$comment_didactic))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$comment_didactic <- gsub("\n"," ",as.character(first_teacher_disc_df$comment_didactic))

## comment_bugs column ----

# delete all comments that do not contain BUG in comment_bugs column
first_teacher_disc_df$comment_bugs <- with(first_teacher_disc_df, ifelse(!grepl("*BUG*", first_teacher_disc_df$comment_bugs), "", first_teacher_disc_df$comment_bugs))

# remove other comments about adresse: or didactic from the comment_bugs column
# BUG ENREGISTREMENT always comes last in the comments, so we need to remove all that comes before
# .*BUG selects all the text that comes before and up to BUG
first_teacher_disc_df$comment_bugs <-gsub(".*BUG","BUG",as.character(first_teacher_disc_df$comment_bugs))

# remove possible \n introduced by a line break when introducing the comment in Trint
first_teacher_disc_df$comment_bugs <- gsub("\n"," ",as.character(first_teacher_disc_df$comment_bugs))

# copy ADRESSE: values in recipient column when value is NA, direction down (default)
first_teacher_disc_df <- first_teacher_disc_df %>% fill(recipient)

# replace values into column to remove ADDRESS and to put in English
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: classe", first_teacher_disc_df$recipient), "class", first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: élève", first_teacher_disc_df$recipient), "pupil", first_teacher_disc_df$recipient))
first_teacher_disc_df$recipient <- with(first_teacher_disc_df, ifelse(grepl("ADRESSE: enseignant", first_teacher_disc_df$recipient), "teacher", first_teacher_disc_df$recipient))