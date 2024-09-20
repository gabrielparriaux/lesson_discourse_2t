# aggregated-lexical-table-creation ----

## rename-some-factors ----

# address to
docvars(dfm_segm_lemm_part_wo_na) <- docvars(dfm_segm_lemm_part_wo_na) %>%
  mutate(To = case_when(
    recipient == "class" ~ "recipient_class",
    recipient == "pupil" ~ "recipient_pupil",
    recipient == "teacher" ~ "recipient_teacher"
  )) %>% mutate(To = as.factor(recipient))

## group-documents-by-variables ----

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_teacher_id <- dfm_group(dfm_segm_lemm_part_wo_na, docvars(dfm_segm_lemm_part_wo_na)$teacher_id)
# Convert to data.frame
df_teacher_id <- convert(dfm_teacher_id, "data.frame")
# Convert column doc_id into rownames
df_teacher_id <- df_teacher_id %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_recipient <- dfm_group(dfm_segm_lemm_part_wo_na, docvars(dfm_segm_lemm_part_wo_na)$recipient)
# Convert to data.frame
df_recipient <- convert(dfm_recipient, "data.frame")
# Convert column doc_id into rownames
df_recipient <- df_recipient %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_time_slice <- dfm_group(dfm_segm_lemm_part_wo_na, docvars(dfm_segm_lemm_part_wo_na)$time_slice)
# Convert to data.frame
df_time_slice <- convert(dfm_time_slice, "data.frame")
# Convert column doc_id into rownames
df_time_slice <- df_time_slice %>% remove_rownames %>% column_to_rownames(var="doc_id")

# Create dfm by grouping around one variable — sums the number of segments containing each word for every modality of the variable
dfm_cluster <- dfm_group(dfm_segm_lemm_part_wo_na, docvars(dfm_segm_lemm_part_wo_na)$lesson_cluster_id)
# Convert to data.frame
df_cluster <- convert(dfm_cluster, "data.frame")
# Convert column doc_id into rownames
df_cluster <- df_cluster %>% remove_rownames %>% column_to_rownames(var="doc_id")

# group all dfm into one
tableau_lexical_questions <- rbind(df_teacher_id, df_recipient)
tableau_lexical_questions <- rbind(tableau_lexical_questions, df_time_slice)
tableau_lexical_questions <- rbind(tableau_lexical_questions, df_cluster)

# Order lexicon decreasing to view high and low values
tableau_lexical_questions <- tableau_lexical_questions[,order(colSums(tableau_lexical_questions), decreasing = TRUE)]

## add-clusters-to-alt-and-compute-mca ----

### group small clusters ----

# If I don’t want to keep a fixed number of clusters, but use a threshold value of number of segments in the cluster under which we group them, this is the way to do it

# How to decide the threshold? Clusters including more than 1% of segments of the corpus represent 12 clusters. We decide to regroup clusters under 1% of the corpus.
# cluster_threshold <- sum(cluster_nb_of_segments)/25
# cluster_nb_of_segments_under_threshold <- cluster_nb_of_segments[cluster_nb_of_segments < cluster_threshold]
# get the names of the small clusters to regroup
# clusters_to_regroup <- names(cluster_nb_of_segments_under_threshold)

# And this is the way to go if I decide to regroup all but the biggest clusters (fixed number of 10)

# create a vector with a list of all clusters numbers, depending on the number of clusters defined at the beginning
clusters_numbers_list <- 1:number_of_lesson_clusters

# create a vector for the small clusters, as a diff between the list of all clusters and the biggest clusters
smallest_clusters <- setdiff(clusters_numbers_list, biggest_clusters)

# group small clusters from ALT into a new row with the sum of their values
# Create a df with sum of all small clusters
sum_small_clusters <- tableau_lexical_questions %>% filter(rownames(tableau_lexical_questions) %in% smallest_clusters) %>% summarise(across(everything(), sum))

# rename the unique row of this dataframe
rownames(sum_small_clusters) <- "small_clusters"

# remove the rows corresponding to small clusters in the generalised aggregated lexical table
tableau_lexical_questions <- tableau_lexical_questions[!(row.names(tableau_lexical_questions) %in% smallest_clusters),]

# add the row with the sum of small clusters
tableau_lexical_questions <- rbind(tableau_lexical_questions, sum_small_clusters)

# rename clusters if they have been analysed
if (clusters_renamed_yes) {
  # extract rownames into a column
  tableau_lexical_questions <- rownames_to_column(tableau_lexical_questions)
  # rename it for merge
  colnames(tableau_lexical_questions)[1] <- "lesson_cluster_id"
  # merge with the biggest clusters df into another df
  tbl_lex_quest_merged <- merge(tableau_lexical_questions, biggest_clusters_df, by = "lesson_cluster_id")
  # rename the column and drop the other
  tbl_lex_quest_merged$lesson_cluster_id <- tbl_lex_quest_merged$name
  tbl_lex_quest_merged$name <- NULL
  # remove the biggest clusters from the generalised aggregated lexical table
  tableau_lexical_questions <- tableau_lexical_questions[!tableau_lexical_questions$lesson_cluster_id %in% biggest_clusters, ]
  # add the renamed clusters
  tableau_lexical_questions <- rbind(tableau_lexical_questions, tbl_lex_quest_merged)
  # transform first column into rownames
  rownames(tableau_lexical_questions) <- tableau_lexical_questions[ , 1]
  tableau_lexical_questions[ , 1] <- NULL
} else {
  # extract rows corresponding to the biggest clusters
  tbl_lex_quest_extract <- tableau_lexical_questions %>% filter(rownames(tableau_lexical_questions) %in% biggest_clusters)
  # rename clusters with cluster_
  rownames(tbl_lex_quest_extract) <- paste0("cluster_", rownames(tbl_lex_quest_extract))
  # remove the biggest clusters from the generalised aggregated lexical table
  tableau_lexical_questions <- tableau_lexical_questions[!rownames(tableau_lexical_questions) %in% biggest_clusters, ]
  # add the renamed clusters
  tableau_lexical_questions <- rbind(tableau_lexical_questions, tbl_lex_quest_extract)
}

# Remove columns with values under… (5?)
# tableau_lexical_questions_extract <- tableau_lexical_questions %>%
#   select_if(~ !any(.x < 5))

# Limit lexicon to a fixed number of words… (70?)
# tableau_lexical_questions_extract <- tableau_lexical_questions[,1:70]

# transpose the aggregated lexical table
tableau_lexical_questions <- as.data.frame(t(tableau_lexical_questions))

# Correspondence Analysis (CA) ----

ca_lesson_disc <- CA(tableau_lexical_questions, graph = FALSE)
# explor(ca_lesson_disc)

# compute-plot-eigenvalues ----

# Eigenvalues
ca_alt_screeplot <- fviz_screeplot(ca_lesson_disc, addlabels = TRUE, ncp = 20, title = "", ylim = c(0, 50), theme = theme_minimal())

# compute-plot-ca ----

set.seed(42)

plot_ca_lesson_disc <- fviz_ca_biplot(
  ca_lesson_disc,
  axes = c(1, 2),
  geom = c("point", "text"),
  select.row = list(cos2 = 50),
  # select.col = list(cos2 = 50),
  labelsize = 6,
  label = "all",
  col.col = "#CC79A7",
  col.row = "#0072B2",
  shape.row = 0,
  shape.col = 4,
  pointsize = 3,
  repel = TRUE,
  title = ""
) + theme(text = element_text(size = 7.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          plot.margin = margin(0, 10, 10, 10, unit = "pt"))

# ggsave(filename="mca.pdf", path = "output", scale = 1.4)

# contrib-cos2-coord-create-tables ----

## coord ----

# create a dataframe with coordinates for all dimensions of CA
# variables
ca_lesson_disc_var_coord <- as.data.frame(ca_lesson_disc[["col"]]$coord)
# lexicon
ca_lesson_disc_lexicon_coord <- as.data.frame(ca_lesson_disc[["row"]]$coord)
# rbind variables and lexicon
ca_lesson_disc_coord <- rbind(ca_lesson_disc_var_coord, ca_lesson_disc_lexicon_coord)

## contrib ----

# create a dataframe with contrib for all dimensions of CA
# variables
ca_lesson_disc_var_contrib <- as.data.frame(ca_lesson_disc[["col"]]$contrib)
# lexicon
ca_lesson_disc_lexicon_contrib <- as.data.frame(ca_lesson_disc[["row"]]$contrib)
# rbind variables and lexicon
ca_lesson_disc_contrib <- rbind(ca_lesson_disc_var_contrib, ca_lesson_disc_lexicon_contrib)

## cos2 ----

# create a dataframe with cos2 for all dimensions of CA
# variables
ca_lesson_disc_var_cos2 <- as.data.frame(ca_lesson_disc[["col"]]$cos2)
# lexicon
ca_lesson_disc_lexicon_cos2 <- as.data.frame(ca_lesson_disc[["row"]]$cos2)
# rbind variables and lexicon
ca_lesson_disc_cos2 <- rbind(ca_lesson_disc_var_cos2, ca_lesson_disc_lexicon_cos2)

# round values of the three tables to 3 digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
ca_lesson_disc_coord <- round_df(ca_lesson_disc_coord, 3)
ca_lesson_disc_contrib <- round_df(ca_lesson_disc_contrib, 3)
ca_lesson_disc_cos2 <- round_df(ca_lesson_disc_cos2, 3)

## Isolate dimension 1 ----

# create a dataframe with coord, contrib and cos2 values for dimension 1
table_ca_lesson_disc_dim1 <- data.frame(ca_lesson_disc_coord[,1], ca_lesson_disc_contrib[,1], ca_lesson_disc_cos2[,1])

# rename rows and columns
rownames(table_ca_lesson_disc_dim1) <- rownames(ca_lesson_disc_coord)
colnames(table_ca_lesson_disc_dim1) <- c('coord', 'contrib', 'cos2')

# order columns in decreasing order according to cos2
table_ca_lesson_disc_dim1 <- table_ca_lesson_disc_dim1 %>% arrange(desc(table_ca_lesson_disc_dim1$cos2))

# keep 30 first rows
table_ca_lesson_disc_dim1_extract <- table_ca_lesson_disc_dim1[1:30, ]

## Isolate dimension 2 ----

# create a dataframe with coord, contrib and cos2 values for dimension 2
table_ca_lesson_disc_dim2 <- data.frame(ca_lesson_disc_coord[,2], ca_lesson_disc_contrib[,2], ca_lesson_disc_cos2[,2])

# rename rows and columns
rownames(table_ca_lesson_disc_dim2) <- rownames(ca_lesson_disc_coord)
colnames(table_ca_lesson_disc_dim2) <- c('coord', 'contrib', 'cos2')

# order columns in decreasing order according to cos2
table_ca_lesson_disc_dim2 <- table_ca_lesson_disc_dim2 %>% arrange(desc(table_ca_lesson_disc_dim2$cos2))

# keep 30 first rows
table_ca_lesson_disc_dim2_extract <- table_ca_lesson_disc_dim2[1:30, ]