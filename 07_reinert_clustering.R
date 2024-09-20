# compute-clustering ----

# copy corpus into a new one (partial) for the clustering phase. It is partial because we will remove the segments with less than 3 tokens and the words present in less than 3 documents
corpus_segm_unlemm_part <- corpus_segm_unlemm_full

# while loop to remove the segments with less than 3 tokens and the words present in less than 3 documents (we loop as long as there are…)
# !all(ntoken(dfm_without_stopwords_and_short_segments) >= 3) returns TRUE as long as there are segments with less than 3 tokens
# !all(docfreq(dfm_without_stopwords_and_short_segments) >= 3) returns TRUE as long as there are words present in less than 3 documents
while (!all(ntoken(dfm_segm_lemm_part) >= 3) | !all(docfreq(dfm_segm_lemm_part) >= 3)) {
  # remove the same documents in the corpus to keep the corpus same size than the dfm
  # find the documents that are shorter than 3 in the dfm and isolate them
  segments_longer_than_threshold <- dfm_subset(dfm_segm_lemm_part, ntoken(dfm_segm_lemm_part) >= 3)
  # get their names
  segments_longer_than_threshold_names <- docnames(segments_longer_than_threshold)
  # subset the corpus with the names of the documents that are longer than 3
  corpus_segm_unlemm_part <- subset(corpus_segm_unlemm_part, docnames(corpus_segm_unlemm_part) %in% segments_longer_than_threshold_names)
  # subset the tokens with the names of the documents that are longer than 3
  tok_segm_lemm_part <- subset(tok_segm_lemm_part, docnames(tok_segm_lemm_part) %in% segments_longer_than_threshold_names)
  # remove documents in dfm with less than 3 tokens
  dfm_segm_lemm_part <- dfm_subset(dfm_segm_lemm_part, ntoken(dfm_segm_lemm_part) >= 3)  
  # remove words present in less than 3 documents
  dfm_segm_lemm_part <- dfm_trim(dfm_segm_lemm_part, min_docfreq = 3)
}

# for the record, list of words appearing less often in the dfm
topfeatures(dfm_segm_lemm_part, decreasing = FALSE)

# for the record, list of document containing less words
# Calculate total words in each document
doc_word_counts <- rowSums(dfm_segm_lemm_part)

# Find the document(s) with the fewest words
fewest_words_docs <- names(which.min(doc_word_counts))

# Print the document name(s) and word count(s)
for (doc in fewest_words_docs) {
  cat("Document:", doc, "- Word count:", doc_word_counts[doc], "\n")
}

# Reinert clustering
res <- rainette(dfm_segm_lemm_part, k = number_of_lesson_clusters, min_segment_size = 10, min_split_members = 8)
# res <- rainette(dfm_segm_lemm_part, k = 18, min_segment_size = 10, min_split_members = 8) # for testing different number of clusters on the fly

# explor
# rainette_explor(res, dfm_segm_lemm_part, corpus_segm_unlemm_part)

# add lesson_cluster_id to the docvars ----

# create a vector with the cluster to which each segment has been classified
groups <- cutree_rainette(res, k = number_of_lesson_clusters, criterion = "chi2")

# add groups as a docvar
docvars(dfm_segm_lemm_part)$lesson_cluster_id <- groups

# add lesson_cluster_id as a docvar in the corpus
docvars(corpus_segm_unlemm_part)$lesson_cluster_id <- docvars(dfm_segm_lemm_part)$lesson_cluster_id

# mutate docvars(corpus_segm_unlemm_part)$lesson_cluster_id to add "less_clust_0" before the number of the cluster if docvars(corpus_segm_unlemm_part)$lesson_cluster_id < 10, else add "less_clust_" before the number of the cluster.
docvars(corpus_segm_unlemm_part)$lesson_cluster_id <- ifelse(docvars(corpus_segm_unlemm_part)$lesson_cluster_id < 10, paste0("l_clust_0", docvars(corpus_segm_unlemm_part)$lesson_cluster_id), paste0("l_clust_", docvars(corpus_segm_unlemm_part)$lesson_cluster_id))

# add lesson_cluster_id as a docvar in the tokens
docvars(tok_segm_lemm_part)$lesson_cluster_id <- docvars(corpus_segm_unlemm_part)$lesson_cluster_id

# Reinert plot dendrogram ----

# because of the fact that the plot is not displayed correctly in the Quarto file if we just use the object created by the plot, we embed the plot in a function that returns the object, and we call the function in the Quarto file. This way, it works correctly (advice from juba)
plot_dendrogram <- function() {
  rainette_dendrogram <- rainette_plot(
    res, dfm_segm_lemm_part, k = number_of_lesson_clusters,
    n_terms = 20,
    free_scales = FALSE,
    measure = "chi2",
    show_negative = TRUE,
    text_size = 8
  )
  return(rainette_dendrogram)
}

# identify-biggest-clusters ----

# get cluster number of segments in a table
cluster_nb_of_segments <- table(groups)

# order table of clusters by highest number of segments
cluster_nb_of_segments <- cluster_nb_of_segments[order(cluster_nb_of_segments,decreasing = TRUE)]

# decide number of biggest clusters to keep for interpretation
cluster_nbr_for_interpretation <- 5

# isolate the names of clusters for interpretation
biggest_clusters <- as.numeric(names(cluster_nb_of_segments[1:cluster_nbr_for_interpretation]))

# create an object (list of list) with tibbles of overrepresented terms in every cluster. It contains tibbles for all clusters (not only the biggest) which are listed in ascending order of cluster number.
features_in_lesson_clusters <- rainette_stats(groups, dfm_segm_lemm_part, n_terms = 20, show_negative = TRUE)

# populate a list that aggregates text from the segments of each cluster (for further interpretation of the meaning of the cluster and LLM). 
# It contains strings for all clusters (not only the biggest) which are listed in ascending order of cluster number.
lesson_clusters_text <- list()
for (i in 1:number_of_lesson_clusters) {
  lesson_clusters_text[[i]] <- corpus_group(corpus_segm_unlemm_part, groups = lesson_cluster_id)[[i]]
}

# populate a list with a for loop to create captions for the biggest clusters
biggest_clusters_captions <- list()
for (i in 1:cluster_nbr_for_interpretation) {
  biggest_clusters_captions[[i]] <- paste("Over- and underrepresented tokens in the cluster", biggest_clusters[i], "(ordered by chi2 value)")
}

# create a LLM request for each cluster interpretation with the over-represented features and the text of the associated segments ----

## create a string for the introductory text
introductory_text <- "I am doing textual data analysis in R on a corpus in French. The corpus is composed of the discourse of teachers during a computer science lesson in primary school. Pupils do programming activities with an online platform called xlogo online where they program the movement of a turtle in logo language. I perform clustering on my corpus to identify the topics that appear in it, grouping together segments of text that have similar vocabulary. The clustering technique used is Reinert clustering, using the R rainette package. Rainette package has a function rainette_stats() that gives all the features associated with one cluster. Here is the output of rainette_stats() for the cluster."
## create a string for the middle text
middle_text <- "And here is the text of all the segments that are classified in the cluster. It is in French:"
## create a string for the end text
end_text <- "Can you help me formulate a short synthesis of between three and six sentences that would caracterise this cluster and propose a name for it? This name should be a single word, or maximum two words."

# populate a list with a for loop to create the llm_requests for all clusters, ordered by cluster size.
biggest_clusters_llm_requests <- list()
# populate a list with the formated tibble for all the biggest clusters, ordered by cluster size.
features_tibble <- list()
formatted_features_tibble <- list()

for (i in 1:cluster_nbr_for_interpretation) {
  # features
  features_tibble[[i]] <- features_in_lesson_clusters[[biggest_clusters[i]]]
  # creating a formatted table from my tibble, so that it appears formatted in the console
  features_tibble[[i]] <- features_tibble[[i]] %>%
    mutate(across(c(chi2, p, n_target, n_reference), as.numeric), # Ensure numeric conversion
           across(c(feature, sign), as.character)) # Ensure character conversion
  formatted_features_tibble[[i]] <- features_tibble[[i]] %>%
    pmap_chr(~ sprintf("Feature: %s, Chi2: %.2f, P-Value: %.2e, Targets: %d, References: %d, Sign: %s",
                       ..1, ..2, ..3, ..4, ..5, ..6))
  biggest_clusters_llm_requests[[i]] <- paste(introductory_text, paste(formatted_features_tibble[[i]], collapse = "\n"), middle_text, lesson_clusters_text[[biggest_clusters[i]]], end_text, sep = "\n\n")
}

# compute-clusters-size-in-segments ----

# Summarize counts for each modality in the current column
modality_counts <- table(docvars(dfm_segm_lemm_part)$lesson_cluster_id)

# Convert the summary table to a data frame
counts_df <- as.data.frame(modality_counts)
colnames(counts_df) <- c("modality", "sample_Size")

# Reorder based on cluster names
counts_df <- counts_df %>% arrange(modality)
# Reorder based on number of segments
# counts_df <- counts_df %>% arrange(desc(sample_Size))

counts_df$modality <- paste0("cluster_", counts_df$modality)

# Create the bar plot using ggplot2
clusters_size_in_segments <- ggplot(counts_df, aes(x = factor(modality, level = modality), y = sample_Size)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sample_Size), vjust = -0.5, color = "black", size = 3) +  # Add labels inside bars
  labs(title = "",
       x = "", y = "Number of segments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Rotate x-axis labels

# remove-NA-values-in-cluster-docvar ----

## in the clustering, it happens that some segments are not classified in any cluster. Their cluster value is then NA. For the next analyses (repartition of clusters over timeslices, but also for the keyness analysis), we need to remove these segments from the dfm and the corpus.

## copy the dfm to a new one to keep the first untouched
dfm_segm_lemm_part_wo_na <- dfm_segm_lemm_part

# if there are NA values in the cluster docvar, we remove the rows with NA values in the dfm and the corresponding documents in the corpus (as it generates an error with textstat_keyness())
if (anyNA(dfm_segm_lemm_part_wo_na$lesson_cluster_id)) {
  
  # Find the rows in docvars(dfm_segm_lemm_part_wo_na) that have a NA value for cluster
  na_rows <- which(is.na(docvars(dfm_segm_lemm_part_wo_na)$lesson_cluster_id))
  
  # loop to remove all rows with NA value for lesson_cluster_id in the corpus
  for (i in na_rows) {
    # find the name of the document with NA value for lesson_cluster_id (example)
    segments_with_NA_cluster_name <- docnames(dfm_segm_lemm_part_wo_na)[i]
  }
  
  # remove the rows with NA value for lesson_cluster_id in the dfm
  dfm_segm_lemm_part_wo_na <- dfm_segm_lemm_part_wo_na[-na_rows, ]
  # remove rows where docvar(lesson_cluster_id) is NA in groups
  groups_for_slices <- groups[-na_rows]
}

# compute-evolution-of-clusters-during-lesson ----

# create a df with time_slice and lesson_cluster_id columns
df_time_slice_cluster <- docvars(dfm_segm_lemm_part_wo_na) %>% select(time_slice, lesson_cluster_id)
rownames(df_time_slice_cluster) <- docnames(dfm_segm_lemm_part_wo_na)

# keep only segments belonging to biggest clusters
df_time_slice_cluster <- df_time_slice_cluster %>% filter(lesson_cluster_id %in% biggest_clusters)

# rename clusters
if (clusters_renamed_yes) {
  biggest_clusters_df <- data.frame(biggest_clusters, biggest_clusters_names)
  colnames(biggest_clusters_df) <- c("lesson_cluster_id", "name")
  df_time_slice_merged <- merge(df_time_slice_cluster, biggest_clusters_df, by = "lesson_cluster_id")
  df_time_slice_merged$lesson_cluster_id <- df_time_slice_merged$name
  df_time_slice_merged$name <- NULL
  df_time_slice_cluster <- df_time_slice_merged
} else {
  df_time_slice_cluster$lesson_cluster_id <- paste0("cluster_", df_time_slice_cluster$lesson_cluster_id)
}

# count the number of segments in each cluster for each time slice
df_time_slice_cluster_grouped <- df_time_slice_cluster %>% count(time_slice, lesson_cluster_id)

# convert count to percentage by time_slice
df_time_slice_cluster_grouped <- df_time_slice_cluster_grouped %>%
  group_by(time_slice) %>%
  mutate(sum_time_slice = sum(n)) %>% 
  group_by(time_slice, lesson_cluster_id) %>% 
  mutate(freq = n/sum_time_slice,
         freq_perc = round(freq*100 %>% round(2)))

## plot-evolution-of-clusters-during-lesson ----

plot_evolution_of_clusters <- ggplot(df_time_slice_cluster_grouped, aes(fill=lesson_cluster_id, y=n, x=time_slice)) + 
  geom_bar(position="fill", stat="identity", width = .98) # + # decide if I want the labels or not
# geom_text(aes(label= paste0(freq_perc, "%")), position=position_fill(vjust=0.5), size=3, colour="black")