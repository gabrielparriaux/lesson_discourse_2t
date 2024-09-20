# Teacher's classroom discourse analysis

This project presents the R code used to prepare and analyse data coming from a lesson recording.

The data is composed of the transcript of two teachers’ discourses during an informatics lesson, with some comments.

It is part of a PhD project in educational sciences.

This file presents the R code for the analysis of a lesson where two teachers are working together and talking to the class, the pupils, or together.

For data protection reasons, the data is not available in this repository. We only provide R code to give an idea of the process setup to analyse the data.

Apart from the RStudio project file itself (`lesson_discourse_2t.Rproj`), the main file is a Quarto document: `class_id_lesson_id.qmd`. 

From this file, and as in our context we use this project multiple times to analyse different lessons, we centralise most of the code and import several .R files.

- `01_libraries.R` imports all the packages needed in the project
- `01bis_import_initial_variables.R` imports the variables provided by another analysis (dataframes called `classes`, `lessons`, `discourses`, `teachers` that centralise all the variables related to the classes, the lessons, the discourses and the teachers
- `02_import.R` imports transcripts and comments as .csv files coming from the Trint platform
- `03_first_teacher_discourse_tables` cleans the data for the first teacher
- `04_end_of_intro.R` cleans the data for the second teacher, merges the discourses of the two teachers, creates the main Quanteda `corpus` and `tokens`objects and computes textual statistics
- `05_splitting_lemmatisation_tokenisation_dfm_creation.R` splits the corpus into segments for the clustering, lemmatises with Spacy, adds as `docvars()` all the variables to the corpus and creates a data-feature matrix (Quanteda `dfm` object)
- `06_plot_frequencies.R` plots the most frequent words in the lexicon for both teachers
- `07_reinert_clustering.R` computes the clustering with `rainette` package, identifies the biggest clusters and plots the evolution of clusters over time
- `08_alt_creation_and_ca.R` creates an aggregated lexical table (ALT), computes a Correspondance Analysis (CA) using `CA()` from `FactoMineR` package, plots it and prepares tables showing the elements most related to the first two axes of the CA
- `09_export.R` exports the Quanteda `corpus` and `tokens` objects for the global analysis grouping all the lessons, as well as all the variables computed as dataframes