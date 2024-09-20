# splitting-lemmatisation-tokenisation-data-feature-matrix-creation ----

## Splitting in segments (with Rainette)
corpus_segm_unlemm_full <- split_segments(corpus_lesson_disc, segment_size = 40)

## Process to create a lemmatized corpus ----

# instructions to create a python virtual environment for spacy
# not needed to run once the environment is created --- keeping it here for archive if needed

# choose the version of python you want
# version <- "3.11.7"

# do a python install with this version
# the function returns the path to the Python executable file
# python_exe <- reticulate::install_python(version)

# create a virtual environment with this python install
# reticulate::virtualenv_create("venvforspacy", python = python_exe)

# install spaCy in it
# unfortunately, using this function spacy_install(), I wasn’t able to specify the environment in which I wanted to install spaCy… so the function didn’t install spaCy in the environment I created just before, it created a new environment (!) with the default name "r-spacyr" and installed spaCy into it… good things: this environment took the python version I decided for the first environment (here 3.11.7); spaCy could be installed with the option [apple] that makes it use the capacities of the GPU of the Apple Silicon chip through Metal Performance Shaders; the language model is the one I wanted.
# spacy_install(version = "apple", lang_models = "fr_dep_news_trf", ask = interactive())

# Lancement de la lemmatisation avec le bon modèle 

## new option with a python virtual environment venv, working
spacy_initialize(model = "fr_dep_news_trf", virtualenv = "r-spacyr")

## Création d’une data.table avec le résultat de la lemmatisation et division en mots du corpus
data_table_lemmatisee <- spacy_parse(corpus_segm_unlemm_full, pos = TRUE, tag = FALSE, lemma = TRUE, entity = TRUE, dependency = TRUE, nounphrase = FALSE, multithread = TRUE)

## Terminer l’environnement spaCy
spacy_finalize()

## correct some wrong pos in the lemmatised data

# In data_table_lemmatisee, replace the POS for the lemma "voilà" from "VERB" to "INTJ"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "voilà", "INTJ", pos))

# In data_table_lemmatisee, replace the POS for the lemma "ok" with "ADV"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "ok", "ADV", pos))
# same for "OK"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "OK", "ADV", pos))

# In data_table_lemmatisee, replace the POS for the lemma "ha" with "INTJ"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "ha", "INTJ", pos))
# same for "Ha"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "Ha", "INTJ", pos))
# same for "hm"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "hm", "INTJ", pos))
# same for "hein"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "hein", "INTJ", pos))
# same for "Hein"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "Hein", "INTJ", pos))

# In data_table_lemmatisee, replace the lemma for the token "plait" with "plaire"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "plait", "plaire", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma for the token "plante" with "planter"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "plante" & pos == "VERB", "planter", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma for the token "tourne" with "tourner"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "tourne" & pos == "VERB", "tourner", lemma))
# same for "Tourne"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "Tourne" & pos == "VERB", "tourner", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma for the token "ferme" with "fermer"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "ferme" & pos == "VERB", "fermer", lemma))
# same for "Ferme"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "Ferme" & pos == "VERB", "fermer", lemma))

# In data_table_lemmatisee, replace the lemma for the token "répèter" with "répéter"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "répèter", "répéter", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma "marche" with "marcher"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "marche" & pos == "VERB", "marcher", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma "donne" with "donner"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(token == "donne" & pos == "VERB", "donner", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma "retourne" with "retourner"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "retourne" & pos == "VERB", "retourner", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma "monte" with "monter"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "monte" & pos == "VERB", "monter", lemma))

# In data_table_lemmatisee, when the POS is "VERB", replace the lemma "cherche" with "chercher"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "cherche" & pos == "VERB", "chercher", lemma))
# same with "montre" and "montrer"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "montre" & pos == "VERB", "montrer", lemma))
# same with "joue" and "jouer"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "joue" & pos == "VERB", "jouer", lemma))
# same with "laisse" and "laisser"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(lemma = ifelse(lemma == "laisse" & pos == "VERB", "laisser", lemma))

# In data_table_lemmatisee, replace the POS for the token "-là" with "ADV"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(token == "-là", "ADV", pos))

# In data_table_lemmatisee, replace the POS for the lemma "oup" with "INTJ"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "oup", "INTJ", pos))

# In data_table_lemmatisee, replace the POS for the lemma "merci" with "INTJ"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "merci", "INTJ", pos))

# In data_table_lemmatisee, replace the POS for the lemma "ch" with "SYM"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "ch", "SYM", pos))

# In data_table_lemmatisee, replace the POS for the lemma "2A" with "SYM"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "2A", "SYM", pos))
# same for "2a"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "2a", "SYM", pos))
# same for "2A."
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "2A.", "SYM", pos))
# same for "2a."
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "2a.", "SYM", pos))

# In data_table_lemmatisee, replace the POS for the lemma "1A" with "SYM"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "1A", "SYM", pos))
# same for "1a"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "1a", "SYM", pos))
# same for "1A."
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "1A.", "SYM", pos))
# same for "1a."
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "1a.", "SYM", pos))
# same for "b."
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "b.", "SYM", pos))

# In data_table_lemmatisee, replace the POS for the lemma "nom_d_un_e_élève" with "PROPN"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "nom_d_un_e_élève", "PROPN", pos))

# In data_table_lemmatisee, replace the POS for the lemma "ci" with "ADV"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "ci", "ADV", pos))

# In data_table_lemmatisee, replace the POS for the lemma "cætera" with "CCONJ"
data_table_lemmatisee <- data_table_lemmatisee %>%
  mutate(pos = ifelse(lemma == "cætera", "CCONJ", pos))

## tokenisation ----

## create a tokens object unlemmatised with POS for export
tok_segm_unlemm_w_pos_full <- as.tokens(
  data_table_lemmatisee,
  concatenator = "/",
  include_pos = c("pos"),
  use_lemma = FALSE
)

## create a tokens object lemmatised with POS for export
tok_segm_lemm_w_pos_full <- as.tokens(
  data_table_lemmatisee,
  concatenator = "/",
  include_pos = c("pos"),
  use_lemma = TRUE
)

## create a tokens object keeping from the preceeding one only the nouns, adjectives and verbs (makes it partial)
tok_segm_lemm_part <- tok_segm_lemm_w_pos_full |>
  tokens_select(
    c("/NOUN$", "/AJD$", "/VERB$"),
    valuetype = "regex"
  ) |>
  tokens_split("/") |>
  tokens_remove(
    c("NOUN", "ADJ", "VERB"),
    valuetype = "fixed",
    case_insensitive = FALSE
  )

## option to take all the lemmatised tokens without POS
## create a tokens object for the lesson analysis (lemmatised without POS)
# tok_segm_lemm_part <- as.tokens(
#   data_table_lemmatisee,
#   concatenator = "/",
#   include_pos = c("none"),
#   use_lemma = TRUE
# )

# for the record, display the list of tokens for one document in the tokens object
# as.list(tok_segm_lemm_part)[1]

# splitter aux apostrophes, aux parenthèses, aux slash et aux tirets
# tok_segm_lemm_part <- tokens_split(tok_segm_lemm_part,"'")
# tok_segm_lemm_part <- tokens_split(tok_segm_lemm_part,"(")
# tok_segm_lemm_part <- tokens_split(tok_segm_lemm_part,"/")
# tok_segm_lemm_part <- tokens_split(tok_segm_lemm_part,"-")
# tok_segm_lemm_part <- tokens_split(tok_segm_lemm_part,"’")

# tokenize and remove punctuation, symbols, numbers and urls
tok_segm_lemm_part <- tokens(tok_segm_lemm_part, remove_punct = TRUE, remove_numbers = TRUE)

# create docvars for the full corpus ---

## add variables from classes as docvars
docvars(corpus_segm_unlemm_full, "class_id") <- this_class_id
docvars(corpus_segm_unlemm_full, "pupils_number") <- pupils_number
docvars(corpus_segm_unlemm_full, "school_level") <- school_level

# add variables from lessons as docvars
docvars(corpus_segm_unlemm_full, "lesson_id") <- this_lesson_id
docvars(corpus_segm_unlemm_full, "lesson_topic") <- lesson_topic
docvars(corpus_segm_unlemm_full, "programming_type") <- programming_type
docvars(corpus_segm_unlemm_full, "number_of_teachers") <- number_of_teachers

# add variables from discourses as docvars
docvars(corpus_segm_unlemm_full, "discourse_id") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, first_discourse_id, NA)
docvars(corpus_segm_unlemm_full, "discourse_id") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, second_discourse_id, corpus_segm_unlemm_full$discourse_id)

docvars(corpus_segm_unlemm_full, "nth_time_teacher_taught_lesson") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, nth_time_first_teacher_taught_lesson, NA)
docvars(corpus_segm_unlemm_full, "nth_time_teacher_taught_lesson") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, nth_time_second_teacher_taught_lesson, corpus_segm_unlemm_full$nth_time_teacher_taught_lesson)

## add variables from teachers as docvars ----

# role_in_project
docvars(corpus_segm_unlemm_full, "role_in_project") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(role_in_project) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "role_in_project") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(role_in_project) %>% as.character()), corpus_segm_unlemm_full$role_in_project)

# gender
docvars(corpus_segm_unlemm_full, "gender") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(gender) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "gender") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(gender) %>% as.character()), corpus_segm_unlemm_full$gender)

# age_range
docvars(corpus_segm_unlemm_full, "age_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(age_range) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "age_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(age_range) %>% as.character()), corpus_segm_unlemm_full$age_range)

# teaching_level
docvars(corpus_segm_unlemm_full, "teaching_level") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(teaching_level) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "teaching_level") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(teaching_level) %>% as.character()), corpus_segm_unlemm_full$teaching_level)

# number_of_disciplines
docvars(corpus_segm_unlemm_full, "number_of_disciplines") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(number_of_disciplines) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "number_of_disciplines") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(number_of_disciplines) %>% as.character()), corpus_segm_unlemm_full$number_of_disciplines)

# disciplines
docvars(corpus_segm_unlemm_full, "disciplines") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(disciplines) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "disciplines") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(disciplines) %>% as.character()), corpus_segm_unlemm_full$disciplines)

# teaching_experience
docvars(corpus_segm_unlemm_full, "teaching_experience") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(teaching_experience) %>% as.numeric()), NA)
docvars(corpus_segm_unlemm_full, "teaching_experience") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(teaching_experience) %>% as.numeric()), corpus_segm_unlemm_full$teaching_experience)

# teaching_experience_range
docvars(corpus_segm_unlemm_full, "teaching_experience_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(teaching_experience_range) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "teaching_experience_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(teaching_experience_range) %>% as.character()), corpus_segm_unlemm_full$teaching_experience_range)

# cs_teaching_experience
docvars(corpus_segm_unlemm_full, "cs_teaching_experience") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(cs_teaching_experience) %>% as.numeric()), NA)
docvars(corpus_segm_unlemm_full, "cs_teaching_experience") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(cs_teaching_experience) %>% as.numeric()), corpus_segm_unlemm_full$cs_teaching_experience)

# cs_teaching_experience_range
docvars(corpus_segm_unlemm_full, "cs_teaching_experience_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(cs_teaching_experience_range) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "cs_teaching_experience_range") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(cs_teaching_experience_range) %>% as.character()), corpus_segm_unlemm_full$cs_teaching_experience_range)

# teacher_degree
docvars(corpus_segm_unlemm_full, "teacher_degree") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(teacher_degree) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "teacher_degree") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(teacher_degree) %>% as.character()), corpus_segm_unlemm_full$teacher_degree)

# pre_or_in_service
docvars(corpus_segm_unlemm_full, "pre_or_in_service") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(pre_or_in_service) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "pre_or_in_service") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(pre_or_in_service) %>% as.character()), corpus_segm_unlemm_full$pre_or_in_service)

# teacher_education_type
docvars(corpus_segm_unlemm_full, "teacher_education_type") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(teacher_education_type) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "teacher_education_type") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(teacher_education_type) %>% as.character()), corpus_segm_unlemm_full$teacher_education_type)

# cs_education_institution
docvars(corpus_segm_unlemm_full, "cs_education_institution") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(cs_education_institution) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "cs_education_institution") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(cs_education_institution) %>% as.character()), corpus_segm_unlemm_full$cs_education_institution)

# cs_education_type
docvars(corpus_segm_unlemm_full, "cs_education_type") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(cs_education_type) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "cs_education_type") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(cs_education_type) %>% as.character()), corpus_segm_unlemm_full$cs_education_type)

# highest_diploma
docvars(corpus_segm_unlemm_full, "highest_diploma") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(highest_diploma) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "highest_diploma") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(highest_diploma) %>% as.character()), corpus_segm_unlemm_full$highest_diploma)

# TPACK_score_didactical_competencies
docvars(corpus_segm_unlemm_full, "TPACK_score_didactical_competencies") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(TPACK_score_didactical_competencies) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "TPACK_score_didactical_competencies") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(TPACK_score_didactical_competencies) %>% as.character()), corpus_segm_unlemm_full$TPACK_score_didactical_competencies)

# TPACK_score_content_and_technology_foundations
docvars(corpus_segm_unlemm_full, "TPACK_score_content_and_technology_foundations") <- ifelse(corpus_segm_unlemm_full$teacher_id == first_teacher, (teachers %>% filter(teacher_id == first_teacher) %>% select(TPACK_score_content_and_technology_foundations) %>% as.character()), NA)
docvars(corpus_segm_unlemm_full, "TPACK_score_content_and_technology_foundations") <- ifelse(corpus_segm_unlemm_full$teacher_id == second_teacher, (teachers %>% filter(teacher_id == second_teacher) %>% select(TPACK_score_content_and_technology_foundations) %>% as.character()), corpus_segm_unlemm_full$TPACK_score_content_and_technology_foundations)

# make docnames unique for further export
docnames(corpus_segm_unlemm_full) <- docnames(corpus_segm_unlemm_full) %>% paste0(this_lesson_id, "_", .)

# make segment_source unique for further export
docvars(corpus_segm_unlemm_full)$segment_source <- docvars(corpus_segm_unlemm_full)$segment_source %>% paste0(this_lesson_id, "_", .)

# copy docvars to all tokens and corpus objects for export ----

# copy docvars from corpus_segm_unlemm_full to tok_segm_unlemm_w_pos_full
docvars(tok_segm_unlemm_w_pos_full) <- docvars(corpus_segm_unlemm_full)

# make docnames unique for further export
docnames(tok_segm_unlemm_w_pos_full) <- docnames(tok_segm_unlemm_w_pos_full) %>% paste0(this_lesson_id, "_", .)

# copy docvars from corpus_segm_unlemm_full to tok_segm_lemm_w_pos_full
docvars(tok_segm_lemm_w_pos_full) <- docvars(corpus_segm_unlemm_full)

# make docnames unique for further export
docnames(tok_segm_lemm_w_pos_full) <- docnames(tok_segm_lemm_w_pos_full) %>% paste0(this_lesson_id, "_", .)

# copy docvars from corpus_segm_unlemm_full to tok_segm_lemm_part
docvars(tok_segm_lemm_part) <- docvars(corpus_segm_unlemm_full)

# make docnames unique for further export
docnames(tok_segm_lemm_part) <- docnames(tok_segm_lemm_part) %>% paste0(this_lesson_id, "_", .)

# create data-feature matrix ----

# create a dfm with the lemmatised and partial tokenisation
dfm_segm_lemm_part <- dfm(tok_segm_lemm_part)

# for the record, display the full list of features in the dfm
# dfm_segm_lemm_part@Dimnames[["features"]]