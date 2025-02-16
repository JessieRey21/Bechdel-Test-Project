install.packages("dplyr")
library(dplyr)
install.packages("skimr")
library(skimr)
install.packages("readr")
library(readr)
install.packages("GGally")
library(GGally)
install.packages("janitor")
library(janitor)
install.packages("ggplot")
library(ggplot)
install.packages("forcats")
library(forcats)


################# Importing data sets #################
bechdel_test_full <- read.csv("bechdeltest.csv")
  ## set from class
top_250 <- read.csv("IMDb_Top_250_Movies.csv")
  ## top 250 movies, does not include IMBD ID
movie_genres <- read.csv("movies_initial.csv")
  ## over 46,000 movies including movies genres - does have ID




################# Joining #################
  ## join 2 data sets first
bechdel_join <- movie_genres %>%
  left_join(bechdel_test_full, by = c("imdbID" = "imdbid"))
  ## went from 10,000 movies to 8,000 with variables that we want

skim(bechdel_join)
  ## no missing values

  ## Join to top_250 df
top_movies <- bechdel_join %>%
  right_join(top_250, by = c("title.x" = "Name"))

skim(top_movies)

################# Looking for duplicates #################
dup_movies <- top_movies %>%
  count(title.x, year.x, director) %>%
  filter(n>1)

  ## look at duplicate entries
top_movies %>%
  filter(title.x %in% dup_movies$title.x,
         year.x %in% dup_movies$year.x,
         director %in% dup_movies$director) %>%
  arrange(title.x)

  ## keep only distinct movies by year and director
drop_dups <- top_movies %>%
  arrange(title.x, year.x) %>%
  distinct(title.x, year.x, director, .keep_all = TRUE)

skim(drop_dups)

################# Cleaning names #################
clean_df <- drop_dups %>%
  rename(title = title.x,
         year = year.x,
         rating = rating.x,
         bechdel_score = rating.y)

skim(clean_df)
## 67 observations don't have a score for bechdel test - drop these

### Dropping NAs
clean_df <- clean_df %>%
  drop_na(bechdel_score)



################# Separate wider variables #################
## Genre
clean_df <- clean_df %>%
  separate_wider_delim(genre, 
                       names = "genre", 
                       delim = ",", too_many = "drop")
  ## dropping multiple genres, only using the main genre


## Director
clean_df <- clean_df %>%
  separate_wider_delim(director, 
                       names = "director", 
                       delim = ",", too_many = "drop")
  ## only looking at the main director
skim(clean_df)


### Cutting down variables because data set is large
cut_df <- clean_df %>%
  select(imdbID, title, year, rating, runtime, genre, director,
         writer,imdbRating, country, bechdel_score, Stars)

################# Converting characters to factors #################
cut_df <- cut_df %>%
  mutate(rating = as.factor(rating)) %>%
  mutate(genre = as.factor(genre)) %>%
  mutate(bechdel_score = as.factor(bechdel_score))


### Converting charcter to numeric
cut_df <- cut_df %>%
  separate_wider_delim(
    runtime,
    delim = " ",
    names = c("length", "measure"),
    too_few = "align_start"
  )


### Cleaning runtime variable
  ## getting rid of measure variable and making length numeric
cut_df <- cut_df %>%
  select(imdbID, title, year, rating, length, genre, director,
         writer,imdbRating, country, bechdel_score, Stars) %>%
  mutate(length = as.numeric(length))

skim(cut_df)

################# Separating stars variable #################
cut_df <- cut_df %>%
  separate_wider_delim(
    Stars,
    delim = ", ", #Dr. Bray modified this-- you needed a space after , 
    #was causing downstream problems
    names = c("star_1", "star_2", "star_3") 
  )



################# Separating director variable #################
cut_df <- cut_df %>%
  separate_wider_delim(
    director,
    delim = " ",
    names = c("d_first_name", "d_last_name"),
    too_many = "merge"
  )

unique((cut_df$first_name))
## all are men 


  ## separating each star into first and last name to categorize by sex 
cut_df <- cut_df %>%
  separate_wider_delim(
    star_1,
    delim = " ",
    names = c("s1_first_name", "s1_last_name"),
    too_many = "merge"
  )

cut_df <- cut_df %>%
  separate_wider_delim(
    star_2,
    delim = " ",
    names = c("s2_first_name", "s2_last_name"),
    too_many = "merge",
    too_few = "align_start")

cut_df <- cut_df %>%
  separate_wider_delim(
    star_3,
    delim = " ",
    names = c("s3_first_name", "s3_last_name"),
    too_many = "merge",
    too_few = "align_start")



##Try another way to get the s2_first_name
#just going to get rid of non-ASCII charcters
cut_df$s2_first_name <- iconv(cut_df$s2_first_name, to = "ASCII//TRANSLIT", sub = "")
cut_df$s2_first_name <- sub(" .*", "", cut_df$s2_first_name) #this is working now

cut_df$s1_first_name <- iconv(cut_df$s1_first_name, to = "ASCII//TRANSLIT", sub = "")
cut_df$s1_first_name <- sub(" .*", "", cut_df$s1_first_name) #this is working now

cut_df$s3_first_name <- iconv(cut_df$s3_first_name, to = "ASCII//TRANSLIT", sub = "")
cut_df$s3_first_name <- sub(" .*", "", cut_df$s3_first_name) #this is working now


unique((cut_df$s1_first_name))

################# Converting stars to sex #################
## Star_1
cut_df <- cut_df %>%
  mutate(star1_sex = if_else(s1_first_name %in% c("Amy", "Bette", "Bibi", "Brie", "Brigitte", "Carole",
        "Chieko", "Ellen", "Faye", "Ivana", "Jodie", "Judy", "Julie", "Maria", "Marilyn", "Myrna",
        "Sigourney", "Uma"), "female", "male")) 

## Star_2
cut_df <- cut_df %>%
  mutate(star2_sex = if_else(s2_first_name == "Jean" & s2_last_name == "Arthur", "female", 
                             s2_first_name == "Jean" & s2_last_name == "Martin", "male")) %>%
  mutate(star2_sex = if_else(s2_first_name %in% c("Alexandra", "Andie", "Ann", "Annette", "Ariadna",
        "Ayano", "Barbara", "Bee", "Bibi", "Carrie-Anne", "Cathy", "Chieko", "Claudette", "Diane",
        "Donna", "Edna", "Ellen", "Emily", "Emma", "Eva", "Faye", "Frances", "Gloria", "Grace",
        "Haley", "Ingrid", "Jane", "Janet", "Joan", "Jodie", "Julie", "Karen", "Karuna", "Kate",
        "Kathryn", "Kim", "Laura", "Leila", "Linda", "Liv", "Louise", "Machiko", "Marion",
        "Marisa", "Marlene", "Martina", "Mary", "Michelle", "M�lissa", "Natalie", "Nicoletta", "Olga",
        "Paulette", "Robin", "Rosamund", "Sharon", "Shelley", "Shirley", "Sophie", "Suzanne","Talia","Uma",
        "Virginia", "Vivien", "Yuriko"), "female", "male"))


## Star_3
cut_df <- cut_df %>%
  mutate(star3_sex = if_else(s3_first_name %in% c("Akemi", "Alida", "Alison", "Andrea", "Anne",
        "Annika", "Antonella", "Bahare", "Barbara", "Beverly", "Bonnie", "Carrie", "Carrie-Anne",
        "Catherine","Claire", "Claudia", "Connie", "Debbie", "Dorothy", "Eleanor", "Elizabeth",
        "Elizabeth", "Holly", "Inge", "Ingrid", "Jackie", "Jennifer", "Jessica", "Joan", "Julianne",
        "Kang", "Keiko", "Laura", "Lea", "Lianella", "Linda", "Margaretha", "Margot", "Mary", "Melissa",
        "Mona", "M�nica", "Octavia", "Olivia", "Rachel", "Scarlett", "Shima", "Vera", "Viola", "Y�ko"),
        "female", "male"))


################# Proportions of male/female stars #################
skim(cut_df)

## changing sex into numeric, female = 1, male = 0
cut_df <- cut_df %>%
  mutate(star1_sex = if_else(star1_sex == "female", 1, 0))

cut_df <- cut_df %>%
  mutate(star2_sex = if_else(star2_sex == "female", 1, 0))

cut_df <- cut_df %>%
  mutate(star3_sex = if_else(star3_sex == "female", 1, 0))

skim(cut_df)

cut_df <- cut_df %>%
  mutate(female_count = star1_sex + star2_sex + star3_sex)

cut_df <- cut_df %>%
  mutate(female_prop = female_count / 3)


################# Sex of Writer #################
#Hey! This actually works!!
cut_df <- cut_df %>%
  mutate(screenplay = sapply(str_extract_all(writer, "\\b[^,]+?\\s\\(screenplay\\)"),
                             function(x) str_c(x, collapse = ", ")))
  ## Chat GPT help


## cutting down screenplay
cut_df <- cut_df %>%
  separate_wider_delim(screenplay, 
                       names = "screenplay_writer", 
                       delim = ",", 
                       too_many = "drop", 
                       too_few = "align_start")

## If no input for screenplay_writer, fill in with NA
cut_df <- cut_df %>%
  mutate(screenplay_writer = replace(screenplay_writer, screenplay_writer == "", NA))
  ## used chatGPT

## cut down writer into only 1
cut_df <- cut_df %>%
  separate_wider_delim(writer,
                      names = "writer",
                      delim = ",",
                      too_many = "drop")

## If screenplay is not provided, using the name given in writer
cut_df <- cut_df %>%
  mutate(screen_writer = coalesce(screenplay_writer, writer))
  ##chatGPT

## getting rid of (screenplay)
cut_df <- cut_df %>%
  separate_wider_delim(screen_writer,
                       names = "screen_writer",
                       delim = "(",
                       too_many = "drop")


## Separate into first and last
cut_df <- cut_df %>%
  separate_wider_delim(screen_writer,
                       names = c("writer_first", "writer_last"),
                      delim = " ",
                      too_many = "merge")

cut_df <- cut_df %>%
  mutate(writer_sex = if_else(writer_first %in% c("Agatha", "Daphne", "Emma",
                      "Fran", "Frances", "Gillian", "Irene", "Laeta", "Leigh", "Meg",
                      "Thea", "Val�rie"), "female", "male"))



################# Cutting down genres #################
unique(final_df$genre)

final_df <- cut_df %>%
  mutate(genre = fct_collapse(genre,
                              Action = "Action",
                              Adventure = "Adventure",
                              Animation = "Animation",
                              Biogrpahy = "Biography",
                              Comedy = "Comedy",
                              Crime = c("Crime", "Film-Noir"),
                              Drama = "Drama",
                              Other = c("Horror", "History", "Mystery", "Sci-Fi", "Western")))



################# Final dataframes #################
## General df
final_df <- final_df %>%
  select(imdbID, title, year, rating, length, genre, imdbRating, bechdel_score, Sl_No, 
         female_prop, writer_sex)
 ## for looking at various scores


## Passing scores df
passing_df <- final_df %>%
  filter(bechdel_score == "3")
  ## for looking just at movies that pass the test 





################# Multivariate Analysis #################
  ## Ordinal Regression

install.packages("foreign")
install.packages("MASS")
install.packages("Hmisc")
install.packages("reshape2")

mult_analysis <- final_df %>%
  select(genre, female_prop, writer_sex, bechdel_score)

head(mult_analysis)

lapply(mult_analysis[, c("bechdel_score", "genre", "female_prop", "writer_sex")], table)
  ## one at a time, table apply, pared, and public

ftable(xtabs(~ genre + female_prop + writer_sex, data = mult_analysis))
  ## three way cross tabs (xtabs) and flatten the table

ggplot(mult_analysis, aes(x = genre, y = bechdel_score)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(female_prop ~ writer_sex, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

summary(mult_analysis$bechdel_score)


m <- polr(bechdel_score ~ genre + female_prop + writer_sex, data = mult_analysis, Hess=TRUE)
summary(m)
## model

(ctable <- coef(summary(m)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m))

confint.default(m)

exp(coef(m))
