## Graphing
skim(final_df)




################# Frequency for Bechdel Scores #################
## Start with counts
ggplot(final_df, aes(x = bechdel_score, fill = bechdel_score)) +
  geom_bar() +
  labs(x = "Bechdel Score",
       y = "Frequency",
       title = "Frequency of Bechdel Scores", 
       fill = "Score")

final_df %>% count(bechdel_score)
# 0 - 44
# 1 - 71
# 2 - 24 
# 3 - 76


## Proportions
bechdel_props <- final_df %>%
  count(bechdel_score) %>%
  mutate(proportion = n / sum(n))
  
ggplot(bechdel_props, aes(x = bechdel_score, y = proportion, fill = bechdel_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Bechdel Score",
       y = "Proportion",
       title = "Proportions of Bechdel Scores", 
       fill = "Score")






################# Scores by Year #################
## Count
skim(final_df)

## Cut years into categories by every 20 years
year_df <- final_df %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = cut(year,
                          breaks = c(1920, 1940, 1960, 1980, 2000, 2020),
                          labels = c("1920-1940", "1940-1960", "1960-1980", "1980-2000", "2000-2020")))
  
ggplot(year_df, aes(x = bechdel_score, fill = year)) +
  geom_bar(position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Count",
       title = "Bechdel Score by Year", 
       fill = "Years")


## Proportions Instead
year_props <- year_df %>%
  count(year, bechdel_score) %>% ## looking at bechdel score and year
  group_by(bechdel_score) %>% ## group by year because we want to look at proportions by year
  mutate(proportion = n / sum(n)) %>%
  ungroup()


ggplot(year_props, aes(x = bechdel_score, y = proportion, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Proportions",
       title = "Proportions of Bechdel Scores by Year", 
       fill = "Years")


## Looking at pass/fail by year instead
success_df <- year_df %>%
  mutate(success = if_else(bechdel_score %in% c("0","1","2"), "fail", "pass"))

ggplot(success_df, aes(x = success, fill = year)) +
  geom_bar(position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Count",
       title = "Frequency of Bechdel Scores Success by Year", 
       fill = "Years")


success_props <- success_df %>%
  count(year, success) %>% ## looking at bechdel score and year
  group_by(success) %>% ## group by year because we want to look at proportions by year
  mutate(proportion = n / sum(n)) %>%
  ungroup()


ggplot(success_props, aes(x = success, y = proportion, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Proportions",
       title = "Proportions of Bechdel Scores Success by Year", 
       fill = "Years")







################# Scores by Rating ################# 
  ## Ended up not analyzing this - not significant
ggplot(final_df, aes(x = bechdel_score, fill = rating)) +
  geom_bar(position = "dodge") +
  labs(x = "rating",
       y = "bechdel score",
       title = "Bechdel Score by Rating",
       subtitle = "subtitle")

ggplot(passing_df, aes(x = bechdel_score, fill = rating)) +
  geom_bar(position = "dodge") +
  labs(x = "rating",
       y = "bechdel score",
       title = "Bechdel Score by Rating",
       subtitle = "subtitle")







################# Score by Movie Length #################
## Chose not to analyze - not significant
ggplot(final_df, aes(x = bechdel_score, y = length, fill = bechdel_score)) +
  geom_boxplot() +
  labs(x = "Rating",
       y = "Movie Length (minutes)",
       title = "Bechdel Score vs. Movie Length", 
       fill = "Bechdel Score")







################# IMDb Rating (1-10) #################
## Chose not to analyze - no major differences between scores
ggplot(final_df, aes(x = bechdel_score, y = imdbRating, fill = bechdel_score)) +
  geom_boxplot() +
  labs(x = "Bechdel Score",
       y = "IMDb Rating",
       title = "Bechdel Score vs. IMDb Rating",
       fill = "Bechdel Score")







################# Genre ################# 
ggplot(final_df, aes(x = genre, fill = bechdel_score)) +
  geom_bar(position = "dodge") +
  labs(title = "Bechdel Score by Genre", 
       x = "Genre", 
       y = "Count of Movies", 
       fill = "Bechdel Score") +
  theme_minimal()
## too many similar categories

collapse_df <- final_df %>%
  mutate(genre = fct_collapse(genre,
                              Action = "Action",
                              Adventure = "Adventure",
                              Animation = "Animation",
                              Biogrpahy = "Biography",
                              Comedy = "Comedy",
                              Crime = c("Crime", "Film-Noir"),
                              Drama = "Drama",
                              Other = c("Horror", "History", "Mystery", "Sci-Fi", "Western")))

ggplot(collapse_df, aes(x = genre, fill = bechdel_score)) +
  geom_bar(position = "dodge") +
  labs(title = "Bechdel Score by Genre", 
       x = "Genre", 
       y = "Count of Movies", 
       fill = "Bechdel Score") +
  theme_minimal()
## cleaner with less categories


## Looking at genres of just passing movies
ggplot(passing_df, aes(x = genre)) +
  geom_bar(position = "dodge") +
  labs(title = "Genres of Films that Passed Bechdel Test", 
       x = "Genre", 
       y = "Count of Movies", 
       fill = "Bechdel Score") +
  theme_minimal()

ggplot(final_df, aes(x = bechdel_score, fill = genre)) +
  geom_bar() +
  facet_wrap(~genre) +  #Split the plot by rating
  labs(title = "Movie Counts by Genre and Rating", 
       x = "Scores", 
       y = "Count of Movies",
       fill = "Genre") +
  theme_minimal()
## did not use this in analysis

genre_props <- final_df %>%
  count(genre, bechdel_score) %>% ## looking at bechdel score and genre
  group_by(bechdel_score) %>% #
  mutate(proportion = n / sum(n)) %>%
  ungroup()


ggplot(genre_props, aes(x = bechdel_score, y = proportion, fill = genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Proportion",
       title = "Proportions of Bechdel Scores", 
       fill = "Genre")


ggplot(genre_props, aes(fill = bechdel_score, y = proportion, x = genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genre",
       y = "Proportion",
       title = "Proportions of Bechdel Scores", 
       fill = "Score")





################# Scores by Writer Sex #################
ggplot(final_df, aes(x = bechdel_score, fill = writer_sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Bechdel Score",
       y = "Count of Movies",
       title = "Bechdel Score by Sex of Writer",
       fill = "Sex")

final_df %>% count(writer_sex)
# female - 15
# male - 200





################# Scores by Female Star Proportions #################
final_df <- final_df %>%
  mutate(female_prop = as.factor(female_prop))

ggplot(final_df, aes(fill = bechdel_score, x = female_prop)) +
  geom_bar(position = 'dodge') +
  labs(x = "Proportion of Female Stars",
       y = "Count of Movies",
       title = "Bechdel Score by Proportion of Female Stars",
       fill = "Bechdel Score")
