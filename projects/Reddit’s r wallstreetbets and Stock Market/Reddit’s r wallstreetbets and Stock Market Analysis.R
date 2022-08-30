# load libraries
pacman::p_load(tidytext, lubridate, janitor, tidyverse, ggplot2, lexicon, tidyquant)

# load retrieved text
wsb_comm_19_21 <- read.csv('wsb_hot_comments_19_21.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
wsb_sub_19_21 <- read.csv('wsb_hot_subs_19_21.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
glimpse(wsb_sub_19_21)

# 1 Clean wasllstreetbet data ----
## Clean wsb_comm_19_21 ----

## drop duplicates and keep the first row
wsb_comm_cleaned <- wsb_comm_19_21 %>%
  ## Remove unwanted author
  ## Remove unwanted comments
  filter(!author %in% c('AutoModerator') &
           !body %in% c('[removed]', '[deleted]', 'Eat my dongus you fuckin nerd.\n\n*I am a bot, and this action was performed automatically. Please [contact the moderators of this subreddit](/message/compose/?to=/r/wallstreetbets) if you have any questions or concerns.*',
                        'Ban')) %>%
  distinct(author, body, created_utc, .keep_all = TRUE)

## Clean wsb_sub_19_21 ----
wsb_sub_cleaned <- wsb_sub_19_21 %>% 
  ## Remove unwanted author
  ## Remove unwanted submissions 
  filter(!author %in% c('AutoModerator') &
           !selftext %in% c('[removed]', '[deleted]') &
           !removed_by_category %in% c('moderator','deleted', 'automod_filtered', 'anti_evil_ops', 'reddit')) %>% 
  ## drop duplicates and keep the first row
  distinct(author, selftext, title, created_utc, .keep_all = TRUE)

## Subset data ----
## Subset wsb_comm_cleaned
wsb_comm_small <- wsb_comm_cleaned %>% 
  ## retain only the columns that will be used
  select(id, body, created_utc, created) %>% 
  rename(text = body)

## Subset wsb_sub_cleaned
wsb_sub_small <- wsb_sub_cleaned %>% 
  ## put title and selftext into the same column
  pivot_longer(cols = c(selftext, title), names_to = NULL, values_to = 'text') %>% 
  ## retain only the columns that will be used
  select(id, text, created_utc, created) %>% 
  ## filter out empty text
  filter(text != '')

## Join wsb_sub_small and wsb_comm_small ----
wsb_text <- rbind(wsb_comm_small, wsb_sub_small)
## Arrange data by date of the text created
wsb_text <- wsb_text %>% 
  mutate(created = ymd(created),
         created_utc = ymd_hms(created_utc)) %>% 
  ## drop duplicates and keep the first row
  distinct(id, text, created_utc, .keep_all = TRUE) %>% 
  arrange(created)

# ***************************** ----

# 2 Data Exploration ----
## initial look at top 25 used words
## filtered out stop_words
wsb_text %>% 
  unnest_tokens(input = text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

## plotting
wsb_text%>%
  unnest_tokens(input = text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(50)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

# ***************************** ----

# 3 Sentiment Score ----
## bing sentiment ----
bing_sent <- wsb_text %>%
  select(created,text)%>%
  group_by(created)%>%
  unnest_tokens(output=word,input=text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(created, sentiment)%>%
  summarize(count = n())%>%
  mutate(proportion = (count/sum(count))*100) %>%
  mutate(sentiment_c = 
           case_when(sentiment == 'positive' & proportion > 50 ~ 'positive',
                     sentiment == 'negative' & proportion > 50 ~ 'negative',
                     proportion == 50 ~ 'neutral')) %>%
  drop_na() %>%
  select(!sentiment) %>%
  distinct()

## afinn sentiment score ----
afinn = get_sentiments('afinn')
afinn_sent <- wsb_text %>%
  group_by(created) %>%
  unnest_tokens(input = text, output = word) %>%
  inner_join(afinn) %>%
  summarize(affin_score = mean(value)) %>%
  mutate(affin_sentiment = ifelse(affin_score > 0, 'positive', 'negative'))

## stock lexicon sentiment score ----
### import stock lexicon ----
stock <- read.csv('stock_lex.csv', stringsAsFactors = F, header = T)
colnames(stock) = c('word', 'POS', 'aff_score', 'neg_score')

### calculate sentiment ----
stock_sent <- wsb_text %>%
  group_by(created) %>%
  unnest_tokens(input = text, output = word) %>%
  inner_join(stock) %>%
  summarize(aff_score = mean(aff_score),
            neg_score = mean(neg_score))

### convert into binary sentiments ----
stock_sent <- stock_sent %>%
  mutate(stock_sentiment = if_else(aff_score > neg_score, 'positive', 'negative'))

## merge 3 sentiment scores ----
### merge bing and afinn ----
bi_affin <- merge(bing_sent, afinn_sent, by.x = 'created', by.y = 'created', all = TRUE)
colnames(bi_affin) = c('created', 'count', 'proportion', 'bi_sentiment', 'affin_score', 'affin_sentiment')

## merge stock with two other lexicons ----
bi_af_stock <- merge(bi_affin, stock_sent, by.x = 'created', by.y = 'created', all = T)
dim(bi_af_stock)

## merge three sentiment scores ----
tri_sent <- bi_af_stock %>%
  select(created, bi_sentiment, affin_sentiment, stock_sentiment) %>%
  mutate(bi_sentiment = case_when(bi_sentiment == "positive" ~ 1,
                                  bi_sentiment == "negative" ~ -1,
                                  bi_sentiment == 'neutral' ~ 0),
         affin_sentiment = case_when(affin_sentiment == "positive" ~ 1,
                                     affin_sentiment == "negative" ~ -1),
         stock_sentiment = case_when(stock_sentiment == "positive" ~ 1,
                                     stock_sentiment == "negative" ~ -1)) %>%
  drop_na()

## calculate total score ----
tri_sent$score = rowSums(tri_sent[2:4])

## convert sentiment score to polarized sentiment ----
tri_sent <- tri_sent %>%
  mutate(sentiment = case_when(score < 0 ~ 'negative',
                               score == 0 ~ 'neutral',
                               score > 0 ~ 'positive'))

# ***************************** ----

# 4 Create stock data ----
## Create all dates ----
date <- data.frame(date = full_seq(c(as.Date('2019-01-01'), Sys.Date()),1))

## create clean stock data function ----
clean_stock_data <- function(stock) {
  cleaned <- stock %>% 
    # Code all existing value as open trading day
    mutate(market_trading = 'opened') %>% 
    # Combine stock data to all dates
    full_join(date, by = 'date') %>% 
    # Code NA values as closed trading day
    mutate(market_trading = as.factor(if_else(is.na(market_trading), 'closed', market_trading))) %>% 
    rename(index = symbol) %>%
    mutate(index = 'nasdaq100') %>% 
    arrange(date) %>% 
    # Use previous trading day value to fill NA values in closed trading days
    fill(open, .direction = 'down') %>% 
    fill(high, .direction = 'down') %>%
    fill(low, .direction = 'down') %>%
    fill(close, .direction = 'down') %>%
    fill(volume, .direction = 'down') %>%
    fill(adjusted, .direction ='down') %>% 
    # Compute the change in closing price from previous closing price
    mutate(close_movement = close - lag(close,1)) %>% 
    # Code the movement in closing price into 'up', 'down', or 'flat
    mutate(close_movement_direction = as.factor(case_when(close_movement > 0 ~ 'up',
                                                          close_movement < 0 ~ 'down',
                                                          close_movement == 0 & market_trading == 'opened' ~ 'flat',
                                                          close_movement == 0 & market_trading == 'closed' ~ NA_character_))) %>% 
    # Remove data from 2018-12-28
    filter(date != "2018-12-28") %>% 
    # fill closing price movement direction with previous trading day close price movement
    fill(close_movement_direction, .direction = 'down') %>% 
    # Retain data from 2019-01-01 to date
    filter(date != '2018-12-31')
}

## Create data for NASDAQ100 ----
nasdaq100 <- tq_get('^NDX',
                    from = "2018-12-28",
                    to = Sys.Date(),
                    get = "stock.prices") %>%
  clean_stock_data()

## Create data for S&P500 ----
sp500 <- tq_get('^GSPC',
                from = "2018-12-28",
                to = Sys.Date(),
                get = "stock.prices") %>%
  clean_stock_data()

## Create data for Dow Jones Industrial Average ----
djia <- tq_get('^DJI',
               from = "2018-12-28",
               to = Sys.Date(),
               get = "stock.prices") %>% 
  clean_stock_data()

# ***************************** ----

# 5 Stock & Sentiment Analysis ----
## Sentiment Accuracy Function ----
senti_accuracy <- function(data, lag = 0) {
  df = data
  df <- df %>%
    mutate(lag_sentiment = lag(sentiment, lag)) %>%
    mutate(right = case_when(lag_sentiment == 'negative' & close_movement_direction == 'down' ~ 1,
                             lag_sentiment == 'positive' & close_movement_direction == 'up' ~ 1,
                             TRUE ~ 0))
  accuracy = mean(df$right)
}

## Combine Stock and Sentiment Function ----
combine <- function(stock_df, sentiment = tri_sent) {
  df <- tri_sent %>%
    mutate(date = as.Date(created),
           sentiment = sentiment) %>%
    inner_join(stock_df, by = 'date') %>%
    select(date, sentiment, close_movement_direction)
}


## nasdaq100 & sentiment ----
nas_senti <- combine(nasdaq100)

### nasdaq100 sentiment accuracy & Chi-Squared Test of Independence----
results_nas = matrix(nrow = 31, ncol = 3)
for (i in 0:30) {
  accu = senti_accuracy(data = nas_senti, lag = i)
  p_value = chisq.test(lag(nas_senti$sentiment, n = i), nas_senti$close_movement_direction)$p.value
  results_nas[i+1 , ] = c(i, accu, p_value)
}
results_nas = data.frame(results_nas)
colnames(results_nas) = c("lag_day", "sentiment_and_movement_accuracy", "chisq_p_value")
which.max(results_nas$sentiment_and_movement_accuracy)

## sp500 & sentiment ----
sp500_senti <- combine(sp500)

### sp500 sentiment accuracy & Chi-Squared Test of Independence----
results_sp500 = matrix(nrow = 31, ncol = 3)
for (i in 0:30) {
  accu = senti_accuracy(data = sp500_senti, lag = i)
  p_value = chisq.test(lag(sp500_senti$sentiment, n = i), sp500_senti$close_movement_direction)$p.value
  results_sp500[i+1 , ] = c(i, accu, p_value)
}

results_sp500 = data.frame(results_sp500)
colnames(results_sp500) = c("lag_day", "sentiment_and_movement_accuracy", "chisq_p_value")
which.max(results_sp500$sentiment_and_movement_accuracy)


## djia & sentiment ----
djia_senti <- combine(djia)

### djia sentiment accuracy & Chi-Squared Test of Independence----
results_djia = matrix(nrow = 31, ncol = 3)
for (i in 0:30) {
  accu = senti_accuracy(data = djia_senti, lag = i)
  p_value = chisq.test(lag(djia_senti$sentiment, n = i), djia_senti$close_movement_direction)$p.value
  results_djia[i+1 , ] = c(i, accu, p_value)
}

results_djia = data.frame(results_djia)
colnames(results_djia) = c("lag_day", "sentiment_and_movement_accuracy", "chisq_p_value")
which.max(results_djia$sentiment_and_movement_accuracy)

# ***************************** ----
# 6 Plot Results ----
## create function to plot charts ----
plot_results <- function(results) {
  results %>% 
    pivot_longer(cols = 2:3) %>% 
    ggplot(aes(x = lag_day, y = value)) +
    geom_col(position = 'dodge') +
    geom_hline(data = h_line, aes(yintercept = hline), color = 'blue') +
    facet_grid(name~., scales = 'free') +
    scale_x_continuous(breaks = seq(0,30, by = 1))
}

# create horizontal line to represent 50% accuracy and 0.05 p-value
h_line <- data.frame(name = c('sentiment_and_movement_accuracy','chisq_p_value'),
                     hline = c(0.5, 0.05))
# plot NASDAQ 100
plot_results(results_nas) +
  annotate('label', x = 9, y = 0.4, label = 'lag: 9 days\nsentiment-stock-movement-accuracy: 51%\np-value: 0.061', size = 4) +
  annotate('label', x = 16, y = 0.2, label = 'lag: 16 days\nsentiment-stock-movement-accuracy: 51%\n p-value: 0.046', size = 4) +
  ggtitle("r/wallstreetbets' sentiment relationship with NASDAQ 100")

# plot S&P 500
plot_results(results_sp500) +
  annotate('label', x = 9, y = 0.4, label = 'lag: 9 days\nsentiment-stock-movement-accuracy: 50%\n p-value: 0.222', size = 4)+
  annotate('label', x = 26, y = 0.3, label = 'lag: 26 days\nsentiment-stock-movement-accuracy: 42%\n p-value: 0.018', size = 4)+
  ggtitle("r/wallstreetbets' sentiment relationship with S&P 500")

# plot DJIA
plot_results(results_djia) +
  annotate('label', x = 18, y = 0.4, label = 'lag: 18 days\nsentiment-stock-movement-accuracy: 50%\np-value: 0.099', size = 4)+
  ggtitle("r/wallstreetbets' sentiment relationship with Dow Jones Industrial Average")
