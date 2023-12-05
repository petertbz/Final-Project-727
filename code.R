library(appler)
library(tidyverse)
library(reshape2)
library(quanteda)
library(topicmodels)
library(tidyverse)
library(knitr)
library(plotly)

# build the list of browers
browser = data.frame(name = c("safari", "edge", "chrome", "firefox", "opera"),
                     fullname = c("Safari", "Microsoft Edge", "Google Chrome",
                                     "Mozilla Firefox","Opera Browser"))

# build a function to acquire the app id from the Apple API
getid = function(name){
  id = search_apple(name, media = "software", entity = "software")[1,]
  trackId = id$trackId
  return(trackId)
}

# extract appid from the Apple API
browser$id = sapply(browser$fullname, getid)

# acquire the reviews of each browser
for (i in 1:dim(browser)[1]) {
  name = browser$name[i]
  review = get_apple_reviews(browser$id[i], country = "us", 
                    all_results = TRUE, sort_by = "mosthelpful")
  assign(name, review)
}

# get ratings of each browser
ratings = data.frame(browser = c("chrome", "edge", "firefox", "safari", "opera"), 
                     rating = c(mean(chrome$rating), mean(edge$rating), mean(firefox$rating), mean(safari$rating), mean(opera$rating)),
                     n_positive = c(sum(chrome$rating >= 3), sum(edge$rating >= 3), sum(firefox$rating >= 3), sum(safari$rating >= 3), sum(opera$rating >= 3)),
                     n_negative = c(sum(chrome$rating < 3), sum(edge$rating < 3), sum(firefox$rating < 3), sum(safari$rating < 3), sum(opera$rating < 3)))


kable(ratings, caption = "ratings")

# Create interactive plot, may not be necessary...static plot or a table should be clear enough
plot_ly(ratings, x = ~browser, y = ~rating, mode = 'markers')

# combine all datasets
chrome = chrome %>% mutate(browser = "chrome")
edge = edge %>% mutate(browser = "edge")
safari = safari %>% mutate(browser = "safari")
opera = opera %>% mutate(browser = "opera")
firefox = firefox %>% mutate(browser = "firefox")

df = rbind(chrome, edge, safari, opera, firefox)
df$review = tolower(df$review) 
df$review = gsub("edge|chrome|safari|firefox|opera", "", df$review, ignore.case = TRUE)

#dfrate = df %>% mutate(good = mutate())

# topicmodels 
corpus_df = corpus(df$review)

# Tokenization
corpus_df_proc = tokens(corpus_df, 
                            remove_punct = TRUE, # remove punctuation
                            remove_numbers = TRUE, # remove numbers
                            remove_symbols = TRUE) %>% # remove symbols (for social media data, could remove everything except letters)
  tokens_tolower() # remove capitalization

# Lemmatization #
lemmaData = read.csv2("baseform_en.tsv", 
                      sep="\t", 
                      header=FALSE, 
                      encoding = "UTF-8", 
                      stringsAsFactors = F) %>% 
  na.omit()

# "Substitute token types based on vectorized one-to-one matching"
corpus_df_proc = tokens_replace(corpus_df_proc, 
                                    lemmaData$V1, 
                                    lemmaData$V2,
                                    valuetype = "fixed") 

# remove stopwords
corpus_df_proc = corpus_df_proc %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(1) 

#  Create dtm
DTM = dfm(corpus_df_proc)

# Minimum
minimumFrequency = 10
DTM = dfm_trim(DTM, 
               min_docfreq = minimumFrequency,
               max_docfreq = 100000)

# keep only letters... brute force
DTM  = dfm_select(DTM, 
                  pattern = "[a-z]", 
                  valuetype = "regex", 
                  selection = 'keep')
colnames(DTM) = stringi::stri_replace_all_regex(colnames(DTM), 
                                                "[^_a-z]","")

DTM = dfm_compress(DTM, "features")

# Drop rows that do not have content left, In Chrome's case, NO

#sel_idx <- rowSums(DTM) > 0
# table(sel_idx) # check if there are blank rows. 
#DTM <- DTM[sel_idx, ]
#textdata <- textdata[sel_idx, ]

K = 15
# Set seed to make results reproducible
set.seed(9161)
topicModel = LDA(DTM, 
                 K, 
                 method="Gibbs", 
                 control=list(iter = 500, 
                              verbose = 25))

tmResult = posterior(topicModel)

# Topics are distributions over the entire vocabulary

beta = tmResult$terms
#glimpse(beta)            

# Each doc has a distribution over k topics

theta = tmResult$topics
#glimpse(theta)               

terms(topicModel, 10)

# Top terms per topic. Use top 5 to interpret topics
top5termsPerTopic = terms(topicModel, 5)

# give the topics more descriptive names than just numbers, concatenate the five most likely terms of each topic to a string that represents a pseudo-name for each topic.
topicNames = apply(top5termsPerTopic, 
                   2, 
                   paste, 
                   collapse=" ")

topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names
sort(topicProportions, decreasing = TRUE) # sort

# What was the value in the previous model?
attr(topicModel, "alpha") 

# Re-estimate model with alpha set by us, keep either topicModel or topicModel2
topicModel2 = LDA(DTM, 
                  K, 
                  method="Gibbs", 
                  control=list(iter = 500, 
                               verbose = 25, 
                               alpha = 0.2))
tmResult = posterior(topicModel2)
theta = tmResult$topics
beta = tmResult$terms


topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names 
sort(topicProportions, decreasing = TRUE) # sort

topicNames = apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # top five terms per topic 

# Topic distributions of example docs
exampleIds = c(2, 100, 200,300,500)
N = length(exampleIds)

topicProportionExamples = as.tibble(theta) %>%
  slice(exampleIds)

colnames(topicProportionExamples) = topicNames

vizDataFrame = melt(cbind(data.frame(topicProportionExamples), 
                          document = factor(1:N)), 
                    variable.name = "topic", 
                    id.vars = "document")  

ggplot(data = vizDataFrame, 
       aes(topic, value, 
           fill = document), 
       ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = N)

# Average theme proportions by decade
topic_proportion_per_decade = aggregate(theta, 
                                        by = list(browser = df$browser), 
                                        mean)
colnames(topic_proportion_per_decade)[2:(K+1)] = topicNames
vizDataFrame = melt(topic_proportion_per_decade, 
                     id.vars = "browser")

if(!require(pals)) install.packages("pals")
library(pals)
ggplot(vizDataFrame, 
       aes(x=browser, 
           y=value, 
           fill=variable)) + 
  geom_bar(stat = "identity")+
  labs(title = "Top 15 Topics for App Store Reviews by Browser", 
       y = "proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics")

###########################

df1 = rbind(chrome, edge, safari, opera, firefox) %>% filter(rating >= 3)
df1$review = tolower(df1$review) 
df1$review = gsub("edge|chrome|safari|firefox|opera", "", df1$review, ignore.case = TRUE)

# topicmodels 
corpus_df1 = corpus(df1$review)

# Tokenization
corpus_df1_proc = tokens(corpus_df1, 
                        remove_punct = TRUE, # remove punctuation
                        remove_numbers = TRUE, # remove numbers
                        remove_symbols = TRUE) %>% # remove symbols (for social media data, could remove everything except letters)
  tokens_tolower() # remove capitalization

# Lemmatization #
lemmaData = read.csv2("baseform_en.tsv", 
                      sep="\t", 
                      header=FALSE, 
                      encoding = "UTF-8", 
                      stringsAsFactors = F) %>% 
  na.omit()

# "Substitute token types based on vectorized one-to-one matching"
corpus_df1_proc = tokens_replace(corpus_df1_proc, 
                                lemmaData$V1, 
                                lemmaData$V2,
                                valuetype = "fixed") 

# remove stopwords
corpus_df1_proc = corpus_df1_proc %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(1) 

#  Create dtm
DTM = dfm(corpus_df1_proc)

# Minimum
minimumFrequency = 10
DTM = dfm_trim(DTM, 
               min_docfreq = minimumFrequency,
               max_docfreq = 100000)

# keep only letters... brute force
DTM  = dfm_select(DTM, 
                  pattern = "[a-z]", 
                  valuetype = "regex", 
                  selection = 'keep')
colnames(DTM) = stringi::stri_replace_all_regex(colnames(DTM), 
                                                "[^_a-z]","")

DTM = dfm_compress(DTM, "features")

# Drop rows that do not have content left, In Chrome's case, NO

#sel_idx <- rowSums(DTM) > 0
# table(sel_idx) # check if there are blank rows. 
#DTM <- DTM[sel_idx, ]
#textdata <- textdata[sel_idx, ]

K = 10
# Set seed to make results reproducible
set.seed(9161)
topicModel = LDA(DTM, 
                 K, 
                 method="Gibbs", 
                 control=list(iter = 500, 
                              verbose = 25))

tmResult = posterior(topicModel)

# Topics are distributions over the entire vocabulary

beta = tmResult$terms
#glimpse(beta)            

# Each doc has a distribution over k topics

theta = tmResult$topics
#glimpse(theta)               

terms(topicModel, 10)

# Top terms per topic. Use top 5 to interpret topics
top5termsPerTopic = terms(topicModel, 5)

# give the topics more descriptive names than just numbers, concatenate the five most likely terms of each topic to a string that represents a pseudo-name for each topic.
topicNames = apply(top5termsPerTopic, 
                   2, 
                   paste, 
                   collapse=" ")

topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names
sort(topicProportions, decreasing = TRUE) # sort

# What was the value in the previous model?
attr(topicModel, "alpha") 

# Re-estimate model with alpha set by us, keep either topicModel or topicModel2
topicModel2 = LDA(DTM, 
                  K, 
                  method="Gibbs", 
                  control=list(iter = 500, 
                               verbose = 25, 
                               alpha = 0.2))
tmResult = posterior(topicModel2)
theta = tmResult$topics
beta = tmResult$terms


topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names 
sort(topicProportions, decreasing = TRUE) # sort

topicNames = apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # top five terms per topic 

# Topic distributions of example docs
exampleIds = c(2, 100, 200,300,500)
N = length(exampleIds)

topicProportionExamples = as.tibble(theta) %>%
  slice(exampleIds)

colnames(topicProportionExamples) = topicNames

vizDataFrame = melt(cbind(data.frame(topicProportionExamples), 
                          document = factor(1:N)), 
                    variable.name = "topic", 
                    id.vars = "document")  

ggplot(data = vizDataFrame, 
       aes(topic, value, 
           fill = document), 
       ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = N)

# Average theme proportions by decade
topic_proportion_per_decade = aggregate(theta, 
                                        by = list(browser = df1$browser), 
                                        mean)
colnames(topic_proportion_per_decade)[2:(K+1)] = topicNames
vizDataFrame = melt(topic_proportion_per_decade, 
                    id.vars = "browser")

if(!require(pals)) install.packages("pals")
library(pals)
ggplot(vizDataFrame, 
       aes(x=browser, 
           y=value, 
           fill=variable)) + 
  geom_bar(stat = "identity")+
  labs(title = "Top 10 Topics for Positive App Store Reviews by Browser", 
      y = "proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics") 

###########################

df2 = rbind(chrome, edge, safari, opera, firefox) %>% filter(rating < 3)
df2$review = tolower(df2$review) 
df2$review = gsub("edge|chrome|safari|firefox|opera", "", df2$review, ignore.case = TRUE)

# topicmodels 
corpus_df2 = corpus(df2$review)

# Tokenization
corpus_df2_proc = tokens(corpus_df2, 
                         remove_punct = TRUE, # remove punctuation
                         remove_numbers = TRUE, # remove numbers
                         remove_symbols = TRUE) %>% # remove symbols (for social media data, could remove everything except letters)
  tokens_tolower() # remove capitalization

# Lemmatization #
lemmaData = read.csv2("baseform_en.tsv", 
                      sep="\t", 
                      header=FALSE, 
                      encoding = "UTF-8", 
                      stringsAsFactors = F) %>% 
  na.omit()

# "Substitute token types based on vectorized one-to-one matching"
corpus_df2_proc = tokens_replace(corpus_df2_proc, 
                                 lemmaData$V1, 
                                 lemmaData$V2,
                                 valuetype = "fixed") 

# remove stopwords
corpus_df2_proc = corpus_df2_proc %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(1) 

#  Create dtm
DTM = dfm(corpus_df2_proc)

# Minimum
minimumFrequency = 10
DTM = dfm_trim(DTM, 
               min_docfreq = minimumFrequency,
               max_docfreq = 100000)

# keep only letters... brute force
DTM  = dfm_select(DTM, 
                  pattern = "[a-z]", 
                  valuetype = "regex", 
                  selection = 'keep')
colnames(DTM) = stringi::stri_replace_all_regex(colnames(DTM), 
                                                "[^_a-z]","")

DTM = dfm_compress(DTM, "features")

# Drop rows that do not have content left, In Chrome's case, NO

#sel_idx <- rowSums(DTM) > 0
# table(sel_idx) # check if there are blank rows. 
#DTM <- DTM[sel_idx, ]
#textdata <- textdata[sel_idx, ]

K = 10
# Set seed to make results reproducible
set.seed(9161)
topicModel = LDA(DTM, 
                 K, 
                 method="Gibbs", 
                 control=list(iter = 500, 
                              verbose = 25))

tmResult = posterior(topicModel)

# Topics are distributions over the entire vocabulary

beta = tmResult$terms
#glimpse(beta)            

# Each doc has a distribution over k topics

theta = tmResult$topics
#glimpse(theta)               

terms(topicModel, 10)

# Top terms per topic. Use top 5 to interpret topics
top5termsPerTopic = terms(topicModel, 5)

# give the topics more descriptive names than just numbers, concatenate the five most likely terms of each topic to a string that represents a pseudo-name for each topic.
topicNames = apply(top5termsPerTopic, 
                   2, 
                   paste, 
                   collapse=" ")

topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names
sort(topicProportions, decreasing = TRUE) # sort

# What was the value in the previous model?
attr(topicModel, "alpha") 

# Re-estimate model with alpha set by us, keep either topicModel or topicModel2
topicModel2 = LDA(DTM, 
                  K, 
                  method="Gibbs", 
                  control=list(iter = 500, 
                               verbose = 25, 
                               alpha = 0.2))
tmResult = posterior(topicModel2)
theta = tmResult$topics
beta = tmResult$terms


topicProportions = colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) = topicNames     # Topic Names 
sort(topicProportions, decreasing = TRUE) # sort

topicNames = apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # top five terms per topic 

# Topic distributions of example docs
exampleIds = c(2, 100, 200,300,500)
N = length(exampleIds)

topicProportionExamples = as.tibble(theta) %>%
  slice(exampleIds)

colnames(topicProportionExamples) = topicNames

vizDataFrame = melt(cbind(data.frame(topicProportionExamples), 
                          document = factor(1:N)), 
                    variable.name = "topic", 
                    id.vars = "document")  

ggplot(data = vizDataFrame, 
       aes(topic, value, 
           fill = document), 
       ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = N)

# Average theme proportions by decade
topic_proportion_per_decade = aggregate(theta, 
                                        by = list(browser = df2$browser), 
                                        mean)
colnames(topic_proportion_per_decade)[2:(K+1)] = topicNames
vizDataFrame = melt(topic_proportion_per_decade, 
                    id.vars = "browser")

if(!require(pals)) install.packages("pals")
library(pals)
ggplot(vizDataFrame, 
       aes(x=browser, 
           y=value, 
           fill=variable)) + 
  geom_bar(stat = "identity")+
  labs(title = "Top 10 Topics for Negative App Store Reviews by Browser", 
       y = "proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics")



