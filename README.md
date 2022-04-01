# Stories of the Pandemic Project

This project is part of the module Methods and Applications in Digital Humanities at Leipzig University in WS21/22. It was supervised by Dr. Andreas Niekler. 

## About
Since the beginning of the year 2020 there is one major topic existing in all kinds of media: the COVID-19 pandemic. This topic is also the subject of many scientific works, statistics and general publications. These provide insights into current developments as well as retrospective ones. What was relevant at what time and how terms have developed can be determined from these texts. In view of the fact that the published articles consider different topics and opinions regarding the pandemic, it is interesting to examine them in more detail. Therefore, this research will explore the use of topic modelling and other text analysis methods to visualize or summarize the different stories of the pandemic in a certain text corpus. 

The main data source will be consisting of a text corpus of various Guardian news articles on the COVID-19 pandemic over the last two years, provided in the so called "Guardian-API": https://rapidapi.com/mikilior1/api/Guardian/details. 

This research study focuses specifically on three research questions: First, What topics regarding the COVID-19 pandemic appear in the Guardian news articles and how have they changed over a certain period of time? Second, Is it possible to visualize and narrow down the stories of the pandemic into a map of knowledge? And Third, What can such a representation do? What are its limitations and how can this representation contribute to enlightenment?. 


## Data

Data-Folder: <br>
* Csv files with the scraped data

R-Files:<br>
* guapi_data_scraping.R: Script for data scraping <br>
* corpus_creating.R: Creation of a CSV file with all articles - based on all individual CSV files <br>
* preprocessing.R: Preprocessing steps (Data reading, data set preprocessing, corpus creation, lemmatization, stop word removal, tokenization) <br>
* plot_articles_frequencies_over_time: Source code for displaying article frequencies over time <br>
* plot_covidwords_frequencies: Source code to show the occurrence of Covid terms <br>
* plot_wordfrequencies: Source code for displaying absolute and relative word frequencies (including their occurrence over time) <br>
* calculateCoocStatistics: Source code for the co-occurrence calculation <br>
* title_analysis: Source code only for the analysis of article headings <br>
* tfidf.R: Source for for the analysis with tf-idf measure <br>
* cooc_analysis: Source code for the generation of cooccurrence networks <br>
* topic_modelling: Source code for investigations with topic modeling <br>
* baseform_en.tsv: File for building a dictionary of lemmas <br>
* stopwords_en: Stop word list <br>

## Copyright

The copright is ©2022
* MIT for the R-Scripts
* CC-BY for all images and text
