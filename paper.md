---
title: "Sentiment Analysis of Twitter Data (saotd)"
authors:
- affiliation: 1
  name: Evan L. Munson
  orcid: 0000-0002-9958-6800
- affiliation: 1
  name: Christopher M. Smith
  orcid: 0000-0002-8288-270X
- affiliation: 1
  name: Bradley C. Boehmke
  orcid: 0000-0002-3611-8516
- affiliation: 1
  name: Jason K. Freels
  orcid: 0000-0002-2415-0340
date: "11 May 2018"
output: pdf_document
bibliography: paper.bib
tags:
- text mining
- sentiment analysis
- natural language processing
- latent dirichlet allocation
- twitter sentiment analysis
affiliations:
- index: 1
  name: Air Force Institure of Technology
---

`saotd` is an R package that provides a programmatic interface to the Twitter API and can be used to acquire tweets based on user-specified \#hashtags. The package will clean and tidy the Twitter data, determine the latent topics within the tweets utilizing Latent Dirichlet Allocation (LDA), determine a sentiment score using the Bing lexicon dictionary, and create output visualizations.

The package is available on [GitHub](https://github.com/evan-l-munson/saotd) and archived on [Zenodo](https://zenodo.org/record/1219852#.WtdNuchryfd). To configure the package a user must follow these steps:

* Creating a personal [Twitter](https://twitter.com) account if they don't already have one.
* Enable a [Twitter Developers Account](https://dev.twitter.com/).
* Create an application which will provide them with API access tokens required by the `saotd` package to function.

The package is laid out in five different categories: Acquire, Explore, Topic Analysis, Sentiment Calculation, and Visualizations.

* Acquire allows a user to acquire tweets of their choosing by accessing the Twitter API.
* Explore provides functions to tidy, explore unigrams, bigrams, tri-grams, in addition to bigram netwrorks and correlation networks.
* Topic analysis allows a user to explore the latent topics buried within the tweets.
* Sentiment calculation utilizes the Bing lexicon dictionary to score the text [@Hu2004].
* Visualizations allow the user to better understand the sentiment of the tweets.

The package utilizes tidy dataframes and therefore depends on the `tidyverse` package [@Wickham2017] and additionally uses the `tidytext` package [@Silge2017].  The number of latent topics is determined using the `ldatuning` package [@Nikita2016] and the latent dirichlet allocation (LDA) topics is determined using the `topicmodels` package [@Grun2011].

The `saotd` package has research applications in many disciplines which need to access tweets from the Twitter platform and carry out sentiment analyses. This package was created to quickly determine the sentiment of Twitter and to inform analysts on the opinions contained within tweets.

# References

