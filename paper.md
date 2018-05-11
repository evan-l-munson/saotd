---
title: "Sentiment Analysis of Twitter Data (SAoTD)"
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

SAoTD is an R interface to the Twitter API and can be used to acquire tweets based on user selected \#hashtags.  The package will clean and tidy the Twitter data, determine the latent topics within the tweets utilizing Latent Dirichlet Allocation (LDA), determine a sentiment score using the Bing lexicon dictionary and output visualizations.

The package is available on [GitHub](https://github.com/evan-l-munson/SAoTD) and archived on [Zenodo](https://zenodo.org/record/1219852#.WtdNuchryfd).  The package was developed with the intention of the user creating a personal [Twitter](hjttps://twitter.com) account which will then allow  a user to access the twitter API throught the [Twitter Developers Account](https://dev.twitter.com/) site.  Once a user has access to the Twitter Developers Account they will have the ability to create an application which will then provide the user with access tokens.  These access tokens will then allow the user to begin acquiring tweets usint eh SAoTD package.

The package is laid out in five different categories: Acquire, Explore, Topic Analysis, Sentiment Calculation, and Visualizations.

* Acquire allows a user to acquire tweets of their choosing by accessing the Twitter API.
* Explore provides functions to tidy, explore unigrams, bigrams, tri-grams, in addition to bigram netwrorks and correlation networks.
* Topic analysis allows a user to explore the latent topics buried within the tweets.
* Sentiment calculation utilizes the Bing lexicon dictionary to score the text [@Hu2004].
* Visualizations allow the user to better understand the twitter sentiment.

The package utilizes tidy dataferames and therefore depends on the tidyverse package [@Wickham2017] and additionally uses the tidytext package [@Silge2017].  The number of latent topics is determined using the ldatuning package [@Nikita2016] and the latent dirichlet allocation (LDA) topics is determined using the topicmodels package [@Grun2011].

The SAoTD package has research applications in many disciplines which intend to use twitter text and sentiment analysis.  The package was created to quickly determine the sentiment of twitter and to inform analysts on the opinions contained within tweets.

# References

