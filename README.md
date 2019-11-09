Exercise 50 from the book "Exercises for Programmers": Movie Recommendations.

This program is a small training exercise I'm doing for myself, without any useful purpose of any kind to anybody.

Since the Rotten Tomatoes data is no longer available, IMDB APIs will be used instead.

All objectives except the graphical display of the score have been met.

Due to this being an exercise and due to boredom overflow, the code has some rough edges, namely:

- no clear architectural separation between the network layer and the caching layer
- terrible handling of a wrong api scenario (this is due to a bug in url-retrive, though…)
- all aswers are cached, temporary failures included
