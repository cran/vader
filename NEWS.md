vader 0.2.0:
- Changed get_vader function so that it returns 1 index item with a character string of individual word scores (instead of multiple index items for each individual word score)
- Changed get_vader function so that it returns a named vector of NAs if the text is blank (rather than a single NA)
- Simplifed vader_df function, changed to lapply rather than for-loop

vader 0.1.1:
Fixed bug where text documents with single words would match with idioms containing multiple words.

vader 0.1.0:
Contains a new function that calls getVader() on multiple text documents and returns a dataframe of results.

vader 0.0.2: 
Minor bug fixes have been completed:

1.) The previous version crashed when emoticons started with open parentheses/brackets, such as :(
2.) The Vader lexicon had a handful of duplicate entries. The sentiment scores will return the average of any duplicate lexicon entries.