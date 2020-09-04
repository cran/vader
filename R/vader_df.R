#' Get a dataframe of vader results for multiple text documents
#'
#' Use vader_df() to calculate the valence of multiple texts contained within a vector or column in a dataframe.
#'
#' @param text to be analyzed; for vader_df(), the text should be a single vector (e.g. 1 column)
#' @param incl_nt defaults to T, indicates whether you wish to incl UNUSUAL n't contractions (e.g., yesn't) in negation analysis
#' @param neu_set defaults to T, indicates whether you wish to count neutral words in calculations
#' @importFrom tm stopwords
#' @return A dataframe containing the valence score for each word; an overall, compound valence score for the text; the weighted percentage of positive, negative, and neutral words in the text; and the frequency of the word "but".
#' @examples
#' vader_df(c("I'm happy", "I'm yesn't happy"))
#' vader_df(c("I'm happy", "I'm yesn't happy"), incl_nt = FALSE)
#' vader_df(c("I'm happy", "I'm yesn't happy"), neu_set = FALSE)
#'
#' @export
#'
#' @section N.B.:
#' In the examples below, "yesn't" is an internet neologism meaning "no", "maybe yes, maybe no", "didn't", etc.
#'
#' @seealso \code{\link{get_vader}} to get vader results for a single text document

vader_df <- function(text, incl_nt = T, neu_set = T){
  df <- lapply(text, function(x) data.frame(as.list(get_vader(x, incl_nt, neu_set))))
  df <- do.call("rbind", df)
  df <- cbind(text, df)
}
