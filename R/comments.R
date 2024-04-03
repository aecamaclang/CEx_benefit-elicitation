#' Compile expert comments
#'
#' This function combines comments by an expert for the same
#' biodiversity value that are spread out over multiple rows
#'
#' @param df dataframe containing the expert comments to be compiled, along with
#'   the columns to be used as key (i.e., Expert name/ID, Biodiversity value)
#' @param experts vector of expert ID or codes/names
#'
#'
compile <- function(df, experts) {
  sp <- levels(df$Biodiversity)
  cols <- names(df)
  out <- data.frame(matrix(nrow=0, ncol = length(cols)))

  for (i in 1:length(sp)) {
    for (j in 1:length(experts)) {
      temp <- df[which(df$Expert == experts[j] & df$Biodiversity==sp[i]),]
      rows <- cbind(j, sp[i], paste(temp$Comments, collapse = " "))
      out <- rbind(out, rows)
    }
  }
  colnames(out) <- cols
  out
}
