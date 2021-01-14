#' This function eliminates calls: 1) classified as noise,
#' and 2) closer than a specific threshold. It
#' then converts the numeric classes to text labels
#' @title Format the classification table
#' @description  Format the classification table
#' for easier interpretation
#' @param output output of function classify_calls()
#' @param class_labels vector of character labels for the categories.
#' Must be in the same order as the numeric equivalent. For instance,
#' if the numeric categories are 0 = noise and 1 = bat, the vector must
#' be c("noise", "bat")
#' @param min_dist minimum distance in ms between consecutive calls.
#' @param remove_noise if TRUE remove the peaks identified as noise
#' from the formated table
#' @param multiple_species if FALSE only the calls belonging to the
#' most represented species are kept in the formated table. If TRUE all
#' calls classified as bats are returned in the formated table
#' @usage tidy_output(output, class_labels, min_dist = 40, remove_noise = T, multiple_species = F)
#' @return A formated classification table
#' @author Bruno Silva
#' @export
tidy_output <- function(output, class_labels, min_dist = 40,
                        remove_noise=T, multiple_species = F){
 try({
  if(remove_noise == T) output <- output[output$spe>0,]
 class_n <-  seq(class_labels) - 1
 colnames(output)[1:length(class_n)] <- class_labels

 output$spe <- as.character(factor(output$spe,
                              levels = c(class_n),
                              labels = c(class_labels)))

 if(multiple_species == F) output <- output[output$spe == get_mode(output$spe),]

intervals <- diff(output$peaks)
filter <- which(intervals < (ms2samples(min_dist))) + 1
if(length(filter) > 0) output <- output[-filter,]




return(output)
})
}
