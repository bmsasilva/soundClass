#' XXXXXXX
#' @title XXXXXXX
#' @description XXXXXXX
#' @param files_path XXXXXXX
#' @param db_path XXXXXXX
#' @param parameters XXXXXXX
#' @usage classify_calls(calls, sound_peaks, model)
#' @return XXXXXXXX
#' @author Bruno Silva
#' @export



classify_calls <- function(calls, sound_peaks, model){

  spec_calls <- calls$spec_calls
  img_rows <- calls$img_rows
  img_cols <- calls$img_cols
  fmaxe <- calls$fmaxe

train_x <- array(spec_calls, dim = c(length(sound_peaks), img_rows, img_cols, 1))

probs <-
  model %>%
  predict(train_x) %>%
  round(3)

preds <- max.col(probs) - 1 #-1 pq tensorflow indexa desde 0
 
max_prob <- apply(probs, 1, max)

   return(data.frame(probs, spe = preds, prob = max_prob, peaks = sound_peaks, fmaxe = fmaxe))

# if (save == TRUE){
#   if (length(picos)>0) {#inicio if
#     for (j in 1:length(picos)) {#inicio for
#       savewav(morc$som[(picos[j]-(morc$fs*tx*10/1000)):(picos[j]+(morc$fs*tx*10/1000))], filename = paste(morc$ficheiro,"_",j,".wav"), f = morc$fs)
#     }#final for
#   }#final if picos
# }#final if save

}
