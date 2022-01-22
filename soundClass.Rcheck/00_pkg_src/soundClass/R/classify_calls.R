#' @author Bruno Silva
#' @keywords internal
#' @noRd

classify_calls <- function(calls, sound_peaks, model) {
  spec_calls <- calls$spec_calls
  img_rows <- calls$img_rows
  img_cols <- calls$img_cols
  fmaxe <- calls$fmaxe

  train_x <- array(spec_calls, dim = c(
    length(sound_peaks),
    img_rows, img_cols, 1
  ))

  probs <-
    model %>%
    stats::predict(train_x) %>%
    round(3)

  preds <- max.col(probs) - 1

  max_prob <- apply(probs, 1, max)

  return(data.frame(probs,
    spe = preds, prob = max_prob, peaks = sound_peaks,
    fmaxe = fmaxe
  ))
}
