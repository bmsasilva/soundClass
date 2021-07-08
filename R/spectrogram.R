#' @title Spectrogram
#' @description Plots a spectrogram or returns a spectrogram matrix
#' @param Audio Object of class "sound", "Wave", "Sample", "audioSample" or
#' a numeric vector (representing a sequence of samples taken from a sound wave)
#' @param SamplingFrequency The sampling frequency/rate of the sound in Hertz.
#' Only needed if Audio parameter is a numeric vector
#' @param WindowLength The desired length in ms of the analysis window used
#' to create the spectrogram
#' @param FrequencyResolution Integer, with higher numbers meaning
#' better resolution. Specifically, for any integer X provided, 1/X the
#' analysis bandwidth (as determined by the number of samples in the
#' analysis window) will be used. Note that this greatly impacts the
#' processing time, so adjust with care!
#' @param TimeStepSize Number of milliseconds that the window will be moved
#' for each adjacent analysis
#' @param Preemphasis Logical. Should pre-emphasis be applied
#' (to all frequency regions, i.e. with a dummy frequency cutoff of 0)? In other
#' words, should the spectral slope at all frequencies increase by 6 dB
#' per octave using a single-pole filter?
#' @param DynamicRange Values less than this many dB below the maximum are
#' 'clipped' to that value. If this is set to NULL, no clipping occurs.
#' @param nTimeSteps The overall total number of time steps. Only needed if
#' TimeStepSize is left NULL
#' @param Omit0Frequency Logical. As the frequency band at 0 Hz is usually at
#' very low values, choose TRUE to omit this frequency band
#' @param WindowType A character string indicating the desired type of window
#' function to be applied to the signal. Can be one of: "rectangular",
#' "blackman", "hanning", "hamming", "cosine", "bartlett", "gaussian", "kaiser"
#' @param WindowParameter Numeric. Only relevant if the WindowType is set to
#' "gaussian" or "kaiser"; it will be ignored in all other cases.
#' @param plot  Logical. Whether the spectrogram should be plotted or not. If
#'  FALSE, no spectrogram is plotted, and instead, a matrix is returned
#' containing the magnitude at each bin center (time across the rows and
#' frequency across the columns)
#' @param PlotFast Logical. If set to FALSE, the filled.contour() function will
#' be used. This produces much better looking graphics (which is best
#' for publications), but takes considerably longer to plot. If set to TRUE,
#' the image() function will be used instead, with 'useRaster' set to TRUE.
#' @param add Logical. Determines whether an entirely new plot is
#' drawn (with all the annotation) or whether just the core image is drawn
#' @param col Color map to use for the plot
#' @param xlim X axis (representing time) limit
#' @param ylim Y axis (representing frequency) limit
#' @param main Main title
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param norm Maximum intensity for spectrogram normalization (in dB)
#' @return If plot=FALSE, returns a spectrogram matrix, with the parameters
#' used in the analysis as attributes. If plot=TRUE a plot of the spectrogram
#' is produced but the matrix is not exported.
#' @usage Spectrogram(Audio,
#'              SamplingFrequency=NULL,
#'              WindowLength = 5,
#'              FrequencyResolution = 4,
#'              TimeStepSize = NULL,
#'              nTimeSteps = NULL,
#'              Preemphasis = TRUE,
#'              DynamicRange = 70,
#'              Omit0Frequency = FALSE,
#'              WindowType = "kaiser",
#'              WindowParameter = NULL,
#'              plot = TRUE,
#'              PlotFast = TRUE,
#'              add = FALSE,
#'              col = NULL,
#'              xlim = NULL,
#'              ylim = NULL,
#'              main = "",
#'              xlab = "Time (ms)",
#'              ylab = "Frequency (kHz)")
#' @details Computes a spectrogram which can be ploted or exported as a matrix
#' @keywords internal
#' @author Bruno Silva

Spectrogram <- function(Audio,
                        SamplingFrequency = NULL,
                        WindowLength = 5,
                        FrequencyResolution = 4,
                        TimeStepSize = NULL,
                        nTimeSteps = NULL,
                        Preemphasis = TRUE,
                        DynamicRange = 70,
                        Omit0Frequency = FALSE,
                        WindowType = "kaiser",
                        WindowParameter = NULL,
                        plot = TRUE,
                        PlotFast = TRUE,
                        add = FALSE,
                        col = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        main = "",
                        xlab = "Time (ms)",
                        ylab = "Frequency (kHz)",
                        norm = 140) {
  AudioClass <- class(Audio)
  AcceptableClasses <- c("sound", "Wave", "Sample", "audioSample", "numeric")
  AudioClassAcceptable <- as.logical(sum(AudioClass == AcceptableClasses))
  if (!AudioClassAcceptable) {
    stop("The 'Audio' argument must be one of the following classes:\n sound,
         Wave, Sample, audioSample, numeric")
  }

  if ((AudioClass == "numeric" & is.null(SamplingFrequency))) {
    stop("Must specify SamplingFrequency.")
  }

  if (AudioClass != "numeric" &
    !is.null(SamplingFrequency)) {
    warning("Specified SamplingFrequency ignored; the one associated with Audio object has been used instead.")
  }

  AudioClass <- class(Audio)
  AcceptableClasses <- c("sound", "Wave", "Sample", "audioSample", "numeric")
  AudioClassAcceptable <- as.logical(sum(AudioClass == AcceptableClasses))
  if (!AudioClassAcceptable) {
    stop("The 'Audio' argument must be one of the following classes:\n sound,
         Wave, Sample, audioSample, numeric")
  }

  if (AudioClass == "numeric") {
    Samples <- Audio
  }

  if (AudioClass == "sound") {
    Samples <- as.numeric(Audio$sound)
    SamplingFrequency <- Audio$fs
  }

  if (AudioClass == "Wave") {
    IsStereo <- attributes(Audio)$stereo
    if (IsStereo) {
      Samples <- (attributes(Audio)$left + attributes(Audio)$right) / 2
    } else {
      Samples <- attributes(Audio)$left
    }
    SamplingFrequency <- attributes(Audio)$samp.rate
  }

  if (AudioClass == "Sample") {
    IsStereo <- (nrow(Audio) == 2)
    if (IsStereo) {
      Samples <- (Audio$sound[1, ] + Audio$sound[2, ]) / 2
    } else {
      Samples <- Audio$sound[1, ]
    }
    SamplingFrequency <- Audio$rate
  }

  if (AudioClass == "audioSample") {
    IsStereo <- is.matrix(Audio)
    if (IsStereo) {
      Samples <- (Audio[1, ] + Audio[2, ]) / 2
    } else {
      Samples <- as.numeric(Audio)
    }
    SamplingFrequency <- Audio$rate
  }

  nSamplesInWindow <- ceiling((SamplingFrequency / 1000) * WindowLength)
  if (nSamplesInWindow %% 2 == 0) {
    nSamplesInWindow <- nSamplesInWindow + 1
  }

  if (!is.null(TimeStepSize) & !is.null(nTimeSteps)) {
    stop("You can only specify either TimeStepSize *or* nTimeSteps, not both.")
  }

  if (is.null(TimeStepSize) & is.null(nTimeSteps)) {
    nTimeSteps <- 400
  }

  if (!is.null(TimeStepSize)) {
    TimeStep <- floor(TimeStepSize / 1000 * SamplingFrequency)
  }

  if (!is.null(nTimeSteps)) {
    TimeStep <- floor(length(Samples) / nTimeSteps)
  }

  if (Preemphasis == TRUE) {
    PreemphasizedSamples <- as.numeric(stats::filter(Samples, c(1, -1), method = "convolution", sides = 1))
    PreemphasizedSamples[1] <- Samples[1]
    Samples <- PreemphasizedSamples
  }

  HalfWindowSize <- floor(nSamplesInWindow / 2)
  ZeroPadding <- rep(0, HalfWindowSize)
  ZeroPaddedSamples <- c(ZeroPadding, Samples, ZeroPadding)
  to <- length(ZeroPaddedSamples) - HalfWindowSize * 2
  LeftEdgeLocations <- seq(from = 1, to = to, by = TimeStep)

  AnalysisBandwidth <- SamplingFrequency / nSamplesInWindow
  FrequencyResolutionFactor <- AnalysisBandwidth / FrequencyResolution
  nSamplesPadding <- (SamplingFrequency - FrequencyResolutionFactor * nSamplesInWindow) / FrequencyResolutionFactor

  if (nSamplesPadding < 0) {
    nSamplesPadding <- 0
  }

  nSamplesInPaddedWindow <- nSamplesInWindow + nSamplesPadding

  Windowing <- function(CurrentLeftEdge) {
    TargetSamples <- ZeroPaddedSamples[CurrentLeftEdge:(CurrentLeftEdge + nSamplesInWindow - 1)]

    times <- FrequencyResolution * length(TargetSamples) - length(TargetSamples)
    TrailingZeroesAppended <- c(TargetSamples, rep(0, times = times))

    AcceptableWindowTypes <- c("rectangular", "square", "blackman", "hann", "hanning", "hamming", "cosine", "sine", "bartlett", "gaussian", "kaiser")
    WindowTypeAcceptable <- as.logical(sum(WindowType == AcceptableWindowTypes))
    if (!WindowTypeAcceptable) {
      stop("The 'WindowType' argument must be one of the following:\n       rectangular / square, blackman, hann / hanning, hamming, cosine / sine, bartlett, gaussian, kaiser")
    }

    IntegerSequence <- 0:(nSamplesInWindow - 1)
    if (WindowType == "rectangular" |
      WindowType == "square") {
      WindowFunction <- rep(1, nSamplesInWindow)
    }
    if (WindowType == "blackman") {
      WindowFunction <- (7938 / 18608) - (9240 / 18608) *
        cos((2 * pi * IntegerSequence) / (nSamplesInWindow - 1)) +
        (1430 / 18608) *
          cos((4 * pi * IntegerSequence) / (nSamplesInWindow - 1))
    }
    if (WindowType == "hann" |
      WindowType == "hanning") {
      WindowFunction <- 0.5 * (1 - cos((2 * pi * IntegerSequence) / (nSamplesInWindow - 1)))
    }
    if (WindowType == "hamming") {
      WindowFunction <- 0.54 - 0.46 * cos((2 * pi * IntegerSequence) / (nSamplesInWindow - 1))
    }
    if (WindowType == "cosine" |
      WindowType == "sine") {
      WindowFunction <- sin((IntegerSequence * pi) / (nSamplesInWindow - 1))
    }
    if (WindowType == "bartlett") {
      WindowFunction <- (2 / (nSamplesInWindow - 1)) *
        ((nSamplesInWindow - 1) / 2 - abs(IntegerSequence - (nSamplesInWindow - 1) / 2))
    }
    if (WindowType == "gaussian") {
      if (is.null(WindowParameter)) {
        WindowParameter <- 0.4
      }
      WindowFunction <- exp(-0.5 * ((IntegerSequence - (nSamplesInWindow - 1) / 2) /
        (WindowParameter * (nSamplesInWindow - 1) / 2))^2)
    }
    if (WindowType == "kaiser") {
      if (is.null(WindowParameter)) {
        WindowParameter <- 3
      }
      WindowFunction <- besselI(WindowParameter * pi * sqrt(1 - (2 * (IntegerSequence) / (nSamplesInWindow - 1) - 1)^2), 0) /
        besselI(WindowParameter * pi, 0)
    }

    if (WindowType != "gaussian" & WindowType != "kaiser" & !is.null(WindowParameter)) {
      warning("The specification of WindowParameter was ignored.\n(WindowParameter is only used if WindowType is 'gaussian' or 'kaiser'.)")
    }

    WindowedSamples <- TrailingZeroesAppended * WindowFunction
    MeanSubtracted <- WindowedSamples - mean(WindowedSamples)
    FFT <- stats::fft(MeanSubtracted)[1:(nSamplesInPaddedWindow / 2 + 1)]
    AbsoluteValueSquared <- abs(FFT)^2
    LogScaled <- log(AbsoluteValueSquared, 10) * 10
    return(LogScaled)
  }

  UnnormalizedSpectrogramMatrix <- t(sapply(X = LeftEdgeLocations, FUN = Windowing))

  SpectrogramMatrix <- UnnormalizedSpectrogramMatrix - norm

  if (!is.null(DynamicRange)) {
    NegativeDecibelThreshold <- -1 * DynamicRange
    SpectrogramMatrix[which(SpectrogramMatrix < NegativeDecibelThreshold)] <- NegativeDecibelThreshold
  }

  TimeSequence <- (LeftEdgeLocations + floor(nSamplesInWindow / 2)) * (1000 / SamplingFrequency)
  FrequencySequence <- (0:(nSamplesInPaddedWindow / 2)) * (SamplingFrequency / nSamplesInPaddedWindow)

  if (Omit0Frequency) {
    SpectrogramMatrix <- SpectrogramMatrix[, -1]
    FrequencySequence <- FrequencySequence[-1]
  }

  if (plot == FALSE) {
    rownames(SpectrogramMatrix) <- as.numeric(round(TimeSequence, 2))
    colnames(SpectrogramMatrix) <- as.numeric(round(FrequencySequence, 2))
    attributes(SpectrogramMatrix) <- append(attributes(SpectrogramMatrix), list(
      SamplingFrequency = SamplingFrequency,
      WindowLength = WindowLength,
      FrequencyResolution = FrequencyResolution,
      TimeStepSize = TimeStepSize,
      nTimeSteps = nTimeSteps,
      Preemphasis = Preemphasis,
      DynamicRange = DynamicRange,
      Omit0Frequency = Omit0Frequency,
      WindowType = WindowType,
      WindowParameter = WindowParameter
    ))
    return(SpectrogramMatrix)
  } else {
    if (is.null(col)) {
      ColorGenerator <- grDevices::colorRampPalette(c("dark blue", "blue", "cyan", "yellow", "orange", "red", "brown"))
    } else {
      if (identical(col, "alternate")) {
        ColorGenerator <- grDevices::colorRampPalette(c("black", "red", "orange", "yellow", "white"))
      } else {
        if (identical(col, "greyscale") | identical(col, "grayscale")) {
          ColorGenerator <- grDevices::colorRampPalette(c("white", "black"))
        } else {
          ColorGenerator <- grDevices::colorRampPalette(col)
        }
      }
    } # End 'if/else' for greyscale, alternate, and NULL

    AmplitudeRange <- range(SpectrogramMatrix, finite = TRUE)
    MinimumAmplitude <- AmplitudeRange[1]
    MaximumAmplitude <- AmplitudeRange[2]

    MinimumAmplitude <- AmplitudeRange[1]
    MaximumAmplitude <- AmplitudeRange[2]
    nColorLevels <- abs(MinimumAmplitude) - abs(MaximumAmplitude)

    ColorLevels <- seq(from = MinimumAmplitude, to = MaximumAmplitude, length.out = nColorLevels + 1)
    ColorPalette <- ColorGenerator(round(nColorLevels))

    FrequencySequence <- FrequencySequence / 1000

    if (is.null(xlim)) {
      xlim <- range(TimeSequence)
    }

    if (is.null(ylim)) {
      ylim <- range(FrequencySequence)
    }


    MaximumFrequency <- ylim[which.max(ylim)]
    if (MaximumFrequency > (SamplingFrequency / 2)) {
      ylim[which.max(ylim)] <- SamplingFrequency / 2
    }

    if (PlotFast == TRUE) {
      graphics::image(
        x = TimeSequence,
        y = FrequencySequence,
        z = SpectrogramMatrix,
        xlim = xlim,
        ylim = ylim,
        zlim = AmplitudeRange,
        col = ColorPalette,
        add = add,
        xaxs = "i",
        yaxs = "i",
        xlab = xlab,
        ylab = ylab,
        main = main,
        oldstyle = FALSE,
        useRaster = TRUE
      )
    } else {
      if (add == FALSE) {
        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")
      }

      graphics::.filled.contour(x = TimeSequence, y = FrequencySequence, z = SpectrogramMatrix, levels = ColorLevels, col = ColorPalette)

      if (add == FALSE) {
        graphics::Axis(TimeSequence, side = 1)
        graphics::Axis(FrequencySequence, side = 2)
        graphics::title(main = main, xlab = xlab, ylab = ylab)
      }
    }

    graphics::box()
  }
}

batsound <- rev(c(
  "#00001F", "#00002F", "#00003F", "#00004F", "#00005F",
  "#00006F", "#00007F", "#00008F", "#00009F", "#0000AF",
  "#0000BF", "#0000CF", "#0000DF", "#0000EF", "#0000FF",
  "#0F00F0", "#1F00E0", "#2F00D0", "#3F00C0", "#4F00B0",
  "#5F00A0", "#6F0090", "#7F0080", "#8F0070", "#9F0060",
  "#AF0050", "#BF0040", "#CF0030", "#DF0020", "#EF0010",
  "#FF0000", "#FF0F00", "#FF1F00", "#FF2F00", "#FF3F00",
  "#FF4F00", "#FF5F00", "#FF6F00", "#FF7F00", "#FF8F00",
  "#FF9F00", "#FFAF00", "#FFBF00", "#FFCF00", "#FFDF00",
  "#FFEF00", "#FFFF00", "#FFFF0F", "#FFFF1F", "#FFFF2F",
  "#FFFF3F", "#FFFF4F", "#FFFF5F", "#FFFF6F", "#FFFF7F",
  "#FFFF8F", "#FFFF9F", "#FFFFAF", "#FFFFBF", "#FFFFCF"
))
