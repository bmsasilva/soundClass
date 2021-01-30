
sound <- import_audio(".//recs//M500-20190704_030003.wav", low = 10, high = 120)
sound <- import_audio("./inst/recordings/20170907_234031_bat.wav", low = 10, high = 120)

spec2<-Spectrogram(as.numeric(sound$sound_samples[1:20000]),                 
            SamplingFrequency=sound$fs,  
            WindowLength = 5,        
            FrequencyResolution = 1, 
            TimeStepSize = 4,     
            nTimeSteps = NULL,       
            Preemphasis = TRUE,      
            DynamicRange = 70,       
            Omit0Frequency = FALSE,  
            WindowType = "hanning",   
            WindowParameter = NULL,  
            plot = F,             
            PlotFast = TRUE,         
            add = FALSE,             
            col = NULL,              
            xlim = NULL,             
            ylim = NULL,             
            main = "",               
            xlab = "Time (ms)",      
            ylab = "Frequency (kHz)")

image(x)
image(denoise_spec(x))

x<-matrix(c(1:15), ncol=5)
