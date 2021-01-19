# test Spectrogram()

sound <- import_audio('.\\inst\\recordings\\20170907_234031_bat.wav',
low=10, high=120)

Spectrogram(sound$sound_samples,                   
            SamplingFrequency=sound$fs,  
            WindowLength = 3,        
            FrequencyResolution = 3, 
            TimeStepSize = 1,     
            nTimeSteps = NULL,       
            Preemphasis = TRUE,      
            DynamicRange = 70,       
            Omit0Frequency = FALSE,  
            WindowType = "hanning",   
            WindowParameter = NULL,  
            plot = TRUE,             
            PlotFast = TRUE,         
            add = FALSE,             
            col = NULL,              
          #  xlim = ranges$x,             
           # ylim = ranges$y,             
            main = sound$file_name,               
            xlab = "Time (ms)",      
            ylab = "Frequency (kHz)")

fs=300000
tx=1
pos <- 300000
ms2samples(pos,inv=T, tx=1, fs=300000)
abline(v=1000)
