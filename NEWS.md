# soundClass 0.0.9

First public release.

# soundClass 0.0.9.1

- Bug fix in overlap parameter of function spectro_calls(). Now
accepts values of 0.5 and 0.75

- Include progress bar in console for function spectro_calls()

- Update bundled model code

- Update download link of example files
 
# soundClass 0.0.9.2

- Change the parameter frequency resolution of function spectro_calls() to 
allow better control of the frequency resolution of the spectrogram data. It 
is now defined as number of frequency bins per kHz. For instance, a frequency 
resolution of 1 now indicates a resolution of 1 bin per kHz, whereas a 
frequency resolution of 10 indicates 10 bins per kHz (or 1 bin per 100 Hz)
