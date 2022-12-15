# soundClass 0.0.9

First public release.

# soundClass 0.0.9.1

- Bug fix in overlap parameter of function spectro_calls(). Now
accepts values of 0.5 and 0.75

- Include progress bar in console for function spectro_calls()

- Update bundled model code

- Update download link of example files
 
# soundClass 0.0.9.2

- Bug fix in the window size parameter of function spectro_calls(). The
y-axis values of the spectrograms generated are now correct

- Bug fix in metadata parameter of function auto_id(). The function
now reads the file path correctly

- Added checkbox input for Butterworth filter in app_label()

- Minor updates in the documentation

# soundClass 0.0.9.3

- Added function to plot training spectrograms

- Added parameter "frequency resolution" to function spectro_calls()

- Added functionality to apply a butterworth filter prior to recording 
processing. Respective parameter added to functions spectro_calls() and
auto_id()

- Updated to remove deprecated function dbplyr::src_sqlite() 

- The "Run model" GUI (in app_model()) now accepts butterworth filter and
custom windows size for spliting the recording into chuncks
