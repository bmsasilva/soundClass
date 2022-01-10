pkgname <- "soundClass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "soundClass-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('soundClass')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("create_db")
### * create_db

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_db
### Title: Create a sqlite3 database
### Aliases: create_db

### ** Examples

dir_path <- tempdir()
create_db(dir_path, 
db_name = "test", 
table_name = "labels",
type = "reference")
file.remove(file.path(dir_path, "test.sqlite3"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_db", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("find_noise")
### * find_noise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: find_noise
### Title: Detect energy peaks in non-relevant recordings
### Aliases: find_noise

### ** Examples

# Create a sample wav file in a temporary directory
recording <- tuneR::noise(duration = 44100)
temp_dir <- tempdir()
rec_path <- file.path(temp_dir, "recording.wav")
tuneR::writeWave(recording, filename = rec_path)
# Import the sample wav file
new_rec <- import_audio(rec_path, butt = FALSE, tx = 1)
find_noise(new_rec, nmax = 1, plot = FALSE)
file.remove(rec_path)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("find_noise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_audio")
### * import_audio

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_audio
### Title: Import a recording
### Aliases: import_audio

### ** Examples

# Create a sample wav file in a temporary directory
recording <- tuneR::sine(440)
temp_dir <- tempdir()
rec_path <- file.path(temp_dir, "recording.wav")
tuneR::writeWave(recording, filename = rec_path)
# Import the sample wav file
new_rec <- import_audio(rec_path, low = 1, high = 20, tx = 1)
new_rec
file.remove(rec_path)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_audio", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ms2samples")
### * ms2samples

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ms2samples
### Title: Convert between time and number of samples in sound files
### Aliases: ms2samples

### ** Examples

ms2samples(150000, fs = 300000, tx = 1, inv = FALSE)
ms2samples(100, fs = 300000, tx = 1, inv = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ms2samples", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
