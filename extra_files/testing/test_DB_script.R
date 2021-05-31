library(dplyr)
library(dbplyr)
library(DBI)
create_db("~/Projectos/R_packages/development/recLabel/inst/recordings/", "test")

my_db <- dplyr::src_sqlite("~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/torunos_misclassifications_day1to15.sqlite3",
                           create = FALSE) # open connection

src_tbls(my_db) #list tables

# # add record
# add_record("~/Projectos/R_packages/development/recLabel/inst/recordings/test.sqlite3",
#            df = data.frame(file="teste.wav", call_pos = 1, spe = "noise"),
#            table = "rec_labels")

# create query to pull table
query <- tbl(my_db, "rec_labels")
collect(query)

# import recording
rec <- import_audio("~/Projectos/R_packages/development/recLabel/inst/recordings/20170907_234031_bat.wav",
                    butt = TRUE, low=10, high=90)

Spectrogram(as.numeric(rec$sound_filt), SamplingFrequency = 300000)

rec2 <- butter_filter(rec$sound_original, low = 20, high = 90, 300000, 1, 12)
Spectrogram(as.numeric(rec2), SamplingFrequency = 300000)

sound_data <- import_audio(path = "~/Projectos/R_packages/development/recLabel/inst/recordings/20170907_234031_bat.wav",
                              low = 10,
                              high = 90)

call_label(sound_samples = sound_data$sound_filt,
           file_name = sound_data$file_name,
           fs = sound_data$fs, tx = sound_data$tx, db = "~/Projectos/R_packages/development/recLabel/inst/recordings/test.sqlite3",
                       t_intervalo = 40, win_size = c(13, 5))




