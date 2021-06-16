
noise_files <- list.files("//home/bruno/packages_test_data/recordings", pattern = "wav|WAV")

for(i in seq(noise_files)){
  
sound <- import_audio(paste0("//home/bruno/packages_test_data/recordings/",noise_files[i]), low=10, high=120)

peak <- find_noise(sound, nmax = 1, plot=F) #nmax vem do modal


output <- data.frame("recording" = sound$file_name,
                     "label_position" = peak,
                     "label_class" = "0",
                     "observations" = NA)

add_record(path = input$db_path, df = output)
}