##### Function to read RDATA #####
# This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, 
# load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

model_path <- "/home/bruno/packages_test_data/recordings/fitted_model.hdf5"
file_path <- "/home/bruno/packages_test_data/recordings/"
csv_file <- "output.csv"
model_metadata_path = "/home/bruno/packages_test_data/recordings/fitted_model_metadata.RDATA"

env <- LoadToEnvironment(model_metadata_path)
metadata <- env[[names(env)[1]]]
rm(env)

# # for shinny
# metadata <- reactive({ # reactive com a lista importada no rdata
#   if(length(input$metadata_path) > 1){ 
#     file_selected <- parseFilePaths(roots, input$metadata_path)
#     env <- LoadToEnvironment(as.character(file_selected$datapath))
#     metadata <- env[[names(env)[1]]]
#     rm(env)
#     return(metadata)
#   }
# })



auto_id_shiny(model_path(), 
                          metadata(),
                          file_path(), 
                          csv_file, 
                          out_dir = c(file_path(), "/", "output/"), # criar a pasta se nao existir
                          save_spec = T,
                          save_png = input$lab_plots, 
                          class_labels = as.character(metadata()$classes$name), 
                          win_size = metadata()$parameters$spec_size * 2,  #minimum distnace between calls and chunck size
              remove_noise = input$rem_noise,          # if model has non-relevant class, eliminate from output  
              plot2console = FALSE, 
                          recursive = FALSE)



