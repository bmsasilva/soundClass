library(keras)
library(zoo)
library(seewave)
library(tuneR)
library(signal)

for(file in list.files("~/Projectos/bat_detector/batdecoder_gui/R", full.names = T)) source(file)


class_labels <- c("Noise", "Bbar","Eisa","Eser","Hsav", "Mbec","Mbly","Mdau","Mema","Mesc",
                  "Mmyo","Mmys","Msch","Nlas","Nlei", "Paur", "Paus", "Pkuh", "Ppip","Ppyg", "Tten")


auto_id(model_path = "~/Projectos/bat_detector/batdecoder_model_train/model_results/inception03_ms01/generation1/modelo_inception02_gen1_v2_16-0.9821.hdf5",
          file_path = "/home/bruno/Projectos/PHD/ms01/results_base_model/misclassifications_gen1/noise_as_bats/days001-015/",
          csv_file = "analise_torunos_noise_recs.csv",
          out_dir = "/home/bruno/Projectos/PHD/ms01/results_base_model/misclassifications_gen1/noise_as_bats/days001-015/",
          class_labels = class_labels,
          save_png = T,
          save_spec = T,
          version = "v2")

auto_id(model_path = "~/Projectos/bat_detector/batdecoder_model_train/model_results/inception03_ms01/generation1/modelo_inception02_gen1_v2_16-0.9821.hdf5",
        file_path = "/home/bruno/Projectos/PHD/ms01/results_base_model/misclassifications_gen1/noise_as_bats/days015-030/",
        csv_file = "analise_torunos_noise_recs.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/results_base_model/misclassifications_gen1/noise_as_bats/days015-030/",
        class_labels = class_labels,
        save_png = T,
        save_spec = T,
        version = "v2")

##### Use model gen02 (retrained with 1-15 days errors) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_06-0.9994.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
        csv_file = "analise_torunos_gen02.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

##### Use model gen03 (retrained with 1-21 days errors) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen03_model/modelo_inception02_lr0.01_gen3_05-0.9994.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
        csv_file = "analise_torunos_gen03.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen03_model/",
        class_labels = class_labels,
        save_png = T,
        save_spec = T,
        version = "v2")

