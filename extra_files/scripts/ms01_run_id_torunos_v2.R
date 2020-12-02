library(keras)
library(zoo)
library(seewave)
library(tuneR)
library(signal)

for(file in list.files("~/Projectos/bat_detector/batdecoder_gui/R", full.names = T)) source(file)


class_labels <- c("Noise", "Bbar","Eisa","Eser","Hsav", "Mbec","Mbly","Mdau","Mema","Mesc",
                  "Mmyo","Mmys","Msch","Nlas","Nlei", "Paur", "Paus", "Pkuh", "Ppip","Ppyg", "Tten")

##### Use base model #####
auto_id(model_path = "~/Projectos/bat_detector/batdecoder_model_train/model_results/inception03_ms01/generation1/modelo_inception02_gen1_v2_16-0.9821.hdf5",
          file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
          csv_file = "analise_torunos_base_model_v2.csv",
          out_dir = "/home/bruno/Projectos/PHD/ms01/output_base_model_v2/",
          class_labels = class_labels,
          save_png = F,
          save_spec = T,
          version = "v2")

##### Use model gen02 (retrained with 10% days errors and sample of original database) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_10percent_03-0.9926.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
        csv_file = "analise_torunos_gen02.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

##### Use model gen02 (retrained with 5% days errors and sample of original database) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_5percent_06-0.9911.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
        csv_file = "analise_torunos_gen02_5percent.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_5percent/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

##### Use model gen02 (retrained with 1% days errors and sample of original database) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_1percent_04-0.9968.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/",
        csv_file = "analise_torunos_gen02_1percent.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_1percent/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

