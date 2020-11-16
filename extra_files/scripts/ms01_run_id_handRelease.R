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
          file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/",
          csv_file = "analise_handRelease_base_model.csv",
          out_dir = "/home/bruno/Projectos/PHD/ms01/output_base_model_studyCase2/",
          class_labels = class_labels,
          save_png = F,
          save_spec = T,
          version = "v2")

##### Use model gen02 (retrained with 10% days errors and sample of original database) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_10percent_handRelease_04-0.9972.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/",
        csv_file = "analise_handRelease_gen02_10percent.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_10percent_studyCase2/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

##### Use model gen02 (retrained with 5% days errors and sample of original database) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_5percent_handRelease_01-0.9776.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/",
        csv_file = "analise_handRelease_gen02_5percent.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_5percent_studyCase2/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

##### Use model gen02 (retrained with 1% days errors and sample of original database) (bad results) #####
auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_1percent_handRelease_01-0.9803.hdf5",
        file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/",
        csv_file = "analise_handRelease_gen02_1percent.csv",
        out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_1percent_studyCase2/",
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2")

# ##### Use model gen02 (retrained with 1% days errors and sample of original database) using V2 cause V1 was bad #####
# auto_id(model_path = "~/Projectos/R_packages/recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_1percentV2_handRelease_05-0.9942.hdf5",
#         file_path = "~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/",
#         csv_file = "analise_handRelease_gen02_1percentV2.csv",
#         out_dir = "/home/bruno/Projectos/PHD/ms01/output_gen02_model_1percentV2_studyCase2/",
#         class_labels = class_labels,
#         save_png = F,
#         save_spec = T,
#         version = "v2")

