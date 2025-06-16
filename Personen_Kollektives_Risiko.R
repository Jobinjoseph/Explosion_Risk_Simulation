
# ---- Benötigte Biblothek ----
library("imager")
library("xlsx")
library("ggplot2")
library("reshape2")
library("png")
library("grid")
library("scales")
library("crayon")
library("circlize")
library("dplyr")
library("tidyr")
library("readxl")
library("ggforce")
library("sf")
library("leaflet")
library("writexl")
library("openxlsx")
library("purrr")
library("fs")
library("webshot")
library("htmlwidgets")
library("raster")
library(pheatmap)

#SimName = "UEV3"

# ---- Benötigte Pfade ----
setwd("xxx")
srcDir <- getwd()
root_directory <- srcDir# Pfad/Verbindung zu dem Ordner festlegen, in dem die Dateien gespeichert sind
Reference_directory <- paste0(srcDir,"/Reference_Dat")# Pfad/Verbindung zu dem Ordner festlegen, in dem die Referenzdateien gespeichert sind
file <- "Explosion_Risk_functions.R"
source(file.path(srcDir,file))
current_date <- Sys.Date()
formatted_date <- format(current_date, "%Y-%m-%d")
#formatted_date <- format(as.Date("2024-11-03"), "%Y-%m-%d")


# ---- lade benötigte CSV files ---- 
load_csv_files <- function(Reference_directory) {
  csv_files <- list.files(Reference_directory, pattern = "\\.csv$", full.names = TRUE)
  dataframes <- lapply(csv_files, function(file) {
    df_name <- tools::file_path_sans_ext(basename(file))
    if (file.exists(file)) {
      tryCatch(
        {
          first_line <- readLines(file, n = 1)
          if (grepl(",", first_line)) {
            separator <- ","
          } else if (grepl(";", first_line)) {
            separator <- ";"
          } else {
            separator <- ","
          }
          
          df <- read.csv(file, sep = separator)
          return(list(name = df_name, data = df))
        },
        error = function(e) {
          # If there's an error reading the file, skip it and print a message
          cat("Error reading file:", file, "\n")
          return(NULL)
        }
      )
    }
  })
  # Remove NULL entries (failed to read)
  dataframes <- Filter(function(x) !is.null(x), dataframes)
  return(dataframes)
}

# ---- Lade excel Blättern ----
read_excel_sheets <- function(Reference_directory) {
  excel_files <- list.files(Reference_directory, pattern = "\\.xlsx$", full.names = TRUE)
  all_dataframes <- list()
  
  for (excel_file in excel_files) {
    sheet_names <- excel_sheets(excel_file)
    excel_dataframes <- list()
    
    for (sheet_name in sheet_names) {
      df <- read_excel(excel_file, sheet = sheet_name)
      excel_dataframes[[sheet_name]] <- df
    }
    
    file_name <- tools::file_path_sans_ext(basename(excel_file))
    all_dataframes[[file_name]] <- excel_dataframes
  }
  
  return(all_dataframes)
}
all_excel_dataframes <- read_excel_sheets(Reference_directory)
WSUME_Lagerlist <- as.data.frame(all_excel_dataframes$xxxx)
Lager_reume <- WSUME_Lagerlist%>% pull(Lager_Gebaeude)



# ---- Für Drueckwelle berechnung nötig ----
matching_data_frames <- list()
pattern <- "^P_t_\\d+TNT$" 

loaded_data_frame_names <- ls() 

for (df_name in loaded_data_frame_names) {
  if (grepl(pattern, df_name)) {
    
    matching_data_frames[[df_name]] <- get(df_name)
  }
}

# ---- CSV zu DF ----
dataframes <- df_info_csv(all_dataframes)
all_dataframes <- list()
for (root in Reference_directory) {
  all_dataframes <- c(all_dataframes, load_csv_files(root))
}
df_info_csv <- function(all_dataframes) {
  individual_dataframes <- list()
  
  for (i in seq_along(all_dataframes)) {
    df_name <- all_dataframes[[i]]$name
    df_data <- all_dataframes[[i]]$data
    
    
    if (!is.null(df_name) && nchar(df_name) > 0) {
      individual_dataframes[[df_name]] <- df_data
    } else {
      print("Skipping data frame with empty or NULL name.")  # Debug output
    }
  }
  
  return(individual_dataframes)
}
# ---- CSV zu DF_II ----
all_csv_dataframes <- load_csv_files(Reference_directory)
data_frames_list <- list()

for (i in 1:length(all_csv_dataframes)){
  df <-  all_csv_dataframes[[i]]$data
  df_name <- all_csv_dataframes[[i]]$name
  assign(df_name, df)
  data_frames_list[[df_name]] <- df
}
# ---- Die am häufigsten verwendeten Referenzdaten enthalten Angaben über doe koordinaten, Die Gelagertmenge, Die Richtung der Türöffnungen, den Winkel, in dem die Wände zur Nord stehen usw.----
Magazine_Dat <- all_excel_dataframes[["Parameter_Truemmerwurf_5000_Kopie"]][["Magazine"]]
Magazine_Dat$Magazin_ALT <- gsub("\\.", "_", Magazine_Dat$Magazin_ALT)
View(Magazine_Dat)
Param_Dat <- Luft_St_Par
Mag_Dat_Exp_Peri_Mitte_üv_3_06_11_24$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_üv_3_06_11_24$Magazin_ALT)
Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$Magazin_ALT)
Mag_Dat_Exp_Peri_Mitte_üv_2_05_11_24$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_üv_2_05_11_24$Magazin_ALT)
Mag_Dat_Exp_Peri_Mitte_üv_1_24_10_24$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_üv_1_24_10_24$Magazin_ALT)
# View(Mag_Dat_Exp_Peri_Mitte_üv_1_24_10_24)
# View(Mag_Dat_Exp_Peri_Mitte_üv_2_05_11_24)
# View(Mag_Dat_Exp_Peri_Mitte_üv_3_06_11_24)
View(Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24)

Mag_Dat_Exp_Peri_Mitte_5000$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_5000$Magazin_ALT)
Mag_Dat_Exp_Peri_Mitte <- Mag_Dat_Exp_Peri_Mitte_5000

Mag_Dat_Exp_Peri_Mitte_5500$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_Peri_Mitte_5500$Magazin_ALT)


Mag_Dat_Exp_5500$Magazin_ALT <- gsub("\\.", "_", Mag_Dat_Exp_5500$Magazin_ALT)
sum(Mag_Dat_Exp_Peri_Mitte_5500$tNEQ[1:22])

# ---- Angaben über Praesenzfaktoren fuer Gebäude, Freifeld, Strassen, Bahn usw.----
Geb_1_PF <- all_excel_dataframes[["PF_Tabelle"]][["Geb_1"]]
Geb_2_PF <- all_excel_dataframes[["PF_Tabelle"]][["Geb_2"]]
FreiF_1_PF <- all_excel_dataframes[["PF_Tabelle"]][["FreiF_1"]]
FreiF_3_PF <- all_excel_dataframes[["PF_Tabelle"]][["FreiF_3"]]
Strasse_1_PF <- all_excel_dataframes[["PF_Tabelle"]][["Strasse_1"]]
Eisenbahn_1_PF <- all_excel_dataframes[["PF_Tabelle"]][["Eisenbahn_1"]]


# ---- Einlesen der übertragungsexcelliste ----
übertragung_RUAG_240830 <- all_excel_dataframes[["übertragung_RUAG_241111"]]

# ---- select specific sheet matching magazine name ----
find_sheet_by_folder_name <- function(übertragung_RUAG_240830, folder_name) {
  matched_sheets <- list()  
  
  for (sheet_name in names(übertragung_RUAG_240830)) {
    print(paste("Checking sheet:", sheet_name, "against folder name:", folder_name))
    
    if (grepl(folder_name, sheet_name, ignore.case = TRUE)) {
      matched_sheets[[sheet_name]] <- übertragung_RUAG_240830[[sheet_name]]
    }
  }
  
  return(matched_sheets)
}



####### Gebäude#######
#### Geb_dat.csv erstellen in alle Ordnern#####
###### Reduktionsfaktoren#####
#Wenn kein Wald vorhanden ist, gilt MA = 100%, wenn Wald in der Nähe ist (Beispiel: Engelrütti-Magazine), 
#gilt MA = 64%, wenn das Gebäude in die Kategorie "Normal" (K3) fällt, gilt MA = 100%,
# und wenn die Gebäudekategorie "Stabil" (K4) ist, gilt MA = 55%, und wenn die Gebäudekategorie "Superstabil" (K5) ist, gilt MA = 29%). 
#MA_reduk = MA*fwald*fGebäude
subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
for (subdirectory in subdirectories) {
  
  print(paste("Processing folder:", subdirectory))
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  match_index <- which(Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[, 1] == folder_name)
  
  
  if (length(match_index) == 0){
    print(paste("Skipping folder:", folder_name, "- No match found in dataset"))
    next
  }

  print(paste("Folder matched:", folder_name, "- Index:", match_index))
  f_wald <- 0.64
  f_geb_norm <- 1
  f_geb_stabil <- 0.55
  f_geb_super_stabil <- 0.29
  
  Mag_Cord_Dat <- data.frame(Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$Magazin_ALT,Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$xCH,Mag_Dat_Exp_Peri_Mitte_üv_3_06_11_24$yCH)
  xCH <- Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[match_index,2]
  yCH <- Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[match_index,3]
  Q <- Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[match_index, 4] / 1000
  Geb_V <- Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[match_index, 5]
  W_LKU <- Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24[match_index, 7]
  
  file_path <- file.path(Reference_directory, "Geb_dat_final.csv")
  if (file.exists(file_path)) {  
    first_line <- readLines(file_path, n = 1)
    if (grepl(",", first_line)) {
      separator <- ","
    } else if (grepl(";", first_line)) {
      separator <- ";"
    } else {
      separator <- ","
    }
  }else {
    print(paste("File not found:", file_path))
    next
  }
  
  Geb_dat <- read.csv(file_path, sep = separator)
  print(paste("Loaded data rows:", nrow(Geb_dat)))
  Geb_dat$MA <- numeric(nrow(Geb_dat))
  r_F <- numeric(nrow(Geb_dat))
  r_G <- numeric(nrow(Geb_dat))
  dx <- abs(xCH - Geb_dat$GKODEg)
  dy <- abs(yCH - Geb_dat$GKODNg)
  distance <- sqrt(dx^2 + dy^2)
  Geb_dat$distance <- distance
  Geb_dat$Q <- Q
  Geb_dat$W_LKU <- W_LKU
  Geb_dat$Geb_V <- Geb_V
  for (x in 1:nrow(Geb_dat)) {
    trummerwurf_kat <- Geb_dat$Truemmerwurf.Kat.[x]
    
    if (trummerwurf_kat == "K3") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_norm
    } else if (trummerwurf_kat == "K4") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_stabil
    } else if (trummerwurf_kat == "K5") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_super_stabil
    }
    
    if (grepl("^G085_", Mag_Dat_Exp_Peri_Mitte$Magazin_ALT[i])) {
      if (trummerwurf_kat == "K3") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      } else if (trummerwurf_kat == "K4") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      } else if (trummerwurf_kat == "K5") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      }
    }
        
  }

  base_factor <- 0.36 * Geb_dat$MA * Q^(-0.58)
  denominator <- -0.047 * Q^(-0.29)
  factors_ra <- c(0.4, 0.65, 1, 1, 1)
  factors_rb <- c(0.8, 1.3, 2, 2, 2)

  Geb_dat$ra_G1 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[1]
  Geb_dat$ra_G2 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[2]
  Geb_dat$ra_G3 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[3]
  Geb_dat$ra_G4 <- log(TrD_Zonen$Gebeude[4] / base_factor) / denominator * factors_ra[4]
  Geb_dat$ra_G5 <- log(TrD_Zonen$Gebeude[5] / base_factor) / denominator * factors_ra[5]
  
  Geb_dat$rb_G1 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[1]
  Geb_dat$rb_G2 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[2]
  Geb_dat$rb_G3 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[3]
  Geb_dat$rb_G4 <- log(TrD_Zonen$Gebeude[4] / base_factor) / denominator * factors_rb[4]
  Geb_dat$rb_G5 <- log(TrD_Zonen$Gebeude[5] / base_factor) / denominator * factors_rb[5]

  # Geb_dat$ra_F1 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_F2 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_F3 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_F4 <- log(TrD_Zonen$Freifeld[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_F5 <- log(TrD_Zonen$Freifeld[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_F1 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_F2 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_F3 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_F4 <- log(TrD_Zonen$Freifeld[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_F5 <- log(TrD_Zonen$Freifeld[5] / base_factor) / denominator * factors_rb[5]
  # 
  # Geb_dat$ra_A1 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_A2 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_A3 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_A4 <- log(TrD_Zonen$Auto[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_A5 <- log(TrD_Zonen$Auto[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_A1 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_A2 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_A3 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_A4 <- log(TrD_Zonen$Auto[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_A5 <- log(TrD_Zonen$Auto[5] / base_factor) / denominator * factors_rb[5]
  # 
  # 
  # Geb_dat$ra_B1 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_B2 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_B3 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_B4 <- log(TrD_Zonen$Bahn[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_B5 <- log(TrD_Zonen$Bahn[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_B1 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_B2 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_B3 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_B4 <- log(TrD_Zonen$Bahn[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_B5 <- log(TrD_Zonen$Bahn[5] / base_factor) / denominator * factors_rb[5]
  # 
  matching_E1_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E1_Ausr)
  matching_E2_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E2_Ausr)
  matching_E3_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E3_Ausr)
  matching_E4_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E4_Ausr)
  matching_E1_Ausr <- matching_E1_Ausr
  matching_E2_Ausr <- matching_E2_Ausr
  matching_E3_Ausr <- matching_E3_Ausr
  matching_E4_Ausr <- matching_E4_Ausr
  E1_Ausr_1 <- matching_E1_Ausr-0
  E2_Ausr_1 <- matching_E2_Ausr-0
  E3_Ausr_1 <- matching_E3_Ausr-0
  E4_Ausr_1 <- matching_E4_Ausr-0
  E1_Ausr_2 <- matching_E1_Ausr+45
  E2_Ausr_2 <- matching_E2_Ausr+45
  E3_Ausr_2 <- matching_E3_Ausr+45
  E4_Ausr_2 <- matching_E4_Ausr+45
  
  angle_matrix <- atan2(dy, dx) * (180 / pi)
  angle_matrix <- ifelse(angle_matrix < 0, angle_matrix + 360, angle_matrix)
  Geb_dat$lethality_component <- ifelse((angle_matrix >= E1_Ausr_1 & angle_matrix <= E1_Ausr_2) |
                                  (angle_matrix >= E2_Ausr_1 & angle_matrix <= E2_Ausr_2) |
                                  (angle_matrix >= E3_Ausr_1 & angle_matrix <= E3_Ausr_2) | 
                                  (angle_matrix >= E4_Ausr_1 & angle_matrix <= E4_Ausr_2),
                                1, 0)
  
  Druck_Wert <- select_QTNT_Karte(Q) %>%
    rename(distance = Distanz)
  find_closest_Pressure <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$Pressure[closest_index]
  }
  
  find_closest_ln_p <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$ln_p[closest_index]
  }
  find_closest_Pulse_incident <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$Pulse_incident[closest_index]
  }
  Geb_dat$ln_p <- sapply(distance, find_closest_ln_p)
  Geb_dat$Pressure <- sapply(distance, find_closest_Pressure)
  Geb_dat$Pulse_incident <- sapply(distance, find_closest_Pulse_incident)
  Geb_dat$ln_p[sapply(Geb_dat$ln_p, length) == 0] <- 0
  Geb_dat$Pressure[sapply(Geb_dat$Pressure, length) == 0] <- 0
  Geb_dat$Pulse_incident[sapply(Geb_dat$Pulse_incident, length) == 0] <- 0
  Geb_dat$ln_p <- sapply(Geb_dat$ln_p, as.numeric)
  Geb_dat$Pressure <- sapply(Geb_dat$Pressure, as.numeric)
  Geb_dat$Pulse_incident <- sapply(Geb_dat$Pulse_incident, as.numeric)
  
  # #Geb_dat$lethality_matrix_F <- Geb_dat$lethality_component * mapply(
  #   calculate_lethality_rb_F,
  #   distance = Geb_dat$distance,
  #   rb_F3 = Geb_dat$rb_F3,
  #   rb_F4 = Geb_dat$rb_F4,
  #   rb_F5 = Geb_dat$rb_F5
  # )
  
  # #Geb_dat$krater_let_F <-  mapply(
  #   calculate_lethality_ra_F,
  #   distance = Geb_dat$distance,
  #   ra_F3 = Geb_dat$ra_F3,
  #   ra_F4 = Geb_dat$ra_F4,
  #   ra_F5 = Geb_dat$ra_F5
  # )
  # 
  # #Geb_dat$Druck_Welle_Let_F <- pnorm(Param_Dat$Frei_Dr[1] * Geb_dat$ln_p + Param_Dat$Frei_Dr[3])
  Geb_dat$lethality_matrix_G <- Geb_dat$lethality_component * mapply(
    calculate_lethality_rb_G,
    distance = Geb_dat$distance,
    rb_G3 = Geb_dat$rb_G3,
    rb_G4 = Geb_dat$rb_G4,
    rb_G5 = Geb_dat$rb_G5
  )
  Geb_dat$krater_let_G <-mapply(
    calculate_lethality_ra_G,
    distance = Geb_dat$distance,
    ra_G3 = Geb_dat$ra_G3,
    ra_G4 = Geb_dat$ra_G4,
    ra_G5 = Geb_dat$ra_G5
  )

  Geb_dat<- update_Gis_Lager_TNT_max(Geb_dat, Param_Dat)
  P_kPa_max <- Geb_dat$Pressure * 100  ## [kPa]
  I_max <- Geb_dat$Pulse_incident
  Geb_dat$Fenster[sapply(Geb_dat$Fenster, length) == 0] <- 0
  Geb_dat$Let_Glass <- LetGlass_gross_TNT_max(P_kPa_max, I_max,Geb_dat)
  if (grepl("G063|G074|G058_6304081|G058_6304965", folder_name)) {
    print(paste("Folder:", folder_name, "matches group 1 (specific calculations)"))
    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- Geb_dat$Druck_Welle_Let_F
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- Geb_dat$lambda
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- Geb_dat$lambda
    
  } else if (grepl("G058_63014|G058_63015|G073_65736", folder_name)) {
    print(paste("Folder:", folder_name, "matches group 2 (specific calculations)"))
    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- 1 - ((1 - Geb_dat$krater_let_F) * (1 - Geb_dat$Druck_Welle_Let_F))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- 1 - ((1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- 1 - ((1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
  } else if (grepl("G085|G029|G034|G040|G046|G051", folder_name)){
    print(paste("Folder:", folder_name, "matches group 3 (specific calculations)"))

    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- 1 - ((1 - Geb_dat$lethality_matrix_F) * (1 - Geb_dat$krater_let_F) * (1 - Geb_dat$Druck_Welle_Let_F))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda)*(1-Geb_dat$Let_Glass))
  
  }else {
    print(paste("Folder:", folder_name, "does not match any group. Verify logic."))
  }
  file_names <- list(
    paste0("Alle_letalität_Mit_Kordinaten_", folder_name, "_", formatted_date, ".csv"))
  
  if (!dir.exists(subdirectory)) {
    print(paste("Creating directory:", subdirectory))
    dir.create(subdirectory, recursive = TRUE)
  }
  
  output_path <- file.path(subdirectory, file_names[[1]])
  print(paste("Writing output to:", output_path))
  write.csv(Geb_dat, output_path, row.names = FALSE)
  
  print(paste("Processing completed for folder:", folder_name))
}


# ---- Load the Gebäude lethality_matrix as per folder_name mit kordniatwerten ----
load_Geb_dat_kord <- function(folder_name) {
  csv_file <- paste0(subdirectory, "/", "Alle_letalität_Mit_Kordinaten_", 
                     folder_name, "_", formatted_date, ".csv")
  first_line <- readLines(csv_file, n = 1)
  if (grepl(";", first_line)) {
    delimiter <- ";"
  } else {
    delimiter <- ","
  }

  Geb_dat <- read.csv(csv_file, sep = delimiter)
  
  return(Geb_dat)
}

# ---- Load the Gebäude lethality_matrix as per magazin_values ----
load_Geb_dat_üKord <- function(folder_name) {
  csv_file <- paste0(root_directory, "/", folder_name, "/", 
                     "Alle_letalität_Mit_Kordinaten_", folder_name, "_", formatted_date, ".csv")

  first_line <- readLines(csv_file, n = 1)
  if (grepl(";", first_line)) {
    delimiter <- ";"
  } else {
    delimiter <- ","
  }

  Geb_dat <- read.csv(csv_file, sep = delimiter)
  
  return(Geb_dat)
}






#---- Gebäude -----

# ---- loop für berechnung des Individuelles Risikosüv3 für UD, DB, IB im Gebäude ohne übertragung nur Produktions und Laborräumen ----
subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("G0\\d{2}_\\d{3,}", subdirectories, value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
for (subdirectory in subdirectories) {
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  Geb_dat <- load_Geb_dat_kord(folder_name)
  Geb_dat$SD_N <- 0.4405
  Geb_dat$SD_A <- 0.2679
  Geb_dat$SD_F <- 0.1488
  Geb_dat$SD_W <- 0.1428
  Geb_dat$PF_A <- 0.9
  Geb_dat$PF_F <- 0.9
  Geb_dat$PF_W <- 0.9
  Geb_dat$PF_N <- 1
  # Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] * 0.01
  Geb_dat$GZ_A <- Geb_dat$SD_A*Geb_dat$PF_A
  Geb_dat$GZ_N <- Geb_dat$SD_N*Geb_dat$PF_N
  Geb_dat$GZ_F <- Geb_dat$SD_F*Geb_dat$PF_F
  Geb_dat$GZ_W <- Geb_dat$SD_W*Geb_dat$PF_W
  Geb_dat$W_LKU_Base <-Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$W_LKU[Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$Magazin_ALT == folder_name]
  # Ereignis ohne Übertragungen während der Arebeitszeit
  Geb_dat$Keine_uebertragung_IR_A <- Geb_dat$W_LKU_Base*Geb_dat$SD_A*Geb_dat$GZ_A*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Nacht
  Geb_dat$Keine_uebertragung_IR_N <- Geb_dat$W_LKU_Base*Geb_dat$SD_N*Geb_dat$GZ_N*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Freizeit
  Geb_dat$Keine_uebertragung_IR_F <- Geb_dat$W_LKU_Base*Geb_dat$SD_F*Geb_dat$GZ_F*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Wochenende
  Geb_dat$Keine_uebertragung_IR_W <- Geb_dat$W_LKU_Base*Geb_dat$SD_W*Geb_dat$GZ_W*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der ganze Woche
  Geb_dat$Keine_uebertragung_IR <- Geb_dat$Keine_uebertragung_IR_A+ Geb_dat$Keine_uebertragung_IR_N+ Geb_dat$Keine_uebertragung_IR_F+ Geb_dat$Keine_uebertragung_IR_W
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path_IR <- file.path(subdirectory, file_name)
  write.csv(Geb_dat, file_path_IR, row.names = FALSE)
  print(paste("Saved Keine_übertragung_IR to:", file_path_IR))
}



# ---- loop für berechnung des Individuelles Risikos für UD, DB, IB  im Gebäude mit übertragung mit Koordinat werten ----
subdirectories <- grep("^.*G085_", list.dirs(srcDir, recursive = FALSE), value = TRUE)

for (subdirectory in subdirectories) {
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  matched_sheets <- find_sheet_by_folder_name(übertragung_RUAG_240830, folder_name)
  Geb_dat <- load_Geb_dat_kord(folder_name)
  magazin_values <- matched_sheets[[1]]$Magazin_ALT 
  Wü_keine_übertragung <- (1-sum(matched_sheets[[1]]$Wü))
  Wü_values <- matched_sheets[[1]]$Wü  
  print(paste("Processing matrix for:", magazin_values[1], "with Wü_values[1]:", Wü_values[1]))
  # Lethalität bei Übertragung auf 1 Magazin
  i <- 1
  Geb_dat_1 <- load_Geb_dat_üKord(magazin_values[1])
  Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[1]
  print(paste("Processing matrix for:", magazin_values[1], "with Wü_values[1]:", Wü_values[1]))
  
  # Lethalität bei Übertragung auf 2 Magazine: direkt oder indirekt
  i <- 2
  Geb_dat_2 <- load_Geb_dat_üKord(magazin_values[2])
  Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[2]
  print(paste("Processing matrix for:", magazin_values[2], "with Wü_values[2]:", Wü_values[2]))
  print(paste("Length of magazin_values:", length(magazin_values)))
  # Lethalität bei Übertragung auf 3 oder 4 Magazine: direkt oder indirekt
  if (length(magazin_values) ==3) {
    i <- 3
    print(paste("Processing matrix for:", magazin_values[3]))
    Geb_dat_3 <- load_Geb_dat_üKord(magazin_values[3])
    Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[3]
    print(paste("Processing matrix for:", magazin_values[3], "with Wü_values[3]:", Wü_values[3]))
  } else if (length(magazin_values) == 4) {
    print(paste("Processing matrix for:", magazin_values[3]))
    i <- 3
    Geb_dat_3 <- load_Geb_dat_üKord(magazin_values[3])
    i <- 4
    Geb_dat_4 <- load_Geb_dat_üKord(magazin_values[4])
    Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[3]
    Geb_dat$ubtg_Let <- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_4$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[4]
    print(paste("Processing matrix for:", magazin_values[4], "with Wü_values[4]:", Wü_values[4]))
  }
  # file_name <- paste0("Gebäude_übertragung_matrix_Let_", folder_name, "_", formatted_date, ".csv")
  # file_path_Let <- file.path(subdirectory, file_name)
  # write.csv(new_lethality_matrix, file_path_Let, row.names = FALSE)
  Geb_dat$SD_N <- 0.4405
  Geb_dat$SD_A <- 0.2679
  Geb_dat$SD_F <- 0.1488
  Geb_dat$SD_W <- 0.1428
  Geb_dat$PF_A <- 0.9
  Geb_dat$PF_F <- 0.9
  Geb_dat$PF_W <- 0.9
  Geb_dat$PF_N <- 1
  # Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] * 0.01
  Geb_dat$GZ_A <- Geb_dat$SD_A*Geb_dat$PF_A
  Geb_dat$GZ_N <- Geb_dat$SD_N*Geb_dat$PF_N
  Geb_dat$GZ_F <- Geb_dat$SD_F*Geb_dat$PF_F
  Geb_dat$GZ_W <- Geb_dat$SD_W*Geb_dat$PF_W
  Geb_dat$W_LKU_Base <-Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$W_LKU[Mag_Dat_Exp_Peri_Mitte_üv_4_11_11_24$Magazin_ALT == folder_name]
  # Ereignis ohne Übertragungen während der Arebeitszeit
  Geb_dat$Keine_uebertragung_IR_A <- Geb_dat$W_LKU_Base*Geb_dat$SD_A*Geb_dat$GZ_A*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Nacht
  Geb_dat$Keine_uebertragung_IR_N <- Geb_dat$W_LKU_Base*Geb_dat$SD_N*Geb_dat$GZ_N*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Freizeit
  Geb_dat$Keine_uebertragung_IR_F <- Geb_dat$W_LKU_Base*Geb_dat$SD_F*Geb_dat$GZ_F*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Wochenende
  Geb_dat$Keine_uebertragung_IR_W <- Geb_dat$W_LKU_Base*Geb_dat$SD_W*Geb_dat$GZ_W*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der ganze Woche
  Geb_dat$Keine_uebertragung_IR <- Geb_dat$Keine_uebertragung_IR_A+ Geb_dat$Keine_uebertragung_IR_N+ Geb_dat$Keine_uebertragung_IR_F+ Geb_dat$Keine_uebertragung_IR_W
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path_IR <- file.path(subdirectory, file_name)
  write.csv(Geb_dat, file_path_IR, row.names = FALSE)
  print(paste("Saved Keine_übertragung_IR to:", file_path_IR))
}



# ---- Kumulative Individuelles Risikoüv4  für UD im Gebäude mit koordinatwerte ----
subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("G0\\d{2}_\\d{3,}", subdirectories, value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
Koordinatwerten <- read.csv(paste0(srcDir,"/Reference_Dat/Koordinatwerten.csv"), sep = ";")

result_df <- as.data.frame(Koordinatwerten)

IB_columns <- list()
UD_columns <- list()

for (subdirectory in subdirectories) {
  folder_name <- basename(subdirectory)
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path <- file.path(subdirectory, file_name)
  print(paste("Using csv file for accumulation:", file_path))

  Individual_Risk_csv <- read.csv(file_path)
  IB_column_name <- paste0(folder_name, "_IB")
  UD_column_name <- paste0(folder_name, "_UD")
  
  IB_columns[[IB_column_name]] <- Individual_Risk_csv$Keine_uebertragung_IR_A
  UD_columns[[UD_column_name]] <- Individual_Risk_csv$Keine_uebertragung_IR
}

IB_matrix <- do.call(cbind, IB_columns)
UD_matrix <- do.call(cbind, UD_columns)

result_df <- cbind(result_df, IB_matrix, UD_matrix)

result_df$IB_Total <- rowSums(IB_matrix, na.rm = TRUE)
result_df$UD_Total <- rowSums(UD_matrix, na.rm = TRUE)

result_df <- as.data.frame(result_df)
colnames(result_df)
print(head(result_df))
View(result_df)
View(UD_matrix)

write.csv(result_df, "xxxx.csv", row.names = FALSE)

ggplot(result_df, aes(x = GKODEg, y = GKODNg, color = UD_Total)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "IR_UD Value Distribution", x = "GKODEg (Longitude)", y = "GKODNg (Latitude)", color = "IR_UD") +
  theme_minimal()

View(result_df)

filtered_df <- result_df %>%
  arrange(desc(UD_Total)) %>%   
  slice_head(n = 50)       
filtered_df <- filtered_df %>%
  mutate(color_group = ifelse(UD_Total >= 3e-5, "red", "viridis"))
filtered_df$formatted_UD_Total <- sprintf("%.0e", filtered_df$UD_Total)
ggplot(filtered_df, aes(x = GKODEg, y = GKODNg)) +
  geom_point(aes(color = color_group), size = 2) +
  scale_color_manual(values = c("red" = "red", "viridis" = viridis::viridis(100)[1])) +
  geom_text(aes(label = formatted_UD_Total),  
            hjust = 0.5, vjust = -0.5, size = 3, color = "black") + 
  labs(title = "UD_Total",
       x = "GKODEg", 
       y = "GKODNg", 
       color = "UD_Total") +
  theme_minimal()



#---- Alte belegung IR alle Gebäuden mit koordinatwerten IR berechnung für Bericht 16.12.2024 ----
####### Gebäude#######
#### Geb_dat.csv erstellen in alle Ordnern#####
###### Reduktionsfaktoren#####

subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
for (subdirectory in subdirectories) {
  
  print(paste("Processing folder:", subdirectory))
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  match_index <- which(Mag_Dat_Exp_5500[, 1] == folder_name)
  
  
  if (length(match_index) == 0){
    print(paste("Skipping folder:", folder_name, "- No match found in dataset"))
    next
  }
  
  print(paste("Folder matched:", folder_name, "- Index:", match_index))
  f_wald <- 0.64
  f_geb_norm <- 1
  f_geb_stabil <- 0.55
  f_geb_super_stabil <- 0.29
  
  Mag_Cord_Dat <- data.frame(Mag_Dat_Exp_5500$Magazin_ALT,Mag_Dat_Exp_5500$xCH,Mag_Dat_Exp_5500$yCH)
  xCH <- Mag_Dat_Exp_5500[match_index,2]
  yCH <- Mag_Dat_Exp_5500[match_index,3]
  Q <- Mag_Dat_Exp_5500[match_index, 4] / 1000
  Geb_V <- Mag_Dat_Exp_5500[match_index, 5]
  W_LKU <- Mag_Dat_Exp_5500[match_index, 7]
  
  file_path <- file.path(Reference_directory, "Geb_dat_final.csv")
  if (file.exists(file_path)) {  
    first_line <- readLines(file_path, n = 1)
    if (grepl(",", first_line)) {
      separator <- ","
    } else if (grepl(";", first_line)) {
      separator <- ";"
    } else {
      separator <- ","
    }
  }else {
    print(paste("File not found:", file_path))
    next
  }
  
  Geb_dat <- read.csv(file_path, sep = separator)
  print(paste("Loaded data rows:", nrow(Geb_dat)))
  Geb_dat$MA <- numeric(nrow(Geb_dat))
  r_F <- numeric(nrow(Geb_dat))
  r_G <- numeric(nrow(Geb_dat))
  dx <- abs(xCH - Geb_dat$GKODEg)
  dy <- abs(yCH - Geb_dat$GKODNg)
  distance <- sqrt(dx^2 + dy^2)
  Geb_dat$distance <- distance
  Geb_dat$Q <- Q
  Geb_dat$W_LKU <- W_LKU
  Geb_dat$Geb_V <- Geb_V
  for (x in 1:nrow(Geb_dat)) {
    trummerwurf_kat <- Geb_dat$Truemmerwurf.Kat.[x]
    
    if (trummerwurf_kat == "K3") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_norm
    } else if (trummerwurf_kat == "K4") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_stabil
    } else if (trummerwurf_kat == "K5") {
      Geb_dat$MA[x] <- (100 * Q + Geb_V * 0.5) * f_geb_super_stabil
    }
    
    if (grepl("^G085_", Mag_Dat_Exp_Peri_Mitte$Magazin_ALT[i])) {
      if (trummerwurf_kat == "K3") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      } else if (trummerwurf_kat == "K4") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      } else if (trummerwurf_kat == "K5") {
        Geb_dat$MA[x] <- Geb_dat$MA[x] * f_wald 
      }
    }
    
  }
  
  base_factor <- 0.36 * Geb_dat$MA * Q^(-0.58)
  denominator <- -0.047 * Q^(-0.29)
  factors_ra <- c(0.4, 0.65, 1, 1, 1)
  factors_rb <- c(0.8, 1.3, 2, 2, 2)
  
  Geb_dat$ra_G1 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[1]
  Geb_dat$ra_G2 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[2]
  Geb_dat$ra_G3 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_ra[3]
  Geb_dat$ra_G4 <- log(TrD_Zonen$Gebeude[4] / base_factor) / denominator * factors_ra[4]
  Geb_dat$ra_G5 <- log(TrD_Zonen$Gebeude[5] / base_factor) / denominator * factors_ra[5]
  
  Geb_dat$rb_G1 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[1]
  Geb_dat$rb_G2 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[2]
  Geb_dat$rb_G3 <- log(TrD_Zonen$Gebeude[3] / base_factor) / denominator * factors_rb[3]
  Geb_dat$rb_G4 <- log(TrD_Zonen$Gebeude[4] / base_factor) / denominator * factors_rb[4]
  Geb_dat$rb_G5 <- log(TrD_Zonen$Gebeude[5] / base_factor) / denominator * factors_rb[5]
  
  # Geb_dat$ra_F1 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_F2 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_F3 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_F4 <- log(TrD_Zonen$Freifeld[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_F5 <- log(TrD_Zonen$Freifeld[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_F1 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_F2 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_F3 <- log(TrD_Zonen$Freifeld[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_F4 <- log(TrD_Zonen$Freifeld[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_F5 <- log(TrD_Zonen$Freifeld[5] / base_factor) / denominator * factors_rb[5]
  # 
  # Geb_dat$ra_A1 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_A2 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_A3 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_A4 <- log(TrD_Zonen$Auto[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_A5 <- log(TrD_Zonen$Auto[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_A1 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_A2 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_A3 <- log(TrD_Zonen$Auto[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_A4 <- log(TrD_Zonen$Auto[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_A5 <- log(TrD_Zonen$Auto[5] / base_factor) / denominator * factors_rb[5]
  # 
  # 
  # Geb_dat$ra_B1 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[1]
  # Geb_dat$ra_B2 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[2]
  # Geb_dat$ra_B3 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_ra[3]
  # Geb_dat$ra_B4 <- log(TrD_Zonen$Bahn[4] / base_factor) / denominator * factors_ra[4]
  # Geb_dat$ra_B5 <- log(TrD_Zonen$Bahn[5] / base_factor) / denominator * factors_ra[5]
  # 
  # Geb_dat$rb_B1 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[1]
  # Geb_dat$rb_B2 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[2]
  # Geb_dat$rb_B3 <- log(TrD_Zonen$Bahn[3] / base_factor) / denominator * factors_rb[3]
  # Geb_dat$rb_B4 <- log(TrD_Zonen$Bahn[4] / base_factor) / denominator * factors_rb[4]
  # Geb_dat$rb_B5 <- log(TrD_Zonen$Bahn[5] / base_factor) / denominator * factors_rb[5]
  # 
  matching_E1_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E1_Ausr)
  matching_E2_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E2_Ausr)
  matching_E3_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E3_Ausr)
  matching_E4_Ausr <- Magazine_Dat %>% 
    filter(Magazin_ALT == folder_name) %>%
    pull(E4_Ausr)
  matching_E1_Ausr <- matching_E1_Ausr
  matching_E2_Ausr <- matching_E2_Ausr
  matching_E3_Ausr <- matching_E3_Ausr
  matching_E4_Ausr <- matching_E4_Ausr
  E1_Ausr_1 <- matching_E1_Ausr-0
  E2_Ausr_1 <- matching_E2_Ausr-0
  E3_Ausr_1 <- matching_E3_Ausr-0
  E4_Ausr_1 <- matching_E4_Ausr-0
  E1_Ausr_2 <- matching_E1_Ausr+45
  E2_Ausr_2 <- matching_E2_Ausr+45
  E3_Ausr_2 <- matching_E3_Ausr+45
  E4_Ausr_2 <- matching_E4_Ausr+45
  
  angle_matrix <- atan2(dy, dx) * (180 / pi)
  angle_matrix <- ifelse(angle_matrix < 0, angle_matrix + 360, angle_matrix)
  Geb_dat$lethality_component <- ifelse((angle_matrix >= E1_Ausr_1 & angle_matrix <= E1_Ausr_2) |
                                          (angle_matrix >= E2_Ausr_1 & angle_matrix <= E2_Ausr_2) |
                                          (angle_matrix >= E3_Ausr_1 & angle_matrix <= E3_Ausr_2) | 
                                          (angle_matrix >= E4_Ausr_1 & angle_matrix <= E4_Ausr_2),
                                        1, 0)
  
  Druck_Wert <- select_QTNT_Karte(Q) %>%
    rename(distance = Distanz)
  find_closest_Pressure <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$Pressure[closest_index]
  }
  
  find_closest_ln_p <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$ln_p[closest_index]
  }
  find_closest_Pulse_incident <- function(d) {
    closest_index <- which.min(abs(Druck_Wert$distance - d))
    Druck_Wert$Pulse_incident[closest_index]
  }
  Geb_dat$ln_p <- sapply(distance, find_closest_ln_p)
  Geb_dat$Pressure <- sapply(distance, find_closest_Pressure)
  Geb_dat$Pulse_incident <- sapply(distance, find_closest_Pulse_incident)
  Geb_dat$ln_p[sapply(Geb_dat$ln_p, length) == 0] <- 0
  Geb_dat$Pressure[sapply(Geb_dat$Pressure, length) == 0] <- 0
  Geb_dat$Pulse_incident[sapply(Geb_dat$Pulse_incident, length) == 0] <- 0
  Geb_dat$ln_p <- sapply(Geb_dat$ln_p, as.numeric)
  Geb_dat$Pressure <- sapply(Geb_dat$Pressure, as.numeric)
  Geb_dat$Pulse_incident <- sapply(Geb_dat$Pulse_incident, as.numeric)
  
  # #Geb_dat$lethality_matrix_F <- Geb_dat$lethality_component * mapply(
  #   calculate_lethality_rb_F,
  #   distance = Geb_dat$distance,
  #   rb_F3 = Geb_dat$rb_F3,
  #   rb_F4 = Geb_dat$rb_F4,
  #   rb_F5 = Geb_dat$rb_F5
  # )
  
  # #Geb_dat$krater_let_F <-  mapply(
  #   calculate_lethality_ra_F,
  #   distance = Geb_dat$distance,
  #   ra_F3 = Geb_dat$ra_F3,
  #   ra_F4 = Geb_dat$ra_F4,
  #   ra_F5 = Geb_dat$ra_F5
  # )
  # 
  # #Geb_dat$Druck_Welle_Let_F <- pnorm(Param_Dat$Frei_Dr[1] * Geb_dat$ln_p + Param_Dat$Frei_Dr[3])
  Geb_dat$lethality_matrix_G <- Geb_dat$lethality_component * mapply(
    calculate_lethality_rb_G,
    distance = Geb_dat$distance,
    rb_G3 = Geb_dat$rb_G3,
    rb_G4 = Geb_dat$rb_G4,
    rb_G5 = Geb_dat$rb_G5
  )
  Geb_dat$krater_let_G <-mapply(
    calculate_lethality_ra_G,
    distance = Geb_dat$distance,
    ra_G3 = Geb_dat$ra_G3,
    ra_G4 = Geb_dat$ra_G4,
    ra_G5 = Geb_dat$ra_G5
  )
  
  Geb_dat<- update_Gis_Lager_TNT_max(Geb_dat, Param_Dat)
  P_kPa_max <- Geb_dat$Pressure * 100  ## [kPa]
  I_max <- Geb_dat$Pulse_incident
  Geb_dat$Fenster[sapply(Geb_dat$Fenster, length) == 0] <- 0
  Geb_dat$Let_Glass <- LetGlass_gross_TNT_max(P_kPa_max, I_max,Geb_dat)
  if (grepl("G063|G074|G058_6304081|G058_6304965", folder_name)) {
    print(paste("Folder:", folder_name, "matches group 1 (specific calculations)"))
    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- Geb_dat$Druck_Welle_Let_F
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda)*(1-Geb_dat$Let_Glass))
    
  } else if (grepl("G058_63014|G058_63015|G073_65736", folder_name)) {
    print(paste("Folder:", folder_name, "matches group 2 (specific calculations)"))
    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- 1 - ((1 - Geb_dat$krater_let_F) * (1 - Geb_dat$Druck_Welle_Let_F))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda)*(1-Geb_dat$Let_Glass))
  } else if (grepl("G085|G029|G034|G040|G046|G051", folder_name)){
    print(paste("Folder:", folder_name, "matches group 3 (specific calculations)"))
    
    #Geb_dat$lethality_Krat_Wand_Druck_Welle_F <- 1 - ((1 - Geb_dat$lethality_matrix_F) * (1 - Geb_dat$krater_let_F) * (1 - Geb_dat$Druck_Welle_Let_F))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda))
    Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass <- 1 - ((1 - Geb_dat$lethality_matrix_G) * (1 - Geb_dat$krater_let_G) * (1 - Geb_dat$lambda)*(1-Geb_dat$Let_Glass))
    
  }else {
    print(paste("Folder:", folder_name, "does not match any group. Verify logic."))
  }
  file_names <- list(
    paste0("Alle_letalität_Mit_Kordinaten_", folder_name, "_", formatted_date, ".csv"))
  
  if (!dir.exists(subdirectory)) {
    print(paste("Creating directory:", subdirectory))
    dir.create(subdirectory, recursive = TRUE)
  }
  
  output_path <- file.path(subdirectory, file_names[[1]])
  print(paste("Writing output to:", output_path))
  write.csv(Geb_dat, output_path, row.names = FALSE)
  
  print(paste("Processing completed for folder:", folder_name))
}


# ---- Load the Gebäude lethality_matrix as per folder_name mit kordniatwerten ----
load_Geb_dat_kord <- function(folder_name) {
  csv_file <- paste0(subdirectory, "/", "Alle_letalität_Mit_Kordinaten_", 
                     folder_name, "_", formatted_date, ".csv")
  first_line <- readLines(csv_file, n = 1)
  if (grepl(";", first_line)) {
    delimiter <- ";"
  } else {
    delimiter <- ","
  }
  
  Geb_dat <- read.csv(csv_file, sep = delimiter)
  
  return(Geb_dat)
}

# ---- Load the Gebäude lethality_matrix as per magazin_values ----
load_Geb_dat_üKord <- function(folder_name) {
  csv_file <- paste0(root_directory, "/", folder_name, "/", 
                     "Alle_letalität_Mit_Kordinaten_", folder_name, "_", formatted_date, ".csv")
  
  first_line <- readLines(csv_file, n = 1)
  if (grepl(";", first_line)) {
    delimiter <- ";"
  } else {
    delimiter <- ","
  }
  
  Geb_dat <- read.csv(csv_file, sep = delimiter)
  
  return(Geb_dat)
}






#---- Gebäude -----

# ---- loop für berechnung des Individuelles Risikosüv3 für UD, DB, IB im Gebäude ohne übertragung nur Produktions und Laborräumen ----
subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("G0\\d{2}_\\d{3,}", subdirectories, value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
for (subdirectory in subdirectories) {
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  Geb_dat <- load_Geb_dat_kord(folder_name)
  Geb_dat$SD_N <- 0.4405
  Geb_dat$SD_A <- 0.2679
  Geb_dat$SD_F <- 0.1488
  Geb_dat$SD_W <- 0.1428
  Geb_dat$PF_A <- 0.9
  Geb_dat$PF_F <- 0.9
  Geb_dat$PF_W <- 0.9
  Geb_dat$PF_N <- 1
  # Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] * 0.01
  Geb_dat$GZ_A <- Geb_dat$SD_A*Geb_dat$PF_A
  Geb_dat$GZ_N <- Geb_dat$SD_N*Geb_dat$PF_N
  Geb_dat$GZ_F <- Geb_dat$SD_F*Geb_dat$PF_F
  Geb_dat$GZ_W <- Geb_dat$SD_W*Geb_dat$PF_W
  Geb_dat$W_LKU_Base <-Mag_Dat_Exp_5500$W_LKU[Mag_Dat_Exp_5500$Magazin_ALT == folder_name]
  # Ereignis ohne Übertragungen während der Arebeitszeit
  Geb_dat$Keine_uebertragung_IR_A <- Geb_dat$W_LKU_Base*Geb_dat$SD_A*Geb_dat$GZ_A*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Nacht
  Geb_dat$Keine_uebertragung_IR_N <- Geb_dat$W_LKU_Base*Geb_dat$SD_N*Geb_dat$GZ_N*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Freizeit
  Geb_dat$Keine_uebertragung_IR_F <- Geb_dat$W_LKU_Base*Geb_dat$SD_F*Geb_dat$GZ_F*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der Wochenende
  Geb_dat$Keine_uebertragung_IR_W <- Geb_dat$W_LKU_Base*Geb_dat$SD_W*Geb_dat$GZ_W*Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass
  # Ereignis ohne Übertragungen während der ganze Woche
  Geb_dat$Keine_uebertragung_IR <- Geb_dat$Keine_uebertragung_IR_A+ Geb_dat$Keine_uebertragung_IR_N+ Geb_dat$Keine_uebertragung_IR_F+ Geb_dat$Keine_uebertragung_IR_W
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path_IR <- file.path(subdirectory, file_name)
  write.csv(Geb_dat, file_path_IR, row.names = FALSE)
  print(paste("Saved Keine_übertragung_IR to:", file_path_IR))
}



# ---- loop für berechnung des Individuelles Risikos für UD, DB, IB  im Gebäude mit übertragung mit Koordinat werten ----
subdirectories <- grep("^.*G085_", list.dirs(srcDir, recursive = FALSE), value = TRUE)

for (subdirectory in subdirectories) {
  folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
  matched_sheets <- find_sheet_by_folder_name(übertragung_RUAG_240830, folder_name)
  Geb_dat <- load_Geb_dat_kord(folder_name)
  magazin_values <- matched_sheets[[1]]$Magazin_ALT 
  Wü_keine_übertragung <- (1-sum(matched_sheets[[1]]$Wü))
  Wü_values <- matched_sheets[[1]]$Wü  
  print(paste("Processing matrix for:", magazin_values[1], "with Wü_values[1]:", Wü_values[1]))
  # Lethalität bei Übertragung auf 1 Magazin
  i <- 1
  Geb_dat_1 <- load_Geb_dat_üKord(magazin_values[1])
  Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[1]
  print(paste("Processing matrix for:", magazin_values[1], "with Wü_values[1]:", Wü_values[1]))
  
  # Lethalität bei Übertragung auf 2 Magazine: direkt oder indirekt
  i <- 2
  Geb_dat_2 <- load_Geb_dat_üKord(magazin_values[2])
  Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[2]
  print(paste("Processing matrix for:", magazin_values[2], "with Wü_values[2]:", Wü_values[2]))
  print(paste("Length of magazin_values:", length(magazin_values)))
  # Lethalität bei Übertragung auf 3 oder 4 Magazine: direkt oder indirekt
  if (length(magazin_values) ==3) {
    i <- 3
    print(paste("Processing matrix for:", magazin_values[3]))
    Geb_dat_3 <- load_Geb_dat_üKord(magazin_values[3])
    Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[3]
    print(paste("Processing matrix for:", magazin_values[3], "with Wü_values[3]:", Wü_values[3]))
  } else if (length(magazin_values) == 4) {
    print(paste("Processing matrix for:", magazin_values[3]))
    i <- 3
    Geb_dat_3 <- load_Geb_dat_üKord(magazin_values[3])
    i <- 4
    Geb_dat_4 <- load_Geb_dat_üKord(magazin_values[4])
    Geb_dat$ubtg_Let<- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[3]
    Geb_dat$ubtg_Let <- (1-(1-Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)*(1-Geb_dat_1$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_2$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_3$lethality_Krat_Wand_Druck_Welle_G)*(1-Geb_dat_4$lethality_Krat_Wand_Druck_Welle_G))*Wü_values[4]
    print(paste("Processing matrix for:", magazin_values[4], "with Wü_values[4]:", Wü_values[4]))
  }
  # file_name <- paste0("Gebäude_übertragung_matrix_Let_", folder_name, "_", formatted_date, ".csv")
  # file_path_Let <- file.path(subdirectory, file_name)
  # write.csv(new_lethality_matrix, file_path_Let, row.names = FALSE)
  Geb_dat$SD_N <- 0.4405
  Geb_dat$SD_A <- 0.2679
  Geb_dat$SD_F <- 0.1488
  Geb_dat$SD_W <- 0.1428
  Geb_dat$PF_A <- 0.9
  Geb_dat$PF_F <- 0.9
  Geb_dat$PF_W <- 0.9
  Geb_dat$PF_N <- 1
  # Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_A[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_F[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_W[Geb_dat$Nutzung == "Lager"] * 0.01
  # Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] <- Geb_dat$PF_N[Geb_dat$Nutzung == "Lager"] * 0.01
  Geb_dat$GZ_A <- Geb_dat$SD_A*Geb_dat$PF_A
  Geb_dat$GZ_N <- Geb_dat$SD_N*Geb_dat$PF_N
  Geb_dat$GZ_F <- Geb_dat$SD_F*Geb_dat$PF_F
  Geb_dat$GZ_W <- Geb_dat$SD_W*Geb_dat$PF_W
  Geb_dat$W_LKU_Base <-Mag_Dat_Exp_5500$W_LKU[Mag_Dat_Exp_5500$Magazin_ALT == folder_name]
  # Ereignis ohne Übertragungen während der Arebeitszeit
  Geb_dat$Keine_uebertragung_IR_A <- Geb_dat$W_LKU_Base*Geb_dat$SD_A*Geb_dat$GZ_A*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Nacht
  Geb_dat$Keine_uebertragung_IR_N <- Geb_dat$W_LKU_Base*Geb_dat$SD_N*Geb_dat$GZ_N*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Freizeit
  Geb_dat$Keine_uebertragung_IR_F <- Geb_dat$W_LKU_Base*Geb_dat$SD_F*Geb_dat$GZ_F*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der Wochenende
  Geb_dat$Keine_uebertragung_IR_W <- Geb_dat$W_LKU_Base*Geb_dat$SD_W*Geb_dat$GZ_W*(Geb_dat$ubtg_Let+Geb_dat$lethality_Krat_Wand_Druck_Welle_G_Mit_Glass)
  # Ereignis ohne Übertragungen während der ganze Woche
  Geb_dat$Keine_uebertragung_IR <- Geb_dat$Keine_uebertragung_IR_A+ Geb_dat$Keine_uebertragung_IR_N+ Geb_dat$Keine_uebertragung_IR_F+ Geb_dat$Keine_uebertragung_IR_W
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path_IR <- file.path(subdirectory, file_name)
  write.csv(Geb_dat, file_path_IR, row.names = FALSE)
  print(paste("Saved Keine_übertragung_IR to:", file_path_IR))
}



# ---- Kumulative Individuelles Risikoüv4  für UD im Gebäude mit koordinatwerte ----
subdirectories <- grep("^.*G0", list.dirs(srcDir, recursive = FALSE), value = TRUE)
subdirectories <- grep("G0\\d{2}_\\d{3,}", subdirectories, value = TRUE)
subdirectories <- grep("^.*G0(?!46)", subdirectories, value = TRUE, perl = TRUE)
subdirectories <- grep("^.*G0(?!29)", subdirectories, value = TRUE, perl = TRUE)
subdirectories
Koordinatwerten <- read.csv(paste0(srcDir,"/Reference_Dat/Koordinatwerten.csv"), sep = ";")

result_df <- as.data.frame(Koordinatwerten)

IB_columns <- list()
UD_columns <- list()

for (subdirectory in subdirectories) {
  folder_name <- basename(subdirectory)
  file_name <- paste0("Geb_dat_", folder_name, "_", formatted_date, ".csv")
  file_path <- file.path(subdirectory, file_name)
  print(paste("Using csv file for accumulation:", file_path))
  
  Individual_Risk_csv <- read.csv(file_path)
  IB_column_name <- paste0(folder_name, "_IB")
  UD_column_name <- paste0(folder_name, "_UD")
  
  IB_columns[[IB_column_name]] <- Individual_Risk_csv$Keine_uebertragung_IR_A
  UD_columns[[UD_column_name]] <- Individual_Risk_csv$Keine_uebertragung_IR
}

IB_matrix <- do.call(cbind, IB_columns)
UD_matrix <- do.call(cbind, UD_columns)

result_df <- cbind(result_df, IB_matrix, UD_matrix)

result_df$IB_Total <- rowSums(IB_matrix, na.rm = TRUE)
result_df$UD_Total <- rowSums(UD_matrix, na.rm = TRUE)

result_df <- as.data.frame(result_df)
colnames(result_df)
print(head(result_df))
View(result_df)
View(UD_matrix)

write.csv(result_df, "xxxxx.csv", row.names = FALSE)

ggplot(result_df, aes(x = GKODEg, y = GKODNg, color = UD_Total)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "IR_UD Value Distribution", x = "GKODEg (Longitude)", y = "GKODNg (Latitude)", color = "IR_UD") +
  theme_minimal()

View(result_df)

filtered_df <- result_df %>%
  arrange(desc(UD_Total)) %>%   
  slice_head(n = 50)       
filtered_df <- filtered_df %>%
  mutate(color_group = ifelse(UD_Total >= 3e-5, "red", "viridis"))
filtered_df$formatted_UD_Total <- sprintf("%.0e", filtered_df$UD_Total)
ggplot(filtered_df, aes(x = GKODEg, y = GKODNg)) +
  geom_point(aes(color = color_group), size = 2) +
  scale_color_manual(values = c("red" = "red", "viridis" = viridis::viridis(100)[1])) +
  geom_text(aes(label = formatted_UD_Total),  
            hjust = 0.5, vjust = -0.5, size = 3, color = "black") + 
  labs(title = "UD_Total",
       x = "GKODEg", 
       y = "GKODNg", 
       color = "UD_Total") +
  theme_minimal()
