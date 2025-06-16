
#### Risikoberechnung ####
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
# ---- Benötigte Pfade ----
root_directory <- "xxxx"# Pfad/Verbindung zu dem Ordner festlegen, in dem die Dateien gespeichert sind
Reference_directory <- "xxxx"# Pfad/Verbindung zu dem Ordner festlegen, in dem die Referenzdateien gespeichert sind

# ---- Function to extract tNEQ for all subdirectories ----
extract_tNEQ_all <- function(Mag_Dat_Exp_Peri_Mitte, subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    
    match_index <- which(Mag_Dat_Exp_Peri_Mitte$Magazin_ALT == folder_name)
    
    if (length(match_index) > 0) {
      selected_rows <- Mag_Dat_Exp_Peri_Mitte[Mag_Dat_Exp_Peri_Mitte$Magazin_ALT == folder_name, c("tNEQ", "V")]
      tNEQ_value <- selected_rows$tNEQ
      TNT <- data.frame(Q = selected_rows$tNEQ/1000, V = selected_rows$V)
      
      combined_output_file <- file.path(subdirectory,paste0(folder_name, "_TNT.csv"))
      
      
      write.csv(TNT, combined_output_file, row.names = FALSE, quote = FALSE)
      
      cat("Folder name:", folder_name, "\n")
      cat("tNEQ value:", tNEQ_value, "\n")
    } else {
      warning("No matching folder name found in Mag_Dat_Exp_Peri_Mitte$Magazin_ALT for subdirectory:", subdirectory)
    }
  }
}


### Calculate r_Values based on maximum QTNT_Storage###
calculate_r_values <- function(Q, MA, TrD_Zonen) {
  r_G <- log(TrD_Zonen$Gebeude / (0.36 * MA * Q^(-0.58))) / (-0.047 * Q^(-0.29))
  r_A <- log(TrD_Zonen$Auto / (0.36 * MA * Q^(-0.58))) / (-0.047 * Q^(-0.29))
  r_B <- log(TrD_Zonen$Bahn / (0.36 * MA * Q^(-0.58))) / (-0.047 * Q^(-0.29))
  r_F <- log(TrD_Zonen$Freifeld / (0.36 * MA * Q^(-0.58))) / (-0.047 * Q^(-0.29))
  
  r_values <- list(r_G = r_G, r_A = r_A, r_B = r_B, r_F = r_F)
  
  return(r_values)
}

# Function to calculate r_values for all sub_subdirectories
calculate_r_values_TNT_Max <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    tnt_file <- file.path(subdirectory,paste0(folder_name, "_TNT.csv"))
    if (file.exists(tnt_file)) {
      first_line <- readLines(tnt_file, n = 1)
      if (grepl(",", tnt_file)) {
        separator <- ","
      } else if (grepl(";", tnt_file)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      TNT <- read.csv(tnt_file, sep = separator)
      Q <- TNT$Q 
      MA <- 100 * Q + TNT$V * 0.5
      r_values <- calculate_r_values(Q, MA, TrD_Zonen)
      r_values_df <- data.frame(r_G = r_values$r_G, r_A = r_values$r_A, r_B = r_values$r_B, r_F = r_values$r_F)
      r_values_output_file <- file.path(subdirectory,paste0(folder_name, "_r_values.csv"))
      write.csv(r_values_df, r_values_output_file, row.names = FALSE, quote = FALSE)
      cat("Calculated r_values for subdirectory:", subdirectory, "\n")
      cat("Folder name:", folder_name, "\n")
      print(r_values_df)
    } else {
      warning("TNT.csv not found in subdirectory:", subdirectory)
    }
  }
}

# ---- Function to calculate r_G,r_B,r_F,r_A (Zone_I, Zone_II) from radius values using equation from TLM ----
Parameters_from_r_values <- function(subdirectories) {
  result_list <- list()  # Create a list to store the global_params_df data frames
  
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    r_values_file <- file.path(subdirectory,paste0(folder_name, "_r_values.csv"))
    if (file.exists(r_values_file)) {
      first_line <- readLines(r_values_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      r_values_df <- read.csv(r_values_file, sep = separator) 
      r_G <- na.omit(r_values_df$r_G)
      r_A <- na.omit(r_values_df$r_A)
      r_B <- na.omit(r_values_df$r_B)
      r_F <- na.omit(r_values_df$r_F)
      
      ra_G <- c(r_G[3] * 0.4, r_G[3] * 0.65, r_G[3], r_G[4], r_G[5])
      ra_F <- c(r_F[3] * 0.4, r_F[3] * 0.65, r_F[3], r_F[4], r_F[5])
      ra_A <- c(r_A[3] * 0.4, r_A[3] * 0.65, r_A[3], r_A[4], r_A[5])
      ra_B <- c(r_B[3] * 0.4, r_B[3] * 0.65, r_B[3], r_B[4], r_B[5])
      
      rb_G <- c(r_G[3] * 0.80, r_G[3] * 1.30, r_G[3] * 2, r_G[4] * 2, r_G[5] * 2)
      rb_F <- c(r_F[3] * 0.80, r_F[3] * 1.30, r_F[3] * 2, r_F[4] * 2, r_F[5] * 2)
      rb_A <- c(r_A[3] * 0.80, r_A[3] * 1.30, r_A[3] * 2, r_A[4] * 2, r_A[5] * 2)
      rb_B <- c(r_B[3] * 0.80, r_B[3] * 1.30, r_B[3] * 2, r_B[4] * 2, r_B[5] * 2)
      
      
      global_rvalues_output_file <- file.path(subdirectory,paste0(folder_name, "_global_rvalues.csv"))
      global_params_df <- data.frame(
        r_G = r_G,
        r_A = r_A,
        r_B = r_B,
        r_F = r_F,
        ra_G = ra_G,
        ra_F = ra_F,
        ra_A = ra_A,
        ra_B = ra_B,
        rb_G = rb_G,
        rb_F = rb_F,
        rb_A = rb_A,
        rb_B = rb_B
      )
      
      write.csv(global_params_df, global_rvalues_output_file, row.names = FALSE, quote = FALSE)
      
      cat("Saved global_rvalues for subdirectory:", subdirectory, "\n")
      print(global_params_df)
      result_list[[subdirectory]] <- global_params_df  
    } else {
      warning("r_values.csv not found in subdirectory:", subdirectory)
    }
  }
  
  return(result_list)  
}

# ---- Berechnung Krater trümmwerwurf mit trümmerdichte, r, MA ----
Leta_Krat_pnorm_max <- function(data_max) {
  Q <- data_max$Q_max 
  MA <- data_max$MA_max 
  rtar <- data_max$Distance
  rho <- 0.36 * MA * Q^-0.58 * exp(-0.047 * rtar* Q^-0.29)  # [kg/m2]
  x <- log(rho)  
  
  if ("z_trummerwurf" %in% colnames(data_max)) {
    data_max$z_trummerwurf <- NULL
  }
  data_max$Z_Trummerwurf <- NA
  data_max$rho_krtrum <- NA
  data_max$x_krtrum <- NA
  data_max$p_krtrum <- NA
  data_max$Z_Trummerwurf <-  -4.103 + 0.4631 * x + 0.2524 * sqrt((x - 3.285)^2 + 39.95)
  
  
  data_max$rho_krtrum <- rho
  data_max$x_krtrum <- x
  data_max$p_krtrum <- pnorm(data_max$Z_Trummerwurf)
  return(data_max)
}


process_Krater_Dat_max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    data_max_file <- file.path(subdirectory, paste0(folder_name, "_Geb_dat_TNT_Menge_Max.csv"))
    #tnt_max_file <- file.path(subdirectory, paste0(folder_name, "_TNT.csv"))
    if (file.exists(data_max_file)) {
      first_line <- readLines(data_max_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      data_max <- read.csv(data_max_file, sep = separator)
      data_max$Distance <- as.numeric(data_max$Distance)
      #TNT_max <- read.csv(tnt_max_file, sep = ",")
      
      result_data <- Leta_Krat_pnorm_max(data_max)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_pnorm_Krat_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}


# ---- Berechnung Die lethalität von Trümmerwurf des Kraterwurfs TNT_Max ----
Leta_krat_max_Zonen <- function(data_krat_max) {
  for (i in 1:length(data_krat_max$Distance)) {
    data_krat_max$Distance[i] <- floor(data_krat_max$Distance[i])
    data_krat_max$lambda_Trummerwurf[i] <- numeric(length(data_krat_max$Distance[i]))
    
    r_value_1[i] <- as.numeric(data_krat_max$ra_G_max_3[i])
    r_value_2[i] <- as.numeric(data_krat_max$ra_G_max_4[i])
    r_value_3[i] <- as.numeric(data_krat_max$ra_G_max_5[i])
    data_krat_max$r_r3[i] <- ifelse(is.na(r_value_1[i]) || r_value_1[i] == 0, 0, data_krat_max$Distance[i] / r_value_1[i])
    
    if (!is.na(data_krat_max$r_r3[i]) && data_krat_max$r_r3[i] >= 0.3295 && data_krat_max$r_r3[i] <= 1) {
      data_krat_max$lambda_Trummerwurf[i] <- (-41.7 + ((46.7 * r_value_1[i]) / data_krat_max$Distance[i])) / 100
    } else if (!is.na(data_krat_max$r_r3[i]) && data_krat_max$r_r3[i] < 0.3295) {
      data_krat_max$lambda_Trummerwurf[i] <- 1
    }
    
    if (!is.na(data_krat_max$lambda_Trummerwurf[i]) && data_krat_max$lambda_Trummerwurf[i] <= 0.01) {
      data_krat_max$lambda_Trummerwurf[i] <- 0.01
      if (!is.na(data_krat_max$Distance[i]) && !is.na(r_value_1[i]) && !is.na(r_value_2[i]) && !is.na(r_value_3[i])) {
        if (data_krat_max$Distance[i] >= r_value_1[i] && data_krat_max$Distance[i] <= r_value_2[i]) {
          data_krat_max$lambda_Trummerwurf[i] <- 0.01
        } else if (data_krat_max$Distance[i] >= r_value_2[i] && data_krat_max$Distance[i] <= r_value_3[i]) {
          data_krat_max$lambda_Trummerwurf[i] <- 0.001
        } else if (data_krat_max$Distance[i] >= r_value_3[i]) {
          data_krat_max$lambda_Trummerwurf[i] <- 0
        }
      }
    }
  }
  return(data_krat_max)
}
process_Krat_data_TLM_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    data_krat_max_file <- file.path(subdirectory, paste0(folder_name, "_pnorm_Krat_TNT_Menge_Max.csv"))
    global_krat_max_rValues_file <- file.path(subdirectory, paste0(folder_name, "_global_rvalues.csv"))
    
    if (file.exists(data_krat_max_file)) {
      first_line <- readLines(data_krat_max_file, n = 1)
      if (grepl(",", first_line)) {
        separator_1 <- ","
      } else if (grepl(";", first_line)) {
        separator_1 <- ";"
      } else {
        separator_1 <- ","
      }
      # 
      # if (file.exists(global_krat_max_rValues_file)) {
      #   first_line_global <- readLines(global_krat_max_rValues_file, n = 1)
      #   if (grepl(",", first_line_global)) {
      #     separator_2 <- ","
      #   } else if (grepl(";", first_line_global)) {
      #     separator_2 <- ";"
      #   } else {
      #     separator_2 <- ","
      #   }
      
      data_krat_max <- read.csv(data_krat_max_file, sep = separator_1)
      data_krat_max$Distance <- as.numeric(data_krat_max$Distance)
      #global_krat_max__rValues <- read.csv(global_krat_max_rValues_file, sep = separator_2)
      r_value_1 <- data_krat_max$ra_G_max_3
      r_value_2 <- data_krat_max$ra_G_max_4
      r_value_3 <- data_krat_max$ra_G_max_5
      result_data <- Leta_krat_max_Zonen(data_krat_max)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_KratTrwrf_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      cat("Processed data and saved as", output_file, "\n")
    } else {
      # Handle the case when data_krat_max_file does not exist
      cat("Data file not found:", data_krat_max_file, "\n")
    }
  }
}
# ---- Berechnung Die lethalität von Trümmerwurf des Wandes TNT_Max ----
Leta_wand_max_Zonen <- function(data_wand_max) {
  for (i in 1:length(data_wand_max$Distance)) {
    data_wand_max$Distance[i] <- floor(data_wand_max$Distance[i])
    data_wand_max$lambda_WandTrummerwurf[i] <- numeric(length(data_wand_max$Distance[i]))
    
    r_value_1[i] <- data_wand_max$rb_G_max_3[i]
    r_value_2[i] <- data_wand_max$rb_G_max_4[i]
    r_value_3[i] <- data_wand_max$rb_G_max_5[i]
    data_wand_max$r_r3[i] <- ifelse(is.na(r_value_1[i]) || r_value_1[i] == 0, 0, data_wand_max$Distance[i] / r_value_1[i])
    
    if (!is.na(data_wand_max$r_r3[i]) && data_wand_max$r_r3[i] >= 0.3295 && data_wand_max$r_r3[i] <= 1) {
      data_wand_max$lambda_WandTrummerwurf[i] <- (-41.7 + ((46.7 * r_value_1[i]) / data_wand_max$Distance[i])) / 100
    } else if (!is.na(data_wand_max$r_r3[i]) && data_wand_max$r_r3[i] < 0.3295) {
      data_wand_max$lambda_WandTrummerwurf[i] <- 1
    }
    
    if (!is.na(data_wand_max$lambda_WandTrummerwurf[i]) && data_wand_max$lambda_WandTrummerwurf[i] <= 0.01) {
      data_wand_max$lambda_WandTrummerwurf[i] <- 0.01
      if (!is.na(data_wand_max$Distance[i]) && !is.na(r_value_1[i]) && !is.na(r_value_2[i]) && !is.na(r_value_3[i])) {
        if (data_wand_max$Distance[i] >= r_value_1[i] && data_wand_max$Distance[i] <= r_value_2[i]) {
          data_wand_max$lambda_WandTrummerwurf[i] <- 0.01
        } else if (data_wand_max$Distance[i] >= r_value_2[i] && data_wand_max$Distance[i] <= r_value_3[i]) {
          data_wand_max$lambda_WandTrummerwurf[i] <- 0.001
        } else if (data_wand_max$Distance[i] >= r_value_3[i]) {
          data_wand_max$lambda_WandTrummerwurf[i] <- 0
        }
      }
    }
  }
  return(data_wand_max)
}
process_Wand_data_TLM_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    data_wand_max_file <- file.path(subdirectory, paste0(folder_name, "_KratTrwrf_TNT_Menge_Max.csv"))
    #global_wand_max_rValues_file <- file.path(subdirectory, paste0(folder_name, "_global_rvalues.csv"))
    if (file.exists(data_wand_max_file)) {
      first_line <- readLines(data_wand_max_file, n = 1)
      if (grepl(",", first_line)) {
        separator_1 <- ","
      } else if (grepl(";", first_line)) {
        separator_1 <- ";"
      } else {
        separator_1 <- ","
      }
      
      # if (file.exists(global_wand_max_rValues_file)) {
      #   first_line_global <- readLines(global_wand_max_rValues_file, n = 1)
      #   if (grepl(",", first_line_global)) {
      #     separator_2 <- ","
      #   } else if (grepl(";", first_line_global)) {
      #     separator_2 <- ";"
      #   } else {
      #     separator_2 <- ","
      #   }
      
      data_wand_max <- read.csv(data_wand_max_file, sep = separator_1)
      data_wand_max$Distance <- as.numeric(data_wand_max$Distance)
      # global_wand_max__rValues <- read.csv(global_wand_max_rValues_file, sep = separator_2)
      r_value_1 <- data_wand_max$rb_G[3]
      r_value_2 <- data_wand_max$rb_G[4]
      r_value_3 <- data_wand_max$rb_G[5]
      result_data <- Leta_wand_max_Zonen(data_wand_max)
      
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_Krat_Wand_Trwrf_TNT_Menge_Max.csv"))
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      cat("Processed data and saved as", output_file, "\n")
    } else {
      # Handle the case when data_wand_max_file does not exist
      cat("Data file not found:", data_wand_max_file, "\n")
    }
  }
}
# ---- Interpolate and calculate TNT_Menge_Max ----
interpolate_and_calculate <- function(dataset) {
  new_distanz <- seq(0, 1200, by = 1)
  
  interpolated_pressure <- approx(x = dataset$Distanz, y = (dataset$Pressure)/100, xout = new_distanz, method = "linear", rule = 2)$y
  interpolated_tip <- approx(x = dataset$Distanz, y = dataset$t_ip, xout = new_distanz, method = "linear", rule = 2)$y
  interpolated_Pulse_incident <- approx(x = dataset$Distanz, y = dataset$Pulse_incident, xout = new_distanz, method = "linear", rule = 2)$y
  interpolated_data <- data.frame(Distanz = new_distanz, Pressure = interpolated_pressure, Tip = interpolated_tip, Pulse_incident=interpolated_Pulse_incident)
  
  interpolated_data$ln_p <- log(interpolated_data$Pressure)
  interpolated_data$P_tip <- (interpolated_data$Pressure^(5/3) * interpolated_data$Tip)
  interpolated_data$ln_P_tip <- log(interpolated_data$P_tip)
  
  return(interpolated_data)
}
P_t_int <- lapply(matching_data_frames, interpolate_and_calculate)
QTNT_6_5_int <- P_t_int$P_t_6500TNT
QTNT_6_int <- P_t_int$P_t_6241TNT
QTNT_5_int <- P_t_int$P_t_5133TNT
QTNT_5_5_int <- P_t_int$P_t_5850TNT
QTNT_4_int <- P_t_int$P_t_4658TNT
QTNT_2_5_int <- P_t_int$P_t_2500TNT
QTNT_2_int <- P_t_int$P_t_2000TNT
QTNT_1_int <- P_t_int$P_t_1000TNT
QTNT_0_5_int <- P_t_int$P_t_500TNT
QTNT_0_8_int <- P_t_int$P_t_800TNT
QTNT_0_4_int <- P_t_int$P_t_400TNT
QTNT_0_3_int <- P_t_int$P_t_300TNT
QTNT_0_22_int <- P_t_int$P_t_220TNT
# ---- Merge and save TNT_Menge_Max ----
select_QTNT_max<- function(tNEQ_value_max) {
  if (tNEQ_value_max[i] >= 6.5) {
    selected_df <- QTNT_6_5_int
  } else if (tNEQ_value_max[i] == 6) {
    selected_df <- QTNT_6_int
  }else if (tNEQ_value_max[i] == 5.5) {
    selected_df <- QTNT_5_5_int
  }else if (tNEQ_value_max[i] == 5) {
    selected_df <- QTNT_5_int
  }else if (tNEQ_value_max[i] == 4) {
    selected_df <- QTNT_4_int
  }else if (tNEQ_value_max[i] == 2.5) {
    selected_df <- QTNT_2_5_int
  }else if (tNEQ_value_max[i] == 2) {
    selected_df <- QTNT_2_int
  } else if (tNEQ_value_max[i] == 1) {
    selected_df <- QTNT_1_int
  }else if (tNEQ_value_max[i] == 0.8) {
    selected_df <- QTNT_0_8_int
  }else if (tNEQ_value_max[i] == 0.5) {
    selected_df <- QTNT_0_5_int
  }else if (tNEQ_value_max[i] == 0.4) {
    selected_df <- QTNT_0_4_int
  }else if (tNEQ_value_max[i] == 0.3) {
    selected_df <- QTNT_0_3_int
  }else if (tNEQ_value_max[i] == 0.22) {
    selected_df <- QTNT_0_22_int
  }else {
    stop("Invalid tNEQ_value_max")
  }
  return(selected_df)
}
merge_and_save_TNT_max <- function(combined_data_TNT_max, QTNT_req_max) {
  merged_data_TNT_max <- merge(combined_data_TNT_max, QTNT_req_max <- select_QTNT_max(tNEQ_value_max), by.x = "Distance", by.y = "Distanz", all.x = TRUE)
  
  duplicate_columns <- grep("^Pressure\\.|^Tip\\.|^Pulse_incident\\.|^ln_p\\.|^P_tip\\.|^ln_P_tip\\.", names(merged_data_TNT_max))
  if (length(duplicate_columns) > 0) {
    merged_data_TNT_max <- merged_data_TNT_max %>% select(-duplicate_columns)
  }
  
  return(merged_data_TNT_max)
}
process_Luftstoss_dat_TNT_max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    # TNT_luft_max_file <- file.path(subdirectory,paste0(folder_name, "_TNT.csv"))
    input_luft_max_file <- file.path(subdirectory, paste0(folder_name, "_Krat_Wand_Trwrf_TNT_Menge_Max.csv"))
    
    if (file.exists(input_luft_max_file)) {
      first_line <- readLines(input_luft_max_file, n = 1)
      if (grepl(",", first_line)) {
        separator_1 <- ","
      } else if (grepl(";", first_line)) {
        separator_1 <- ";"
      } else {
        separator_1 <- ","
      }
      # if (file.exists(TNT_luft_max_file)) {
      #   first_line <- readLines(TNT_luft_max_file, n = 1)
      #   if (grepl(",", first_line)) {
      #     separator_2 <- ","
      #   } else if (grepl(";", first_line)) {
      #     separator_2 <- ";"
      #   } else {
      #     separator_2 <- ","
      #   }
      
      combined_data_TNT_max <- read.csv(input_luft_max_file, sep = separator_1)
      # QTNT_max <- read.csv(TNT_luft_max_file, sep = separator_2)
      tNEQ_value_max <- combined_data_TNT_max$Q_max
      
      # Check if tNEQ_value is zero
      if (tNEQ_value_max[i] == 0) {
        cat("Skipping folder", subdirectory, "because tNEQ_value is 0\n")
        next  # Skip to the next folder in the loop
      }
      
      QTNT_req_max <- select_QTNT_max(tNEQ_value_max)
      
      result_data <- merge_and_save_TNT_max(combined_data_TNT_max, QTNT_req_max)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_Luft_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      cat("Processed data and saved as", output_file, "\n")
    } else {
      # Handle the case when input_luft_max_file does not exist
      cat("Data file not found:", input_luft_max_file, "\n")
    }
  }
}

# ---- Update Gis_Lager_TNT_Menge_Max ----
update_Gis_Lager_TNT_max <- function(input_dataframe_TNT_max, Param_Dat) {
  
  input_dataframe_TNT_max$z <- ifelse(input_dataframe_TNT_max$Luftstoss.Kat. == "K3",
                                      ifelse(input_dataframe_TNT_max$Pressure >= 0.0594,
                                             Param_Dat$Bul3_Dr[1] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul3_Dr[3],
                                             Param_Dat$Bul3_Dr[2] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul3_Dr[4]),
                                      ifelse(input_dataframe_TNT_max$Luftstoss.Kat. == "K2",
                                             ifelse(input_dataframe_TNT_max$Pressure >= 0.101,
                                                    Param_Dat$Bul2_Dr[1] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul2_Dr[3],
                                                    Param_Dat$Bul2_Dr[2] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul2_Dr[4]),
                                             ifelse(input_dataframe_TNT_max$Luftstoss.Kat. == "K1",
                                                    ifelse(input_dataframe_TNT_max$Pressure >= 0.192,
                                                           Param_Dat$Bul1_Dr[1] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul1_Dr[3],
                                                           Param_Dat$Bul1_Dr[2] * input_dataframe_TNT_max$ln_p + Param_Dat$Bul1_Dr[4]),
                                                    Param_Dat$Frei_Dr[1] * input_dataframe_TNT_max$ln_p + Param_Dat$Frei_Dr[3])))
  
  input_dataframe_TNT_max$lambda <- pnorm(input_dataframe_TNT_max$z)
  return(input_dataframe_TNT_max)
}
update_Luftstoss_dat_TNT_max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    input_Luft_TNT_Menge_Max_file <- file.path(subdirectory, paste0(folder_name, "_Luft_TNT_Menge_Max.csv"))
    
    if (file.exists(input_Luft_TNT_Menge_Max_file)) {
      first_line <- readLines(input_Luft_TNT_Menge_Max_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      input_Luft_TNT_Menge_Max_dataframe <- read.csv(input_Luft_TNT_Menge_Max_file, sep = separator)
      
      
      result_data <- update_Gis_Lager_TNT_max(input_Luft_TNT_Menge_Max_dataframe, Param_Dat)
      
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_Luftstoss_Lambda_TNT_Menge_Max.csv"))
      
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}


# ---- Wahrscheinlichkeit für letale Verletzungen - "grosse" Fenster ----
LetGlass_gross_TNT_max <- function(P_kPa_max, I_max, input_dataframe_TNT_max) {
  results <- numeric(length(P_kPa_max))
  
  for (i in seq_along(P_kPa_max)) {
    P_kPa_max_i <- P_kPa_max[i]
    I_max_i <- I_max[i]
    A_max <- seq(0, 100, 0.1)
    C_max <- exp(1.3 + 2.23 * log(A_max))
    B_max <- I_max_i - (C_max / (P_kPa_max_i - A_max))
    
    if (input_dataframe_TNT_max$Fenster[i] == 0) {
      results[i] <- 0
    }
    else if (input_dataframe_TNT_max$Fenster[i] == 1) {
      Pr_A_max_1 <- exp(0.9597 + (0.001226 * A_max^1.5) - 11.630 / (A_max^2))
      Z_werte_A_max_1 <- Pr_A_max_1 - 5
      Lambda_A_max_1 <- pnorm(Z_werte_A_max_1)
      Pr_B_max_1 <- exp(1.024 + 0.0001758 * B_max - 19.844 / B_max)
      Z_werte_B_max_1 <- Pr_B_max_1 - 5
      Lambda_B_max_1 <- pnorm(Z_werte_B_max_1)
      diff_1 <- abs(Lambda_B_max_1 - Lambda_A_max_1)
      Lambda_Dat_1 <- data.frame(A_max, Lambda_A_max_1, B_max, Lambda_B_max_1, diff_1)
      indR_max_1 <- min(which(Lambda_B_max_1 - Lambda_A_max_1 < 0))
      Result_max_1 <- Lambda_A_max_1[indR_max_1]
      results[i] <- Result_max_1
    }
    else if (input_dataframe_TNT_max$Fenster[i] == 2) {
      Pr_A_max_2 <- exp(1.1023 + (0.0001550 * A_max^2) - 3.1628 / (A_max^1.5))
      Z_werte_A_max_2 <- Pr_A_max_2 - 5
      Lambda_A_max_2 <- pnorm(Z_werte_A_max_2)
      Pr_B_max_2 <- exp(1.044 + 0.0002281 * B_max - 8.121 / B_max)
      Z_werte_B_max_2 <- Pr_B_max_2 - 5
      Lambda_B_max_2 <- pnorm(Z_werte_B_max_2)
      diff_2 <- abs(Lambda_B_max_2 - Lambda_A_max_2)
      Lambda_Dat_2 <- data.frame(A_max, Lambda_A_max_2, B_max, Lambda_B_max_2, diff_2)
      indR_max_2 <- min(which(Lambda_B_max_2 - Lambda_A_max_2 < 0))
      Result_max_2 <- Lambda_A_max_2[indR_max_2]
      results[i] <- Result_max_2
    }
    else if (input_dataframe_TNT_max$Fenster[i] == 3) {
      Pr_A_max_3 <- exp(1.1076 + (0.0002735 * A_max^2) - 1.374 / (A_max^1.5))
      Z_werte_A_max_3 <- Pr_A_max_3 - 5
      Lambda_A_max_3 <- pnorm(Z_werte_A_max_3)
      Pr_B_max_3 <- exp(1.053 + 0.0003526 * B_max - 3.279 / B_max)
      Z_werte_B_max_3 <- Pr_B_max_3 - 5
      Lambda_B_max_3 <- pnorm(Z_werte_B_max_3)
      diff_3 <- abs(Lambda_B_max_3 - Lambda_A_max_3)
      Lambda_Dat_3 <- data.frame(A_max, Lambda_A_max_3, B_max, Lambda_B_max_3, diff_3)
      indR_max_3 <- min(which(Lambda_B_max_3 - Lambda_A_max_3 < 0))
      Result_max_3 <- Lambda_A_max_3[indR_max_3]
      results[i] <- Result_max_3
    }
  }
  
  return(results)
}
update_LetGlass_gross_TNT_dat_max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    input_Glasbruch_TNT_max_file <- file.path(subdirectory, paste0(folder_name, "_Luftstoss_Lambda_TNT_Menge_Max.csv"))
    
    if (file.exists(input_Glasbruch_TNT_max_file)) {
      first_line <- readLines(input_Glasbruch_TNT_max_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      input_dataframe_TNT_max <- read.csv(input_Glasbruch_TNT_max_file, sep = separator)
      P_kPa_max <- input_dataframe_TNT_max$Pressure * 100  ## [kPa]
      I_max <- input_dataframe_TNT_max$Pulse_incident ## [kPa-ms]
      
      input_dataframe_TNT_max$Let_Glass_gross <- LetGlass_gross_TNT_max(P_kPa_max, I_max,input_dataframe_TNT_max)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_LetGlass_Lambda_TNT_Menge_Max.csv"))
      
      write.csv(input_dataframe_TNT_max, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}

# ---- Function for defining the ZoneB with 45°angle ----
Zone_B_Mapping <- function(data_in, Magazine_Dat) {
  reference_row <- Magazine_Dat[match_index,]
  if (nrow(reference_row) == 0) {
    stop("Reference data not found in Magazine_Dat.")
  }
  
  # Check for missing values in reference_row columns
  if (any(is.na(reference_row$xCH)) || any(is.na(reference_row$yCH)) ||
      any(is.na(reference_row$E3_Ausr)) || any(is.na(reference_row$E4_Ausr)) ||
      any(is.na(reference_row$E2_Ausr)) || any(is.na(reference_row$E1_Ausr))) {
    stop("Reference data contains missing values.")
  }
  
  reference_building <- c(reference_row$xCH, reference_row$yCH)
  nowand <- 130#as.numeric(reference_row$E3_Ausr)
  sowand <- 220#as.numeric(reference_row$E4_Ausr)
  swwand <- 310#as.numeric(reference_row$E2_Ausr)
  nwwand <- 40#as.numeric(reference_row$E1_Ausr)
  
  is_in_45_degree_zone <- function(reference, target) {
    # Check for missing values in target_building
    if (any(is.na(target))) {
      return(0)  # Handle missing values as not in the zone
    }
    
    vector_to_target <- target - reference
    
    angle <- atan2(vector_to_target[2], vector_to_target[1])
    angle_degrees <- angle * 180 / pi
    if (angle_degrees < 0) {
      angle_degrees <- angle_degrees + 360
    }
    
    if (reference_row$Richtung_Oeffnung == "Nord" && 
        (angle_degrees >= nowand - 22.5 && angle_degrees <= nowand + 22.5)) {
      return(1)
    } else if (reference_row$Richtung_Oeffnung == "Sued" && 
               (angle_degrees >= sowand - 22.5 && angle_degrees <= sowand + 22.5)) {
      return(1)
    } else if (reference_row$Richtung_Oeffnung == "West" && 
               (angle_degrees >= nwwand - 22.5 && angle_degrees <= nwwand + 22.5)) {
      return(1)
    } else if (reference_row$Richtung_Oeffnung == "Ost" && 
               (angle_degrees >= swwand - 22.5 && angle_degrees <= swwand + 22.5)) {
      return(1)
    } else if (reference_row$Richtung_Oeffnung == "Alle" && 
               ((angle_degrees >= nowand - 22.5 && angle_degrees <= nowand + 22.5) || 
                (angle_degrees >= sowand - 22.5 && angle_degrees <= sowand + 22.5) ||
                (angle_degrees >= swwand - 22.5 && angle_degrees <= swwand + 22.5) ||
                (angle_degrees >= nwwand - 22.5 && angle_degrees <= nwwand + 22.5))) {
      return(1)
    } else {
      return(0)
    }
  }
  
  calculate_angle_degrees <- function(reference, target) {
    # Check for missing values in target_building
    if (any(is.na(target))) {
      return(NA)  # Handle missing values with NA
    }
    
    vector_to_target <- target - reference
    
    angle <- atan2(vector_to_target[2], vector_to_target[1])
    angle_degrees <- angle * 180 / pi
    if (angle_degrees < 0) {
      angle_degrees <- angle_degrees + 360
    }
    return(angle_degrees)
  }
  
  data_in$In_45_Degree_Zone <- sapply(1:nrow(data_in), function(i) {
    target <- c(data_in$GKODEg[i], data_in$GKODNg[i])
    is_in_45_degree_zone(reference_building, target)
  })
  
  data_in$Angle_degrees <- sapply(1:nrow(data_in), function(i) {
    target <- c(data_in$GKODEg[i], data_in$GKODNg[i])
    calculate_angle_degrees(reference_building, target)
  })
  
  return(data_in)
}
Zone_Mapping_TNT_Max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    match_index <- which(Magazine_Dat$Magazin_ALT == folder_name)
    input_file <- file.path(subdirectory, paste0(folder_name, "_LetGlass_Lambda_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      data_in <- read.csv(input_file, sep = separator)
      
      result_data <- Zone_B_Mapping(data_in, Magazine_Dat)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_zone_mapped_lambda_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}

# ---- Function to calculate Lambda ----
calculate_lambda_f_max <- function(input_file) {
  input_file$lambda[is.na(input_file$lambda)] <- 0
  input_file$lambda_WandTrummerwurf[is.na(input_file$lambda_WandTrummerwurf)] <- 0
  input_file$lambda_Trummerwurf[is.na(input_file$lambda_Trummerwurf)] <- 0
  input_file$Let_Glass_gross[is.na(input_file$Let_Glass_gross)] <- 0
  
  input_file$lambda_f <- ifelse(input_file$In_45_Degree_Zone == 0 & input_file$Fenster == 0,
                                1 - ((1 - input_file$lambda_Trummerwurf) * (1 - input_file$lambda)),
                                ifelse(input_file$In_45_Degree_Zone == 0 & input_file$Fenster %in% c(1, 2, 3),
                                       1 - ((1 - input_file$lambda_Trummerwurf) * (1 - input_file$lambda) * (1 - input_file$Let_Glass_gross)),
                                       ifelse(input_file$In_45_Degree_Zone == 1 & input_file$Fenster == 0,
                                              1 - ((1 - input_file$lambda_Trummerwurf) * (1 - input_file$lambda) * (1 - input_file$lambda_WandTrummerwurf)),
                                              ifelse(input_file$In_45_Degree_Zone == 1 & input_file$Fenster %in% c(1, 2, 3),
                                                     1 - ((1 - input_file$lambda_Trummerwurf) * (1 - input_file$lambda) * (1 - input_file$lambda_WandTrummerwurf) * (1 - input_file$Let_Glass_gross)),
                                                     NA
                                              )
                                       )
                                )
  )
  
  input_file$lambda_Kr_Luft <- ifelse(input_file$Fenster == 0,
                                      1 - ((1 - input_file$lambda) * (1 - input_file$lambda_Trummerwurf)),
                                      1 - ((1 - input_file$lambda) * (1 - input_file$lambda_Trummerwurf) * (1 - input_file$Let_Glass_gross))
  )
  
  input_file$lambda_Luft_Glassbruch <- ifelse(input_file$Fenster %in% c(1, 2, 3),
                                              1 - (1-input_file$Let_Glass_gross),
                                              0
  )
  
  return(input_file)
}
Total_Lambda_TNT_Max_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    input_file <- file.path(subdirectory, paste0(folder_name, "_zone_mapped_lambda_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      combined_data <- read.csv(input_file, sep = separator)
      
      result_data <- calculate_lambda_f_max(combined_data)
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}
# ---- Function to calculate Lambda_Cumulative  ----
collect_lambda_f_values <- function(subdirectories) {
  lambda_f_values <- data.frame(Subdirectory = character(), lambda_f = numeric(), stringsAsFactors = FALSE)
  
  for (subdirectory in subdirectories) {
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      combined_data <- read.csv(input_file, sep = ",")
      
      
      filtered_data <- combined_data %>%
        filter(GKODEg == xxxx & GKODNg == xxxx)
      
      
      if (nrow(filtered_data) > 0) {
        lambda_f_value <- filtered_data$lambda_f
        lambda_f_values <- rbind(lambda_f_values, data.frame(Subdirectory = folder_name, lambda_f = lambda_f_value, stringsAsFactors = FALSE))
      } else {
        warning("No data found for the specified conditions in subdirectory:", subdirectory)
      }
      
    } else {
      warning("Processed file not found in subdirectory:", subdirectory)
    }
  }
  
  return(lambda_f_values)
}

#lambda_f_table <- collect_lambda_f_values(subdirectories)

#lambda_f_table$lambda_cumulative <- 1 - prod(1 - lambda_f_table$lambda_f[1:22], na.rm = TRUE)
# ---- W_LKU_Werte einfügen ----
W_LKU_All_Dat <- Mag_Dat_Exp_Peri_Mitte
W_LKU_Mapping <- function(data_in, W_LKU_All_Dat) {
  
  match_index <- which(W_LKU_All_Dat$Magazin_ALT == folder_name)
  
  if (any(is.na(match_index))) {
    warning("Some indices in data_in were not found in W_LKU_All_Dat.")
  }
  
  
  data_in$W_LKU <- W_LKU_All_Dat$W_LKU[match_index]
  
  return(data_in)
}
W_LKU_data_in_subdirectories <- function(subdirectories) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    match_index <- which(W_LKU_All_Dat$Magazin_ALT == folder_name)
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      data_in <- read.csv(input_file, sep = separator)
      data_in$W_LKU <- W_LKU_All_Dat$W_LKU[match_index]
      result_data <- data_in
      
      output_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}

# ---- Paths for saving individual risk data for reference buildings ----

Ind_risk_Induistrie_West_TNT_max_directory <- paste0(srcDir,"/xxx")
Ind_risk_Induistrie_Peri_mitte_TNT_max_directory <- paste0(srcDir,"/xxx")

Ind_risk_Induistrie_West_TNT_aktuell_directory <- paste0(srcDir,"/xxx")
Ind_risk_Induistrie_Peri_mitte_TNT_aktuell_directory <- paste0(srcDir,"xx")

Ind_risk_Sud_Engelrutti_TNT_max_directory <- paste0(srcDir,"xx")
Ind_risk_Sud_Ost_Engelrutti_TNT_max_directory <- paste0(srcDir,"xx")

Ind_risk_Sud_Engelrutti_TNT_aktuell_directory <- paste0(srcDir,"xx")
Ind_risk_Sud_Ost_Engelrutti_TNT_aktuell_directory <- paste0(srcDir,"xx")


# ---- Function for Extracting data for Individuelle Risik calculation für Arbeitende in feld ----- 
IR_Arbeitende_Westlich_Engelrutti_data_in_subdirectories <- function(subdirectories, root_directory) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    if (substr(folder_name, 1, 1) != "G") {
      next  
    }
    
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      data_in <- read.csv(input_file, sep = separator)
      
      result_data <- data_in[data_in$GKODEg == xxx & data_in$GKODNg == xxx, ]
      
      
      result_data$FolderName <- folder_name
      
      
      output_file <- file.path(Ind_risk_Induistrie_West_TNT_max_directory, paste0(folder_name, "_Ind_Risk_Industrie_Geb_West_TNT_Max.csv"))
      
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}
# ---- Function for Extracting data for Individuelle Risik calculation für Unbetiligte Dritte in feld xxx	xxx ----
ind_ris_directory<-paste0(srcDir,"/xxx")
IR_UD_data_in_subdirectories <- function(subdirectories, root_directory) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    if (substr(folder_name, 1, 1) != "G") {
      next  
    }
    
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      data_in <- read.csv(input_file, sep = separator)
      
      result_data <- data_in[data_in$GKODEg == xxxx & data_in$GKODNg == xxxxx, ]
      
      
      result_data$FolderName <- folder_name
      
      
      output_file <- file.path(ind_ris_directory, paste0(folder_name, "_xxxx.csv"))
      
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}
# ---- Function for Extracting data for Individuelle Risik calculation  für Arbeitende in PerMitte (direkt Betroffen) ----
IR_Arbeitende_Perimetermitte_data_in_subdirectories <- function(subdirectories, root_directory) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    if (substr(folder_name, 1, 1) != "G") {
      next  
    }
    
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      data_in <- read.csv(input_file, sep = separator)
      
      result_data <- data_in[data_in$GKODEg == xxxx & data_in$GKODNg == xxxx, ]
      
      
      result_data$FolderName <- folder_name
      
      
      output_file <- file.path(Ind_risk_Induistrie_Peri_mitte_TNT_max_directory, paste0(folder_name, "_Ind_Risk_Industrie_Geb_Peri_mitte_TNT_Max.csv"))
      
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}

# ---- Function for Extracting data for Individuelle Risik calculation  für Wohnbevölkerung mit Wandtrümmerwurf  ----
IR_Wohnbevolkerung_Sud_Engelrutti_data_in_subdirectories <- function(subdirectories, root_directory) {
  for (subdirectory in subdirectories) {
    
    folder_name <- gsub("_", "_", tail(strsplit(subdirectory, "/")[[1]], 1))
    if (substr(folder_name, 1, 1) != "G") {
      next  
    }
    
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      data_in <- read.csv(input_file, sep = separator)
      
      result_data <- data_in[data_in$GKODEg == xxxx & data_in$GKODNg == xxxxx, ]
      
      
      result_data$FolderName <- folder_name
      
      
      output_file <- file.path(Ind_risk_Sud_Engelrutti_TNT_max_directory, paste0(folder_name, "_Ixxx.csv"))
      
      
      write.csv(result_data, file = output_file, row.names = FALSE, sep = ",")
      
      cat("Processed data and saved as", output_file, "\n")
    } else {
      warning("Input file not found in subdirectory:", subdirectory)
    }
  }
}
# ---- Function for calculating Individuelle Risk für alle die sich in Wohngebäuden befindeten Wohnbevölkerung ----
calculate_individual_risk_alle_UD <- function(subdirectories, Geb_1_PF, Geb_2_PF, srcDir) {
  combined_data <- data.frame()
  
  for (subdirectory in subdirectories) {
    
    folder_name <- tail(strsplit(subdirectory, "/")[[1]], 1)
    
    if (substr(folder_name, 1, 1) != "G") {
      next  
    }
    
    input_file <- file.path(subdirectory, paste0(folder_name, "_lambda_Total_W_LKU_TNT_Menge_Max.csv"))
    
    if (file.exists(input_file)) {
      cat("Reading file:", input_file, "\n") 
      
      first_line <- readLines(input_file, n = 1)
      if (grepl(",", first_line)) {
        separator <- ","
      } else if (grepl(";", first_line)) {
        separator <- ";"
      } else {
        separator <- ","
      }
      
      sub_data <- read.csv(input_file, sep = separator)
      
      sub_data$Magazin_Name <- folder_name
      
      combined_data <- rbind(combined_data, sub_data)
    } else {
      warning(paste("Skipping", input_file, "- File not found."))
    }
  }
  combined_data <- combined_data %>%
    mutate(
      PF_N = case_when(
        grepl("^Perimeter|^Lager|^Magazin|^EMKO|^Neuland|^Industrie ", ObjTyp) ~ as.numeric(Geb_1_PF[12, 5]),
        grepl("^Mehrfamilienhaus|^Einfamilienhaus", ObjTyp) ~ as.numeric(Geb_1_PF[9, 5]),
        grepl("^Gewerbebetrieb", ObjTyp) ~ as.numeric(Geb_1_PF[11, 5]),
        grepl("^Kirche", ObjTyp) ~ as.numeric(Geb_2_PF[7, 6]),
        grepl("^Camping", ObjTyp) ~ 0.8,
        grepl("^Schwimmbad", ObjTyp) ~ as.numeric(Geb_2_PF[9, 6]),
        grepl("^Bauernhaus", ObjTyp) ~ as.numeric(Geb_1_PF[3, 5]),
        grepl("^Turnhalle", ObjTyp) ~ as.numeric(Geb_2_PF[12, 6]),
        grepl("^Gemeindeverwaltung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 6]),
        grepl("^Hotel", ObjTyp) ~ 0.8,
        grepl("^Dienstleistung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 6]),
        grepl("^KapelleKultur", ObjTyp) ~ as.numeric(Geb_2_PF[3, 6]),
        grepl("^Schulhaus", ObjTyp) ~ as.numeric(Geb_2_PF[1, 6]),
        grepl("^Spital", ObjTyp) ~ as.numeric(Geb_2_PF[4, 6]),
        TRUE ~ 0
      ),
      PF_A = case_when(
        grepl("^Perimeter|^Lager|^Magazin|^EMKO|^Neuland|^Industrie ", ObjTyp) ~ as.numeric(Geb_1_PF[12, 3]),
        grepl("^Mehrfamilienhaus|^Einfamilienhaus", ObjTyp) ~ as.numeric(Geb_1_PF[9, 3]),
        grepl("^Gewerbebetrieb", ObjTyp) ~ as.numeric(Geb_1_PF[11, 3]),
        grepl("^Kirche", ObjTyp) ~ as.numeric(Geb_2_PF[7, 4]),
        grepl("^Camping", ObjTyp) ~ 0.3,
        grepl("^Schwimmbad", ObjTyp) ~ as.numeric(Geb_2_PF[9, 4]),
        grepl("^Bauernhaus", ObjTyp) ~ as.numeric(Geb_1_PF[3, 3]),
        grepl("^Turnhalle", ObjTyp) ~ as.numeric(Geb_2_PF[12, 4]),
        grepl("^Gemeindeverwaltung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 4]),
        grepl("^Hotel", ObjTyp) ~ 0.2,
        grepl("^Dienstleistung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 4]),
        grepl("^KapelleKultur", ObjTyp) ~ as.numeric(Geb_2_PF[3, 4]),
        grepl("^Schulhaus", ObjTyp) ~ as.numeric(Geb_2_PF[1, 4]),
        grepl("^Spital", ObjTyp) ~ as.numeric(Geb_2_PF[4, 4]),
        TRUE ~ 0
      ),
      PF_F = case_when(
        grepl("^Perimeter|^Lager|^Magazin|^EMKO|^Neuland|^Industrie ", ObjTyp) ~ as.numeric(Geb_1_PF[12, 4]),
        grepl("^Mehrfamilienhaus|^Einfamilienhaus", ObjTyp) ~ as.numeric(Geb_1_PF[9, 4]),
        grepl("^Gewerbebetrieb", ObjTyp) ~ as.numeric(Geb_1_PF[11, 4]),
        grepl("^Kirche", ObjTyp) ~ as.numeric(Geb_2_PF[7, 5]),
        grepl("^Camping", ObjTyp) ~ 0.6,
        grepl("^Schwimmbad", ObjTyp) ~ as.numeric(Geb_2_PF[9, 5]),
        grepl("^Bauernhaus", ObjTyp) ~ as.numeric(Geb_1_PF[3, 4]),
        grepl("^Turnhalle", ObjTyp) ~ as.numeric(Geb_2_PF[12, 5]),
        grepl("^Gemeindeverwaltung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 5]),
        grepl("^Hotel", ObjTyp) ~ 0.6,
        grepl("^Dienstleistung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 5]),
        grepl("^KapelleKultur", ObjTyp) ~ as.numeric(Geb_2_PF[3, 5]),
        grepl("^Schulhaus", ObjTyp) ~ as.numeric(Geb_2_PF[1, 5]),
        grepl("^Spital", ObjTyp) ~ as.numeric(Geb_2_PF[4, 5]),
        TRUE ~ 0
      ),
      PF_W = case_when(
        grepl("^Perimeter|^Lager|^Magazin|^EMKO|^Neuland|^Industrie ", ObjTyp) ~ as.numeric(Geb_1_PF[12, 6]),
        grepl("^Mehrfamilienhaus|^Einfamilienhaus", ObjTyp) ~ as.numeric(Geb_1_PF[9, 6]),
        grepl("^Gewerbebetrieb", ObjTyp) ~ as.numeric(Geb_1_PF[11, 6]),
        grepl("^Kirche", ObjTyp) ~ as.numeric(Geb_2_PF[7, 7]),
        grepl("^Camping", ObjTyp) ~ 0.4,
        grepl("^Schwimmbad", ObjTyp) ~ as.numeric(Geb_2_PF[9, 7]),
        grepl("^Bauernhaus", ObjTyp) ~ as.numeric(Geb_1_PF[3, 6]),
        grepl("^Turnhalle", ObjTyp) ~ as.numeric(Geb_2_PF[12, 7]),
        grepl("^Gemeindeverwaltung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 7]),
        grepl("^Hotel", ObjTyp) ~ 0.2,
        grepl("^Dienstleistung", ObjTyp) ~ as.numeric(Geb_2_PF[3, 7]),
        grepl("^KapelleKultur", ObjTyp) ~ as.numeric(Geb_2_PF[3, 7]),
        grepl("^Schulhaus", ObjTyp) ~ as.numeric(Geb_2_PF[1, 7]),
        grepl("^Spital", ObjTyp) ~ as.numeric(Geb_2_PF[4, 7]),
        TRUE ~ 0
      )
    )
  
  
  combined_data$PF_N <- as.numeric(combined_data$PF_N)
  combined_data$PF_N[is.na(combined_data$PF_N)] <- 0
  combined_data$PF_A <- as.numeric(combined_data$PF_A)
  combined_data$PF_A[is.na(combined_data$PF_A)] <- 0
  combined_data$PF_F <- as.numeric(combined_data$PF_F)
  combined_data$PF_F[is.na(combined_data$PF_F)] <- 0
  combined_data$PF_W <- as.numeric(combined_data$PF_W)
  combined_data$PF_W[is.na(combined_data$PF_W)] <- 0
  
  
  combined_data$SD_N <- 0.4405
  combined_data$SD_A <- 0.2679
  combined_data$SD_F <- 0.1488
  combined_data$SD_W <- 0.1428
  
  
  combined_data$War_faktor_N <- combined_data$PF_N * combined_data$SD_N
  combined_data$War_faktor_A <- combined_data$PF_A * combined_data$SD_A
  combined_data$War_faktor_F <- combined_data$PF_F * combined_data$SD_F
  combined_data$War_faktor_W <- combined_data$PF_W * combined_data$SD_W
  
  combined_data$rpi_Wand_N <- combined_data$lambda_f * combined_data$W_LKU * combined_data$War_faktor_N * combined_data$SD_N
  combined_data$rpi_Wand_A <- combined_data$lambda_f * combined_data$W_LKU * combined_data$War_faktor_A * combined_data$SD_A
  combined_data$rpi_Wand_F <- combined_data$lambda_f * combined_data$W_LKU * combined_data$War_faktor_F * combined_data$SD_F
  combined_data$rpi_Wand_W <- combined_data$lambda_f * combined_data$W_LKU * combined_data$War_faktor_W * combined_data$SD_W
  
  combined_data$rpi_Nowand_N <- combined_data$lambda_Kr_Luft * combined_data$W_LKU * combined_data$War_faktor_N * combined_data$SD_N
  combined_data$rpi_Nowand_A <- combined_data$lambda_Kr_Luft * combined_data$W_LKU * combined_data$War_faktor_A * combined_data$SD_A
  combined_data$rpi_Nowand_F <- combined_data$lambda_Kr_Luft * combined_data$W_LKU * combined_data$War_faktor_F * combined_data$SD_F
  combined_data$rpi_Nowand_W <- combined_data$lambda_Kr_Luft * combined_data$W_LKU * combined_data$War_faktor_W * combined_data$SD_W
  
  write.csv(combined_data, file = paste0(srcDir, "/Individuelles_Risiko/Ind_Risk_Geb_Alle_Dat.csv"), row.names = FALSE)
  combined_data <- read.csv(paste0(srcDir, "/Individuelles_Risiko/Ind_Risk_Geb_Alle_Dat.csv"),sep = ",")
  in_ri_al_Geb_al_Mag_Peri_mitte <- combined_data %>%
    group_by(GKODEg, GKODNg) %>%
    summarise(
      Total_rpi_Wand_N = sum(rpi_Wand_N),
      Total_rpi_Wand_A = sum(rpi_Wand_A),
      Total_rpi_Wand_F = sum(rpi_Wand_F),
      Total_rpi_Wand_W = sum(rpi_Wand_W),
      sum_rpi_Nowand_N = sum(rpi_Nowand_N),
      sum_rpi_Nowand_A = sum(rpi_Nowand_A),
      sum_rpi_Nowand_F = sum(rpi_Nowand_F),
      sum_rpi_Nowand_W = sum(rpi_Nowand_W),
      
      ID = first(ID)
    ) %>%
    ungroup()
  in_ri_al_Geb_al_Mag_Peri_mitte$Total_ir_A_N_F_W_Wand<-in_ri_al_Geb_al_Mag_Peri_mitte$Total_rpi_Wand_A+in_ri_al_Geb_al_Mag_Peri_mitte$Total_rpi_Wand_N+in_ri_al_Geb_al_Mag_Peri_mitte$Total_rpi_Wand_F+in_ri_al_Geb_al_Mag_Peri_mitte$Total_rpi_Wand_W
  write.csv(in_ri_al_Geb_al_Mag_Peri_mitte, file = paste0(srcDir, "/Individuelles_Risiko/xxxxx.csv"), row.names = FALSE)
  return(in_ri_al_Geb_al_Mag_Peri_mitte)  
} 






# ---- Function for calculating Individuelle Risk  für Arbeitende in Gebäude Westlich von Engelrütti ----
calculate_individual_risk_alle_DB <- function(subdirectories, Geb_1_PF, Geb_2_PF, srcDir) {
  csv_files <- list.files(path = paste0(srcDir, "/Individuelles_Risiko/xxxxx"), pattern = "_Ind_Risk_Industrie_Geb_West_TNT_Max.csv$", full.names = TRUE)
  combined_data <- NULL
  header <- NULL
  
  for (i in seq_along(csv_files)) {
    csv_file <- csv_files[i]
    
    data <- read.csv(csv_file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
    if (i == 1) {
      combined_data <- data
    } else {
      combined_data <- rbind(combined_data, data)
    }
  }
  combined_data$SDN <- 0.4405
  combined_data$SDA <- 0.2679
  combined_data$SDF <- 0.1488
  combined_data$SDW <- 0.1428
  combined_data$PF_N <- 0
  combined_data$PF_A <- 1
  combined_data$PF_F <- 0
  combined_data$PF_W <- 0
  combined_data$tpiN <- combined_data$SDN*combined_data$PF_N
  combined_data$tpiA <- combined_data$SDA*combined_data$PF_A
  combined_data$tpiF <- combined_data$SDF*combined_data$PF_F
  combined_data$tpiW <- combined_data$SDW*combined_data$PF_W
  combined_data$rpi_N_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Wand_Summe <- sum(combined_data$rpi_N_Wand)
  combined_data$rpi_A_Wand_Summe <- sum(combined_data$rpi_A_Wand)
  combined_data$rpi_F_Wand_Summe <- sum(combined_data$rpi_F_Wand)
  combined_data$rpi_W_Wand_Summe <- sum(combined_data$rpi_W_Wand)
  combined_data$rpi_N_Kr_Lu_Summe <- sum(combined_data$rpi_N_Kr_Lu)
  combined_data$rpi_A_Kr_Lu_Summe <- sum(combined_data$rpi_A_Kr_Lu)
  combined_data$rpi_W_Kr_Lu_Summe <- sum(combined_data$rpi_W_Kr_Lu)
  combined_data$rpi_F_Kr_Lu_Summe <- sum(combined_data$rpi_F_Kr_Lu)
  combined_data$Summe_RP_Wand <- combined_data$rpi_N_Wand_Summe+combined_data$rpi_A_Wand_Summe+combined_data$rpi_F_Wand_Summe+combined_data$rpi_W_Wand_Summe
  combined_data$Summe_RP_Kr_Lu <-combined_data$rpi_N_Kr_Lu_Summe+combined_data$rpi_A_Kr_Lu_Summe+combined_data$rpi_F_Kr_Lu_Summe+combined_data$rpi_W_Kr_Lu_Summe
  output_file <- file.path(srcDir,"Individuelles_Risiko","Ind_Risk_Industrie_Geb_West_TNT_Max_alle.csv")
  write.csv(combined_data, file = output_file, row.names = FALSE, sep = ",", col.names = TRUE)
  cat("Combined data has been saved as", output_file, "\n")
}
# ---- Function for calculating Individuelle Risik calculation für UD in feld  ----
calculate_individual_risk_alle_UD_2691938.16_1191176.919_Feld <- function(subdirectories, Geb_1_PF, Geb_2_PF, srcDir) {
  csv_files <- list.files(path = paste0(srcDir, "/Individuelles_Risiko/IR_RUAG_BAG_19062024"), pattern = "_Ind_Risk_RUAG_BAG_5500_19062024.csv$", full.names = TRUE)
  combined_data <- NULL
  header <- NULL
  
  for (i in seq_along(csv_files)) {
    csv_file <- csv_files[i]
    
    data <- read.csv(csv_file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
    if (i == 1) {
      combined_data <- data
    } else {
      combined_data <- rbind(combined_data, data)
    }
  }
  combined_data$SDA <- 0.2679
  combined_data$SDN <- 0.4405
  combined_data$SDF <- 0.1488
  combined_data$SDW <- 0.1428
  combined_data$PF_N <- 1
  combined_data$PF_A <- 0.7
  combined_data$PF_F <- 0.8
  combined_data$PF_W <- 1
  combined_data$tpiN <- combined_data$SDN*combined_data$PF_N
  combined_data$tpiA <- combined_data$SDA*combined_data$PF_A
  combined_data$tpiF <- combined_data$SDF*combined_data$PF_F
  combined_data$tpiW <- combined_data$SDW*combined_data$PF_W
  combined_data$rpi_N_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Wand_Summe <- sum(combined_data$rpi_N_Wand)
  combined_data$rpi_A_Wand_Summe <- sum(combined_data$rpi_A_Wand)
  combined_data$rpi_F_Wand_Summe <- sum(combined_data$rpi_F_Wand)
  combined_data$rpi_W_Wand_Summe <- sum(combined_data$rpi_W_Wand)
  combined_data$rpi_N_Kr_Lu_Summe <- sum(combined_data$rpi_N_Kr_Lu)
  combined_data$rpi_A_Kr_Lu_Summe <- sum(combined_data$rpi_A_Kr_Lu)
  combined_data$rpi_W_Kr_Lu_Summe <- sum(combined_data$rpi_W_Kr_Lu)
  combined_data$rpi_F_Kr_Lu_Summe <- sum(combined_data$rpi_F_Kr_Lu)
  combined_data$Summe_RP_Wand <- combined_data$rpi_N_Wand_Summe+combined_data$rpi_A_Wand_Summe+combined_data$rpi_F_Wand_Summe+combined_data$rpi_W_Wand_Summe
  combined_data$Summe_RP_Kr_Lu <-combined_data$rpi_N_Kr_Lu_Summe+combined_data$rpi_A_Kr_Lu_Summe+combined_data$rpi_F_Kr_Lu_Summe+combined_data$rpi_W_Kr_Lu_Summe
  output_file <- file.path(srcDir, "Individuelles_Risiko", "xxxx.csv")
  write.csv(combined_data, file = output_file, row.names = FALSE, sep = ",", col.names = TRUE)
  cat("Combined data has been saved as", output_file, "\n")
}

# ---- Function for calculating Individuelle Risik calculation für Arbeitende in PerMitte (direkt Betroffen) ----
calculate_individual_risk_alle_DB_Perimeter_Mitte<-function(subdirectories, Geb_1_PF, Geb_2_PF, srcDir){
  csv_files <- list.files(path = paste0(srcDir, "/xxxx"), pattern = "_Ind_Risk_Industrie_Geb_Peri_mitte_TNT_Max.csv$", full.names = TRUE)
  combined_data <- NULL
  header <- NULL
  
  for (i in seq_along(csv_files)) {
    csv_file <- csv_files[i]
    
    data <- read.csv(csv_file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
    if (i == 1) {
      combined_data <- data
    } else {
      combined_data <- rbind(combined_data, data)
    }
  }
  combined_data$SDN <- 0.4405
  combined_data$SDA <- 0.2679
  combined_data$SDF <- 0.1488
  combined_data$SDW <- 0.1428
  combined_data$PF_N <- 0
  combined_data$PF_A <- 1
  combined_data$PF_F <- 0
  combined_data$PF_W <- 0
  combined_data$tpiN <- combined_data$SDN*combined_data$PF_N
  combined_data$tpiA <- combined_data$SDA*combined_data$PF_A
  combined_data$tpiF <- combined_data$SDF*combined_data$PF_F
  combined_data$tpiW <- combined_data$SDW*combined_data$PF_W
  combined_data$rpi_N_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Wand <- combined_data$lambda_f*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiN*combined_data$SDN
  combined_data$rpi_A_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiA*combined_data$SDA
  combined_data$rpi_F_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiF*combined_data$SDF
  combined_data$rpi_W_Kr_Lu <- combined_data$lambda_Kr_Luft*combined_data$W_LKU_max*combined_data$tpiW*combined_data$SDW
  combined_data$rpi_N_Wand_Summe <- sum(combined_data$rpi_N_Wand)
  combined_data$rpi_A_Wand_Summe <- sum(combined_data$rpi_A_Wand)
  combined_data$rpi_F_Wand_Summe <- sum(combined_data$rpi_F_Wand)
  combined_data$rpi_W_Wand_Summe <- sum(combined_data$rpi_W_Wand)
  combined_data$rpi_N_Kr_Lu_Summe <- sum(combined_data$rpi_N_Kr_Lu)
  combined_data$rpi_A_Kr_Lu_Summe <- sum(combined_data$rpi_A_Kr_Lu)
  combined_data$rpi_W_Kr_Lu_Summe <- sum(combined_data$rpi_W_Kr_Lu)
  combined_data$rpi_F_Kr_Lu_Summe <- sum(combined_data$rpi_F_Kr_Lu)
  combined_data$Summe_RP_Wand <- combined_data$rpi_N_Wand_Summe+combined_data$rpi_A_Wand_Summe+combined_data$rpi_F_Wand_Summe+combined_data$rpi_W_Wand_Summe
  combined_data$Summe_RP_Kr_Lu <-combined_data$rpi_N_Kr_Lu_Summe+combined_data$rpi_A_Kr_Lu_Summe+combined_data$rpi_F_Kr_Lu_Summe+combined_data$rpi_W_Kr_Lu_Summe
  output_file <- file.path(Ind_risk_Induistrie_Peri_mitte_TNT_max_directory, "Ind_Risk_Industrie_Geb_Peri_mitte_TNT_Max_Alle.csv")
  write.csv(combined_data, file = output_file, row.names = FALSE, sep = ",", col.names = TRUE)
  cat("Combined data has been saved as", output_file, "\n")
}



# ---- Function for calculating Lethality_Wand, Kratertrümmer (Freifeld) for gefährdungskarte ----
calculate_lethality_Wand_Freifeld <- function(distance, rb_F) {
  if (distance == 0) {
    return(1) # Lethality is max at the explosion site
  } else {
    if (distance >= rb_F[3] && distance <= rb_F[4]) {
      lethality_Wand <- 0.01
    } else if (distance >= rb_F[4] && distance <= rb_F[5]) {
      lethality_Wand <- 0.001
    } else if (distance >= rb_F[5]) {
      lethality_Wand <- 0
    } else {
      lethality_Wand <- (-41.7 + (46.7 * rb_F[3] / distance)) / 100
      lethality_Wand <- max(0, min(lethality_Wand, 1)) # Ensure lethality is between 0 and 1
    }
    
    return(lethality_Wand)
  }
}


calculate_lethality_Krater_Freifeld <- function(distance, ra_F) {
  if (distance == 0) {
    return(1) # Lethality is max at the explosion site
  } else {
    if (distance >= ra_F[3] && distance <= ra_F[4]) {
      lethality_Krater <- 0.01
    } else if (distance >= ra_F[4] && distance <= ra_F[5]) {
      lethality_Krater <- 0.001
    } else if (distance >= ra_F[5]) {
      lethality_Krater <- 0
    } else {
      lethality_Krater <- (-41.7 + (46.7 * ra_F[3] / distance)) / 100
      lethality_Krater <- max(0, min(lethality_Krater, 1)) # Ensure lethality is between 0 and 1
    }
    
    return(lethality_Krater)
  }
}




# ---- Function for calculating Lethality_Wand, Kratertrümmer (Gebäude) for gefährdungskarte ----
calculate_lethality_Wand_Geb <- function(distance, rb_G) {
  if (distance == 0) {
    return(1) # Lethality is max at the explosion site
  } else {
    if (distance >= rb_G[3] && distance <= rb_G[4]) {
      lethality_Wand <- 0.01
    } else if (distance >= rb_G[4] && distance <= rb_G[5]) {
      lethality_Wand <- 0.001
    } else if (distance >= rb_G[5]) {
      lethality_Wand <- 0
    } else {
      lethality_Wand <- (-41.7 + (46.7 * rb_G[3] / distance)) / 100
      lethality_Wand <- max(0, min(lethality_Wand, 1)) # Ensure lethality is between 0 and 1
    }
    
    return(lethality_Wand)
  }
}


calculate_lethality_Krater_Geb <- function(distance, ra_G) {
  if (distance == 0) {
    return(1) # Lethality is max at the explosion site
  } else {
    if (distance >= ra_G[3] && distance <= ra_G[4]) {
      lethality_Krater <- 0.01
    } else if (distance >= ra_G[4] && distance <= ra_G[5]) {
      lethality_Krater <- 0.001
    } else if (distance >= ra_G[5]) {
      lethality_Krater <- 0
    } else {
      lethality_Krater <- (-41.7 + (46.7 * ra_G[3] / distance)) / 100
      lethality_Krater <- max(0, min(lethality_Krater, 1)) # Ensure lethality is between 0 and 1
    }
    
    return(lethality_Krater)
  }
}




# ---- General function to calculate lethality (wand or krater) ----
calculate_lethality <- function(distance, r_G) {

  lethality_value <- ifelse(distance == 0, 1, 0)

  lethality_value[distance >= r_G[3] & distance < r_G[4]] <- 0.01
  lethality_value[distance >= r_G[4] & distance < r_G[5]] <- 0.001
  lethality_value[distance > 0 & distance < r_G[3]] <- (-41.7 + (46.7 * r_G[3] / distance[distance > 0 & distance < r_G[3]])) / 100

  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}

# ---- General function to calculate lethality (Krater für Gebäude) using coordinate values ----
calculate_lethality_ra_G <- function(distance, ra_G3,ra_G4,ra_G5) {
  
  lethality_value <- ifelse(distance == 0, 1, 0)
  
  lethality_value[distance >= ra_G3 & distance < ra_G4] <- 0.01
  lethality_value[distance >= ra_G4 & distance < ra_G5] <- 0.001
  lethality_value[distance > 0 & distance < ra_G3] <- (-41.7 + (46.7 * ra_G3 / distance[distance > 0 & distance < ra_G3])) / 100
  
  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}

# ---- General function to calculate lethality (Wand für Gebäude) using coordinate values ----
calculate_lethality_rb_G <- function(distance, rb_G3,rb_G4,rb_G5) {
  
  lethality_value <- ifelse(distance == 0, 1, 0)
  
  lethality_value[distance >= rb_G3 & distance < rb_G4] <- 0.01
  lethality_value[distance >= rb_G4 & distance < rb_G5] <- 0.001
  lethality_value[distance > 0 & distance < rb_G3] <- (-41.7 + (46.7 * rb_G3 / distance[distance > 0 & distance < rb_G3])) / 100
  
  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}


# ---- General function to calculate lethality (Krater im Freien) using coordinate values ----
calculate_lethality_ra_F <- function(distance, ra_F3,ra_F4,ra_F5) {
  
  lethality_value <- ifelse(distance == 0, 1, 0)
  
  lethality_value[distance >= ra_F3 & distance < ra_F4] <- 0.01
  lethality_value[distance >= ra_F4 & distance < ra_F5] <- 0.001
  lethality_value[distance > 0 & distance < ra_F3] <- (-41.7 + (46.7 * ra_F3 / distance[distance > 0 & distance < ra_F3])) / 100
  
  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}

# ---- General function to calculate lethality (Wand im Freien) using coordinate values ----
calculate_lethality_rb_F <- function(distance, rb_F3,rb_F4,rb_F5) {
  
  lethality_value <- ifelse(distance == 0, 1, 0)
  
  lethality_value[distance >= rb_F3 & distance < rb_F4] <- 0.01
  lethality_value[distance >= rb_F4 & distance < rb_F5] <- 0.001
  lethality_value[distance > 0 & distance < rb_F3] <- (-41.7 + (46.7 * rb_F3 / distance[distance > 0 & distance < rb_F3])) / 100
  
  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}


# ---- Select tNEQ Max just for erstellung der Gefährdungskarte ----
select_QTNT_Karte<- function(tNEQ_value_max) {
  if (tNEQ_value_max >= 6.5) {
    selected_df <- QTNT_6_5_int
  } else if (tNEQ_value_max == 6) {
    selected_df <- QTNT_6_int
  }else if (tNEQ_value_max == 5.5) {
    selected_df <- QTNT_5_5_int
  }else if (tNEQ_value_max == 5) {
    selected_df <- QTNT_5_int
  }else if (tNEQ_value_max == 4) {
    selected_df <- QTNT_4_int
  }else if (tNEQ_value_max == 3) {
    selected_df <- QTNT_2_5_int
  }else if (tNEQ_value_max == 2.5) {
    selected_df <- QTNT_2_5_int
  }else if (tNEQ_value_max == 2) {
    selected_df <- QTNT_2_int
  } else if (tNEQ_value_max == 1) {
    selected_df <- QTNT_1_int
  }else if (tNEQ_value_max == 0.8) {
    selected_df <- QTNT_0_8_int
  }else if (tNEQ_value_max == 0.5) {
    selected_df <- QTNT_0_5_int
  }else if (tNEQ_value_max == 0.4) {
    selected_df <- QTNT_0_4_int
  }else if (tNEQ_value_max == 0.3) {
    selected_df <- QTNT_0_3_int
  }else if (tNEQ_value_max == 0.22) {
    selected_df <- QTNT_0_22_int
  }else {
    return(0)
  }
  
}



# ---- modified wandletalität function for matrix operations Freifeld ----
calculate_lethality_Wand_matrix_F <- function(distance_matrix, rb_F) {
  
  lethality_matrix_F[distance_matrix == 0] <- 1  # Lethality is 1 at distance 0 (explosion point)
  
  non_zero_indices <- distance_matrix != 0  # Find indices where distance is not zero
  distances_non_zero <- distance_matrix[non_zero_indices]
  
  lethality_values <- (-41.7 + (46.7 * rb_F[3] / distances_non_zero)) / 100
  
  # Ensure lethality values are between 0 and 1
  lethality_values <- pmax(0, pmin(lethality_values, 1))
  
  # Assign the calculated lethality values back to the appropriate positions in the matrix
  lethality_matrix_F[non_zero_indices] <- lethality_values
  
  return(lethality_matrix_F)
}

# ---- modified kraterletalität function for matrix operations Freifeld ----
calculate_lethality_Krat_matrix_F <- function(distance_matrix, ra_F) {
  
  lethality_matrix_F[distance_matrix == 0] <- 1  # Lethality is 1 at distance 0 (explosion point)
  
  non_zero_indices <- distance_matrix != 0  # Find indices where distance is not zero
  distances_non_zero <- distance_matrix[non_zero_indices]
  
  lethality_values <- (-41.7 + (46.7 * ra_F[3] / distances_non_zero)) / 100
  
  # Ensure lethality values are between 0 and 1
  lethality_values <- pmax(0, pmin(lethality_values, 1))
  
  # Assign the calculated lethality values back to the appropriate positions in the matrix
  lethality_matrix_F[non_zero_indices] <- lethality_values
  
  return(lethality_matrix_F)
}

# ---- modified wandletalität function for matrix operations Gebäude ----
calculate_lethality_Wand_matrix_G <- function(distance, rb_G) {
  
  if (distance<=0) {
    lethality_value <- 1
  } else {
    lethality_value <- (-41.7 + (46.7 * rb_G[3] / distance)) / 100  
  }
  
  lethality_value <- pmax(0, pmin(lethality_value, 1))
  
  return(lethality_value)
}

# ---- modified kraterletalität function for matrix operations Gebäude ----
calculate_lethality_Krat_matrix_G <- function(distance_matrix, ra_G) {
  
  lethality_matrix_G <- matrix(0, nrow = length(y_coords), ncol = length(x_coords))
  
  
  lethality_matrix_G[distance_matrix == 0] <- 1  # Lethality is 1 at distance 0 (explosion point)
  
  non_zero_indices <- distance_matrix != 0  # Find indices where distance is not zero
  distances_non_zero <- distance_matrix[non_zero_indices]
  
  lethality_values <- (-41.7 + (46.7 * ra_G[3] / distances_non_zero)) / 100
  
  # Ensure lethality values are between 0 and 1
  lethality_values <- pmax(0, pmin(lethality_values, 1))
  
  # Assign the calculated lethality values back to the appropriate positions in the matrix
  lethality_matrix_G[non_zero_indices] <- lethality_values
  
  return(lethality_matrix_G)
}
