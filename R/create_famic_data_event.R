#' Create FAMIC database for event analysis
#'
#' This function searches for .xlsm files in the specified folder (and
#' optionally sub-folders), and checks whether they are all named in a
#' ID_WEEKNUMBER_weeks.xlsm format, and whether they contain the appropriate number
#' of codes. If these checks are passed, the function then extracts all data
#' from the coded .xlsm files, and organises them in a tibble object, ready for
#' event analysis.
#'
#' @param path Path to the folder containing coded .xlsm files
#' @param recursive Boolean specifying whether sub-folders should also be searched
#' @return A dataframe object
#' @export
create_famic_data_event <- function(path, recursive = TRUE) {
  
  c_m <- code <- code_codes <- code_names <- e_r <- sec <- NULL
  
  file_paths <- list.files(path = path, pattern = "*.xlsm", full.names = TRUE, recursive = recursive)
  
  file_names <- basename(file_paths)
  
  for (i in 1:length(file_names)) {
    if(!grepl("[a-zA-Z0-9]+_\\d_[a-zA-Z]+\\.[a-zA-Z]+", file_names[i])) {
      stop(paste("The name of file located at", file_paths[i], "is not in the format ID_WEEKNUMBER_weeks.xlsm"))
    }
  }
  
  codes <- data.frame(
    c_m = c(rep("c", 19), rep("m", 20)),
    e_r = c(rep("e", 23), rep("r", 16)),
    code_names = c("non_com", "ltp", "mo", "yawn", "in_mouth", "smile", "raised_frown", "neg_mouth", "neg_eye", "cry", "voc", "neg_voc", "bio", "gaze_m", "gaze_e", "gaze_a", "gaze_u", "pos_limb", "neg_limb", "m_ltp", "m_mo", "m_smile", "m_nod", "d_mir", "e_mir", "p_mir", "m_mir", "vit_s", "vit_ns", "aff_s", "aff_ns", "cont_s", "cont_ns", "reg_cont", "hyper_mir", "hyper_mar", "hypo_mir", "hypo_mar", "neg"),
    code_codes = c("A1", "A2a", "A2b", "A3", "A4", "B1", "B2", "B3a", "B3b", "B4", "C1", "C2", "C3", "D1", "D2", "D3", "D4", "E1", "E2", "M1", "M2", "M3", "M4", "A1", "A2", "A3", "A4", "A5s", "A5ns", "A6as", "A6ans", "A6bs", "A6bns", "B1", "C1a", "C1b", "C2a", "C2b", "C3"))
  
  mother_infant_data <- data.frame()
  
  for (f in 1:length(file_names)) {
    raw_data <- suppressMessages(readxl::read_excel(file_paths[f], sheet = "Coding", skip = 2))
    
    if (nrow(raw_data) != 40) {
      stop(paste("The file located at", file_paths[i], "does not contain all codes"))
    }
    
    colnames(raw_data) <- c("code", "behav", paste0("sec", seq(1:(ncol(raw_data) - 2))))
    
    raw_data <- raw_data[!is.na(raw_data$code), ]
    
    raw_data$code <- paste0(c(rep("c", 19), rep("m", 20)), raw_data$code)
    
    database_data <- suppressMessages(readxl::read_excel(file_paths[f], sheet = "Database", skip = 2))
    
    colnames(database_data) <- c("code", "behav", paste0("sec", seq(1:(ncol(database_data) - 2))))
    
    database_data <- database_data[!is.na(database_data$code), ]
    
    database_data$code <- paste0(c(rep("c", 19), rep("m", 20)), database_data$code)
    
    database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)][!(matrix((unlist(gregexpr("First", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]))) == 3) %in% TRUE, nrow = nrow(database_data) - which(database_data$code == "mA1") + 1))] <- NA
    
    for (i in 3:ncol(raw_data)) {
      if (sum(!is.na(raw_data[1:13, i]), na.rm = TRUE) > 1) {
        stop(paste("There are multiple infant behaviours at second", i - 2, "of file", file_names[f]))
      }
      if (sum(!is.na(raw_data[24:39, i]), na.rm = TRUE) > 1) {
        stop(paste("There are multiple maternal responses at second", i - 2, "of file", file_names[f]))
      }
    }
    
    if (length(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1))) != 
        length(unique(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1))))) {
      stop(paste("The infant behaviour located in cell(s)", 
                 paste(unique(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1))[duplicated(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1)))]), collapse = " and "),
                 "of file",
                 file_names[f],
                 "has(have) multiple responses"))
    }
    
    interaction_data <- data.frame(sec = seq(1:(ncol(raw_data) - 2)))
    
    for (i in 1:nrow(codes)) {
      if (codes$e_r[[i]] == "e") {
        interaction_data[, codes$code_names[[i]]] <-
          as.numeric(raw_data[raw_data$code == paste0(
            codes$c_m[[i]],
            codes$code_codes[[i]]), ][, c(-1, -2)])
        
      } else if (codes$e_r[[i]] == "r") {
        interaction_data[, codes$code_names[[i]]] <-
          as.character(raw_data[raw_data$code == paste0(
            codes$c_m[[i]],
            codes$code_codes[[i]]), ][, c(-1, -2)])
      }
    }
    
    for (i in 1:nrow(codes[codes$e_r == "r", ])) {
      
      interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]] <-
        ifelse(interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]] == "NA", NA,
               ifelse(
                 grepl(
                   "-R|-S|-RS",
                   interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]]
                 ), NA,
                 stringr::str_remove(
                   interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]],
                   "-.*")))
      
      for (j in 1:nrow(codes[codes$c_m == "c", ])) {
        interaction_data[, paste(
          codes[codes$e_r == "r", ]$code_names[[i]],
          codes[codes$c_m == "c", ]$code_names[[j]], sep = "_2_")] <-
          ifelse(
            grepl(
              codes[codes$c_m == "c", ]$code_codes[[j]],
              interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]]),
            interaction_data[, codes[codes$e_r == "r", ]$code_names[[i]]], NA)
      }
      interaction_data <-
        interaction_data[ ,!(colnames(interaction_data) == codes[codes$e_r == "r", ]$code_names[[i]])]
      
    }
    
    mother_infant_data <-
      rbind(mother_infant_data,
            cbind(data.frame(
              id = sub("\\_.*", "", file_names[f]),
              week = as.numeric(sub("\\_.*", "", substring(file_names[f], nchar(sub("\\_.*", "", file_names[f])) + 2))),
              seconds = nrow(interaction_data)), (data.frame(as.list(sapply(interaction_data[, -1], function(x) sum(!is.na(x))))))))
    
  }
  
  mother_infant_data$prespeech <- mother_infant_data$ltp + mother_infant_data$mo
  
  mother_infant_data$soc_behav <- mother_infant_data$ltp + mother_infant_data$mo + mother_infant_data$smile + mother_infant_data$voc
  
  mother_infant_data$mouth_movements <- mother_infant_data$soc_behav + mother_infant_data$non_com
  
  mother_infant_data$yawn_bio <- mother_infant_data$yawn + mother_infant_data$bio
  
  mother_infant_data$negative_affect <- mother_infant_data$neg_mouth + mother_infant_data$cry + mother_infant_data$neg_voc
  
  mother_infant_data$all_behav <- mother_infant_data$mouth_movements + mother_infant_data$yawn_bio + mother_infant_data$negative_affect
  
  mother_infant_data$gaze_mother <- (mother_infant_data$gaze_m * 100) / mother_infant_data$seconds
  
  mother_infant_data$mir_2_prespeech <- mother_infant_data$d_mir_2_ltp + mother_infant_data$e_mir_2_ltp + mother_infant_data$m_mir_2_ltp + mother_infant_data$d_mir_2_mo + mother_infant_data$e_mir_2_mo + mother_infant_data$m_mir_2_mo
  mother_infant_data$vit_s_2_prespeech <- mother_infant_data$vit_s_2_ltp + mother_infant_data$vit_s_2_mo
  mother_infant_data$aff_s_2_prespeech <- mother_infant_data$aff_s_2_ltp + mother_infant_data$aff_s_2_mo
  mother_infant_data$vit_ns_2_prespeech <- mother_infant_data$vit_ns_2_ltp + mother_infant_data$vit_ns_2_mo
  mother_infant_data$aff_ns_2_prespeech <- mother_infant_data$aff_ns_2_ltp + mother_infant_data$aff_ns_2_mo
  mother_infant_data$aff_mark_s_2_prespeech <- mother_infant_data$vit_s_2_prespeech + mother_infant_data$aff_s_2_prespeech
  mother_infant_data$aff_mark_ns_2_prespeech <- mother_infant_data$vit_ns_2_prespeech + mother_infant_data$aff_ns_2_prespeech
  mother_infant_data$neg_resp_2_prespeech <- mother_infant_data$hyper_mir_2_ltp + mother_infant_data$hyper_mar_2_ltp + mother_infant_data$hypo_mir_2_ltp + mother_infant_data$hypo_mar_2_ltp + mother_infant_data$neg_2_ltp + mother_infant_data$hyper_mir_2_mo + mother_infant_data$hyper_mar_2_mo + mother_infant_data$hypo_mir_2_mo + mother_infant_data$hypo_mar_2_mo + mother_infant_data$neg_2_mo
  mother_infant_data$pos_resp_2_prespeech <- mother_infant_data$mir_2_prespeech + mother_infant_data$aff_mark_s_2_prespeech
  mother_infant_data$all_resp_2_prespeech <- mother_infant_data$d_mir_2_ltp + mother_infant_data$e_mir_2_ltp + mother_infant_data$m_mir_2_ltp + mother_infant_data$vit_s_2_ltp + mother_infant_data$vit_ns_2_ltp + mother_infant_data$aff_s_2_ltp + mother_infant_data$aff_ns_2_ltp + mother_infant_data$hyper_mir_2_ltp + mother_infant_data$hyper_mar_2_ltp + mother_infant_data$hypo_mir_2_ltp + mother_infant_data$hypo_mar_2_ltp + mother_infant_data$neg_2_ltp + mother_infant_data$d_mir_2_mo + mother_infant_data$e_mir_2_mo + mother_infant_data$m_mir_2_mo + mother_infant_data$vit_s_2_mo + mother_infant_data$vit_ns_2_mo + mother_infant_data$aff_s_2_mo + mother_infant_data$aff_ns_2_mo + mother_infant_data$hyper_mir_2_mo + mother_infant_data$hyper_mar_2_mo + mother_infant_data$hypo_mir_2_mo + mother_infant_data$hypo_mar_2_mo + mother_infant_data$neg_2_mo
  
  mother_infant_data$d_mir_2_prespeech <- mother_infant_data$d_mir_2_ltp + mother_infant_data$d_mir_2_mo
  mother_infant_data$d_mir_2_soc_behav <- mother_infant_data$d_mir_2_prespeech + mother_infant_data$d_mir_2_smile + mother_infant_data$d_mir_2_voc
  mother_infant_data$d_mir_2_yawn_bio <- mother_infant_data$d_mir_2_yawn + mother_infant_data$d_mir_2_bio
  mother_infant_data$d_mir_2_negative_affect <- mother_infant_data$d_mir_2_neg_mouth + mother_infant_data$d_mir_2_cry + mother_infant_data$d_mir_2_neg_voc
  mother_infant_data$d_mir_2_all <- mother_infant_data$d_mir_2_non_com + mother_infant_data$d_mir_2_soc_behav + mother_infant_data$d_mir_2_yawn_bio + mother_infant_data$d_mir_2_negative_affect
  
  mother_infant_data$e_mir_2_prespeech <- mother_infant_data$e_mir_2_ltp + mother_infant_data$e_mir_2_mo
  mother_infant_data$e_mir_2_soc_behav <- mother_infant_data$e_mir_2_prespeech + mother_infant_data$e_mir_2_smile + mother_infant_data$e_mir_2_voc
  mother_infant_data$e_mir_2_yawn_bio <- mother_infant_data$e_mir_2_yawn + mother_infant_data$e_mir_2_bio
  mother_infant_data$e_mir_2_negative_affect <- mother_infant_data$e_mir_2_neg_mouth + mother_infant_data$e_mir_2_cry + mother_infant_data$e_mir_2_neg_voc
  mother_infant_data$e_mir_2_all <- mother_infant_data$e_mir_2_non_com + mother_infant_data$e_mir_2_soc_behav + mother_infant_data$e_mir_2_yawn_bio + mother_infant_data$e_mir_2_negative_affect
  
  mother_infant_data$m_mir_2_prespeech <- mother_infant_data$m_mir_2_ltp + mother_infant_data$m_mir_2_mo
  mother_infant_data$m_mir_2_soc_behav <- mother_infant_data$m_mir_2_prespeech + mother_infant_data$m_mir_2_smile + mother_infant_data$m_mir_2_voc
  mother_infant_data$m_mir_2_yawn_bio <- mother_infant_data$m_mir_2_yawn + mother_infant_data$m_mir_2_bio
  mother_infant_data$m_mir_2_negative_affect <- mother_infant_data$m_mir_2_neg_mouth + mother_infant_data$m_mir_2_cry + mother_infant_data$m_mir_2_neg_voc
  mother_infant_data$m_mir_2_all <- mother_infant_data$m_mir_2_non_com + mother_infant_data$m_mir_2_soc_behav + mother_infant_data$m_mir_2_yawn_bio + mother_infant_data$m_mir_2_negative_affect
  
  mother_infant_data$mir_2_non_com <- mother_infant_data$d_mir_2_non_com + mother_infant_data$e_mir_2_non_com + mother_infant_data$m_mir_2_non_com
  mother_infant_data$aff_mark_s_2_non_com <- mother_infant_data$vit_s_2_non_com + mother_infant_data$aff_s_2_non_com
  mother_infant_data$aff_mark_ns_2_non_com <- mother_infant_data$vit_ns_2_non_com + mother_infant_data$aff_ns_2_non_com
  mother_infant_data$neg_resp_2_non_com <- mother_infant_data$hyper_mir_2_non_com + mother_infant_data$hyper_mar_2_non_com + mother_infant_data$hypo_mir_2_non_com + mother_infant_data$hypo_mar_2_non_com + mother_infant_data$neg_2_non_com
  mother_infant_data$pos_resp_2_non_com <- mother_infant_data$mir_2_non_com + mother_infant_data$aff_mark_s_2_non_com
  mother_infant_data$all_resp_2_non_com <- mother_infant_data$pos_resp_2_non_com + mother_infant_data$neg_resp_2_non_com + mother_infant_data$aff_mark_ns_2_non_com
  
  mother_infant_data$mir_2_soc_behav <- mother_infant_data$mir_2_prespeech + mother_infant_data$d_mir_2_smile + mother_infant_data$e_mir_2_smile + mother_infant_data$m_mir_2_smile + mother_infant_data$d_mir_2_voc + mother_infant_data$e_mir_2_voc + mother_infant_data$m_mir_2_voc
  mother_infant_data$aff_mark_s_2_soc_behav <- mother_infant_data$vit_s_2_prespeech + mother_infant_data$vit_s_2_smile + mother_infant_data$vit_s_2_voc + mother_infant_data$aff_s_2_prespeech + mother_infant_data$aff_s_2_smile + mother_infant_data$aff_s_2_voc
  mother_infant_data$aff_mark_ns_2_soc_behav <- mother_infant_data$vit_ns_2_prespeech + mother_infant_data$vit_ns_2_smile + mother_infant_data$vit_ns_2_voc + mother_infant_data$aff_ns_2_prespeech + mother_infant_data$aff_ns_2_smile + mother_infant_data$aff_ns_2_voc
  mother_infant_data$neg_resp_2_soc_behav <- mother_infant_data$neg_resp_2_prespeech + mother_infant_data$hyper_mir_2_smile + mother_infant_data$hyper_mar_2_smile + mother_infant_data$hypo_mir_2_smile + mother_infant_data$hypo_mar_2_smile + mother_infant_data$neg_2_smile + mother_infant_data$hyper_mir_2_voc + mother_infant_data$hyper_mar_2_voc + mother_infant_data$hypo_mir_2_voc + mother_infant_data$hypo_mar_2_voc + mother_infant_data$neg_2_voc
  mother_infant_data$pos_resp_2_soc_behav <- mother_infant_data$mir_2_soc_behav + mother_infant_data$aff_mark_s_2_soc_behav
  mother_infant_data$all_resp_2_soc_behav <- mother_infant_data$pos_resp_2_soc_behav + mother_infant_data$neg_resp_2_soc_behav + mother_infant_data$aff_mark_ns_2_soc_behav
  mother_infant_data$all_resp_2_smile <- mother_infant_data$d_mir_2_smile + mother_infant_data$e_mir_2_smile + mother_infant_data$m_mir_2_smile + mother_infant_data$vit_s_2_smile + mother_infant_data$vit_ns_2_smile + mother_infant_data$aff_s_2_smile + mother_infant_data$aff_ns_2_smile + mother_infant_data$hyper_mir_2_smile + mother_infant_data$hyper_mar_2_smile + mother_infant_data$hypo_mir_2_smile + mother_infant_data$hypo_mar_2_smile + mother_infant_data$neg_2_smile
  mother_infant_data$all_resp_2_voc <- mother_infant_data$d_mir_2_voc + mother_infant_data$e_mir_2_voc + mother_infant_data$m_mir_2_voc + mother_infant_data$vit_s_2_voc + mother_infant_data$vit_ns_2_voc + mother_infant_data$aff_s_2_voc + mother_infant_data$aff_ns_2_voc + mother_infant_data$hyper_mir_2_voc + mother_infant_data$hyper_mar_2_voc + mother_infant_data$hypo_mir_2_voc + mother_infant_data$hypo_mar_2_voc + mother_infant_data$neg_2_voc
  
  mother_infant_data$mir_2_mouth_movements <- mother_infant_data$mir_2_soc_behav + mother_infant_data$mir_2_non_com
  mother_infant_data$aff_mark_s_2_mouth_movements <- mother_infant_data$aff_mark_s_2_soc_behav + mother_infant_data$vit_s_2_non_com + mother_infant_data$aff_s_2_non_com
  mother_infant_data$aff_mark_ns_2_mouth_movements <- mother_infant_data$aff_mark_ns_2_soc_behav + mother_infant_data$vit_ns_2_non_com + mother_infant_data$aff_ns_2_non_com
  mother_infant_data$neg_resp_2_mouth_movements <- mother_infant_data$neg_resp_2_soc_behav + mother_infant_data$neg_resp_2_non_com
  mother_infant_data$pos_resp_2_mouth_movements <- mother_infant_data$mir_2_mouth_movements + mother_infant_data$aff_mark_s_2_mouth_movements
  mother_infant_data$all_resp_2_mouth_movements <- mother_infant_data$pos_resp_2_mouth_movements + mother_infant_data$neg_resp_2_mouth_movements + mother_infant_data$aff_mark_ns_2_mouth_movements
  
  mother_infant_data$mir_2_yawn_bio <- mother_infant_data$d_mir_2_yawn + mother_infant_data$e_mir_2_yawn + mother_infant_data$m_mir_2_yawn + mother_infant_data$d_mir_2_bio + mother_infant_data$e_mir_2_bio + mother_infant_data$m_mir_2_bio
  mother_infant_data$aff_mark_s_2_yawn_bio <- mother_infant_data$vit_s_2_yawn + mother_infant_data$aff_s_2_yawn + mother_infant_data$vit_s_2_bio + mother_infant_data$aff_s_2_bio
  mother_infant_data$aff_mark_ns_2_yawn_bio <- mother_infant_data$vit_ns_2_yawn + mother_infant_data$aff_ns_2_yawn + mother_infant_data$vit_ns_2_bio + mother_infant_data$aff_ns_2_bio
  mother_infant_data$neg_resp_2_yawn_bio <- mother_infant_data$hyper_mir_2_yawn + mother_infant_data$hyper_mar_2_yawn + mother_infant_data$hypo_mir_2_yawn + mother_infant_data$hypo_mar_2_yawn + mother_infant_data$neg_2_yawn + mother_infant_data$hyper_mir_2_bio + mother_infant_data$hyper_mar_2_bio + mother_infant_data$hypo_mir_2_bio + mother_infant_data$hypo_mar_2_bio + mother_infant_data$neg_2_bio
  mother_infant_data$pos_resp_2_yawn_bio <- mother_infant_data$mir_2_yawn_bio + mother_infant_data$aff_mark_s_2_yawn_bio
  mother_infant_data$all_resp_2_yawn_bio <- mother_infant_data$pos_resp_2_yawn_bio + mother_infant_data$neg_resp_2_yawn_bio + mother_infant_data$aff_mark_ns_2_yawn_bio
  
  mother_infant_data$mir_2_negative_affect <- mother_infant_data$d_mir_2_neg_mouth + mother_infant_data$e_mir_2_neg_mouth + mother_infant_data$m_mir_2_neg_mouth + mother_infant_data$d_mir_2_cry + mother_infant_data$e_mir_2_cry + mother_infant_data$m_mir_2_cry + mother_infant_data$d_mir_2_neg_voc + mother_infant_data$e_mir_2_neg_voc + mother_infant_data$m_mir_2_neg_voc
  mother_infant_data$aff_mark_s_2_negative_affect <- mother_infant_data$vit_s_2_neg_mouth + mother_infant_data$aff_s_2_neg_mouth + mother_infant_data$vit_s_2_cry + mother_infant_data$aff_s_2_cry + mother_infant_data$vit_s_2_neg_voc + mother_infant_data$aff_s_2_neg_voc
  mother_infant_data$aff_mark_ns_2_negative_affect <- mother_infant_data$vit_ns_2_neg_mouth + mother_infant_data$aff_ns_2_neg_mouth + mother_infant_data$vit_ns_2_cry + mother_infant_data$aff_ns_2_cry + mother_infant_data$vit_ns_2_neg_voc + mother_infant_data$aff_ns_2_neg_voc
  mother_infant_data$neg_resp_2_negative_affect <- mother_infant_data$hyper_mir_2_neg_mouth + mother_infant_data$hyper_mar_2_neg_mouth + mother_infant_data$hypo_mir_2_neg_mouth + mother_infant_data$hypo_mar_2_neg_mouth + mother_infant_data$neg_2_neg_mouth + mother_infant_data$hyper_mir_2_cry + mother_infant_data$hyper_mar_2_cry + mother_infant_data$hypo_mir_2_cry + mother_infant_data$hypo_mar_2_cry + mother_infant_data$neg_2_cry + mother_infant_data$hyper_mir_2_neg_voc + mother_infant_data$hyper_mar_2_neg_voc + mother_infant_data$hypo_mir_2_neg_voc + mother_infant_data$hypo_mar_2_neg_voc + mother_infant_data$neg_2_neg_voc
  mother_infant_data$pos_resp_2_negative_affect <- mother_infant_data$mir_2_negative_affect + mother_infant_data$aff_mark_s_2_negative_affect
  mother_infant_data$all_resp_2_negative_affect <- mother_infant_data$pos_resp_2_negative_affect + mother_infant_data$neg_resp_2_negative_affect + mother_infant_data$aff_mark_ns_2_negative_affect
  
  mother_infant_data$mir_2_all <- mother_infant_data$mir_2_mouth_movements + mother_infant_data$mir_2_yawn_bio + mother_infant_data$mir_2_negative_affect
  mother_infant_data$aff_mark_s_2_all <- mother_infant_data$aff_mark_s_2_mouth_movements + mother_infant_data$aff_mark_s_2_yawn_bio + mother_infant_data$aff_mark_s_2_negative_affect
  mother_infant_data$aff_mark_ns_2_all <- mother_infant_data$aff_mark_ns_2_mouth_movements + mother_infant_data$aff_mark_ns_2_yawn_bio + mother_infant_data$aff_mark_ns_2_negative_affect
  mother_infant_data$neg_resp_2_all <- mother_infant_data$neg_resp_2_mouth_movements + mother_infant_data$neg_resp_2_yawn_bio + mother_infant_data$neg_resp_2_negative_affect
  mother_infant_data$pos_resp_2_all <- mother_infant_data$pos_resp_2_mouth_movements + mother_infant_data$pos_resp_2_yawn_bio + mother_infant_data$pos_resp_2_negative_affect
  mother_infant_data$all_resp_2_all <- mother_infant_data$all_resp_2_mouth_movements + mother_infant_data$all_resp_2_yawn_bio + mother_infant_data$all_resp_2_negative_affect
  mother_infant_data$neg_2_all <- mother_infant_data$neg_2_non_com + mother_infant_data$neg_2_ltp + mother_infant_data$neg_2_mo + mother_infant_data$neg_2_yawn + mother_infant_data$neg_2_smile + mother_infant_data$neg_2_neg_mouth + mother_infant_data$neg_2_cry + mother_infant_data$neg_2_voc + mother_infant_data$neg_2_neg_voc + mother_infant_data$neg_2_bio
  mother_infant_data$hyper_hypo_2_all <- mother_infant_data$neg_resp_2_all - mother_infant_data$neg_2_all
  
  mother_infant_data$mir_2_non_soc_behav <- mother_infant_data$mir_2_all - mother_infant_data$mir_2_soc_behav
  mother_infant_data$aff_mark_s_2_non_soc_behav <- mother_infant_data$aff_mark_s_2_all - mother_infant_data$aff_mark_s_2_soc_behav
  mother_infant_data$aff_mark_ns_2_non_soc_behav <- mother_infant_data$aff_mark_ns_2_all - mother_infant_data$aff_mark_ns_2_soc_behav
  mother_infant_data$neg_resp_2_non_soc_behav <- mother_infant_data$neg_resp_2_all - mother_infant_data$neg_resp_2_soc_behav
  mother_infant_data$pos_resp_2_non_soc_behav <- mother_infant_data$pos_resp_2_all - mother_infant_data$pos_resp_2_soc_behav
  mother_infant_data$all_resp_2_non_soc_behav <- mother_infant_data$all_resp_2_all - mother_infant_data$all_resp_2_soc_behav
  
  mother_infant_data$mir_2_gaze_mother <- mother_infant_data$d_mir_2_gaze_m + mother_infant_data$e_mir_2_gaze_m + mother_infant_data$m_mir_2_gaze_m
  mother_infant_data$aff_mark_s_2_gaze_mother <- mother_infant_data$vit_s_2_gaze_m + mother_infant_data$aff_s_2_gaze_m
  mother_infant_data$aff_mark_ns_2_gaze_mother <- mother_infant_data$vit_ns_2_gaze_m + mother_infant_data$aff_ns_2_gaze_m
  mother_infant_data$neg_resp_2_gaze_mother <- mother_infant_data$hyper_mir_2_gaze_m + mother_infant_data$hyper_mar_2_gaze_m + mother_infant_data$hypo_mir_2_gaze_m + mother_infant_data$hypo_mar_2_gaze_m + mother_infant_data$neg_2_gaze_m
  mother_infant_data$pos_resp_2_gaze_mother <- mother_infant_data$mir_2_gaze_mother + mother_infant_data$aff_mark_s_2_gaze_mother
  mother_infant_data$all_resp_2_gaze_mother <- mother_infant_data$pos_resp_2_gaze_mother + mother_infant_data$neg_resp_2_gaze_mother + mother_infant_data$aff_mark_ns_2_gaze_mother
  
  id_week <-
    data.frame(
      id = rep(unique(mother_infant_data$id), 3),
      week = rep(c(5, 7, 9), each = length(unique(mother_infant_data$id)))
    )
  
  mother_infant_data <-
    merge(x = id_week, y = mother_infant_data, by = c("id", "week"), all.x = TRUE)
  
  mother_infant_data_pred <-
    data.frame(
      id = mother_infant_data[mother_infant_data$week != 5, ]$id,
      week = mother_infant_data[mother_infant_data$week != 5, ]$week - 2,
      seconds_pred = mother_infant_data[mother_infant_data$week != 5, ]$seconds,
      non_com_pred = mother_infant_data[mother_infant_data$week != 5, ]$non_com,
      soc_behav_pred = mother_infant_data[mother_infant_data$week != 5, ]$soc_behav,
      mouth_movements_pred = mother_infant_data[mother_infant_data$week != 5, ]$mouth_movements,
      yawn_bio_pred = mother_infant_data[mother_infant_data$week != 5, ]$yawn_bio,
      negative_affect_pred = mother_infant_data[mother_infant_data$week != 5, ]$negative_affect,
      all_behav_pred = mother_infant_data[mother_infant_data$week != 5, ]$all_behav,
      gaze_mother_pred = mother_infant_data[mother_infant_data$week != 5, ]$gaze_mother
    )
  
  mother_infant_data <-
    merge(x = mother_infant_data, y = mother_infant_data_pred, by = c("id", "week"), all.x = TRUE)
  
  mother_infant_data$visit <- factor(paste("Week", mother_infant_data$week))
  
  all_events <- 
    data.frame(
      id = mother_infant_data$id,
      visit = mother_infant_data$visit,
      no_resp_2_non_com = mother_infant_data$non_com - mother_infant_data$all_resp_2_non_com,
      all_resp_2_non_com = mother_infant_data$all_resp_2_non_com,
      no_resp_2_soc_behav = mother_infant_data$soc_behav - mother_infant_data$all_resp_2_soc_behav,
      all_resp_2_soc_behav = mother_infant_data$all_resp_2_soc_behav,
      no_resp_2_prespeech = mother_infant_data$prespeech - mother_infant_data$all_resp_2_prespeech,
      all_resp_2_prespeech = mother_infant_data$all_resp_2_prespeech,
      no_resp_2_smile = mother_infant_data$smile - mother_infant_data$all_resp_2_smile,
      all_resp_2_smile = mother_infant_data$all_resp_2_smile,
      no_resp_2_voc = mother_infant_data$voc - mother_infant_data$all_resp_2_voc,
      all_resp_2_voc = mother_infant_data$all_resp_2_voc,
      no_resp_2_yawn_bio = mother_infant_data$yawn_bio - mother_infant_data$all_resp_2_yawn_bio,
      all_resp_2_yawn_bio = mother_infant_data$all_resp_2_yawn_bio,
      no_resp_2_negative_affect = mother_infant_data$negative_affect - mother_infant_data$all_resp_2_negative_affect,
      all_resp_2_negative_affect = mother_infant_data$all_resp_2_negative_affect
    )
  all_events_id <- rep(all_events$id, ncol(all_events) - 2)
  all_events_visit <- rep(all_events$visit, ncol(all_events) - 2)
  all_events <- reshape(all_events[, -1:-2], dir = "long", varying = names(all_events[, -1:-2]), 
                        v.names = "value", timevar = "child_behav", times = names(all_events[, -1:-2]))
  all_events$id <- rep(all_events_id, ncol(all_events) - 2)
  all_events$visit <- rep(all_events_visit, ncol(all_events) - 2)
  rownames(all_events) <- NULL
  all_events <- all_events[!is.na(all_events$value), c(3, 4, 1, 2)]
  
  all_events <- cbind(all_events[, -which(names(all_events) == "child_behav")], t(as.data.frame(strsplit(all_events$child_behav, '_2_'), row.names=c("response", "child_behav"))), row.names=NULL)[, c(1, 2, 4, 5, 3)]
  
  all_events <-
  all_events[rep(seq_len(nrow(all_events)), all_events$value), -5]
  
  all_events2 <-  
    data.frame(
      id = mother_infant_data$id,
      visit = mother_infant_data$visit,
      no_resp_2_non_com = mother_infant_data$non_com - (mother_infant_data$all_resp_2_non_com),
      mir_2_non_com = mother_infant_data$mir_2_non_com,
      aff_mark_s_2_non_com = mother_infant_data$vit_s_2_non_com + mother_infant_data$aff_s_2_non_com,
      aff_mark_ns_2_non_com = mother_infant_data$vit_ns_2_non_com + mother_infant_data$aff_ns_2_non_com,
      neg_resp_2_non_com = mother_infant_data$neg_resp_2_non_com,
      no_resp_2_soc_behav = mother_infant_data$soc_behav - mother_infant_data$all_resp_2_soc_behav,
      mir_2_soc_behav = mother_infant_data$mir_2_soc_behav,
      aff_mark_s_2_soc_behav = mother_infant_data$aff_mark_s_2_soc_behav,
      aff_mark_ns_2_soc_behav = mother_infant_data$aff_mark_ns_2_soc_behav,
      neg_resp_2_soc_behav = mother_infant_data$neg_resp_2_soc_behav,
      no_resp_2_prespeech = mother_infant_data$prespeech - mother_infant_data$all_resp_2_prespeech,
      mir_2_prespeech = mother_infant_data$mir_2_prespeech,
      aff_mark_s_2_prespeech = mother_infant_data$vit_s_2_prespeech + mother_infant_data$aff_s_2_prespeech,
      aff_mark_ns_2_prespeech = mother_infant_data$vit_ns_2_prespeech + mother_infant_data$aff_ns_2_prespeech,
      neg_resp_2_prespeech = mother_infant_data$neg_resp_2_prespeech,
      no_resp_2_smile = mother_infant_data$smile - mother_infant_data$all_resp_2_smile,
      mir_2_smile = mother_infant_data$d_mir_2_smile + mother_infant_data$e_mir_2_smile + mother_infant_data$m_mir_2_smile,
      aff_mark_s_2_smile = mother_infant_data$vit_s_2_smile + mother_infant_data$aff_s_2_smile,
      aff_mark_ns_2_smile = mother_infant_data$vit_ns_2_smile + mother_infant_data$aff_ns_2_smile,
      neg_resp_2_smile = mother_infant_data$hyper_mir_2_smile + mother_infant_data$hyper_mar_2_smile + mother_infant_data$hypo_mir_2_smile + mother_infant_data$hypo_mar_2_smile + mother_infant_data$neg_2_smile,
      no_resp_2_voc = mother_infant_data$voc - mother_infant_data$all_resp_2_voc,
      mir_2_voc = mother_infant_data$d_mir_2_voc + mother_infant_data$e_mir_2_voc + mother_infant_data$m_mir_2_voc,
      aff_mark_s_2_voc = mother_infant_data$vit_s_2_voc + mother_infant_data$aff_s_2_voc,
      aff_mark_ns_2_voc = mother_infant_data$vit_ns_2_voc + mother_infant_data$aff_ns_2_voc,
      neg_resp_2_voc = mother_infant_data$hyper_mir_2_voc + mother_infant_data$hyper_mar_2_voc + mother_infant_data$hypo_mir_2_voc + mother_infant_data$hypo_mar_2_voc + mother_infant_data$neg_2_voc,
      no_resp_2_yawn_bio = mother_infant_data$yawn_bio - mother_infant_data$all_resp_2_yawn_bio,
      mir_2_yawn_bio = mother_infant_data$mir_2_yawn_bio,
      aff_mark_s_2_yawn_bio = mother_infant_data$aff_mark_s_2_yawn_bio,
      aff_mark_ns_2_yawn_bio = mother_infant_data$aff_mark_ns_2_yawn_bio,
      neg_resp_2_yawn_bio = mother_infant_data$neg_resp_2_yawn_bio,
      no_resp_2_negative_affect = mother_infant_data$negative_affect - mother_infant_data$all_resp_2_negative_affect,
      mir_2_negative_affect = mother_infant_data$mir_2_negative_affect,
      aff_mark_s_2_negative_affect = mother_infant_data$aff_mark_s_2_negative_affect,
      aff_mark_ns_2_negative_affect = mother_infant_data$aff_mark_ns_2_negative_affect,
      neg_resp_2_negative_affect = mother_infant_data$neg_resp_2_negative_affect
    )
  all_events2_id <- rep(all_events2$id, ncol(all_events2) - 2)
  all_events2_visit <- rep(all_events2$visit, ncol(all_events2) - 2)
  all_events2 <- reshape(all_events2[, -1:-2], dir = "long", varying = names(all_events2[, -1:-2]), 
                        v.names = "value", timevar = "child_behav", times = names(all_events2[, -1:-2]))
  all_events2$id <- rep(all_events2_id, ncol(all_events2) - 2)
  all_events2$visit <- rep(all_events2_visit, ncol(all_events2) - 2)
  rownames(all_events2) <- NULL
  all_events2 <- all_events2[!is.na(all_events2$value), c(3, 4, 1, 2)]
  
  all_events2 <- cbind(all_events2[, -which(names(all_events2) == "child_behav")], t(as.data.frame(strsplit(all_events2$child_behav, '_2_'), row.names=c("response", "child_behav"))), row.names=NULL)[, c(1, 2, 4, 5, 3)]
  
  all_events2 <-
    all_events2[rep(seq_len(nrow(all_events2)), all_events2$value), -5]
  
  all_events <- all_events[order(all_events$id, all_events$visit),]
  all_events2 <- all_events2[order(all_events2$id, all_events2$visit),]
  
  all_events <-
    cbind(all_events[, 1:3], mat_resp = all_events2$response, child_behav = all_events[, 4])
  
  all_events$child_behav <-
    factor(
      all_events$child_behav,
      levels = c("non_com", "soc_behav", "prespeech", "smile", "voc", "yawn_bio", "negative_affect"),
      labels = c(
        "Non-Social Mouth Movements",
        "Social Behaviours",
        "Pre-Speech",
        "Smiles",
        "Vocalisations",
        "Biological Events",
        "Negative Affect"
      )
    )
  all_events$response <-
    factor(
      all_events$response,
      levels = c("no_resp", "all_resp"),
      labels = c("Non-Responded", "Responded")
    )
  all_events$mat_resp <-
    factor(
      all_events$mat_resp,
      levels = c("mir", "aff_mark_s", "aff_mark_ns", "neg_resp", "no_resp"),
      labels = c(
        "Mirroring",
        "Positive Marking",
        "Neutral Marking",
        "Negative Response",
        "Not Responsed"
      )
    )
  all_events$child_behav <-
    relevel(all_events$child_behav, "Non-Social Mouth Movements")
  
  all_events$mir = ifelse(all_events$mat_resp == "Mirroring", "Mirroring", "Non-Mirroring")
  all_events$aff_mark_s = ifelse(all_events$mat_resp == "Positive Marking", "Positive Marking", "Non-Positive Marking")
  all_events$aff_mark_ns = ifelse(
    all_events$mat_resp == "Neutral Marking",
        "Neutral Marking",
        "Non-Neutral Marking"
      )
  all_events$neg_resp = ifelse(
    all_events$mat_resp == "Negative Response",
        "Negative Response",
        "Non-Negative Response"
      )
  all_events$non_soc_mouth_mov = ifelse(
    all_events$child_behav == "Non-Social Mouth Movements",
        "Non-Social Mouth Movements",
        "Non-Non-Social Mouth Movements"
      )
  all_events$soc_behav = ifelse(
    all_events$child_behav == "Social Behaviours",
        "Social Behaviours",
        "Non-Social Behaviours"
      )
  all_events$pre_speech = ifelse(all_events$child_behav == "Pre-Speech", "Pre-Speech", "Non-Pre-Speech")
  all_events$smile = ifelse(all_events$child_behav == "Smiles", "Smiles", "Non-Smiles")
  all_events$voc = ifelse(
    all_events$child_behav == "Vocalisations",
        "Vocalisations",
        "Non-Vocalisations"
      )
  all_events$bio = ifelse(
    all_events$child_behav == "Biological Events",
        "Biological Events",
        "Non-Biological Events"
      )
  all_events$negative_affect = ifelse(
    all_events$child_behav == "Negative Affect",
        "Negative Affect",
        "Non-Negative Affect"
      )
  
  all_events$mir <- factor(all_events$mir)
  all_events$mir <- relevel(all_events$mir, ref = "Non-Mirroring")
  all_events$aff_mark_s <- factor(all_events$aff_mark_s)
  all_events$aff_mark_s <- relevel(all_events$aff_mark_s, ref = "Non-Positive Marking")
  all_events$aff_mark_ns <- factor(all_events$aff_mark_ns)
  all_events$aff_mark_ns <-
    relevel(all_events$aff_mark_ns, ref = "Non-Neutral Marking")
  all_events$neg_resp <- factor(all_events$neg_resp)
  all_events$neg_resp <- relevel(all_events$neg_resp, ref = "Non-Negative Response")
  all_events$non_soc_mouth_mov <- factor(all_events$non_soc_mouth_mov)
  all_events$non_soc_mouth_mov <-
    relevel(all_events$non_soc_mouth_mov, ref = "Non-Non-Social Mouth Movements")
  all_events$soc_behav <- factor(all_events$soc_behav)
  all_events$soc_behav <-
    relevel(all_events$soc_behav, ref = "Non-Social Behaviours")
  all_events$pre_speech <- factor(all_events$pre_speech)
  all_events$pre_speech <- relevel(all_events$pre_speech, ref = "Non-Pre-Speech")
  all_events$smile <- factor(all_events$smile)
  all_events$smile <- relevel(all_events$smile, ref = "Non-Smiles")
  all_events$voc <- factor(all_events$voc)
  all_events$voc <- relevel(all_events$voc, ref = "Non-Vocalisations")
  all_events$bio <- factor(all_events$bio)
  all_events$bio <-
    relevel(all_events$bio, ref = "Non-Biological Events")
  all_events$negative_affect <- factor(all_events$negative_affect)
  all_events$negative_affect <- relevel(all_events$negative_affect, ref = "Non-Negative Affect")
  
  baserate_db <- 
    data.frame(
      id = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$id,
      visit = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$visit,
      non_soc_mouth_mov = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$non_com / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      soc_behav = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$soc_behav / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      pre_speech = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$prespeech / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      smile = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$smile / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      voc = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$voc / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      bio = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$yawn_bio / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60),
      negative_affect = mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$negative_affect / (mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit),]$seconds / 60)
    )
  
  baserate_db_id <- rep(baserate_db$id, ncol(baserate_db) - 2)
  baserate_db_visit <- rep(baserate_db$visit, ncol(baserate_db) - 2)
  baserate_db <- reshape(baserate_db[, -1:-2], dir = "long", varying = names(baserate_db[, -1:-2]), 
                         v.names = "value", timevar = "cb", times = names(baserate_db[, -1:-2]))
  baserate_db$id <- rep(baserate_db_id, ncol(baserate_db) - 2)
  baserate_db$visit <- rep(baserate_db_visit, ncol(baserate_db) - 2)
  rownames(baserate_db) <- NULL
  baserate_db <- baserate_db[baserate_db$value > 0, c(3, 4, 1, 2)]
  baserate_db <- baserate_db[!is.na(baserate_db$value), ]
  
  baserate_db2 <-
    mother_infant_data[order(mother_infant_data$id, mother_infant_data$visit), which(names(mother_infant_data) %in% c("id", "visit", "non_com", "soc_behav", "prespeech", "smile", "voc", "yawn_bio", "negative_affect"))][, c(1, 9, 2, 6, 5, 3, 4, 7, 8)]
  baserate_db2_id <- rep(baserate_db2$id, ncol(baserate_db2) - 2)
  baserate_db2_visit <- rep(baserate_db2$visit, ncol(baserate_db2) - 2)
  baserate_db2 <- reshape(baserate_db2[, -1:-2], dir = "long", varying = names(baserate_db2[, -1:-2]), 
                         v.names = "value", timevar = "cb", times = names(baserate_db2[, -1:-2]))
  baserate_db2$id <- rep(baserate_db2_id, ncol(baserate_db2) - 2)
  baserate_db2$visit <- rep(baserate_db2_visit, ncol(baserate_db2) - 2)
  rownames(baserate_db2) <- NULL
  baserate_db2 <- baserate_db2[baserate_db2$value > 0, c(3, 4, 1, 2)]
  baserate_db2 <- baserate_db2[!is.na(baserate_db2$value), ]
  baserate_db2$rep <- baserate_db2$value
  
  baserate <-
    cbind(
      baserate_db,
      rep = baserate_db2$rep
    )
  
  baserate <- baserate[order(baserate$id, baserate$visit),]
  
  baserate <- baserate[rep(seq_len(nrow(baserate)), baserate$rep), -5]
  
  all_events <-
    cbind(all_events,
          baserate = baserate$value)
  rownames(all_events) <- 1:nrow(all_events)
  
  return(all_events)
}
