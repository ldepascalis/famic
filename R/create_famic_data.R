#' Create FAMIC database
#'
#' This function searches for .xlsm files in the specified folder (and
#' optionally sub-folders), and checks whether they are all named in a
#' ID_WEEKNUMBER_weeks.xlsm format, and whether they contain the appropriate number
#' of codes. If these checks are passed, the function then extracts all data
#' from the coded .xlsm files, and organises them in a tibble object.
#'
#' @param path Path to the folder containing coded .xlsm files
#' @param recursive Boolean specifying whether sub-folders should also be searched
#' @return A dataframe object
#' @export
create_famic_data <- function(path, recursive = TRUE) {

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
  
  if (length(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1))) != 
      length(unique(gsub("[\\(\\)]", "", lapply(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))[lengths(regmatches(as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)]), gregexpr("\\(.*?\\)", as.matrix(database_data[which(database_data$code == "mA1"):nrow(database_data),3:ncol(database_data)])))) >0], "[[", 1))))) {
    stop(paste("The infant behaviour(s) located in cell(s)", 
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

  return(mother_infant_data)
}
