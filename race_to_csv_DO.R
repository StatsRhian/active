############
# This converts a single text or pdf race result file to a csv and saves
# Designed to be called from "race_to_csv_ALL.R" as variables are not all defined here but
# are instead held in a list called "vars".
############

file_name <- str_c(path_root, "Results_raw/", vars$name)
# Read pdf
if(is_pdf(file_name)){
  txt <- pdf_text(file_name)
}else{
  txt <- readLines(file_name)
}

# Extract entries as character vector with one entry per runner
# Trims irrelevant lines
lines <- str_split(txt,"\n") %>% unlist
keep <- vapply(lines, FUN=is_numeric_first, FUN.VALUE=logical(1))
lines <- lines[keep][1 : vars$n_entries]

# Process entries to match fellrunner data form 
n <- length(lines)
fr_entries <- character(n)
for (i in 1 : n){
  fr_entries[i] <- convert_entry(lines[i], name_loc=c(vars$name_loc, vars$name_loc2),
                                 cat_loc=vars$cat_loc, club_start=vars$club_start, 
                                 after_club=vars$after_club, cat_missing = vars$cat_missing) %>%
    str_c(collapse=",")
}

# Save as CSV, adding a line for headers 
files_root <- str_sub(vars$name, 1, -5) %>% unlist #remove ".pdf" etc.
csv_name <- str_c(path_root, "Results_CSV/", files_root, ".csv")
headings <- "name,club,category,time"
cat(headings, fr_entries, file=csv_name, sep="\n")
# cat seems to automatically add a \r to each newline when saving so using sep='\n' results in 
# a \r\n after each entry which is what I want.
