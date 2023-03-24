## -------------------------------------------
##
## Script name: Process_SEER
##
## Author: Ivy Zhang
##
## Date Last Modified: 2020-02-16
## Last Modified by: Ivy Zhang
##
## -------------------------------------------
##
## Notes:  
##  Create a merged dataset for some UC schools'
##  faculty + 2021 salary 
##
##  
## -------------------------------------------

# Local imports ----
cur_wd <- getwd()
source(paste0(cur_wd,"/constants.R"))
all_faculty_names <- read.csv(paste0(cur_wd,"/final_df_2022_12_16.csv"))
salary <- read.csv(paste0(cur_wd,"/university-of-california-2021.csv"))

# Helper functions ----
'%!in%' <- function(x,y)!('%in%'(x,y))

# Merge faculty names and salary ----
uc_faculty <- all_faculty_names[
  (all_faculty_names$school %in% schools) & (all_faculty_names$role == "faculty"), 
  covariates
  ]

# Unfortunate manual name adjustments
# Some are because they were not on 2021 payroll (recent UC hire)
# Others are because of different name used
uc_faculty$name[uc_faculty$name == "Ani Adhikari"] <- "Anindita Adhikari"
uc_faculty$name[uc_faculty$name == "Will Fithian"] <- "William Fithian"
uc_faculty$name[uc_faculty$name == "Fletcher Hank Ibser"] <- "Fletcher Ibser"
uc_faculty$name[uc_faculty$name == "Nicholas P. Jewell"] <- "Nicholas Jewell"
uc_faculty$name[uc_faculty$name == "Sam Pimentel"] <- "Samuel Pimentel"
uc_faculty$name[uc_faculty$name == "Gaston Sanchez"] <- "Gaston Sanchez Trujillo"
uc_faculty$name[uc_faculty$name == "Sam Pimentel"] <- "Samuel Pimentel"
uc_faculty$name[uc_faculty$name == "Yun S. Song"] <- "Yun Song"
uc_faculty$name[uc_faculty$name == "Philip B. Stark"] <- "Philip Stark"
uc_faculty$name[uc_faculty$name == "Jacob Steinhardt"] <- "Jacob Noah Steinhardt"
uc_faculty$name[uc_faculty$name == "Jairo Fúquene Patiño"] <- "Jairo Fuquene Patino"
uc_faculty$name[uc_faculty$name == "Jane-ling Wang"] <- "Jane-Ling Wang"
uc_faculty$name[uc_faculty$name == "Bala Rajaratnam"] <- "Balakanapathy Rajaratnam"
uc_faculty$name[uc_faculty$name == "Maxime Guiffo Pouokam"] <- "Maxime Pouokam"
uc_faculty$name[uc_faculty$name == "Wesley O. Johnson"] <- "Wesley Johnson"
uc_faculty$name[uc_faculty$name == "Oscar Madrid Padilla"] <- "Oscar Hernan Madrid Padilla"
uc_faculty$name[uc_faculty$name == "Rick Schoenberg"] <- "Frederic Paik Schoenberg"
uc_faculty$name[uc_faculty$name == "Song-chun Zhu"] <- "Song-Chun Zhu"
uc_faculty$name[uc_faculty$name == "Dave Zes"] <- "David Zes"
uc_faculty$name[uc_faculty$name == "Marcela Alfaro-Córdoba"] <- "Marcela Alfaro Cordoba"
uc_faculty$name[uc_faculty$name == "Herbert Lee"] <- "Herbie Lee"
uc_faculty$name[uc_faculty$name == "Paul A. Parker"] <- "Paul Parker"
uc_faculty$name[uc_faculty$name == "Bruno Sansó"] <- "Bruno Sanso"

# Merge and write
uc_salary <- salary[
  salary$Employee.Name %in% uc_faculty$name,c("Employee.Name", "Total.Pay")
  ]
uc_salary$Total.Pay <- as.numeric(uc_salary$Total.Pay)
uc_salary <- uc_salary[order(uc_salary$Employee.Name, -uc_salary$Total.Pay),]

# Found online but not in csv
uc_salary <- rbind(uc_salary, c("Can Le", 136617))

final_df <- merge(
  uc_faculty, uc_salary[!duplicated(uc_salary$Employee.Name),], by.x="name", 
  by.y="Employee.Name", all.x = F, all.y = F
)
final_df$Total.Pay <- as.numeric(final_df$Total.Pay)

# 49 excluded
missing_names <- uc_faculty[uc_faculty$name %!in% final_df$name, c("name")]

write.csv(final_df, paste0(cur_wd,"/salary_uc.csv"))
