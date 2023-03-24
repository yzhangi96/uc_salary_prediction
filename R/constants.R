# Data cleaning constants ----
schools <- c("berkeley", "ucdavis", "ucla", "uci", "ucsc")
names_to_add <- c(
  "Graeme Blair",
  "Hao Chen",
  "Jennie Brand",
  "Jiantao Jiao",
  "Jie Peng",
  "Jiming Jiang",
  "Juhee Lee",
  "Paul Parker",
  "Peng Ding",
  "Krishnakumar Balasubramanian",
  "Qing Zhou",
  "Samuel Pimentel",
  "Shizhe Chen",
  "Song Mei",
  "Tao Gao",
  "Xiaodong Li",
  "Bingling Wang",
  "Michael Tsiang"
)

incorrect_gc <- c(
  "Alistair Sinclair",
  "Andrew Bray",
  "Anindita Adhikari",
  "Bernd Sturmfels",
  "Christina Palmer",
  "David Rigby",
  "Guang Cheng",
  "Herbie Lee",
  "Raquel Prado",
  "Rebecca Emigh",
  "Scott Bartell",
  "Steven Evans",
  "Theodore Porter",
  "Thomas Lee",
  "Yun Song",
  "Adam Lucas",
  "Andrew Farris",
  "David Armstrong",
  "David Zes",
  "Juana Sanchez",
  "Maria Cha",
  "Robert Gould",
  "Thomas Ferguson",
  "Vivian Lew",
  "Wesley Johnson",
  "William Clark"
)

# Visualization and modeling constants ----
y <- "Total.Pay"
num_covariates <- c("school_score",
                    "h_index",
                    "count_unique_coauthor",
                    "count_pub",
                    "year_first_pub",
                    "cited_by",
                    "year_last_pub")
cat_covariates <- c("pred_male","tenure_track")

train_perc <- 0.85