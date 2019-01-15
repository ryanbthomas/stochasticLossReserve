
library(here)
library(purrr)
library(glue)

main_url <- "https://www.casact.org/research/reserve_data/"

lobs <- c("ppauto_pos.csv",
          "wkcomp_pos.csv",
          "comauto_pos.csv",
          "medmal_pos.csv",
          "prodliab_pos.csv",
          "othliab_pos.csv"
)


if (!dir.exists("cache")) {
    dir.create("cache")
}

walk(lobs, ~ download.file(glue("{main_url}{.}"), destfile = glue("cache/{.}")))






