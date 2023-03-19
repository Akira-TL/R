chic = readr::read_csv("/home/tangl/documents/Rtest.csv")
tibble::glimpse(chic)

library(ggplot2)
library(tidyverse)
g <- ggplot(chic, aes(x = date, y = temp))
