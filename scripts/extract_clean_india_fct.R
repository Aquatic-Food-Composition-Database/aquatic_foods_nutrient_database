

library(tidyverse);library(tabulizer);library(here)

india_pdf <- here("data","India_ifct-doc.pdf")

india_tables <- extract_tables(india_pdf)
