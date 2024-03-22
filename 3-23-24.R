library(tidyverse)
library(pdftools)

# link 1
pdf1 <- pdftools::pdf_text("https://boe.cuyahogacounty.gov/docs/default-source/boe/maps-and-data-library/pollinglocationlist.pdf?sfvrsn=6d0955e8_22")

df1 <- tibble(raw = str_split(pdf1, "\\n")) |> 
  unnest(raw) |>
  filter(str_detect(raw, " [A-Z]{2,}-")) |> 
  mutate(raw = str_trim(raw),
         raw = str_replace(raw, "   ", ";")) |> 
  separate_wider_delim(cols = "raw", delim = ";", names = c("code","location"), too_many = "drop") |> 
  separate_wider_delim(cols = "location", delim = ",", names = c("name","street","zip")) |>
  mutate(across(everything(), str_trim))

df1

# link 2
pdf2 <- pdftools::pdf_text("https://boe.cuyahogacounty.gov/docs/default-source/boe/voters-page/voter-registration-locations.pdf?sfvrsn=2feea41c_6")

raw2 <- tibble(raw = str_split(pdf2, "\\n")) |>
  unnest(raw) 

df2 <- tibble(city = as.character(), vote = as.character())

raw2_vector <- raw2 |> pull(raw)

city <- "test"
vote <- "test"

for (i in 1:length(raw2_vector)) {
  if (str_detect(raw2_vector[i], "^[A-Z]")){
    df2 <- df2 |>
      add_row(tibble(city = city,
                     vote = vote))
    vote <- c()
    city <- raw2_vector[i]
    next
  }
  if (str_detect(raw2_vector[i], "•")) {
      vote <- c(vote, str_extract(raw2_vector[i], "(?<=•   ).*"))
  }
}

df2 <- df2 |>
  filter(city != "test")

df2
