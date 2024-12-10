
#### load libraries
library(tidyverse)

#### load data
patents_df_all <- readRDS("exam_TWEA1.R")

#### initial investigation

### rows
# one row for each chemical patent subclassification and year
# year the patent was granted

### columns
## [column_name], [short description], [opt. #unique values/comment on values]
## uspto_class, classifies patents according to technology fields, 4625
## grntyr, year of patent grant, 1939 - 1875 + 1 = 65 unique values
## count_usa, no. patents developed in US, 42 unique values, lowest 0 highest 68
## count_france, I guess the same as count_usa but for France
## count_germany, I guess the as count_usa but for for Germany
## count ??
## count_for ??
## count_noger ??
## count_for_noger ??
## main ?
## mainclass_id, main class code, 10 unique values, the values and no. obs. are:
#     8    74   101   106   204   416   430   528   534   536
# 26910 48880 37050 64134 25394 18980 29380 19110 21385  9360
# all were affected by TWEA, see online_appendix_from_article.pdf
## subcl, subclass code, 2287 unique values
## year_conf ??
## count_cl, number of patents adopted under forced licensing, between 0 and 7
# ~98.7% is 0
## licensed_class ?
## confiscated_class ?
## class_id ?
## year_conf_2 ?
## anypatent, 1 if any patent have been adopted under forced licensing, binary
## itt ??
## count_for_2 ??
## year_conf_itt ?
## count_cl_itt, number of available patents under forced licensing
## count_cl_2 ??
## twea, Whether TWEA was enacted, binary
# 0 for all uspto_class up until 1919 then 1 for 4623 of them.




# Q: How was the development of patents in USA, France and Germany? time-series
# how many, in the same classes etc, ...


# Q: is there data on all subclasses all years

tabl_uspto_class <- table(patents_df_all$uspto_class) # if 65 obs. then obs.
                                                      # all years in dataset

# classes where there is not observations all years
not_all_years <- names(tabl_uspto_class)[tabl_uspto_class < 65]

# two not very influental subcategories -> remove them

# Q: was there stolen patents in all subclasses?



# Split the dataset up if just repeating values for the TWEA?


#### WANT to see how compulsory licensing effects innovation
#### WANT TO SEE HOW TWEA AFFECTED NO. PATENTS DEVELOPED IN US
# condition on:
# - is a chemical subcategory
# - TWEA was in-acted
# so we regard 10 out of 19 uspto main classes were twea was
# in-acted (only the chemicals)
# and x out of y chemical classes

patent_df <- patents_df_all |> filter(!uspto_class %in% not_all_years) |>
                           select(
                             mainclass_id,    # no change
                             subcl,           # no change
                             uspto_class,     # no change
                             grntyr,          # change every year
                             count_usa,       # change every year
                             count_france,    # change every year
                             count_germany,   # change every year
                             count_cl,        # change after 1918 only
                             count_cl_itt,     # change after 1918 only
                             twea
                           ) |>
                           mutate(mainclass_name = case_when(
                             mainclass_id == 8 ~"Bleaching and dyeing; fluid  treatment and chemical modification of textiles and fibers",
                             mainclass_id == 74 ~ "Machine element or mechanism",
                             mainclass_id == 101 ~ "Printing",
                             mainclass_id == 106 ~ "Compositions: coating or plastic",
                             mainclass_id == 204 ~ "Chemistry: electrical and wave energy",
                             mainclass_id == 416 ~ "Fluid reaction surfaces (i.e., impellers)",
                             mainclass_id == 430 ~ "Radiation imagery chemistry: process, composition, or products",
                             mainclass_id == 528 ~ "Synthetic resins or natural rubbers",
                             mainclass_id == 534 ~ "Organic Compounds—Containing a noble gas",
                             mainclass_id == 536 ~ "Organic Compounds—Carbohydrates and derivatives",
                             TRUE ~ "error"
                           ),
                           mainclass_id = as.character(mainclass_id)
                           ) |> group_by(uspto_class) |>
                                 mutate(cum_usa = cumsum(count_usa),
         cum_france = cumsum(count_france),
         cum_germany = cumsum(count_germany)) |>
         relocate(
           mainclass_name, .after = mainclass_id
         )

rm(not_all_years, tabl_uspto_class)

save(patent_df, file = "patent_df.RData")

glimpse(patent_df)

## Documentation

docu <- matrix(c(
  "name", "Description", "Yearly changes",
  "mainclass_id", "ID number of 10 unique classes of patents", "No",
  "mainclass_name", "Name of mainclass", "No",
  "subcl", "2287 unique subclassifications", "No",
  "uspto_class", "4623 unique subsubclassifications", "No",
  "grntyr", "Year patent was granted", "Yes",
  "count_usa", "Number of patents granted in USA", "Yes",
  "count_france", "Number of patents granted in France", "Yes",
  "count_germany", "Number of patents granted in Germany", "Yes",
  "count_cl", "Number of patents adopted under forced licensing", "Only one change after 1918",
  "count_cl_itt", "Number of available patents under forced licensing", "Only one change after 1918",
  "cum_usa", "Cumulative sum of count_usa per uspto_class", "Yes",
  "cum_france", "Cumulative sum of count_france per uspto_class", "Yes",
  "cum_germany", "Cumulative sum of count_germany per uspto_class", "Yes",
  "twea", "1 if grntyr >= 1919, 0 otherwise"
), byrow = TRUE, ncol = 3)

docu

save(docu, file = "docu.RData")
