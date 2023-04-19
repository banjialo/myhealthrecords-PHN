# import employment data
library(readxl)
df <- read_excel("Employment.xlsx",
                 # last line with valid data is 2669 in spreadsheet
                 n_max = 2668)
# check bottom lines
tail(df)

library(stringr)
# only keep first 4 characters for postcodes
df$`POA Codes` <- str_sub(df$`POA Codes`, end = 4)
# quick checks:
unique(df$`POA Codes`) #
unique(str_length(df$`POA Codes`)) # all of length 4

# import the postcode table
pc <- read_excel("PostCodes.xls",
                 sheet = 4,
                 # last interesting line in spreadsheet is 2843
                 n_max = 2842)
# check bottom lines
tail(pc)

# merge with dplyr
library(dplyr)
merged <- left_join(df, pc,
                    by = c("POA Codes" = "POA_CODE_2016"))

# check validity
sample_n(merged, 20) %>% View()

# check a split postcode
filter(merged,
       `POA Codes` == "0872") %>% 
  View() # present three times

# in original employment dataset
filter(df,
       `POA Codes` == "2000") %>% 
  View() # only one in original employment dataset

# this means that we've got repeated data,
# so we need to multiply numbers by corresponding ratio
data_ratiod <- merged %>%
  mutate_at(vars(`Employed, worked full-time`:Total),
            funs(. * RATIO))

# check total population in dataset
summarise(data_ratiod, pop = sum(Total))
# ... does it match the total Australian population?

# sum one single variable according to region
data_ratiod %>%
  group_by(PHN_NAME_2017) %>% 
  summarise(sum(`Employed, worked full-time`))

# sum several variables, grouped by region
data_ratiod %>%
  group_by(PHN_NAME_2017) %>% 
  summarise_at(vars(`Employed, worked full-time`:`Total`),
               funs(sum(.))) %>% 
  View()

# save as new object to later merge with shapefile
sum_data <- data_ratiod %>%
  group_by(PHN_NAME_2017) %>% 
  summarise_at(vars(`Employed, worked full-time`:`Total`),
               funs(sum(., na.rm = TRUE))) # ...and ignore NAs!

