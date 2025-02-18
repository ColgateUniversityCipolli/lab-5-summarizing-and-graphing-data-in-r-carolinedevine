################################################################################
# Lab 5
# Caroline Devine 
################################################################################
# This is a continuation of the previous work we have done in lab.



################################################################################
# Lab 5: Coding Task - Summarize the Data
################################################################################

library("tidyverse")
help("tidyverse")

# Upload data 
essentia.data <- read_csv("data/essentia.data.csv")
view(essentia.data)

essentia.data.allentown <- read_csv("data/essentia.data.allentown.csv")
view(essentia.data.allentown)

################################################################################
# Step 1: Out of Range Function 
################################################################################

range.allentown <- function(data, data.allentown, feature){
  # get feature value for Allentown
  allentown.feature <- get(feature, data.allentown)
  
  summary.data <- data %>%
  group_by(artist) %>%
  summarize(
      min = min(get(feature), na.rm=T),
      Q1 = quantile(get(feature), 0.25, na.rm=T),
      Q3 = quantile(get(feature), 0.75, na.rm=T),
      IQR = IQR(get(feature), na.rm=T),
      LF = Q1 - 1.5*IQR,
      UF = Q3 + 1.5*IQR,
      max = max(get(feature), na.rm=T)
    )%>%
    
  #ungroup()
  
  # New columns for comparison
  mutate(
    out.of.range = (allentown.feature < min | allentown.feature > max),
    unusual = (allentown.feature < LF | allentown.feature > UF),
    description = case_when(
      out.of.range ~ "Out of Range",
      unusual ~ "Outlying",
      TRUE ~ "Within Range",
    )
  )
  
  return(summary.data)
}


final <- range.allentown(essentia.data, essentia.data.allentown, "overall_loudness")
final <- final |>
  select(artist, min, LF, UF, max, out.of.range, unusual, description)
print(final)

################################################################################
# Step 2
################################################################################

# numeric columns
numeric.data <- names(essentia.data)[sapply(essentia.data, is.numeric)]
view(numeric.data)

final_all <- lapply(numeric.data, function(feature){result <- range.allentown(essentia.data, 
                                                                essentia.data.allentown, feature)
                                                    result$feature <- feature # adding to result data frame
                                                    return(result)})
final_all <- bind_rows(final_all)
view(final_all)
