################################################################################
# Lab 5
# Caroline Devine 
################################################################################


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
# Step 2: Numeric Data
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

################################################################################
# Step 3: Identify Features and Create LaTex Table
################################################################################

# Look at 20 features that between the three bands only one is within range
# This will give us the information on who had the most influence whereas the rest
# of the features do not give us direct information that allows us to dissect who 
# had more of an influence

select.features <- c("positivewords", "OtherP", "Perception", "conj", "chords_strength",
                     "average_loudness", "barkbands_flatness_db","barkbands_skewness", "dissonance",
                     "erbbands_flatness_db", "erbbands_skewness", "melbands_flatness_db", 
                     "melbands_spread", "spectral_centroid", "spectral_complexity", 
                     "spectral_energyband_middle_high", "spectral_entropy", 
                     "spectral_kurtosis", "spectral_rolloff", "spectral_skewness")
selected.data <- final_all |>
  filter(feature %in% select.features)
view(selected.data) 

# isolate the artist, feature, and description (within range, outlying, or out of range)
select.data <- selected.data |>
  select("artist", "feature", "description")
view(select.data) 

isolated.features <- select.data |>
  filter(feature %in% c("average_loudness", "chords_strength", "dissonance", "spectral_rolloff",
                        "positivewords", "OtherP", "Perception", "conj"))
view(isolated.features)

library("xtable")
table <- xtable(isolated.features)
view(table)
print(table, include.rownames = F) # 24 rows (slightly large) but 4 features from LIWC and 4 features from Essentia


################################################################################
# Step 4: Graphing Selected Features
################################################################################
library(tidyverse)
library(patchwork)

# #graph.isolated.features <- selected.data |>
#  # filter(feature %in% c("average_loudness", "chords_strength", "dissonance", "spectral_rolloff",
#  #                       "positivewords", "OtherP", "Perception", "conj"))
# #view(graph.isolated.features)
# 
# lyrical.features <- c("positivewords", "OtherP", "Perception", "conj")
# sound.features <- c("average_loudness", "chords_strength", "dissonance", "spectral_rolloff")
# 
# lyrical.df <- isolated.features |>
#   filter(feature %in% lyrical.features, description == "Within Range")
# view(lyrical.df)
# 
# lyric.plot <- ggplot(lyrical.df, aes(x = feature, fill = artist)) +
#   geom_bar(position = "dodge") + 
#   theme_bw() +
#   xlab("Lyrical Feature") +
#   ylab("Count of Within Range") + 
#   ggtitle("Lyrical Features")
#   
# sound.df <- isolated.features |>
#   filter(feature %in% sound.features, description == "Within Range")
# view(sound.df)
# 
# sound.plot <- ggplot(sound.df, aes(x = feature, fill = artist)) +
#   geom_bar(position = "dodge") + 
#   theme_bw() +
#   xlab("Sound Feature") +
#   ylab("Count of Within Range") + 
#   ggtitle("Sound Features")
# 

# Box Plot

essentia.data
view(essentia.data)

library(ggplot2)

view(essentia.data.allentown)

### Essentia Data: Sound ###

# Dissonance

dissonance.plot <- ggplot(data=essentia.data,                       # Specify the data
                aes(x=artist, y=dissonance)) +         # Specify x and y
        geom_violin(fill="grey80")+
        geom_boxplot(width = 0.1) +
        theme_bw()+                                       # Remove the grey background
        xlab("Artist")+                                   # Label x axis
        ylab("Dissonance")+                           # Label y axis
        geom_hline(yintercept = essentia.data.allentown$dissonance, 
                  linetype = "dotted", linewidth = 1)
dissonance.plot


# Average Loudness

average.plot <- ggplot(data=essentia.data,                       # Specify the data
                aes(x=artist, y=average_loudness)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Average Loudness")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$average_loudness, 
             linetype = "dotted", linewidth = 1)

average.plot

# Chords Strength

chords.plot <- ggplot(data=essentia.data,                       # Specify the data
                       aes(x=artist, y=chords_strength)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Chords Strength")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$chords_strength, 
             linetype = "dotted", linewidth = 1)

chords.plot

# Spectral Rolloff

rolloff.plot <- ggplot(data=essentia.data,                       # Specify the data
                      aes(x=artist, y=spectral_rolloff)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Spectral Rolloff")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$spectral_rolloff, 
             linetype = "dotted", linewidth = 1)

rolloff.plot


#### LIWC Data: Lyrical ####

# Perception

perception.plot <- ggplot(data=essentia.data,                       # Specify the data
                       aes(x=artist, y=Perception)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Perception")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$Perception, 
             linetype = "dotted", linewidth = 1)

perception.plot

# Positive Words

positive.plot <- ggplot(data=essentia.data,                       # Specify the data
                          aes(x=artist, y=positivewords)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Positive Words")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$positivewords, 
             linetype = "dotted", linewidth = 1)

positive.plot


# Other Parts of Speech

other.plot <- ggplot(data=essentia.data,                       # Specify the data
                        aes(x=artist, y=OtherP)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("OtherP")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$OtherP, 
             linetype = "dotted", linewidth = 1)

other.plot

# Conjunctions

conj.plot <- ggplot(data=essentia.data,                       # Specify the data
                      aes(x=artist, y=conj)) +         # Specify x and y
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.1) +
  theme_bw()+                                       # Remove the grey background
  xlab("Artist")+                                   # Label x axis
  ylab("Conjunctions")+                           # Label y axis
  geom_hline(yintercept = essentia.data.allentown$conj, 
             linetype = "dotted", linewidth = 1)

conj.plot


### Combine Plots ####
library(patchwork)
help(patchwork)
combined.lyric.plot <- conj.plot + other.plot + positive.plot + perception.plot + plot_annotation(title = "Lyrical Features")
combined.lyric.plot

combined.sound.plot <- average.plot + dissonance.plot + chords.plot + rolloff.plot + plot_annotation(title = "Sound Features")
combined.sound.plot
