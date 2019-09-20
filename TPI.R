#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                                                        +
# +  File: TPI.R                                                           +
# +  Aim: Calculating TPI index for one or more metro areas                +
# +  Description: This script allows for the calculation of the TPI index, +
# +               as in Pritchard et al. (2019)                            +
# +  Authors: Anna Zanchetta                                               +
# +  Note: depends on script 'DatabaseCreation_groups.R'                   +
# +  Date: ~July 2019                                                      +
# +  Version: 1                                                            +
# +                                                                        +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(dplyr)
library(tidyr)
library(tidyverse)
require(ggplot2)

# Functions declaration ----
## Creating the areal function for calculation of the TPI. f_diff is the distance function of choice. In this case the L1 distance is used (f(x) - g(x)).
areal_fun <- function(f, g, lower_lim, upper_lim) {
   dist_fun <- function(x) (f(x) - g(x))
   round(integrate(f = f_diff, 
                   lower = lower_lim, 
                   upper = upper_lim)$value,
         3)
}

# 1. Calculations ----

## Defining the vector in input of the For loop, namely a vector of strings that are the names of the  Metropolitan areas for the analysis (in our case they are 49 towns, one could run this for a single town of course)
towns_list <- as.character(metros_codes_list$MetroName) # Change according to need

## Creating indexing variable for the For loop
index <- 0
## Creating the matrix for the results output of the For loop
metro_areas_matrix <- matrix(NA,
                             nrow = length(towns_list), 
                             ncol = 3) # (the output is defined with three columns, they will be: distance, area, TPI)
## Creating index for the matrix of the TPI results as output of the For loop, each row will be filled up within the For loop and the index gives the row number. Each row of the matrix is one metro area:
row_index <- 0
## In the following For loop each metro areas will be considered singularly and the TPI calculation performed
for(m in towns_list){
   print(m) # Checking the status of the calculations, good when dealing with many metro areas
   index = index +1
   row_index <- row_index + 1
   metro <- subset(group_data_metros, MetroName == m) # Creating the dataframe with data of the single metro area by subsetting the bigger dataframe containing all the mtros on the column "MetroName". Each row is a Block Group.
   metro_removeNA <-metro[complete.cases(metro[ , "PTacc"]),] # Removing NA cells in order for the following command to work properly
   metro_orderacc <- metro_removeNA[order(metro_removeNA$PTacc), ] # Ordering the dataframe by growing PT accessibility
   
   TT <- sum(metro_orderacc$CarlessPOP, # Creating the target variable (TT) summing on the column that contaings the amount of target population of interest ## IMPORTANT! Choose here the population!
             na.rm = TRUE)
   P <- sum(metro_orderacc$POP, # Creating the total population variable (P) by summing the column with population amounts
            na.rm = TRUE)
   Tperc <- round(TT / P, # Obtaining the share of the target population as a percentage over the total population
                  3)
   
   metro_cumsums <- metro_orderacc %>% # Creating dataframe with cumulative sums and 
      mutate(cum_nocar = cumsum(replace_na(CarlessPOP, 0)), ## IMPORTANT: choose the same column as at line 51
             cum_pop = cumsum(replace_na(POP, 0)) ,
             cum_nocar_norm = round(cum_nocar / P, 2),
             cum_pop_norm = round(cum_pop / P, 2)
      )
   
   # # 6. AREA ANALYSIS ---
   # Assigning values to x axes for curves creation:
   x_nocar <- metro_cumsums$cum_pop_norm
   x_worst <- c(0, Tperc, 1)
   x_best <- c(0, 1 - Tperc, 1)
   
   # Assigning values to y axes for curves creation:
   y_nocar <- metro_cumsums$cum_nocar_norm
   y_worst <- c(0, Tperc, Tperc)
   y_best <- c(0, 0, Tperc)
   
   # Function of the carless people curve:
   Fnocar = approxfun(x_nocar,
                      y_nocar)
   # Function of the worst case scenario curve:
   Fworst = approxfun(x_worst,
                      y_worst)
   # Function of the best case scenario curve:
   Fbest = approxfun(x_best,
                     y_best)
   
   A <- areal_fun(Fworst, Fbest, 0, 1)
   
   areal_dist <- areal_fun(Fnocar, Fbest, 0, 1)
   
   TPI <- round( areal_dist / A,
                 3)
   metro_row <- c(areal_dist, A, TPI)
   # Adding the result to the matrix for each metro (where each metro is a row)
   metro_areas_matrix[row_index, ] <- metro_row
}
metro_areas_df <- data.frame(metro_areas_matrix,
                             stringsAsFactors = F)

colnames(metro_areas_df) <- c("areal_dist", "A", "TPI")
colnames(metro_areas_df) <- c("areal_dist", "A_car", "TPI_car")
colnames(metro_areas_df) <- c("areal_dist_income", "A_income", "TPI_income")
metro_areas_df$CBSAfips <- metros_codes_list$CBSAfips
metro_areas_df$MetroName <- metros_codes_list$MetroName

# only once, for saving the data with all the towns:
TPItable_car <- metro_areas_df
write.csv(TPItable_car,
          file = "TPI_car.csv")
TPItable_income <- metro_areas_df
write.csv(TPItable_income,
          file = "TPI_income.csv")

# Plotting ----
plot(Fnocar, 0, 1)
plot(Fbest, 0, 1)
plot(Fworst, 0, 1)

ggplot(data.frame(x = c(0, 1)),
       aes(x)) + 
   stat_function(fun = Fnocar, size = 0.7) +
   stat_function(fun = Fworst, size = 0.7) +
   stat_function(fun = Fbest, size = 0.7) +
   #theme_classic() +
   theme(panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_text(size = 15),
         axis.text = element_text(size = 12),
         axis.title.y = element_text(size = 15)) +
   labs(x = "p",
        y = "t") +
   # annotate("text", x = 0.08, y = 0.88, label = "w", size = 5) +
   # annotate("text", x = 0.5, y = 0.55, label = "e", size = 5) +
   # annotate("text", x = 0.7, y = 0.44, label = "a", size = 5) +
   # annotate("text", x = 0.9, y = 0.1, label = "b", size = 5) +
   # annotate("text", x = 0.72, y = 0.18, label = "A", size = 10) +
   expand_limits(x = 0, y = 0) +
   scale_x_continuous(expand = c(0, 0),
                      breaks = c(0,0.5,1))+
   scale_y_continuous(expand = c(0, 0),
                      breaks = c(0,Tperc)) + #,
   #                    limits = c(0,Tperc)) +
   coord_fixed(ratio=1) +
   theme_minimal()
#theme_set(theme_linedraw())



# Ranking ----
# Ranking metros by TPI value (min to max)
ranking <- data.frame(metro_areas_df[, c(6,7, 1,3,4)],
                      apply(metro_areas_df[, c(1,3,4)],
                            2, # apply the rank function on columns
                            rank,
                            ties.method='min'))
write.csv(ranking,
          file = "ranking_areas.csv")
write.csv(ranking,
          file = "ranking_areas_income.csv")

# Clustering ----
# Clustering on the results of the area analysis, made in order to show geogaphical pattern of the results. Clusters are 'better' or 'worse doing' metros in terms of values of TPI
Ks <- 4
# use kmean to find the centers of the PTacc clusters:
centers <- kmeans(metro_areas_df$TPI, 
                  Ks)$centers
# order the centers
centers <- sort(centers)
# call kmeans again but this time passing the centers calculated in the previous step
metro_areas_df$clusters <- kmeans(metro_areas_df$TPI, 
                                  centers = centers)$cluster

metro_all <- merge(tot_pop_df,
                   metro_areas_df)
metro_all$RatCarless <- round(metro_all$CarlessPOP / metro_all$TotPOP,
                              2)
ranking <- data.frame(metro_all,
                      apply(metro_all[, c(6,8,9)],
                            2, # apply the rank function on columns
                            rank,
                            ties.method='min'))

write.csv(ranking,
          file = "metro_all.csv")

# Extra stuff -----
# Columns to add in a table:
# states
# CBSA fips
# weighed acc:
# sum(pop * acc) / sum pop
# average acc
# tot pop
# % carless
metros_tot <- group_data_metros %>%
   group_by(CBSAfips) %>%
   summarise(TotPOP = sum(POP, na.rm = TRUE),
             CarlessPOP = sum(CarlessPOP, na.rm = TRUE),
             PercCarless = round(CarlessPOP / TotPOP * 100, 1),
             #AverageAcc = round(mean(PTacc, na.rm = TRUE)),
             WeighedAcc = round(sum(POP * PTacc, 
                                    na.rm = TRUE) / TotPOP))
metros_tot["MetroName"] <- tot_pop_df[match(metros_tot$CBSAfips, 
                                            tot_pop_df$CBSAfips), 
                                      "MetroName"]
write.csv(metros_tot,
          file = "metros_tot.csv")

# for the Qgis maps:
ranking <- data.frame(TPItable[, c(4,5,1,2,3)],
                      apply(TPItable[,c(2,3)], # Columns with A and TPI values
                            2, # apply the rank function on columns
                            rank,
                            ties.method='min'))
colnames(ranking) <- c("CBSAfips", "MetroName", "areal_dist", "A", "TPI", "rankA", "rankTPI")
Ks <- 4
# use kmean to find the centers of the PTacc clusters:
centers <- kmeans(TPItable$TPI, 
                  Ks)$centers
# order the centers
centers <- sort(centers)
# call kmeans again but this time passing the centers calculated in the previous step
metros_tot$clusters <- kmeans(TPItable$TPI, 
                              centers = centers)$cluster
metros_qgis <- merge(ranking, metros_tot)
write.csv(metros_qgis,
          file = "metros_qgis.csv")


# 2. Plotting for the paper ----
#theme_set(theme_linedraw())
ggplot(data.frame(x = c(0, 1)),
       aes(x)) + 
   geom_segment(aes(x = 0, xend = 1, y = Tperc, yend = Tperc), 
                linetype = 2,
                size = 0.2) +
   
   stat_function(fun = Fworst, size = 0.5) +
   stat_function(fun = Fnocar,
                 geom = 'area', fill = "#d8daeb") +
   stat_function(fun = Fnocar, size = 0.7) +
   stat_function(fun = Fbest,
                 geom = 'area', fill = "white") +
   stat_function(fun = Fbest, size = 0.5) +
   theme_classic() +
   theme(panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_text(size = 15),
         axis.text = element_text(size = 12),
         axis.title.y = element_text(size = 15)) +
   geom_segment(aes(x = 1, xend = 1, y = 0, yend = Tperc), 
                linetype = 2,
                size = 0.2) +
   labs(x = "p",
        y = "t") +
   annotate("text", x = 0.12, y = 0.15, label = "w", size = 5) +
   annotate("text", x = 0.88, y = 0.1, label = "b", size = 5) +
   annotate("text", x = 0.6, y = 0.15, label = "a", size = 5) +
   expand_limits(x = 0, y = 0) +
   scale_x_continuous(expand = c(0, 0),
                      breaks = c(0,1-Tperc, 1),
                      labels = c("0", "P - T", "P"))+
   scale_y_continuous(expand = c(0, 0),
                      breaks = c(0,Tperc, 0.2),
                      labels = c("0", "T", "")) +
   coord_cartesian(xlim = c(0, 1.2),
                   ylim =c(0, 0.4))
#geom_hline(yintercept = Tperc)#+
#coord_fixed(ratio=1)

# 3. Carless AND income analysis ----
#1. Putting the two df together:
TPI_carincome_temp <- merge(TPItable_car,
                            TPItable_income,
                            by = "CBSAfips",
                            all = T)
TPI_carincome <- TPI_carincome_temp %>%
   select(CBSAfips, MetroName.x, A_car, TPI_car, A_income, TPI_income)
colnames(TPI_carincome)[2] <- "MetroName"
write.csv(TPI_carincome,
          file = "TPI_carincome.csv")


#2. Clustering on the two TPI columns:
Ks <- 4 # number of clusters

# # https://uc-r.github.io/kmeans_clustering
# k4 <- kmeans(TPI_carincome[c(4,6)], centers = 4)
# library(cluster)
# library(factoextra)
# plot_k <- fviz_cluster(k4, geom = "point", data = TPI_carincome[c(4,6)]) + ggtitle("k = 2")
# # https://stackoverflow.com/questions/26895507/k-means-clustering-of-variable-with-multiple-values
# use kmean to find the centers of the PTacc clusters:
set.seed(1)
centers <- kmeans(TPI_carincome[c(4,6)], # column 4: TPIcar, column 6: TPI income
                  Ks,
                  iter.max = 5, 
                  nstart = 5)$centers
# # order the centers
# centers <- sort(centers)
# TPI_carincome$clust <- kmeans(TPI_carincome[c(4,6)],
#                               centers = 4)$clust
# 
# # use kmean to find the centers of the PTacc clusters:
# centers <- kmeans(TPI_carincome[c(4,6)], 
#                   Ks)$centers
# # order the centers
# centers <- sort(centers)
# # call kmeans again but this time passing the centers calculated in the previous step
# TPI_carincome$clusters <- kmeans(TPI_carincome[c(4,6)],
#                                  centers = centers)$cluster
set.seed(1)
TPI_carincome$clust_carinc <- kmeans(TPI_carincome[c(4,6)],
                                     centers = centers,
                                     iter.max = 5, 
                                     nstart = 5)$clust
plot(TPI_income~TPI_car,
     TPI_carincome,
     col = TPI_carincome$clust,
     pch = 20,
     size = 30)


# Plotting the TPI for carless and low-income, for all the metros, in Black&White. Few towns' names (see list 'labels_under') need to be plotted under the marking point, rather than on the right. We are putting them on the right because we decided (after few trials) that this was the best position for the labels. Though those towns fit better if (only them) are positioned below the point.
# changing town's names to character (for now they are factors):
TPI_carincome <- TPI_carincome %>% # need to change the factors to character in order to use the filter function hereafter
   dplyr::mutate_if(., is.factor, as.character)
labels_under <- TPI_carincome %>% # the four towns here go under
   dplyr::filter(., stringr::str_detect("Buffalo|Dallas|Houston|Washington", 
                                        MetroName))

labels_right <- TPI_carincome %>%
   dplyr::filter(., ! (stringr::str_detect(MetroName, # negating the four towns of before (taking the rest from the list)
                                           ("Buffalo|Dallas|Houston|Washington"))))
# all the above is from here: https://thatdatatho.com/2019/02/13/grammar-of-graphics-ggplot2/


ggplot(TPI_carincome, aes(x=TPI_car, 
                          y=TPI_income)) + #, 
   #                          color = factor(clust))) +
   geom_abline(intercept = 0, 
               slope = 1, 
               color = "grey") +
   geom_point(aes(shape = factor(clust_carinc)), 
              size = 5) + #alpha = 0.2, size = 8) +
   # geom_text(aes(label = MetroName), #rowindex
   #            size = 4,
   #           hjust = 0, 
   #           nudge_x = 0.003) +
   geom_text(data = labels_right,
             aes(label = MetroName), #rowindex
             size = 4,
             hjust = 0, 
             nudge_x = 0.0025) +   
   geom_text(data = decision,
             aes(label = MetroName), #rowindex
             size = 4,
             vjust = 0, 
             nudge_y = -0.005) +
   theme_linedraw() +
   coord_fixed() +
   #theme(aspect.ratio=1) +
   guides(colour = FALSE, 
          shape = FALSE) +
   # scale_x_continuous(limits = c(0, 0.5),
   #                   breaks = c(0,0.5)) +
   # scale_y_continuous(limits = c(0, 0.5),
   #                    breaks = c(0,0.5)) +
   scale_shape_manual(values = c(15,2,5,19)) +
   xlab("TPI carless") +
   ylab("TPI low-income")

# Same, without clusters:
ggplot(TPI_carincome, aes(x=TPI_car, 
                          y=TPI_income)) + #, 
   #                          color = factor(clust))) +
   geom_abline(intercept = 0, 
               slope = 1, 
               color = "grey") +
   geom_point() + #aes(shape = factor(clust_carinc)), 
   #size = 5) + #alpha = 0.2, size = 8) +
   # geom_text(aes(label = MetroName), #rowindex
   #            size = 4,
   #           hjust = 0, 
   #           nudge_x = 0.003) +
   geom_text(data = labels_right,
             aes(label = MetroName), #rowindex
             size = 4,
             hjust = 0, 
             nudge_x = 0.0025) +   
   geom_text(data = decision,
             aes(label = MetroName), #rowindex
             size = 4,
             vjust = 0, 
             nudge_y = -0.005) +
   theme_linedraw() +
   coord_fixed() +
   #theme(aspect.ratio=1) +
   guides(colour = FALSE, 
          shape = FALSE) +
   # scale_x_continuous(limits = c(0, 0.5),
   #                   breaks = c(0,0.5)) +
   # scale_y_continuous(limits = c(0, 0.5),
   #                    breaks = c(0,0.5)) +
   #scale_shape_manual(values = c(20)) +
   xlab("TPI carless") +
   ylab("TPI low-income")

ggplot(newyork, aes(x = PoorPOP, y = CarlessPOP)) + 
   geom_point(alpha = 0.6, color = "cornflowerblue") +
   geom_smooth(method = "lm", 
               se = FALSE,
               colour = "darkgrey",
               alpha = 0.8,
               size = 0.5) +
   theme_light() +
   heme(aspect.ratio=1)  +
   coord_fixed()
# scale_x_continuous(limits = c(0, 1),
#                   breaks = c(0,1)) +
# scale_y_continuous(limits = c(0,1),
#                    breaks = c(0,1)) #+
#facet_wrap(~MetroName)
# 
# ggplot(iris_clustered, aes(x=Petal.Width, y=Sepal.Width, color=cluster, 
#                            shape=Species)) + geom_point()
# 
# metro_all <- merge(tot_pop_df,
#                    metro_areas_df)
# metro_all$RatCarless <- round(metro_all$CarlessPOP / metro_all$TotPOP,
#                              2)
# ranking and things for Qgis/paper -----
# Columns to add in a table:
# states
# CBSA fips
# weighed acc:
# sum(pop * acc) / sum pop
# average acc
# tot pop
# % carless
metros_tot <- group_data_metros %>%
   group_by(CBSAfips) %>%
   summarise(TotPOP = sum(POP, na.rm = TRUE),
             CarlessPOP = sum(CarlessPOP, na.rm = TRUE),
             PercCarless = round(CarlessPOP / TotPOP * 100, 1),
             PoorPOP = sum(PoorPOP, na.rm = TRUE),
             PercPoor = round(PoorPOP / TotPOP * 100, 1),
             #AverageAcc = round(mean(PTacc, na.rm = TRUE)),
             WeighedAcc = round(sum(POP * PTacc, 
                                    na.rm = TRUE) / TotPOP))
metros_tot["MetroName"] <- tot_pop_df[match(metros_tot$CBSAfips, 
                                            tot_pop_df$CBSAfips), 
                                      "MetroName"]
write.csv(metros_tot,
          file = "metros_tot.csv")

# for the table in the paper:
ranking <- data.frame(TPI_carincome,
                      apply(TPI_carincome[,c(4,6)], # Columns with TPI carless and TPI income
                            2, # apply the rank function on columns
                            rank,
                            ties.method='min'))
colnames(ranking) <- c("CBSAfips", "MetroName", "A_car", "TPI_car", "A_income", "TPI_income", "clust_carinc", "Rank_TPIcar", "Rank_TPIincome")


Ks <- 4
# Clusters by TPI income:
set.seed(1)
centers_income <- kmeans(TPI_carincome$TPI_income, 
                         Ks,
                         iter.max = 5, 
                         nstart = 5)$centers
centers_income <- sort(centers_income) # order the centers
set.seed(1)
TPI_carincome$clust_income <- kmeans(TPI_carincome$TPI_income, # call kmeans again but this time passing the centers calculated in the previous step
                                     centers = centers_income,
                                     iter.max = 5, 
                                     nstart = 5)$cluster
# Clusters by TPI car-less:
set.seed(1)
centers_car <- kmeans(TPI_carincome$TPI_car, 
                      Ks,
                      iter.max = 5, 
                      nstart = 5)$centers
centers_car <- sort(centers_car) # order the centers
set.seed(1)
TPI_carincome$clust_car <- kmeans(TPI_carincome$TPI_car, # call kmeans again but this time passing the centers calculated in the previous step
                                  centers = centers_car,
                                  iter.max = 5, 
                                  nstart = 5)$cluster

# Putting all the infos together
metros_qgis <- merge(ranking, metros_tot)
write.csv(metros_qgis,
          file = "metros_qgis.csv")


