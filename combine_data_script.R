library(tidyverse)
library(nflreadr)
library(corrplot)
library(gridExtra)
library(gt)
library(scales)

#initial load of combine data
combine_dataset = load_combine()

#initial graph to see what the data looks like
combine_dataset %>% 
  filter(pos == "QB", !is.na(draft_round)) %>% 
  ggplot(aes(ht, wt)) + geom_point() +
  theme_bw() +
  labs(x = "Height",
       y = "Weight",
       title = "QBs Combine Results",
       subtitle = "From 2000 to 2021 | QBs who were drafted") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle  = element_text(hjust = 0.5)
        )

##### Prep to move to Python

#turn height into inches for simplicity
combine_dataset =
  combine_dataset %>%
  filter(!is.na(draft_round)) %>%
    mutate(feet = as.numeric(substr(ht, 1, 1)) ,
           inch = as.numeric(substr(ht, 3, 6))) %>%
    mutate(inch_height = feet * 12 + inch) %>%
  select(player_name, pos, inch_height, wt)

#qb dataset --> ready for Python
qb_combine = 
  combine_dataset %>%
    filter(pos == "QB") %>%
    select(player_name, inch_height, wt)

#save to desktop --> easy to bring into Python
save(qb_combine, file = 'qb_dataset')

write.csv(qb_combine,"C:\\Users\\drez_\\Desktop\\qb_combine_dataset.csv", row.names = TRUE)


### Code to bring Python clustered data into R
qb_clusters = read.csv('https://raw.githubusercontent.com/ddale23/k_means_combine/main/final_data.csv')

#convert cluster to character 
qb_clusters$cluster = as.character(qb_clusters$cluster)

#bring back combine dataset
combine_dataset = load_combine()

#get the QB metrics that we want
qbs_combine =
  combine_dataset %>%
    filter(!is.na(draft_round), pos == "QB") %>% 
  select(player_name, forty, vertical, broad_jump, cone, shuttle)

#join data tables
joined_combine_qb =
  inner_join(qbs_combine, qb_clusters, by = c('player_name' = 'qb_name'))

#let's give these clusters some names for simplicity
joined_combine_qb =
  joined_combine_qb %>%
    mutate(cluster_name = case_when(
      cluster == 0 ~ 'Undersized',
      cluster == 1 ~ 'Tall & Big',
      cluster == 2 ~ 'Tall & Norm',
      cluster == 3 ~ 'Mid & Big',
      cluster == 4 ~ 'Mid & Small',
    ))


#recreate the cluster viz with names on clusters
joined_combine_qb %>%
  ggplot(aes(height, weight, color = cluster_name)) + 
  annotate(geom = 'label', x = 75, y = 205, label = "Mid & Small") +
  annotate(geom = 'label', x = 71.5, y = 240, label = "Mid & Big") +
  annotate(geom = 'label', x = 72, y = 190, label = "Undersized") + 
  annotate(geom = 'label', x = 78, y = 220, label = "Tall & Norm") +
  annotate(geom = 'label', x = 78, y = 255, label = "Tall & Big") +
  geom_point() +
  labs(x = 'Height (in Inches)',
       y = 'Weight (in Lbs.)',
       title = 'QB Cluster Vizualization',
       subtitle = 'With Cluster Names') +
  scale_color_discrete(name = "Cluster Name") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(70,80) 


#want to see how athletic each qb is in relation to their cluster --> forty time
forty_percs =
joined_combine_qb %>%
  filter(!is.na(forty)) %>%
  group_by(cluster) %>%
  mutate(forty_percentile = 1 - (round((rank(forty, ties.method = "average") - 0.5)/length(forty), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, forty_percentile) %>%
  select(player_name, forty_percentile)
  


#want to see how athletic each qb is in relation to their cluster --> vertical
vertical_percs =
joined_combine_qb %>%
  filter(!is.na(vertical)) %>%
  group_by(cluster) %>%
  mutate(vertical_percentile = 1 - (round((rank(-vertical, ties.method = "average") - 0.5) / length(vertical), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, vertical_percentile) %>%
  select(player_name, vertical_percentile)



#want to see how athletic each qb is in relation to their cluster --> broad_jump
broad_jump_percs =
joined_combine_qb %>%
  filter(!is.na(broad_jump)) %>%
  group_by(cluster) %>%
  mutate(broad_jump_percentile = 1 - (round((rank(-broad_jump, ties.method = "average") - 0.5) / length(broad_jump), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, broad_jump_percentile) %>%
  select(player_name, broad_jump_percentile)



#want to see how athletic each qb is in relation to their cluster --> cone
cone_percs =
joined_combine_qb %>%
  filter(!is.na(cone)) %>%
  group_by(cluster) %>%
  mutate(cone_percentile = 1 - (round((rank(cone, ties.method = "average") - 0.5) /length(cone), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, cone_percentile) %>%
  select(player_name, cone_percentile)



#want to see how athletic each qb is in relation to their cluster --> cone
shuttle_percs =
joined_combine_qb %>%
  filter(!is.na(shuttle)) %>%
  group_by(cluster) %>%
  mutate(shuttle_percentile = 1 - (round((rank(shuttle, ties.method = "average") - 0.5) / length(shuttle), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, shuttle_percentile) %>%
  select(player_name, shuttle_percentile)



#joining percentile calcs to joined_combine_qb
joined_combine_qb = 
  left_join(joined_combine_qb, forty_percs, by = "player_name")





joined_combine_qb =
  left_join(joined_combine_qb, vertical_percs, by = "player_name")



joined_combine_qb =
  left_join(joined_combine_qb, broad_jump_percs, by = "player_name")




joined_combine_qb =
  left_join(joined_combine_qb, cone_percs, by = "player_name")



joined_combine_qb =
  left_join(joined_combine_qb, shuttle_percs, by = "player_name")








#master qb percentile rank data set
qb_percentile_ranks_master = 
  joined_combine_qb %>% 
    select(player_name, height, weight, cluster, cluster_name, forty, forty_percentile, vertical, vertical_percentile, 
           broad_jump, broad_jump_percentile, cone, cone_percentile, shuttle, shuttle_percentile) %>%
    arrange(cluster, shuttle_percentile)

#now make an average athletic score

qb_percentile_ranks_master %>% 
  mutate(avg_perc = forty_percentile + vertical_percentile + broad_jump_percentile + cone_percentile + shuttle_percentile) %>% 
  view()

#save athletic score
qb_athletic = 
qb_percentile_ranks_master %>%
  mutate(total_percentile = case_when(
    !is.na(forty_percentile) ~ forty_percentile,
    is.na(forty_percentile) ~ 0
  ),
  total_count = case_when(
    !is.na(forty_percentile) ~ 1,
    is.na(forty_percentile) ~ 0
  )) %>% 
  mutate(total_percentile = case_when(
    !is.na(vertical_percentile) ~ total_percentile + vertical_percentile,
    is.na(vertical_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(vertical_percentile) ~ total_count + 1,
    is.na(vertical_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(broad_jump_percentile) ~ total_percentile + broad_jump_percentile,
    is.na(broad_jump_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(broad_jump_percentile) ~ total_count + 1,
    is.na(broad_jump_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(cone_percentile) ~ total_percentile + cone_percentile,
    is.na(cone_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(cone_percentile) ~ total_count + 1,
    is.na(cone_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(shuttle_percentile) ~ total_percentile + shuttle_percentile,
    is.na(shuttle_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(shuttle_percentile) ~ total_count + 1,
    is.na(shuttle_percentile) ~ total_count
  )) %>%
  mutate(avg_perc = round((total_percentile / total_count), digits = 2))

#view qb athletic
qb_athletic %>% 
  filter(player_name == "Colt McCoy") %>%
  view()


#bring in 2022 combine data from desktop
qb_2022_data = read.csv(file.choose())

#clean up the data set
########## we do not use this bc I already cleaned the data to get it on Github but it was creative & good work
cleaned_2022_qbs_combine =
  qb_2022_data %>%
    mutate(draft_year = 2022,
          player_name = paste(FIRST.NAME, LAST.NAME, sep = " "),
           school = SCHOOL,
           height = HEIGHT,
           inch_height = (as.numeric(substr(HEIGHT, 1,1)) * 12) +
             as.numeric(substr(HEIGHT, 3, 3)) +
             round(as.numeric((substr(HEIGHT, 4, 4))) / 8, digits = 0),
           weight = WEIGHT,
           forty = X40.TIME,
           vertical = VERT,
           broad_jump = BROAD,
           inch_broad_jump = case_when(
             as.numeric(substr(BROAD, 1, 1)) == 1 ~ 10 * 12 + as.numeric(substr(BROAD, 4, 4)),
             as.numeric(substr(BROAD, 1, 1)) != 1 ~ as.numeric(substr(BROAD, 1,1)) * 12 +
               as.numeric(substr(BROAD, 3, 3))
           ),
          shuttle = SHUTTLE,
          three_cone = CONE) %>%
    select(draft_year, player_name, school, height, inch_height, weight, forty, vertical, broad_jump, inch_broad_jump, 
           shuttle, three_cone)

###this data height & weight for 2022 class is now on github
#load in 2022 qb data
qb_2022_data = read.csv('https://raw.githubusercontent.com/ddale23/k_means_combine/main/qb_2022_combine.csv')


#now bring in combine data from python
all_qbs = read.csv(file.choose())

#want to create some cluster names
all_qbs$cluster = as.character(all_qbs$cluster)


all_qbs %>%
  ggplot(aes(height, weight, color = cluster)) + geom_point()



#let's give these clusters some names for simplicity
all_qbs = 
  all_qbs %>%
  mutate(cluster_name = case_when(
    cluster == 1 ~ 'Undersized',
    cluster == 3 ~ 'Tall & Big',
    cluster == 4  ~ 'Tall & Bigish',
    cluster == 2  ~ 'Mid & Bigish',
    cluster == 0 ~ 'Mid & Smallish',
  ))


#recreate the cluster viz with names on clusters
all_qbs %>%
  ggplot(aes(height, weight, color = cluster_name)) + 
  annotate(geom = 'label', x = 76, y = 203, label = "Mid & Smallish") +
  annotate(geom = 'label', x = 71.5, y = 240, label = "Mid & Bigish") +
  annotate(geom = 'label', x = 72, y = 190, label = "Undersized") + 
  annotate(geom = 'label', x = 78, y = 220, label = "Tall & Bigish") +
  annotate(geom = 'label', x = 78, y = 255, label = "Tall & Big") +
  geom_point() +
  labs(x = 'Height (in Inches)',
       y = 'Weight (in Lbs.)',
       title = 'QB Cluster Vizualization') +
  scale_color_discrete(name = "Cluster Name") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(69,80) 



all_qbs %>% 
  filter(cluster_name == "Undersized") %>%
  view()



#time to stack 2022 combine data on top of 2000 to 2021 combine data
combine_dataset = load_combine()

#get the QB metrics that we want
qbs_combine =
  combine_dataset %>%
  filter(!is.na(draft_round), pos == "QB") %>% 
  select(draft_year, player_name, forty, vertical, broad_jump, cone, shuttle)



#combine times and drill results --> 2000 - present QBs
qbs_combine


#combine times and drill results --> 2022 QBs
cleaned_2022_qbs_combine



#need to reduce columns in 2022 data set 
cleaned_2022_qbs_combine = 
cleaned_2022_qbs_combine %>% 
  mutate(broad_jump = inch_broad_jump,
         cone = three_cone) %>%
  select(draft_year, player_name, forty, vertical, broad_jump, cone, shuttle)











trial_bind =
rbind(qbs_combine, cleaned_2022_qbs_combine)


trial_bind %>% view()


trial_bind 



all_qbs %>% view()

joined_final_all_qbs =
inner_join(trial_bind, all_qbs, by = c("player_name" = "qb_name"))


joined_final_all_qbs %>% view()


#now compute athletic scores for qbs


forty_percs %>% view



#want to see how athletic each qb is in relation to their cluster --> forty time
forty_percs =
  joined_final_all_qbs %>%
  filter(!is.na(forty)) %>%
  group_by(cluster) %>%
  mutate(forty_percentile = 1 - (round((rank(forty, ties.method = "average") - 0.5)/length(forty), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, forty_percentile) %>%
  select(player_name, forty_percentile)



#want to see how athletic each qb is in relation to their cluster --> vertical
vertical_percs =
  joined_final_all_qbs %>%
  filter(!is.na(vertical)) %>%
  group_by(cluster) %>%
  mutate(vertical_percentile = 1 - (round((rank(-vertical, ties.method = "average") - 0.5) / length(vertical), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, vertical_percentile) %>%
  select(player_name, vertical_percentile)



#want to see how athletic each qb is in relation to their cluster --> broad_jump
broad_jump_percs =
  joined_final_all_qbs %>%
  filter(!is.na(broad_jump)) %>%
  group_by(cluster) %>%
  mutate(broad_jump_percentile = 1 - (round((rank(-broad_jump, ties.method = "average") - 0.5) / length(broad_jump), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, broad_jump_percentile) %>%
  select(player_name, broad_jump_percentile)



#want to see how athletic each qb is in relation to their cluster --> cone
cone_percs =
  joined_final_all_qbs %>%
  filter(!is.na(cone)) %>%
  group_by(cluster) %>%
  mutate(cone_percentile = 1 - (round((rank(cone, ties.method = "average") - 0.5) /length(cone), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, cone_percentile) %>%
  select(player_name, cone_percentile)



#want to see how athletic each qb is in relation to their cluster --> cone
shuttle_percs =
  joined_final_all_qbs %>%
  filter(!is.na(shuttle)) %>%
  group_by(cluster) %>%
  mutate(shuttle_percentile = 1 - (round((rank(shuttle, ties.method = "average") - 0.5) / length(shuttle), digits = 2))) %>% 
  ungroup() %>%
  arrange(cluster, shuttle_percentile) %>%
  select(player_name, shuttle_percentile)



#joining percentile calcs to joined_combine_qb
joined_final_all_qbs = 
  left_join(joined_final_all_qbs, forty_percs, by = "player_name")





joined_final_all_qbs =
  left_join(joined_final_all_qbs, vertical_percs, by = "player_name")



joined_final_all_qbs =
  left_join(joined_final_all_qbs, broad_jump_percs, by = "player_name")




joined_final_all_qbs =
  left_join(joined_final_all_qbs, cone_percs, by = "player_name")



joined_final_all_qbs =
  left_join(joined_final_all_qbs, shuttle_percs, by = "player_name")



joined_final_all_qbs %>% view()




#master qb percentile rank data set
qb_percentile_ranks_master = 
  joined_final_all_qbs %>% 
  select(draft_year, player_name, height, weight, cluster, cluster_name, forty, forty_percentile, vertical, vertical_percentile, 
         broad_jump, broad_jump_percentile, cone, cone_percentile, shuttle, shuttle_percentile) %>%
  arrange(cluster, shuttle_percentile)

#now make an average athletic score
qb_percentile_ranks_master %>% 
  mutate(avg_perc = forty_percentile + vertical_percentile + broad_jump_percentile + cone_percentile + shuttle_percentile) %>% 
  view()

#save athletic score
qb_athletic = 
  qb_percentile_ranks_master %>%
  mutate(total_percentile = case_when(
    !is.na(forty_percentile) ~ forty_percentile,
    is.na(forty_percentile) ~ 0
  ),
  total_count = case_when(
    !is.na(forty_percentile) ~ 1,
    is.na(forty_percentile) ~ 0
  )) %>% 
  mutate(total_percentile = case_when(
    !is.na(vertical_percentile) ~ total_percentile + vertical_percentile,
    is.na(vertical_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(vertical_percentile) ~ total_count + 1,
    is.na(vertical_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(broad_jump_percentile) ~ total_percentile + broad_jump_percentile,
    is.na(broad_jump_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(broad_jump_percentile) ~ total_count + 1,
    is.na(broad_jump_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(cone_percentile) ~ total_percentile + cone_percentile,
    is.na(cone_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(cone_percentile) ~ total_count + 1,
    is.na(cone_percentile) ~ total_count
  )) %>%
  mutate(total_percentile = case_when(
    !is.na(shuttle_percentile) ~ total_percentile + shuttle_percentile,
    is.na(shuttle_percentile) ~ total_percentile + 0
  ),
  total_count = case_when(
    !is.na(shuttle_percentile) ~ total_count + 1,
    is.na(shuttle_percentile) ~ total_count
  )) %>%
  mutate(avg_perc = round((total_percentile / total_count), digits = 2))


qb_athletic %>% 
  filter(!is.na(avg_perc)) %>%
  view()

qb_athletic %>% colnames()

qb_athletic %>% filter(draft_year == 2022) %>% view()


# table with 2022 prospects
table_data =
qb_athletic %>%
  filter(draft_year == 2022) %>%
  select(player_name, cluster_name, forty_percentile, vertical_percentile,
         broad_jump_percentile, cone_percentile, shuttle_percentile,
         avg_perc
         ) %>%
  arrange(-avg_perc) %>%
  mutate(avg_perc = as.numeric(avg_perc))

table_data %>%
  gt(rowname_col = "player_name") %>%
  tab_header(
    title = "2022 QB Prospects",
    subtitle =  "") %>%
  cols_label(cluster_name = "Cluster",
             forty_percentile = "40",
             vertical_percentile = "Vert",
             broad_jump_percentile = "Broad Jump",
             cone_percentile = "3 Cone",
             shuttle_percentile = "Shuttle",
             avg_perc = "Avg Percentile"
             ) %>%
  tab_spanner(label = "Combine Metrics",
              columns = c(forty_percentile, vertical_percentile,
                          broad_jump_percentile, cone_percentile,
                          shuttle_percentile)) %>%
   cols_width(
     player_name ~ 100,
     cluster_name ~150,
     forty_percentile ~ 75,
     vertical_percentile ~ 75,
     broad_jump_percentile ~ 75,
     cone_percentile ~ 75,
     shuttle_percentile ~ 75,
     avg_perc ~ 150
   ) %>%
  data_color(
    columns = c(avg_perc,forty_percentile, broad_jump_percentile,cone_percentile, 
                shuttle_percentile, vertical_percentile),
    colors = scales::col_numeric(
      palette = c(
        "red", "orange", "green", "blue"),
      domain = c(0, 1))) %>%
  data_color (
    
    columns = cluster_name,
    colors = scales::col_factor(
      palette = c(
        "green", "blue", "pink", "orange", "yellow"), levels = c(
          'Tall & Big', 'Tall & Bigish', 'Undersized', 'Mid & Bigish', 'Mid & Smallish')))
 


#still want to readjust color scheme















