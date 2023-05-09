library(here)
setwd(here())
library(tidyverse)
library(jtools)
library(ggdist)


#####################################################
## Gamma values DF: "Varying Gamma Alpha Percent" ----
#####################################################

gvdf <- read_csv(here("code",
                      "Language Change Varying Gamma Alpha Percent-table.csv"),
                 show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- gvdf |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes)) |> 
  rename(equilibrium = mean_state_of_nodes)

gvdf_proc <- gvdf |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, initial_gamma) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)

# Graphing for GVDF:
ggplot(gvdf_proc, aes(x = initial_gamma, y = eq_percent,
                      group = factor(initial_gamma)))+
  xlab("Initial gamma")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.001)+
  labs(caption = "50 runs per configuration, 1000 steps total per run")

ggplot(gvdf|> 
         filter(step == 1000),
       aes(x = percent_grammar_1, y = mean_state_of_nodes,
           color = factor(initial_gamma),
           group = factor(initial_gamma)))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Initial gamma",# type = "qual",
                     palette = 5)+ 
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Impact of Gamma on final state")+
  geom_text(aes(x = 80, y = 0.9, label = "Grammar 1\nas majority"))+
  geom_text(aes(x = 15, y = 0.1, label = "Grammar 0\nas majority"))+
  labs(caption = "50 runs per configuration, 1000 steps total per run")

# Writing out processed files:
write_csv(gvdf_proc, here("paper","processed_data","gvdf_proc.csv"))

gvdf |> 
  filter(step == 1000) |> 
  write_csv(here("paper","processed_data","gvdf_final-only.csv"))

rm(gvdf, final_counts, gvdf_proc)
gc()



#####################################################
## Probabilistic gamma DF: "probabilistic-gamma" ----
#####################################################

lndf2 <- read_csv(here("code",
                       "Language Change probabilistic-gamma-table.csv"),
                  show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- lndf2 |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes)) |> 
  rename(equilibrium = mean_state_of_nodes)

lndf2_proc <- lndf2 |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, initial_gamma, gamma_sd) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)

# Graphing for SD variation:
ggplot(lndf2 |> 
         filter(step == 1000),
       aes(x = percent_grammar_1, y = mean_state_of_nodes,
           color = factor(initial_gamma),
           group = factor(initial_gamma)))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Initial gamma",# type = "qual",
                     palette = 5)+ 
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Impact of Gamma on final state")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  facet_wrap(~gamma_sd)

ggplot(lndf2_proc, aes(x = initial_gamma, y = eq_percent,
                      group = factor(initial_gamma)))+
  xlab("Initial gamma")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", fill = "lightgreen", color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.001)+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  facet_wrap(~gamma_sd)

# Writing out processed files:
write_csv(lndf2_proc, here("paper","processed_data","lndf2_proc.csv"))

lndf2 |> 
  filter(step == 1000) |> 
  write_csv(here("paper","processed_data","lndf2_final-only.csv"))

rm(lndf2, final_counts, lndf2_proc)
gc()




#####################################################
## lndf3: "gamma-based-on-age" ----
# NOTE: Gamma decreases with age at a constant rate.
#####################################################

lndf3 <- read_csv(here("code",
                       "Language Change gamma-based-on-age-table.csv"),
                  show_col_types = F, skip = 6) |> 
  janitor::clean_names()

# skimr::skim(lndf3)

final_counts <- lndf3 |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes)) |> 
  rename(equilibrium = mean_state_of_nodes)

lndf3_proc <- lndf3 |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, initial_gamma, age_sd, deterministic_age) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)

det_age <- lndf3 |> 
  filter(step == 1000) |> 
  mutate(graph_group = if_else(deterministic_age == T,
                               "Deterministic",
                               if_else(deterministic_age == F & age_sd == 30,
                                       "Probabilistic (SD = 30)",
                                       if_else(deterministic_age == F & age_sd == 50,
                                               "Probabilistic (SD = 50)",
                                               if_else(deterministic_age == F & age_sd == 100,
                                                       "Probabilistic (SD = 100)",
                                                       "N/A")))))

# Deterministic plots
det_age |> 
  ggplot(aes(x = percent_grammar_1, y = mean_state_of_nodes,
             color = factor(graph_group, levels = c("Deterministic", "Probabilistic (SD = 30)", "Probabilistic (SD = 50)", "Probabilistic (SD = 100)")),
             group = factor(graph_group, levels = c("Deterministic", "Probabilistic (SD = 30)", "Probabilistic (SD = 50)", "Probabilistic (SD = 100)"))))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Age type", type = "qual",
                     palette = 2)+
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Age determines gamma", subtitle = "Deterministic")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  ggtitle("Final state of system")

lndf3_proc <- lndf3_proc |> 
  mutate(graph_group = if_else(deterministic_age == T,
                               "Deterministic",
                               if_else(deterministic_age == F & age_sd == 30,
                                       "Probabilistic (SD = 30)",
                                       if_else(deterministic_age == F & age_sd == 50,
                                               "Probabilistic (SD = 50)",
                                               if_else(deterministic_age == F & age_sd == 100,
                                                       "Probabilistic (SD = 100)",
                                                       "N/A")))))

ggplot(lndf3_proc, aes(x = factor(graph_group, levels = c("Deterministic", "Probabilistic (SD = 30)", "Probabilistic (SD = 50)", "Probabilistic (SD = 100)")),
                       y = eq_percent))+
  xlab("Deterministic age?")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", fill = "darkgreen", color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25)+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  ggtitle("Deterministic vs probabilistic age for time to equilibrium")


# Writing out processed files:
write_csv(lndf3_proc, here("paper","processed_data","lndf3_proc.csv"))

write_csv(det_age, here("paper","processed_data","lndf3_final-only.csv"))

rm(lndf3, final_counts, lndf3_proc, det_age)
gc()


#####################################################
## Varying gamma perseverance: ----
#####################################################

vgp <- read_csv(here("code",
                     "Language Change vary-gamma-perseverance-table.csv"),
                  show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- vgp |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes)) |> 
  rename(equilibrium = mean_state_of_nodes)

vgp_proc <- vgp |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, perseverance) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)

# Graphing:
ggplot(vgp |> 
         filter(step == 1000),
       aes(x = percent_grammar_1, y = mean_state_of_nodes,
           color = factor(perseverance),
           group = factor(perseverance)))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Perseverance",# type = "qual",
                     palette = 5)+ 
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Perseverance of learning")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")

ggplot(vgp_proc, aes(x = factor(perseverance), y = eq_percent,
                       group = factor(perseverance)))+
  xlab("Perseverance of learning")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", fill = "turquoise", color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25)+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  ggtitle("Impact of perseverance on equilibrium")

# Writing out processed files:
write_csv(vgp_proc, here("paper","processed_data","vgp_proc.csv"))

vgp |> 
  filter(step == 1000) |> 
  write_csv(here("paper","processed_data","vgp_final-only.csv"))

rm(vgp, final_counts, vgp_proc)
gc()



#####################################################
## Two groups: ----
# Variable terms: percent-cohort2, cohort2-age
#####################################################

tcg <- read_csv(here("code",
                     "Language Change two-groups-table.csv"),
                show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- tcg |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes)) |> 
  rename(equilibrium = mean_state_of_nodes)

tcg_proc <- tcg |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, percent_cohort2, cohort2_age, percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)

# Graphing:
ggplot(tcg |> 
         filter(step == 1000),
       aes(x = percent_grammar_1, y = mean_state_of_nodes,
           color = factor(percent_cohort2),
           group = factor(percent_cohort2)))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Group 2 percent",# type = "qual",
                     palette = 5)+ 
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Two groups")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  facet_grid(~cohort2_age)
  # No difference

ggplot(tcg_proc, aes(x = factor(percent_cohort2), y = eq_percent,
                     fill = factor(cohort2_age),
                     group = factor(percent_cohort2)))+
  xlab("Cohort 2 percentage")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", color = "black")+
  scale_fill_brewer(name = "Group 2 age", type = "qual",
                     palette = 1)+ 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25)+
  labs(caption = "Faceted by cohort 2 age; 50 runs per configuration, 1000 steps total per run")+
  ggtitle("Impact of second cohort age on equilibrium")+
  facet_grid(~cohort2_age)
  # Increasing the second group age increases equilibrium

# Group outcomes:
final_group_counts <- tcg |> 
  filter(step == 1000) |> 
  select(c(run_number, cohort1_state, cohort2_state)) |> 
  rename(c1_equil = cohort1_state,
         c2_equil = cohort2_state)

tcg1 <- tcg |> 
  left_join(final_group_counts, by = "run_number") |> 
  mutate(c1_equil_reached = abs(c1_equil-cohort1_state)<0.01) |> 
  filter(c1_equil_reached == T) |> 
  group_by(run_number, percent_cohort2, cohort2_age, percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps,
         type = "cohort1_equilibrium")

tcg2 <- tcg |> 
  left_join(final_group_counts, by = "run_number") |> 
  mutate(c2_equil_reached = abs(c2_equil-cohort2_state)<0.01) |> 
  filter(c2_equil_reached == T) |> 
  group_by(run_number, percent_cohort2, cohort2_age) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps,
         type = "cohort2_equilibrium")

tcg_group_proc <- rbind(tcg1, tcg2)
rm(tcg1, tcg2)
gc()

tcg |> 
  filter(step == 1000) |> 
  select(c(percent_grammar_1, percent_cohort2, 
           cohort2_age, cohort1_state, cohort2_state)) |> 
  pivot_longer(cols = c(cohort1_state, cohort2_state), 
               values_to = "mean_state_of_nodes") |> 
  ggplot(aes(x = percent_grammar_1, y = mean_state_of_nodes,
           color = factor(percent_cohort2),
           group = factor(percent_cohort2)))+
  theme_bw()+
  xlab("Initial percentage of Grammar 1")+
  ylab("Mean state of nodes")+
  scale_color_brewer(name = "Group 2 percent",# type = "qual",
                     palette = 5)+ 
  stat_summary(fun = mean, geom = "point")+
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  ggtitle("Two groups")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  facet_grid(name~cohort2_age)
  # No difference in final states? 

ggplot(tcg_group_proc, aes(x = factor(percent_cohort2), y = eq_percent,
                     fill = factor(cohort2_age),
                     group = factor(percent_cohort2)))+
  xlab("Cohort 2 percentage")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun = mean, geom = "bar", color = "black")+
  scale_fill_brewer(name = "Group 2 age", type = "qual",
                    palette = 1)+ 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25)+
  labs(caption = "Faceted by cohort 2 age; 50 runs per configuration, 1000 steps total per run")+
  ggtitle("Impact of second cohort age on equilibrium")+
  facet_grid(type~cohort2_age)
  # Inverse pattern???

# Writing out processed files:
write_csv(tcg_group_proc, here("paper","processed_data","tcg_group_proc.csv"))
write_csv(tcg_proc, here("paper","processed_data","tcg_proc.csv"))

tcg |> 
  filter(step == 1000)|> 
  write_csv(here("paper","processed_data","tcg_final-only.csv"))

tcg |> 
  filter(step == 1000) |> 
  select(c(percent_grammar_1, percent_cohort2, cohort2_age, cohort1_state, cohort2_state)) |> 
  pivot_longer(cols = c(cohort1_state, cohort2_state), values_to = "mean_state_of_nodes")|> 
  write_csv(here("paper","processed_data","tcg_final-only-by-cohort.csv"))

#####################################################
## Different starting percentages: ----
# Variable terms: percent_cohort_2, C1-percent-grammar-1, C2-percent-grammar-1
#####################################################

vgg <- read_csv(here("code",
                     "Language Change vary-group-starting-percentage-table.csv"),
                show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- vgg |> 
#   filter(step == 1000  & percent_cohort2 == 50) |> 
#   select(c(run_number,mean_state_of_nodes)) |> 
#   rename(equilibrium = mean_state_of_nodes)
# 
# vgg_proc <- vgg |> 
#   left_join(final_counts, by = "run_number") |> 
#   mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
#   filter(equilibrium_reached == T) |> 
#   group_by(run_number, c1_percent_grammar_1, c2_percent_grammar_1) |> 
#   summarize(eq_step = min(step)) |> 
#   mutate(max_steps = 1000,
#          eq_percent = eq_step / max_steps)

final_group_counts <- vgg |> 
  filter(step == 1000  & percent_cohort2 == 50) |> 
  select(c(run_number, cohort1_state, cohort2_state,mean_state_of_nodes)) |> 
  rename(c1_equilibrium = cohort1_state,
         c2_equilibrium = cohort2_state,
         group_equilibrium = mean_state_of_nodes)

vgg1 <- vgg |> 
  left_join(final_group_counts, by = "run_number") |> 
  mutate(c1_equil_reached = abs(c1_equilibrium-cohort1_state)<0.01) |> 
  filter(c1_equil_reached == T) |> 
  group_by(run_number, c1_percent_grammar_1, c2_percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps,
         type = "cohort1_equilibrium")

vgg2 <- vgg |> 
  left_join(final_group_counts, by = "run_number") |> 
  mutate(c2_equil_reached = abs(c2_equilibrium-cohort2_state)<0.01) |> 
  filter(c2_equil_reached == T) |> 
  group_by(run_number, c1_percent_grammar_1, c2_percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps,
         type = "cohort2_equilibrium")

vgg_m <- vgg |> 
  left_join(final_group_counts, by = "run_number") |> 
  mutate(group_equil_reached = abs(group_equilibrium-cohort2_state)<0.01) |> 
  filter(group_equil_reached == T) |> 
  group_by(run_number, c1_percent_grammar_1, c2_percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps,
         type = "mean_equilibrium")

vgg_proc <- rbind(vgg1, vgg2, vgg_m) |> 
  mutate(type = if_else(type == "cohort1_equilibrium", "Cohort 1\nequilibrium",
                        if_else(type == "cohort2_equilibrium", "Cohort 2\nequilibrium",
                                "Overall\nequilibrium")))
  

# Graphing:

vgg_types <- vgg |> 
  filter(step == 1000 & percent_cohort2 == 50) |> 
  select(c(c1_percent_grammar_1, c2_percent_grammar_1,
           cohort1_state, cohort2_state, mean_state_of_nodes)) |> 
  pivot_longer(cols = c(cohort1_state, cohort2_state, mean_state_of_nodes),
               values_to = "final_state", names_to = "type")

## Group 1 as not having grammar 1, but group 2 varies

vgg_types |> 
  filter(c1_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  ggplot(aes(x = c2_percent_grammar_1, y = final_state, 
             group = type, shape = type, color = type))+
  xlab("Percentage of grammar 1 (Cohort 1)")+
  ylab("Final state")+
  theme_bw()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3)+
  stat_summary(fun = mean, geom = "point", size = 2)+
  stat_summary(fun = mean, geom = "line")+
  scale_shape_manual(name = "Type", values = c(15, 17, 16))+
  scale_color_brewer(name = "Type", palette = 2, type = "qual")+
  # geom_density()+
  # geom_bar(stat = "bin", bins = 10, position = position_dodge())+
  labs(caption = "Gamma decreases at constant rate; Cohort 2 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")

vgg_types |> 
  filter(c1_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  ggplot(aes(x = type, y = final_state, group = type, fill = type, color = type))+
  xlab("Type")+
  ylab("Mean state")+
  theme_bw()+
  geom_violin()+
  # geom_density()+
  # geom_bar(stat = "bin", bins = 10, position = position_dodge())+
  labs(caption = "Gamma decreases at constant rate; Cohort 1 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")+
  facet_grid(~c2_percent_grammar_1)+
  theme(legend.position = "none")
  # When cohort 1 does not use grammar 1, as cohort 2's starting percentage of having grammar 1 varies, they will drive the population towards using grammar 1

ggplot(vgg_proc |> filter(c1_percent_grammar_1 == 10),
       aes(x = type, y = eq_percent, group = type, fill = type, color = type))+
  xlab("Type")+
  ylab("Time to equilibrium")+
  theme_bw()+
  geom_violin()+
  stat_summary(fun = mean, geom = "point", color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "black")+
  labs(caption = "Gamma decreases at constant rate; Cohort 1 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")+
  facet_grid(~c2_percent_grammar_1)+
  theme(legend.position = "none")

vgg_types  |> 
  filter(c1_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  write_csv(here("paper","processed_data","vgg_types_group2_variation.csv"))
  

## Group 1 varies, group 2 as not having grammar 1

vgg_types |> 
  filter(c2_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  ggplot(aes(x = c1_percent_grammar_1, y = final_state, 
             group = type, shape = type, color = type))+
  xlab("Percentage of grammar 1 (Cohort 1)")+
  ylab("Final state")+
  theme_bw()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3)+
  stat_summary(fun = mean, geom = "point", size = 2)+
  stat_summary(fun = mean, geom = "line")+
  scale_shape_manual(name = "Type", values = c(15, 17, 16))+
  scale_color_brewer(name = "Type", palette = 2, type = "qual")+
  # geom_density()+
  # geom_bar(stat = "bin", bins = 10, position = position_dodge())+
  labs(caption = "Gamma decreases at constant rate; Cohort 2 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")

vgg_types |> 
  filter(c2_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  ggplot(aes(x = type, y = final_state, group = type, fill = type, color = type))+
  xlab("Equilibrium reached")+
  theme_bw()+
  geom_violin()+
  # geom_density()+
  # geom_bar(stat = "bin", bins = 10, position = position_dodge())+
  labs(caption = "Gamma decreases at constant rate; Cohort 2 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")+
  facet_grid(~c1_percent_grammar_1)+
  theme(legend.position = "none")
  # However, when cohort 2 does not use grammar 1, cohort 1 starts with increasingly higher percentages of using grammar 1, they themselves may end up with using grammar 1 more, but it does not drive the overall group usage.
  # Innovators and reservoir

ggplot(vgg_proc |> filter(c2_percent_grammar_1 == 10),
       aes(x = c1_percent_grammar_1, y = eq_percent, 
           group = type, shape = type, color = type))+
  xlab("Percentage of grammar 1 (Cohort 1)")+
  ylab("Time to equilibrium")+
  theme_bw()+
  # geom_violin()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3)+
  stat_summary(fun = mean, geom = "point", size = 2)+
  stat_summary(fun = mean, geom = "line")+
  scale_shape_manual(name = "Type", values = c(15, 17, 16))+
  scale_color_brewer(name = "Type", palette = 2, type = "qual")+
  labs(caption = "Gamma decreases at constant rate; Cohort 2 does not have grammar 1; 50% Cohort 2; 50 runs per configuration, 1000 steps total per run")
  # facet_grid(~c1_percent_grammar_1)+
  # theme(legend.position = "none")

vgg_types  |> 
  filter(c2_percent_grammar_1 == 10) |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1\nmean",
                        if_else(type == "cohort2_state", "Cohort 2\nmean",
                                "Overall\nmean"))) |> 
  write_csv(here("paper","processed_data","vgg_types_group1_variation.csv"))



#####################################################
## Group preference: ----
# Variable terms: percent grammar 1, percent cohort 2, chance listen to outgroup
#####################################################

pref_df <- read_csv(here("code",
                     "Language Change group-preference-table.csv"),
                show_col_types = F, skip = 6) |> 
  janitor::clean_names()

final_counts <- pref_df |> 
  filter(step == 1000) |> 
  select(c(run_number,mean_state_of_nodes, cohort1_state, cohort2_state)) |> 
  rename(equilibrium = mean_state_of_nodes,
         cohort1_equil = cohort1_state,
         cohort2_equil = cohort2_state)

pref_types <- pref_df |> 
  filter(step == 1000) |> 
  select(c(chance_listen_to_outgroup, percent_grammar_1, 
           cohort1_state, cohort2_state, mean_state_of_nodes)) |> 
  pivot_longer(cols = c(cohort1_state, cohort2_state, mean_state_of_nodes),
               values_to = "final_state", names_to = "type")

pref_proc <- pref_df |> 
  left_join(final_counts, by = "run_number") |> 
  mutate(equilibrium_reached = abs(mean_state_of_nodes-equilibrium)<0.01) |> 
  filter(equilibrium_reached == T) |> 
  group_by(run_number, chance_listen_to_outgroup, percent_grammar_1) |> 
  summarize(eq_step = min(step)) |> 
  mutate(max_steps = 1000,
         eq_percent = eq_step / max_steps)



# Graphing

pref_types |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1 mean",
                        if_else(type == "cohort2_state", "Cohort 2 mean",
                                "Overall mean"))) |> 
  ggplot(aes(x = percent_grammar_1, y = final_state,
             color = factor(chance_listen_to_outgroup),
             group = factor(chance_listen_to_outgroup)))+
  theme_bw()+
  xlab("Initial percent of grammar 1")+
  ylab("Final state")+
  scale_color_brewer(name = "Willingness to\nlisten to outgroup", type = "div",
                     palette = 2)+ 
  # scale_shape_manual(name = "type", values = c(14, 15, 16))+
  # geom_point(alpha = 0.5)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2)+
  stat_summary(fun = mean, geom = "point", size = 2)+
  stat_summary(fun = mean, geom = "line")+
  # ggtitle("Different starting percentages")+
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  facet_grid(~factor(type))
  # No difference in resulting group-level equilibria, but groups that are less willing to listen have lower equilibria?

pref_types |> 
  mutate(type = if_else(type == "cohort1_state", "Cohort 1 mean",
                        if_else(type == "cohort2_state", "Cohort 2 mean",
                                "Overall mean"))) |>
  write_csv(here("paper","processed_data","preference_final_state.csv"))

ggplot(pref_proc, aes(x = percent_grammar_1, y = eq_percent,
                           color = factor(chance_listen_to_outgroup),
                           group = factor(chance_listen_to_outgroup)))+
  xlab("Percent grammar 1")+
  ylab("Equilibrium reached")+
  theme_bw()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25)+
  stat_summary(fun = mean, geom = "point", size = 2)+
  stat_summary(fun = mean, geom = "line")+
  scale_color_brewer(name = "Willingnessto\nlisten to outgroup", type = "div",
                    palette = 1)+ 
  labs(caption = "50 runs per configuration, 1000 steps total per run")+
  ggtitle("Impact of second cohort age on equilibrium")

write_csv(pref_proc, here("paper","processed_data","preference_proc.csv"))
