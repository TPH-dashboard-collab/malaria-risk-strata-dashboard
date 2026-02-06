
# History cleanup
rm(list=ls())
#gc()

# Load the necessary packages
library(devtools)
library(RSQLite)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(sf)
library(patchwork)
library(cowplot)
library(ggpattern)
# helpers functions -----------------------------------------------------------------------


risk_strata = read.csv("DashboardTasks/code-snippets/risk-strata/risk_strata.csv")

sim_plan = read_csv2("DashboardTasks/code-snippets/risk-strata/Tz_NSP_latest_ST_26_08_2024.csv")
#view(sim_plan)
sim_plan$DHIS2_Dist[sim_plan$DHIS2_Dist=="Mpanda District Council"] = "Tanganyika District Council"
sim_plan$DHIS2_Dist[sim_plan$DHIS2_Dist=="Kilombero District Council"] = "Mlimba District Council"


planInterested= c("BAU_country", "BAU_Cal", "NSP_country", "Counterfactual_LLIN",
                  "NSP_country_LSM80", "BAU_country_LSM80", "BAU_PBO" , "NSP_PBO_iccm",
                  "NSP_PBO_IRS_IPtsc_iccm","NSP_PBO_IPtsc_iccm","BAU_PBO_IPTSc","BAU_PBO_irs")

risk_strata_update <- risk_strata %>%
  select(-Region, -District_ITN_nmcp)

risk_strata_update <- risk_strata_update %>%
  filter(DHIS2_Dist != "Kigamboni Municipal Council")


TZA_data <- readRDS(
  "DashboardTasks/code-snippets/risk-strata/RiskStrataInputs.rds"
)

view(TZA_data)


#proportion of districts BAU IPTSc
IPTSc_BAU_districts <- sim_plan %>% filter(IPTSc_curr == "Yes") %>% select(DHIS2_Dist)
IPTSc_BAU_districts  <- as.character(IPTSc_BAU_districts$DHIS2_Dist)



nb_dist_pr_BAU_IPTsc = TZA_data |>
  filter(plan %in% planInterested[c(11,10)],
         DHIS2_Dist %in% IPTSc_BAU_districts,
         age_group=="2-10",
         year %in% c(2020,2030)) |> #available in postprocessing |>
  group_by(year, plan) |>
  summarise("very low" = sum(ind_mean<=0.019),
            "low" = sum(ind_mean>0.019 & ind_mean<=0.033),
            "moderate" = sum(ind_mean>0.033&ind_mean<=0.11),
            "high"= sum(ind_mean>0.11)) |>
  pivot_longer(cols = 3:6)
nb_dist_pr_BAU_IPTsc$name = factor(nb_dist_pr_BAU_IPTsc$name, levels = unique(nb_dist_pr_BAU_IPTsc$name))
nb_dist_pr_BAU_IPTsc$no_districts = nb_dist_pr_BAU_IPTsc$value

nb_dist_pr_BAU_IPTsc |>
  ggplot() +
  geom_bar(
    aes(x = as.factor(year), y = value, fill = plan),
    alpha = 0.6,
    stat = "identity",
    position = position_dodge()
  ) +
  xlab("Year") +
  ylab("Number of districts\n with PfPR in 2–10") +
  facet_wrap(name ~ ., scales = "free_y") +
  scale_fill_brewer(
    palette = "Set2",
    name = "Plan",
    labels = c("BAU_PBO_IPTSc", "NSP_PBO_IPTSc_iCCM")
  ) +
  theme_cowplot(font_size = 40 / .pt) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  coord_cartesian(ylim = c(0, 120))


# Calculate proportions
prop_dist_pr_BAU_IPTSc<- nb_dist_pr_BAU_IPTsc %>%
  group_by(year, plan) %>%
  mutate(total_per_plan = sum(no_districts)) %>%
  mutate(proportion = no_districts / total_per_plan * 100) %>%
  select(year, plan, name, no_districts, proportion) %>%
  arrange(year, plan, name)


prop_dist_pr_BAU_IPTSc |>
  ggplot() +
  geom_bar(
    aes(x = as.factor(year), y = proportion, fill = plan),
    alpha = 0.6,
    stat = "identity",
    position = position_dodge()
  ) +
  xlab("Year") +
  ylab("Proportion of districts\n with PfPR in 2–10") +
  facet_wrap(name ~ ., scales = "fixed") +
  scale_fill_brewer(
    palette = "Set2",
    name = "Plan",
    labels = c("BAU_PBO_IPTSc", "NSP_PBO_IPTSc_iCCM")
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(labels = function(x) ifelse(x == "2027", "2030", x)) +
  theme_cowplot(font_size = 40 / .pt) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  coord_cartesian(ylim = c(0, 100))


view(prop_dist_pr_BAU_IPTSc)


#proportion of districts NSP IPTSc
IPTSc_NSP_districts <- sim_plan %>% filter(IPTSc_NSP_New == "yes") %>% select(DHIS2_Dist)
IPTSc_NSP_districts  <- as.character(IPTSc_NSP_districts$DHIS2_Dist)


nb_dist_pr_NSP_IPTsc = TZA_data |>
  filter(plan %in% planInterested[c(11,10)],
         DHIS2_Dist %in% IPTSc_NSP_districts,
         age_group=="2-10",
         year %in% c(2020,2030)) |> #available in postprocessing |>
  group_by(year, plan) |>
  summarise("very low" = sum(ind_mean<=0.019),
            "low" = sum(ind_mean>0.019 & ind_mean<=0.033),
            "moderate" = sum(ind_mean>0.033&ind_mean<=0.11),
            "high"= sum(ind_mean>0.11)) |>
  pivot_longer(cols = 3:6)
nb_dist_pr_NSP_IPTsc$name = factor(nb_dist_pr_NSP_IPTsc$name, levels = unique(nb_dist_pr_NSP_IPTsc$name))
nb_dist_pr_NSP_IPTsc$no_districts = nb_dist_pr_NSP_IPTsc$value

nb_dist_pr_NSP_IPTsc |>
  ggplot() +
  geom_bar(
    aes(x = as.factor(year), y = value, fill = plan),
    alpha = 0.6,
    stat = "identity",
    position = position_dodge()
  ) +
  xlab("Year") +
  ylab("Number of districts\n with PfPR in 2–10") +
  facet_wrap(name ~ ., scales = "free_y") +
  scale_fill_brewer(
    palette = "Set2",
    name = "Plan",
    labels = c("BAU_PBO_IPTSc", "NSP_PBO_IPTSc_iCCM")
  ) +
  theme_cowplot(font_size = 40 / .pt) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  coord_cartesian(ylim = c(0, 120))



# Calculate proportions
prop_dist_pr_NSP_IPTSc<- nb_dist_pr_NSP_IPTsc %>%
  group_by(year, plan) %>%
  mutate(total_per_plan = sum(no_districts)) %>%
  mutate(proportion = no_districts / total_per_plan * 100) %>%
  select(year, plan, name, no_districts, proportion) %>%
  arrange(year, plan, name)


prop_dist_pr_NSP_IPTSc |>
  ggplot() +
  geom_bar(
    aes(x = as.factor(year), y = proportion, fill = plan),
    alpha = 0.6,
    stat = "identity",
    position = position_dodge()
  ) +
  xlab("Year") +
  ylab("Proportion of districts\n with PfPR in 2–10") +
  facet_wrap(name ~ ., scales = "fixed") +
  scale_fill_brewer(
    palette = "Set2",
    name = "Plan",
    labels = c("BAU_PBO_IPTSc", "NSP_PBO_IPTSc_iCCM")
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(labels = function(x) ifelse(x == "2027", "2030", x)) +
  theme_cowplot(font_size = 40 / .pt) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  coord_cartesian(ylim = c(0, 100))

