source("preprocess.R")

fly_df <- medfly_preprocess()
mean_ci_df <- create_mean_ci_df(fly_df, c("lhf", "lhm", "sm", "sf"))

if (!dir.exists("plots")) {
  dir.create("plots")
}

#### Figure 1 ####
ggarrange(
  plotlist =
    list(
      ggplot(
        data = fly_df %>% filter(age <= 45),
        aes(
          x = age,
          y = lhf
        )
      ) +
        geom_line(aes(
          group = cage,
          color = cage
        )) +
        geom_line(
          data = mean_ci_df %>%
            filter(pred == "lhf"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "red"
        ) +
        xlab("Day") +
        ylab("Log-Hazard Function") +
        labs(title = "Female") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_viridis_c("Cage"),
      ggplot(
        data = fly_df %>% filter(age <= 45),
        aes(
          x = age,
          y = lhm
        )
      ) +
        geom_line(aes(
          group = cage,
          color = cage
        )) +
        geom_line(
          data = mean_ci_df %>%
            filter(pred == "lhm"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "red"
        ) +
        xlab("Day") +
        ylab("Log-Hazard Function") +
        labs(title = "Male") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_viridis_c("Cage")
    ), common.legend = TRUE,
  legend = "top"
) %>% ggsave(
  filename = "plots/obs_lh.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)



#### Figure 2 ####
ggarrange(
  plotlist =
    list(
      ggplot(
        data = fly_df %>% filter(age <= 45),
        aes(
          x = age,
          y = sf
        )
      ) +
        geom_line(aes(
          group = cage,
          color = cage
        )) +
        geom_line(
          data = mean_ci_df %>%
            filter(pred == "sf"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "red"
        ) +
        xlab("Day") +
        ylab("Survival Function") +
        labs(title = "Female") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_viridis_c("Cage"),
      ggplot(
        data = fly_df %>% filter(age <= 45),
        aes(
          x = age,
          y = sm
        )
      ) +
        geom_line(aes(
          group = cage,
          color = cage
        )) +
        geom_line(
          data = mean_ci_df %>%
            filter(pred == "sm"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "red"
        ) +
        xlab("Day") +
        ylab("Survival Function") +
        labs(title = "Male") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_viridis_c("Cage")
    ), common.legend = TRUE,
  legend = "top"
) %>% ggsave(
  filename = "plots/obs_surv.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)


#### Figure 3 ####
ggarrange(
  plotlist =
    list(
      ggplot(
        data = fly_df %>% filter(cage == 160 | cage == 82, age <= 45),
        aes(
          group = cage,
          linetype = factor(cage)
        )
      ) +
        geom_line(aes(x = age, y = lhf, color = "Female")) +
        geom_line(aes(x = age, y = lhm, color = "Male")) +
        labs(title = "Log-Hazard Functions") +
        ylab("Log-Hazard Functions") +
        xlab("Days") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(name = "Sex", values = c("pink", "light blue")) +
        scale_linetype_discrete(name = "Cage"),
      ggplot(
        data = fly_df %>% filter(cage == 160 | cage == 82, age <= 45),
        aes(
          group = cage,
          linetype = factor(cage)
        )
      ) +
        geom_line(aes(x = age, y = sf, color = "Female")) +
        geom_line(aes(x = age, y = sm, color = "Male")) +
        labs(title = "Survival Functions") +
        ylab("Survival Functions") +
        xlab("Days") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(name = "Sex", values = c("pink", "light blue")) +
        scale_linetype_discrete(name = "Cage")
    ),
  common.legend = TRUE
) %>% ggsave(
  filename = "plots/cage_effect.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)


#### Figure 4 ####

ggarrange(
  plotlist = list(
    ggplot(data = mean_ci_df %>%
      filter(pred == "lhf" | pred == "lhm"), aes(group = pred)) +
      geom_ribbon(
        aes(
          x = CIgrid,
          ymin = lower,
          ymax = upper,
          fill = pred
        ),
        alpha = 0.7
      ) +
      geom_line(aes(x = CIgrid, y = mean)) +
      xlab("Days") +
      xlim(0, 45) +
      ylab("Log-Hazard Function") +
      scale_fill_manual(
        name = "Sex",
        labels = c("Female", "Male"),
        values = c("pink", "light blue")
      ),
    ggplot(data = mean_ci_df %>%
      filter(pred == "sf" | pred == "sm"), aes(group = pred)) +
      geom_ribbon(
        aes(
          x = CIgrid,
          ymin = lower,
          ymax = upper,
          fill = pred
        ),
        alpha = 0.7
      ) +
      geom_line(aes(x = CIgrid, y = mean)) +
      xlab("Days") +
      ylab("Survival Function") +
      xlim(0, 45) +
      scale_fill_manual(
        name = "Sex",
        labels = c("Female", "Male"),
        values = c("pink", "light blue")
      )
  ),
  common.legend = TRUE
) %>% ggsave(
  filename = "plots/mean_funcs.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)

set.seed(101)
t <- sample(1:40, 87, replace = TRUE)

#### Figure 5 ####
fig_5 <- append(
  frechet_plot(fly_df, t, "nf", "age", "Age", "y", "Female Density"),
  frechet_plot(fly_df, t, "nf", "age", "Age", "lhf", "Log-Hazard Female")
)

ggarrange(
  plotlist = fig_5,
  common.legend = TRUE
) %>% ggsave(
  filename = "plots/nf~age.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)

#### Figure 6 ####
fig_6 <- append(
  frechet_plot(fly_df, t, "nf", c("age", "sm_sa"), c("Age", "SM-SA"), "y", "Female Density"),
  frechet_plot(fly_df, t, "nf", c("age", "sm_sa"), c("Age", "SM-SA"), "lhf", "Log-Hazard Female")
)

ggarrange(
  plotlist = fig_6,
  common.legend = TRUE
) %>% ggsave(
  filename = "plots/nf~age+sm_sa.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)


#### Figure 7 ####
fig_7 <- append(
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sm_sa", "SM-SA", "y", "Female Density")),
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sm_sa", "SM-SA", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_7,
  common.legend = TRUE,
  nrow = 2,
  ncol = 4,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25"),
  label.x = 0.45,
  label.y = 1.05,
  font.label = list(face = "plain", size = 10)
) %>% ggsave(
  filename = "plots/nf~sm_sa.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)



#### Figure 8 ####
fig_8 <- append(
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sf", "SF", "y", "Female Density")),
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sf", "SF", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_8,
  common.legend = TRUE,
  nrow = 2,
  ncol = 4,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25"),
  label.x = 0.45,
  label.y = 1.05,
  font.label = list(face = "plain", size = 10),
  legend = "top"
) %>% ggsave(
  filename = "plots/nf~sf.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)



#### Figure 9 ####
fig_9 <- append(
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sm", "SM", "y", "Male Density")),
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sm", "SM", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_9,
  common.legend = TRUE,
  nrow = 2,
  ncol = 4,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25"),
  label.x = 0.45,
  label.y = 1.05,
  font.label = list(face = "plain", size = 10)
) %>% ggsave(
  filename = "plots/nm~sm.pdf", device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)

#### Figure 10 ####
fig_10 <- append(
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "na", "sa", "SA", "y", "All Density")),
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "na", "sa", "SA", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_10,
  common.legend = TRUE,
  nrow = 2,
  ncol = 4,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25"),
  label.x = 0.45,
  label.y = 1.05,
  font.label = list(face = "plain", size = 10)
) %>% ggsave(
  filename = "plots/na~sa.pdf",
  device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)

#### Figure 11 ####
fig_11 <- append(
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sf_sa", "SF-SA", "y", "Male Density")),
  c(10, 15, 20, 25) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sf_sa", "SF-SA", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_11,
  common.legend = TRUE,
  nrow = 2,
  ncol = 4,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25"),
  label.x = 0.45,
  label.y = 1.05,
  font.label = list(face = "plain", size = 10)
) %>% ggsave(
  filename = "plots/nm~sf_sa.pdf",
  device = "pdf",
  width = 20,
  height = 10,
  units = "cm"
)


plan(strategy = "multicore", workers = 4)

#### Figure 8 ####
# bootstrap(fly_df, "lhf", c("sa", "sm"), "sf~sa+sm")
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm"), c("SA", "SM"), "sf~sa+sm")

#### Figure 9 ####
# bootstrap(fly_df, "lhf", "sm_sa", "sf~(sa-sm)")
bootstrap_ci_plot(fly_df, "lhf", "sm_sa", "SM-SA", "sf~(sa-sm)")

#### Figure 10 ####
# bootstrap(fly_df, "lhf", c("sa", "sm_sa"), "sf~sa+(sa-sm)")
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm_sa"), c("Survival All", "Survival Male - Survival All"), "lhf~sa+(sm-sa)")
