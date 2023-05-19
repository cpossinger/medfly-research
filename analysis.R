source("preprocess.R")

fly_df <- medfly_preprocess()
mean_ci_df <- create_mean_ci_df(fly_df, c("lhf", "lhm", "sm", "sf"))

if (!dir.exists("plots")) {
  dir.create("plots")
}

fly_df %>%
  summarise(lhfm = mean(sf)) %>%
  filter(lhfm == min(lhfm) | lhfm == max(lhfm))



ggarrange(

          plotlist = list(
ggplot()+
geom_line(
data = fly_df %>% filter(age <= 45, age >= 5),
          aes(
           x = age,
           y = lhf,
           group = cage
          ),
color = "#fa8072",
alpha = 0.1
)+
geom_line(
data = fly_df %>% filter(age <= 45),
          aes(
           x = age,
           y = lhm,
           group = cage
          ),
color = "#40e0d0",
alpha = 0.1
) +
geom_line(
          data = mean_ci_df %>%
            filter(pred == "lhf"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "black",
          alpha = 1
        )+
geom_line(
          data = mean_ci_df %>%
            filter(pred == "lhm"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "black",
          alpha = 1
        )+

      geom_ribbon(
data = mean_ci_df %>%
      filter(pred == "lhf" | pred == "lhm"),
        aes(
          x = CIgrid,
          ymin = lower,
          ymax = upper,
          group = pred,
          fill = pred
        ),
        alpha = 0.7,
      )+
          scale_fill_manual(values = c("#fa8072","#40e0d0"),
                            labels = c("Female","Male"),
                            name = "Sex")+
          xlab("Day")+
          ylab("Log-Hazard Function")+
          theme_minimal(),


ggplot()+
geom_line(
data = fly_df %>% filter(age <= 45, age >= 5),
          aes(
           x = age,
           y = sf,
           group = cage
          ),
color = "#fa8072",
alpha = 0.2
)+
geom_line(
data = fly_df %>% filter(age <= 45),
          aes(
           x = age,
           y = sm,
           group = cage
          ),
color = "#40e0d0",
alpha = 0.2
) +
geom_line(
          data = mean_ci_df %>%
            filter(pred == "sf"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "black",
          alpha = 1
        )+
geom_line(
          data = mean_ci_df %>%
            filter(pred == "sm"),
          aes(
            x = CIgrid,
            y = mean
          ),
          color = "black",
          alpha = 1
        )+

      geom_ribbon(
data = mean_ci_df %>%
      filter(pred == "sf" | pred == "sm"),
        aes(
          x = CIgrid,
          ymin = lower,
          ymax = upper,
          group = pred,
          fill = pred
        ),
        alpha = 0.7,
      )+
scale_fill_manual(values = c("#fa8072","#40e0d0"),
                            labels = c("Female","Male"),
                            name = "Sex")+
          xlab("Day")+
          ylab("Survival Function")+
          theme_minimal()


        ),
          common.legend = TRUE
) %>% ggsave(
  filename = "plots/functions_combo.pdf", device = "pdf",
  width = 28,
  height = 20,
  units = "cm"
)






















ggplot()+
  geom_line(
            data = fly_df %>%
              pivot_longer(cols = c(lhf,lhm))
            %>% select(name,value,age,cage)
            %>% filter(age <= 45),
            aes(
                x = age,
                y = value,
                color = name,
                group = cage
            )
  )








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
        scale_color_viridis_c("Cage"),
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
          ),
          common.legend = TRUE,
    ncol = 2,
    nrow = 2,
  legend = "top"
) %>% ggsave(
  filename = "plots/functions.pdf", device = "pdf",
  width = 20,
  height = 20,
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
)




%>% ggsave(
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
        data = fly_df %>% filter(cage == 19 | cage == 70, age <= 45),
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
        scale_color_manual(name = "Sex", values = c("#fa8072","#40e0d0")) +
        scale_linetype_discrete(name = "Cage")+
        theme_minimal(),
      ggplot(
        data = fly_df %>% filter(cage == 19 | cage == 70, age <= 45),
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
        scale_color_manual(name = "Sex", values = c("#fa8072","#40e0d0")) +
        scale_linetype_discrete(name = "Cage")+
        theme_minimal()
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
          ymax = upper
        ),
        alpha = 0.7,
        fill = "grey"
      ) +
      geom_line(aes(x = CIgrid, y = mean, color = pred)) +
      xlab("Days") +
      xlim(0, 45) +
      ylab("Log-Hazard Function") +
      scale_color_manual(
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
          ymax = upper
        ),
        alpha = 0.7,
        fill = "grey"
      ) +
      geom_line(aes(x = CIgrid, y = mean, color = pred)) +
      xlab("Days") +
      ylab("Survival Function") +
      xlim(0, 45) +
      scale_color_manual(
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

nonparam_plot  <-  function(df,t,y_axis,x_axis,y_axis_name,x_axis_name,title,y_limits){

if ((t %>% unique()) != 10) {
    y_axis_name <- ""
  }

cage_lst <- df %>% group_split()

y  <-  cage_lst %>%
    map2(t, function(cage, t) {
      cage %<>% filter(age >= t)
      resp <- cage$age - t
      bin <- cage[[y_axis]]
      rep(resp, bin)
    }) %>% map_dbl(~ .x %>% mean()) 
x  <- fly_df %>% filter(age == (t %>% unique())) %>% extract2(x_axis) 

plot_df  <- data.frame(x = x, y = y)

ggplot(plot_df,aes(x = x, y = y)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "loess")+
  xlab(x_axis_name)+
  ylab(y_axis_name)+
  theme_minimal() +
  theme(text = element_text(size = 9)) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(
        limits = y_limits
      )

}


#### Figure 7 ####
fig_7 <- append(
  c(10,20,30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sm_sa", "SM-SA", "lhf",str_wrap("Female Log Hazard",10), "Remaining Lifespan", c(-3,-1))),
  c(10,20,30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sm_sa", "SM-SA", "y", str_wrap("Female Remaining Lifespan Distribution",10), "Remaining Lifespan", c(0,0.12)))
) %>% flatten()

fig_7 <- c(c(10,20,30) %>% map2(list("t = 10 days, FVE = 0.14",  "t = 20 days, FVE = 0.09","t = 30 days, FVE = 0.21"),~nonparam_plot(fly_df,rep(.x, 87),"nf","sm_sa",str_wrap("Female Mean Remaining Lifespan",10),"SM-SA",.y,c(5,10))),fig_7)

ggarrange(
  plotlist = fig_7,
  common.legend = TRUE,
  legend = "right"
) %>% ggsave(filename ="frechetFemale2Male.pdf",
path = "plots",
width = 183, 
height = 103, 
dpi = 600,
units = "mm"
)


fly_df %>% group_map(~.x %>% filter(age == 1) %>% select(nf,nm)) %>% bind_rows() %>% extract2("nm") %>% mean 
fly_df %>% group_map(~.x %>% filter(age == 1) %>% select(nf,nm)) %>% bind_rows() %>% extract2("nf") %>% mean 

fly_df 

#### Figure 8 ####
fig_8 <- append(
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sf", "SF", "y", "Female Density")),
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nf", "sf", "SF", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_8,
  common.legend = TRUE,
  nrow = 2,
  ncol = 5,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25", "t = 30"),
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
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sm", "SM", "y", "Male Density")),
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sm", "SM", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_9,
  common.legend = TRUE,
  nrow = 2,
  ncol = 5,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25", "t = 30"),
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
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "na", "sa", "SA", "y", "All Density")),
  c(10, 15, 20, 25, 30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "na", "sa", "SA", "lhf", "Log-Hazard Female"))
) %>% flatten()

ggarrange(
  plotlist = fig_10,
  common.legend = TRUE,
  nrow = 2,
  ncol = 5,
  labels = c("t = 10", "t = 15", "t = 20", "t = 25", "t = 30"),
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
  c(10,20,30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sf_sa", "SF-SA", "lhm",str_wrap("Male Log Hazard",10), "Remaining Lifespan", c(-3,-0.5))),
  c(10,20,30) %>% map(~ frechet_plot(fly_df, rep(.x, 87), "nm", "sf_sa", "SF-SA", "y", str_wrap("Male Remaining Lifespan Distribution",10), "Remaining Lifespan", c(0,0.15)))
) %>% flatten()



fig_11 <- c(c(10,20,30) %>% map2(list("t = 10 days, FVE = 0.17",  "t = 20 days, FVE = 0.41","t = 30 days, FVE = 0.22"),~nonparam_plot(fly_df,rep(.x, 87),"nm","sf_sa",str_wrap("Male Mean Remaining Lifespan",10),"SF-SA",.y,c(4,12))),fig_11)


# t = 28     FVE = 0.39

ggarrange(
  plotlist = fig_11,
  common.legend = TRUE,
  legend = "right"
) %>% ggsave(filename ="frechetMale2Female.pdf",
path = "plots", 
width = 183, 
height = 103, 
dpi = 600,
units = "mm")
 

source("preprocess.R")

fly_df <- medfly_preprocess()

fly_df <- fly_df %>%
  mutate(
    lhf = lhf - (GetMeanCurve(
      Ly = fly_df %>% group_split() %>% map(~ .x %>% pull(lhf)),
      Lt = fly_df %>% group_split() %>% map(~ .x %>% pull(age))
    )$mu),
lhm = lhm - (GetMeanCurve(
      Ly = fly_df %>% group_split() %>% map(~ .x %>% pull(lhm)),
      Lt = fly_df %>% group_split() %>% map(~ .x %>% pull(age))
    )$mu),
    sm = sm - (GetMeanCurve(
      Ly = fly_df %>% group_split() %>% map(~ .x %>% pull(sm)),
      Lt = fly_df %>% group_split() %>% map(~ .x %>% pull(age))
    )$mu),
    sf = sf - (GetMeanCurve(
      Ly = fly_df %>% group_split() %>% map(~ .x %>% pull(sf)),
      Lt = fly_df %>% group_split() %>% map(~ .x %>% pull(age))
    )$mu),
    sa = sa - (GetMeanCurve(
      Ly = fly_df %>% group_split() %>% map(~ .x %>% pull(sa)),
      Lt = fly_df %>% group_split() %>% map(~ .x %>% pull(age))
    )$mu),
    sm_sa = sm - sa,
    sf_sa = sf - sa
  ) %>%
  filter(age <= 50, age > 0)


bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm"), c("Survival All", "Survival Male"), "lhf~sa+sm")
bootstrap_ci_plot(fly_df, "lhf", "sm_sa", "Survival Male - Survival All ", "lhf~sm_sa")
bootstrap_ci_plot(fly_df, "lhf", "sf_sa", "Survival Female - Survival All ", "lhf~sf_sa")

bootstrap(fly_df, "lhf", c("sa", "sm"), "lhf~sa+sm", 100)
bootstrap(fly_df, "lhf", "sm_sa", "lhf~sm_sa", 100)

bootstrap(fly_df, "lhf", "sf_sa", "lhf~sf_sa", 100)




bootstrap(fly_df, "lhm", c("sa", "sf"), "lhm~sa+sf", 2000)
bootstrap(fly_df, "lhm", c("sa", "sf_sa"), "lhm~sa+sf_sa", 2000)



bootstrap(fly_df, "lhf", c("sa", "sm_sa"), "lhf~sa+sm_sa", 2000)
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm_sa"), c("Survival All", "Survival Male - Survival All "), "lhf~sa+sm_sa")

bootstrap_ci_plot(fly_df, "lhf", "sm_sa", "Survival Male - Survival All","lhf~sm_sa")


bootstrap_ci_plot(fly_df, "lhm", c("sa", "sf"), c("Survival All", "Survival Female"), "lhm~sa+sf")
bootstrap_ci_plot(fly_df, "lhm", c("sa", "sf_sa"), c("Survival All", "Survival Female - Survival All "), "lhm~sa+sf_sa")

# model new limits
bootstrap_ci_plot(fly_df, "lhm", "sf_sa", "Survival Female - Survival All", "lhm~sf-sa",c(-250,50))



bootstrap(fly_df, "lhf", "sm_sa", "lhf~sm-sa", 10000)


#bootstrap(fly_df, "lhf", c("sa", "sm_sa"), "lhf~sa+(sm-sa)", 10000)
#source("preprocess.R")
#fly_df <- medfly_preprocess()




test <- readRDS(paste0("rst/", "lhf~sa+(sm-sa)", ".rds"))
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm_sa"), c("Survival All", "Survival Male - Survival All"), "lhf~sa+(sm-sa)")



#### Figure 8 ####
bootstrap(fly_df, "lhf", c("sa", "sm"), "lhf~sa+sm", 10)
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm"), c("Survival All", "Survival Male"), "lhf~sa+sm")

#### Figure 9 ####
bootstrap(fly_df, "lhf", "sm_sa", "lhf~(sa-sm)", 10)
bootstrap_ci_plot(fly_df, "lhf", "sm_sa", "SM-SA", "lhf~(sa-sm)")

#### Figure 10 ####
# bootstrap(fly_df, "lhf", c("sa", "sm_sa"), "lhf~sa+(sa-sm)")
bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm_sa"), c("Survival All", "Survival Male - Survival All"), "lhf~sa+(sm-sa)")


bootstrap_ci_plot(fly_df, "lhf", c("sa", "sm_sa"), c("Survival All", "Survival Male - Survival All"), "lhf~sa+(sm-sa)")




#### FVE ####

frechet_fve <- function(df, t, response, pred) {
  cage_lst <- df %>% group_split()

  xin <- pred %>%
    map(function(pred) {
      cage_lst %>%
        map2_dbl(t, ~ .x %>%
          pull(pred) %>%
          extract(.y))
    }) %>%
    reduce(cbind)

  hin <- cage_lst %>%
    map2(t, function(cage, t) {
      cage %<>% filter(age >= t)
      resp <- cage$age - t
      bin <- cage[[response]]
      hist(rep(resp, bin), plot = FALSE)
    })

  GloDenReg(
    xin = xin,
    hin = hin,
    optns = list("Rsquared" = TRUE)
  )$Rsq
}

#### Figure 5 ####
frechet_fve(fly_df, t, "nf", "age")
#### Figure 6 ####
frechet_fve(fly_df, t, "nf", c("age", "sm_sa"))
#### Figure 7 ####
c(10,20,30) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nf", "sm_sa"))
c(10) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nf", "sm_sa"))
#### Figure 8 ####
c(10, 15, 20, 25, 30) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nf", "sf"))
#### Figure 9 ####
c(10, 15, 20, 25, 30) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nm", "sm"))
#### Figure 10  ####
c(10, 15, 20, 25, 30) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "na", "sa"))
#### Figure 11 ####
c(10, 15, 20, 25, 30,28) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nm", "sf_sa"))
c(20,30) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nm", "sf_sa"))
c(10) %>% map(~ frechet_fve(fly_df, rep(.x, 87), "nm", "sf_sa"))
