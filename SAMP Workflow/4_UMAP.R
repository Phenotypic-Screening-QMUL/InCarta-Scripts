# UMAP
library(embed)

Ind_Scale <-  na.omit(Ind_Scale)

umap_rec <- recipe(~., data = Ind_Scale)  %>%
  update_role(colnames, new_role = "id") %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

umap_prep

library(ggplot2)

juice(umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = colnames)) +
  geom_point(aes(color = colnames ), size = 0.1, shape = 16) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "arial") +
  labs(color = NULL, size= 5) + theme(legend.text=element_text(size=15)) +
  guides(color=guide_legend(override.aes=list(size=5))) + 
  theme (axis.text = element_text (size= 15), axis.title = element_text (size =15))
#+ xlim(-5,5)