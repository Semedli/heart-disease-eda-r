library(tidyverse)

# Datanın oxunması
heart <- read_csv("heart.csv")

# Cinsiyyətə görə xəstəlik faizinin hesablanması
gender_analysis <- heart %>% 
  group_by(sex) %>% 
  summarise(
    disease_rate = mean(target == 1) * 100,
    total_patients = n()
  )
print(gender_analysis)

# Sinə ağrısına görə maksimum xolesterin
chest_pain_analysis <- heart %>% 
  group_by(cp) %>% 
  summarise(maksimum_chol = max(chol, na.rm = TRUE))
print(chest_pain_analysis)

# Cinsiyyətə görə Ürək Xəstəliyi Nisbəti Qrafiki
heart_graph_data <- heart %>% 
  mutate(
    Cins = ifelse(sex == 1, "Kişi", "Qadın"),
    Status = ifelse(target == 1, "Xəstə", "Sağlam")
  )

ggplot(heart_graph_data, aes(x = Cins, fill = Status)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Sağlam" = "#2ECC71", "Xəstə" = "#E74C3C")) +
  labs(
    title = "Cinsiyyətə görə Ürək Xəstəliyi Nisbəti",
    x = "Cinsiyyət", y = "Faiz Göstəricisi", fill = "Diaqnoz"
  ) +
  theme_minimal()
