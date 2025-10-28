library(httr)
library(stringr)
library(readr)
library(progress) 
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(performance)
library(ggResidpanel)
library(sandwich)
library(lmtest)
library(stargazer)
library(broom)

dta1 <- read.csv("Dataset.csv")

dta1$date <- substr(dta1$timestamp, 1, 10)

dta1$date <- as.Date(dta1$date)
class(dta1$date)

dta1 <- dta1 |>
  mutate(Board = case_when(
    date < "2024-09-01" ~ 1,
    date >= "2024-09-01" &
      date < "2025-09-01" ~ 2,
    date >= "2025-09-01" ~ 3
  ))

write_xlsx(dta1, "Pizdec.xlsx")

dta2 <- read_xlsx("Dataset1.xlsx")

dta2 <- dta2 |>
  mutate(Board1 = case_when(
    Board == 1 ~ 1,
    Board != 1 ~ 0 
    ),
    Board2 = case_when(
      Board == 2 ~ 1,
      Board != 2 ~ 0
    ),
    Board3 = case_when(
      Board == 3 ~ 1,
      Board != 3 ~ 0
    ),
    Social = case_when(
      EngagementType == "Social" ~ 1,
      EngagementType != "Social" ~ 0
    ),
    Cultural = case_when(
      EngagementType == "Cultural" ~ 1,
      EngagementType != "Cultural" ~ 0
    ),
    Political = case_when(
      EngagementType == "Political" ~ 1,
      EngagementType != "Political" ~ 0
    ),
    Activism = case_when(
      EngagementType == "Activism" ~ 1,
      EngagementType != "Activism" ~ 0
    ))

write.csv(dta2, "Dataset_Final.csv")

#####################

dta <- read.csv("https://raw.githubusercontent.com/CIREnjoyer/ABBA_Report/refs/heads/main/Dataset_Final.csv")

dta$date <- as.Date(dta$date)

dta <- dta |>
  filter(!likesCount < 0) |>
  mutate(Board1 = as.factor(Board1),
         Board2 = as.factor(Board2),
         Board3 = as.factor(Board3),
         Board = as.factor(Board),
         Cultural = as.factor(Cultural),
         Activism = as.factor(Activism),
         Political = as.factor(Political),
         Social = as.factor(Social),
         EngagementType = as.factor(EngagementType),
         type = as.factor(type))

dta_use <- dta |>
  filter(Activism == 0 & Board3 == 0)

summary(dta$likesCount)

likes <- ggplot(dta_use, aes(likesCount)) + #likes count 
  geom_histogram(
    fill = "lightgrey",
    colour = "black"
  ) +
  labs(x = "Number of Likes", y = "Count")

ggsave("likes.pdf", plot = likes)

comments <- ggplot(dta, aes(commentsCount)) + #comments count 
  geom_histogram(
    fill = "lightgrey",
    colour = "black"
  ) +
  labs(x = "Number of Comments", y = "Count")

ggsave("comments.pdf", plot = comments)

likes_out <- ggplot(dta, aes(likesCount)) + #likes count without outliers
  geom_histogram(
    fill = "lightgrey",
    colour = "black"
  ) +
  xlim(0,250) +
  labs(x = "Number of Likes", y = "Count")

ggsave("likes_out.pdf", plot = likes_out)

comms_out <- ggplot(dta, aes(commentsCount)) + #comments count without outliers
  geom_histogram(
    fill = "lightgrey",
    colour = "black"
  ) +
  xlim(0,50) +
  labs(x = "Number of Comments", y = "Count")

ggsave("comms_out.pdf", plot = comms_out)

time <- ggplot(dta, aes(date, likesCount)) + #likes over time + median
  geom_step() +
  geom_hline(yintercept = median(dta$likesCount), colour = "red") +
  labs(x = "Date", y = "Number of Likes") +
  scale_x_date(date_breaks = "3 months")

ggsave("time.pdf", plot = time)

content <- ggplot(dta, aes(x = type, fill = Board)) + #type of content by board
  geom_bar(width = 0.5,
           position = "dodge") + 
  scale_fill_manual(values = c("darkblue", "darkred", "darkgreen")) +
  labs(x = "Type of Publication", y = "Count")

ggsave("content.pdf", plot = content)

engagement <- ggplot(dta, aes(x = EngagementType, fill = Board)) + #type of engagement by board
         geom_bar(position = "dodge",
                  width = 0.4) +
  scale_fill_manual(values = c("darkblue", "darkred", "darkgreen")) + 
  labs(x = "Engagement Type", y = "Count")

ggsave("engagement.pdf", plot = engagement)

englikes <- ggplot(dta, aes(x = EngagementType, y = likesCount)) + #likes by engagement
  geom_col(
    fill = "lightgrey",
    colour = "black",
    width = 0.4
  ) +
  labs(x = "Engagement Type", y = "Number of Likes")

ggsave("englikes.pdf", plot = englikes)

engcomms <- ggplot(dta, aes(x = EngagementType, y = commentsCount)) + #comments by engagement
  geom_col(
    fill = "lightgrey",
    colour = "black",
    width = 0.4
  ) + 
  labs(x = "Engagement Type", y = "Number of Comments")
  
ggsave("engcomms.pdf", plot = engcomms)

typelikes <- ggplot(dta, aes(x = type, y = likesCount)) + #likes by content type
  geom_col(
    fill = "lightgrey",
    colour = "black",
    width = 0.4
  ) + 
  labs(x = "Content Type", y = "Number of Likes")

ggsave("typelikes.pdf", plot = typelikes)

typecomms <- ggplot(dta, aes(x = type, y = commentsCount)) + #comments by content type
  geom_col(
    fill = "lightgrey",
    colour = "black",
    width = 0.4
  ) + 
  labs(x = "Content Type", y = "Number of Comments")

ggsave("typecomms.pdf", plot = typecomms)

boxplot <- ggplot(dta_use, aes(x = Board, y = likesCount)) + #boxplot likes by board
  geom_boxplot() + 
  labs(y = "Number of Likes")

ggsave("boxplot.pdf", plot = boxplot)

boxout <- ggplot(dta_use, aes(x = Board, y = likesCount)) + #boxplot likes by board without outliers
  geom_boxplot() +
  coord_cartesian(ylim = c(0,250)) + 
  labs(y = "Number of Likes")

ggsave("boxout.pdf", plot = boxout)

boxenglikes <- ggplot(dta_use, aes(x = EngagementType, y = likesCount)) + #boxplot likes by engagement
  geom_boxplot() + 
  labs(x = "Engagement Type", y = "Number of Likes")

ggsave("boxenglikes.pdf", plot = boxenglikes)

zoomboxenglikes <- ggplot(dta_use, aes(x = EngagementType, y = likesCount)) + #boxplot likes by engagement (zoomed)
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,200)) + 
  labs(x = "Engagement Type", y = "Number of Likes")

ggsave("zoomboxenglikes.pdf", plot = zoomboxenglikes)

boxtypelikes <- ggplot(dta_use, aes(x = type, y = likesCount)) + #boxplot likes by content
  geom_boxplot() + 
  labs(x = "Content Type", y = "Number of Likes")

ggsave("boxtypelikes.pdf", plot = boxtypelikes)

zoomboxtypelikes <- ggplot(dta_use, aes(x = type, y = likesCount)) + #boxplot likes by content (zoomed)
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,200)) + 
  labs(x = "Content Type", y = "Number of Likes")

ggsave("zoomboxtypelikes.pdf", plot = zoomboxtypelikes)



t.test(likesCount ~ Board, data = dta_use) #t-test
t.test(likesCount ~ Board, data = subset(dta_use, likesCount < 150)) #t-test without outliers



model1 <- lm(likesCount ~ Board + Cultural + Political + Social + type, data = dta_use)
coeftest(model1, vcov = vcovHC(model1, type = "HC2"))
summary(model1)

check_collinearity(model1)
resid_panel(model1, plots = "cookd")
model1_aug <- augment(model1)
summary(model1_aug$.std.resid)
summary(subset(model1_aug, likesCount != 1057)$.std.resid)

model1.2 <- lm(likesCount ~ type + Board, data = subset(dta_use, likesCount != 1057))
coeftest(model1.2, vcov = vcovHC(model1.2, type = "HC2"))
summary(model1.2)

model2 <- lm(likesCount ~ Cultural + Board, data = subset(dta_use, likesCount != 1057))
coeftest(model2, vcov = vcovHC(model2, type = "HC2"))
summary(model2)

check_collinearity(model2)
check_autocorrelation(model2)

model3 <- lm(likesCount ~ Political + Board, data = subset(dta_use, likesCount != 1057))
coeftest(model3, vcov = vcovHC(model3, type = "HC2"))
summary(model3)

model4 <- lm(likesCount ~ Social + Board, data = subset(dta_use, likesCount != 1057))
coeftest(model4, vcov = vcovHC(model4, type = "HC2"))
summary(model4)

model5 <- lm(likesCount ~ Board + Cultural + Political + Social + type, data = subset(dta_use, likesCount != 1057))
coeftest(model5, vcov = vcovHC(model5, type = "HC2"))
summary(model5)

resid_panel(model5, plots = "cookd")
check_heteroscedasticity(model5)



robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC2")))
robust_se1.2 <- sqrt(diag(vcovHC(model1.2, type = "HC2")))
robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC2")))
robust_se3 <- sqrt(diag(vcovHC(model3, type = "HC2")))
robust_se4 <- sqrt(diag(vcovHC(model4, type = "HC2")))
robust_se5 <- sqrt(diag(vcovHC(model5, type = "HC2")))



stargazer(model1, model5,
          se = list(robust_se1, robust_se5),
          title = "OLS Regression Board on Likes (Robust SE)",
          dep.var.labels = "Number of Likes",
          covariate.labels = c("Second Board", "Cultural", "Political", "Social", "Sidecar", "Video"),
          omit.stat = c("f", "ser"),
          type = "latex",
          out = "main model.tex")

stargazer(model1.2, model2, model3, model4,
          se = list(robust_se1.2, robust_se2, robust_se3, robust_se4),
          title = "OLS Regression Engagement Type on Likes (Robust SE)",
          dep.var.labels = "Number of Likes",
          covariate.labels = c("Sidecar", "Video", "Cultural", "Political", "Social" ,"Second Board"),
          omit.stat = c("f", "ser"),
          type = "latex",
          out = "types model.tex")

stargazer(model5,
          se = list(robust_se5),
          title = "OLS Regression Board on Likes Without Outliers (Robust SE)",
          dep.var.labels = "Number of Likes",
          covariate.labels = c("Second Board", "Cultural", "Political", "Social", "Sidecar", "Video"),
          omit.stat = c("f", "ser"),
          type = "text")








