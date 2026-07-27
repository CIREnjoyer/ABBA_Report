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
library(emmeans)

setwd("C:/Users/skots/Desktop/Нова папка/ABBA/Report 2")

##### Event Data #####

#event <- read_excel("https://github.com/CIREnjoyer/ABBA_Report/raw/main/Report_2/Event.xlsx")
event <- read_xlsx("Event.xlsx")

event <- event |>
  mutate(board = case_when(
    acyear == "2022/2023" | acyear == "2023/2024" ~ 1,
    acyear == "2024/2025" ~ 2,
    acyear == "2025/2026" ~ 3),
    date = as.Date(date),
    attnum = if_else(is.na(attnum) & board == 1, floor(median(subset(event, board == 1)$attnum, na.rm = T)), attnum),
    attnum = if_else(is.na(attnum) & board == 2, floor(median(subset(event, board == 2)$attnum, na.rm = T)), attnum),
    attnum = if_else(is.na(attnum) & board == 3, floor(median(subset(event, board == 3)$attnum, na.rm = T)), attnum),
    inext = case_when(
      inext == "Semi-internal" ~ "Internal",
      .default = inext
    )
  )

event$board <- factor(event$board)
event$dprtm <- factor(event$dprtm)
event$board1 <- factor(ifelse(event$acyear == "2022/2023" | event$acyear == "2023/2024", 1, 0))
event$board2 <- factor(ifelse(event$acyear == "2024/2025", 1, 0))
event$board3 <- factor(ifelse(event$acyear == "2025/2026", 1, 0))
event$fee_f <- factor(ifelse(is.na(event$fee), 0, 1))
event$fee <- ifelse(is.na(event$fee), 0, event$fee)

##### Descriptives

ggplot(event, aes(date, attnum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  theme_minimal(20) +
  labs(x = "Date", y = "Sign-Ups")

#ggsave("trend.pdf",
 #      width = 12,
  #     height = 8)

ggplot(event, aes(attnum)) + 
  geom_histogram(binwidth = 15,
                 fill = "lightblue",
                 colour = "black") +
  labs(x = "Sign-Ups", y = "Count") +
  theme_minimal()

ggplot(subset(event, board1 == 1), aes(attnum)) + 
  geom_histogram(binwidth = 15,
                 fill = "lightblue",
                 colour = "black") +
  labs(x = "Sign-Ups", y = "Count") +
  theme_minimal()

ggplot(subset(event, board2 == 1), aes(attnum)) + 
  geom_histogram(binwidth = 15,
                 fill = "lightblue",
                 colour = "black") +
  labs(x = "Sign-Ups", y = "Count") +
  theme_minimal()

ggplot(subset(event, board3 == 1), aes(attnum)) + 
  geom_histogram(binwidth = 10,
                 fill = "lightblue",
                 colour = "black") +
  labs(x = "Sign-Ups", y = "Count") +
  theme_minimal()

ggplot(event, aes(board)) +
  geom_bar(width = 0.5,
           fill = "darkred",
           colour = "black") +
  scale_y_continuous(breaks = seq(0,50, by = 4)) +
  labs(x = "Board", y = "Count") +
  theme_minimal(20)

#ggsave("boardeve.pdf",
 #      width = 12,
  #     height = 8)

ggplot(event, aes(board, attnum)) +
  geom_col(width = 0.5,
           fill = "darkred") +
  scale_y_continuous() +
  labs(x = "Board", y = "Sign-Ups") +
  theme_minimal(20)

#ggsave("boardatt.pdf",
 #       width = 12,
  #     height = 8)

ggplot(event, aes(dprtm, fill = board)) +
  geom_bar(position = "dodge", colour = "black") +
  scale_fill_manual(values = c("darkblue", "darkred", "darkgreen")) +
  scale_y_continuous(breaks = seq(0,25, by = 2)) +
  labs(x = "Department", y = "Count") +
  theme_minimal(20)

#ggsave("depeve.pdf",
      # width = 12,
      # height = 8)

ggplot(event, aes(dprtm, attnum, fill = board)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkblue", "darkred", "darkgreen")) +
  scale_y_continuous() +
  labs(x = "Department", y = "Sign-Ups") +
  theme_minimal(20)

#ggsave("depatt.pdf",
 #      width = 12,
  #     height = 8)

##### Models

date_n <- as.numeric(event$date) / 30.44

model_tr <- lm(attnum ~ date_n, data = event)
summary(model_tr)

stargazer(model_tr,
          type = "latex",
          dep.var.labels = "Sign-Ups",
          covariate.labels = c("Date"),
          star.cutoffs = c(0.05, 0.01, 0.001))



model1 <- lm(attnum ~ board, data = event)
summary(model1)

resid_panel(model1, plots = "resid")



model2 <- lm(attnum ~ dprtm + board + factor(inext) + factor(collab) + fee_f, data = event)
summary(model2)

check_heteroskedasticity(model2)
resid_panel(model2, plots = "resid")
resid_panel(model2, plots = "cookd")
check_collinearity(model2)

SE <- coeftest(model2, vcov = vcovHC(model2, type = "HC3"))[, "Std. Error"]

stargazer(model2, type = "latex",
          se = list(SE),
          dep.var.labels = "Sign-Ups",
          covariate.labels = c("Culture", "Other", "Social", "Ukraine", "Second Board", "Third Board", "Internal Event", "Collab - Yes", "Fee - Yes"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "OLS Regression with robust SE (HC3). The reference groups are: First Board, Academia, No collab, No fee")

model3 <- lm(attnum ~ dprtm*board + factor(inext) + factor(collab) + fee_f, data = subset(event, attnum < 350))
summary(model3)

check_heteroskedasticity(model3)
resid_panel(model3, plots = "resid")
resid_panel(model3, plots = "cookd")

SE <- coeftest(model3, vcov = vcovHC(model3, type = "HC3"))[, "Std. Error"]

stargazer(model3, type = "text",
          se = list(SE),
          dep.var.labels = "Sign-Ups",
          covariate.labels = c("Culture", "Other", "Social", "Ukraine", "Second Board", "Third Board", "Internal Event", "Collab - Yes", "Fee - Yes", "Second Board * Culture", "Second Board * Other", "Second Board * Social", "Second Board * Ukraine", "Third Board * Culture", "Third Board * Other", "Third Board * Social", "Third Board * Ukraine"),
          star.cutoffs = c(0.05, 0.01, 0.001))


model_f <- lm(attnum ~ fee, data = event)
summary(model_f)

resid_panel(model_f, plots = "resid")
check_heteroskedasticity(model_f)

##### Insta Data #####

insta1 <- read.csv("https://raw.githubusercontent.com/CIREnjoyer/ABBA_Report/refs/heads/main/Report_2/insta.csv")

insta1 <- insta[, -1]
insta1$date <- as.Date(insta1$date)
insta <- filter(insta1, likesCount > 0)

##### Descriptives 

ggplot(insta, aes(date, likesCount)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(ylim = c(0, 500)) +
  scale_x_date(date_breaks = "6 months") +
  theme_minimal(20) +
  labs(x = "Date", y = "Likes")

#ggsave("liketrend.pdf",
 #    width = 12,
  #    height = 8)

ggplot(subset(insta, Activism == 0), aes(as.factor(Board))) +
  geom_bar(fill = "darkred",
           width = 0.5) +
  theme_minimal(20) +
  labs(x = "Board", y = "Count")

#ggsave("boardcount.pdf",
 #      width = 12,
  #     height = 8)

ggplot(subset(insta, Activism == 0), aes(as.factor(Board), likesCount)) +
  geom_col(width = 0.5,
           fill = "darkred") +
  labs(x = "Board", y = "Likes") +
  theme_minimal(20) +
  scale_y_continuous(breaks = seq(0, 13000, by = 500))

#ggsave("boardlikes.pdf",
 #      width = 12,
  #     height = 8)

ggplot(subset(insta, Activism == 0), aes (as.factor(EngagementType), fill = as.factor(Board))) +
  geom_bar(position = "dodge",
           colour = "black") +
  scale_fill_manual(values = c("darkred", "darkblue", "darkgreen")) +
  labs(x = "Theme", y = "Count", fill = "Board") +
  theme_minimal(20)

#ggsave("themecount.pdf",
 #      width = 12,
  #     height = 8)

ggplot(subset(insta, Activism == 0 & likesCount < 1000), aes(as.factor(EngagementType), likesCount, fill = as.factor(Board))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkred", "darkblue", "darkgreen")) +
  labs(x = "Theme", y = "Likes", fill = "Board") + 
  theme_minimal(20)

#ggsave("themelikes.pdf",
 #      width = 12,
  #     height = 8)

ggplot(subset(insta, is.na(attnum) == F), aes(likesCount, attnum)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  theme_minimal(20) + 
  labs(x = "Likes", y = "Sign-Ups")

#ggsave("attbylikes.pdf",
 #      width = 12,
  #     height = 8)

##### Models

insta$date_n <- as.numeric(insta$date) / 30.44 

model <- lm(likesCount ~ date_n, data = subset(insta, Activism == 0))
summary(model)


model1 <- lm(likesCount ~ Board2 + Board3 + Political + Cultural + Workshop + Video + Carrousel, data = subset(insta, Activism == 0 & likesCount < 1000))
summary(model1)

resid_panel(model1, plots = "cookd")
check_heteroskedasticity(model1)
check_collinearity(model1)

SE <- coeftest(model1, vcov = vcovHC(model1, type = "HC3"))[, "Std. Error"]

stargazer(model1,
          type = "text",
          se = list(SE),
          star.cutoffs = c(0.05, 0.01, 0.001))





