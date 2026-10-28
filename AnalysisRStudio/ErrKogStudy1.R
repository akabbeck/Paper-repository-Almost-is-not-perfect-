# clear workspace
rm( list=ls( all=TRUE ) ) 

# set workingdirectory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#install.packages("readxl")
library(readxl)

# Read the Excel file 
data <- read_excel("data_ErrKog_sosci.xlsx",  col_names = TRUE)
data <- data[-1, ]
names(data)[names(data) == "PB03_01"] <- "Code"

columns_to_convert <- data[, 11:254]
numerical_variables <- apply(columns_to_convert, 2, as.numeric)
data[, 11:254] <- numerical_variables

#MPS-F, Perfectionism
#perfectionistic striving: PS Personal Standard + 0 Organization =Perfectionistic Striving, 13 Items
data$PerfektPS <- rowSums(data[, c("MP_04", "MP_06", "MP_12", "MP_16", "MP_19", "MP_24",  "MP_30", "MP_02", "MP_07", "MP_08", "MP_27", "MP_29", "MP_31")], na.rm = TRUE)

#perfectionistic concern: CMD Concern over Mistakes and Doubts, 13 Items 
data$PerfektPC <- rowSums(data[, c("MP_09", "MP_10", "MP_13", "MP_14", "MP_18", "MP_21",  "MP_23", "MP_25", "MP_34",  "MP_17" , "MP_28" , "MP_32" , "MP_33")], na.rm = TRUE)

#EEG-Data
dataRewP <- read_excel("eeg.xlsx",  col_names = TRUE)

#install.packages("reshape2")
library(reshape2)
#long format
longdataRewP <- melt(dataRewP, id = "File", measured = c( "FCz-BC_corr_1_long", "FCz-BC_corr_2_long",  "FCz-BC_corr_3_long",  "FCz-BC_corr_new4_long"))
names(longdataRewP)<-c("participant", "group", "amplitude")

#factors 
#1.factor levels, 2. participants
longdataRewP$outcome<-gl(4, 62, labels = c("1", "2", "3", "4"))

#delete "errkog_"
longdataRewP <- longdataRewP %>% mutate(participant = gsub('errkog', '', participant))

#outliers
mean_amplitude <- mean(longdataRewP$amplitude, na.rm = TRUE)
sd_amplitude <- sd(longdataRewP$amplitude, na.rm = TRUE)
# borders
upper_limit <- mean_amplitude + 2.5 * sd_amplitude
lower_limit <- mean_amplitude - 2.5 * sd_amplitude
# identificate
outliers <- longdataRewP %>% 
  filter(amplitude < lower_limit | amplitude > upper_limit)
# show me
outliers
outlier_participants <- outliers$participant
# delete them
longdataRewP <- longdataRewP %>%filter(!(participant %in% outlier_participants))



#boxplots
#install.packages("ggplot2")
library(ggplot2)

ggplot(longdataRewP, aes(x = outcome, y = amplitude)) +
  geom_boxplot() +
  scale_y_reverse() +
  theme_minimal() +
  labs(title = "",
       x = "response outcome",
       y = "Amplitude")  + 
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 15, face = "bold")
  )


#merge
merged_data <- left_join(longdataRewP, data, by = c("participant" = "Code"))

longdataRewP$PerfektPS <- merged_data$PerfektPS
longdataRewP$PerfektPC <- merged_data$PerfektPC


#distributions
hist(longdataRewP$PerfektPS, probability=TRUE, col="lightblue",  xlab="PerfektPS")
abline(v = mean(longdataRewP$PerfektPS, na.rm=TRUE), col="red", lty=2)
abline(v = mean(longdataRewP$PerfektPS, na.rm=TRUE) + c(-1,1)*sd(longdataRewP$PerfektPS, na.rm=TRUE), col="darkgreen", lty=3)

hist(longdataRewP$PerfektPC, probability=TRUE, col="lightblue",  xlab="PerfektPC")
abline(v = mean(longdataRewP$PerfektPC, na.rm=TRUE), col="red", lty=2)
abline(v = mean(longdataRewP$PerfektPC, na.rm=TRUE) + c(-1,1)*sd(longdataRewP$PerfektPC, na.rm=TRUE), col="darkgreen", lty=3)

#z-stand.
longdataRewP <- longdataRewP %>%mutate(Perfekt_PSz = (PerfektPS - mean(PerfektPS, na.rm = TRUE)) / sd(PerfektPS, na.rm = TRUE))
longdataRewP <- longdataRewP %>%mutate(Perfekt_PCz = (PerfektPC - mean(PerfektPC, na.rm = TRUE)) / sd(PerfektPC, na.rm = TRUE))


#descriptive statistics
#install.packages("pastecs")
library(pastecs)
#Means in microvolt μV
mean(longdataRewP$amplitude)
sd(longdataRewP$amplitude)

#desc traits
vars <- c("Perfekt_PSz", "Perfekt_PCz")
summary_stats <- function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    SD   = sd(x, na.rm = TRUE),
    Min  = min(x, na.rm = TRUE),
    Max  = max(x, na.rm = TRUE)
  )
}
results <- t(sapply(longdataRewP[vars], summary_stats))
results

#describe VP in wide format
var <- data[, c("AP02", "Code", "AP05x01", "AP05x02", "AP05x03", "AP05x04", "AP05x05", "AP05x06", "AP01_01", "AP03", "AP04")]
desc_data <- reshape(longdataRewP, idvar = "participant", timevar = "group", direction = "wide")
desc_data <- left_join(desc_data, var, by = c("participant" = "Code"))

#age
summary(desc_data$AP01_01)
sd(desc_data$AP01_01, na.rm = TRUE)

nrow(desc_data) # number participants
table(desc_data$AP02) # 1=female, 2=male, 3=diverse
table(desc_data$AP05x01) # 2=Ausbildung
table(desc_data$AP05x02) # 2=Studium 
table(desc_data$AP05x03) # 2=Psy Studium 
table(desc_data$AP05x04) # 2=anderes Studium 
table(desc_data$AP05x05) # 2=berufstätig 
table(desc_data$AP05x06) # 2=arbeitssuchend
table(desc_data$AP03) # Händigkeit 5= rechts
table(desc_data$AP04) # höchster Abschluss 5= Abitur

#desc outcome
summary_table <- longdataRewP %>%
  group_by(outcome) %>%
  summarise(
    mean_amplitude = mean(amplitude, na.rm = TRUE),
    sd_amplitude   = sd(amplitude, na.rm = TRUE),
    n              = n()
  )

summary_table


library(ggplot2)
library(dplyr)

#with standart error
sumdat <- longdataRewP %>%
  group_by(outcome) %>%
  summarise(
    mean = mean(amplitude, na.rm = TRUE),
    sd   = sd(amplitude, na.rm = TRUE),
    se   = sd / sqrt(sum(!is.na(amplitude))),
    .groups = "drop"
  )

ggplot(sumdat, aes(x = outcome, y = mean)) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.08,
    linewidth = 0.4
  ) +
  geom_line(
    aes(group = 1),
    linewidth = 0.5
  ) +
  geom_point(
    size = 2.0
  ) +
  scale_y_reverse(
    name = "amplitude (µV)",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  xlab("response condition") +
  theme_classic(base_size = 9) +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(size = 8),
    axis.text.y  = element_text(size = 8),
    axis.line    = element_line(linewidth = 0.5),
    axis.ticks   = element_line(linewidth = 0.4),
    plot.margin  = margin(4, 4, 4, 4, "mm")
  )

#Sirius programms
#ko*p9i+---+-gg

#install.packages("lmerTest")
library(lmerTest)


modelPS_PC <- lmer (amplitude ~ outcome    + Perfekt_PSz + Perfekt_PCz + 
                         outcome:Perfekt_PSz + outcome:Perfekt_PCz + 
                         (1 | participant), data = longdataRewP)
summary(modelPS_PC)
confint(modelPS_PC)
vcov(modelPS_PC)
icc(modelPS_PC)

library(effectsize)
standardize_parameters(modelPS_PC)

#install.packages("emmeans")
library(emmeans)

# Randmittelwerte
summary(contrast(emmeans(modelPS_PC, ~ outcome), method = "pairwise"))
emmeans(modelPS_PC,
        pairwise ~ outcome,
        at = list(Perfekt_PSz = c(0), #or 1
                  Perfekt_PCz  = c(0))) 


p <- emmip(
  modelPS_PC,
  Perfekt_PSz ~ outcome,
  at = list(
    Perfekt_PSz = c(0, 1),
    Perfekt_PCz = 0
  )
)

p +
  scale_y_reverse(
    name = "amplitude (µV)",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  xlab("response condition") +
  theme_classic(base_size = 9) +
  labs(color = "PS") +
  theme(
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(size = 8),
    axis.text.y  = element_text(size = 8),
    axis.line    = element_line(linewidth = 0.5),
    axis.ticks   = element_line(linewidth = 0.4),
    plot.margin  = margin(4, 4, 4, 4, "mm")
  )

#with rondom slope
#modelslope <- lmer(amplitude ~ outcome    + Perfekt_PSz + Perfekt_PCz + outcome:Perfekt_PSz + outcome:Perfekt_PCz + (outcome | participant), data = longdataRewP)
#summary(modelslope)


#install.packages("brms")
#library(brms)
#model_bayes <- brm(amplitude ~ outcome  * logPerfekt_PSz * logPerfekt_PCz + (1 | participant), data = longdataRewP)
#summary(model_bayes)
#plot(model_bayes)
#conditional_effects(model_bayes)

#--------------
#diagnostics
#residual distributions
library(lme4)
library(ggplot2)

# Histogramm
ggplot(data.frame(resid = residuals(modelPS_PC)), aes(resid)) +
  geom_histogram(bins = 30)

#Normally distributed errors
residuals <- resid(modelPS_PC)
qqnorm(residuals)
qqline(residuals, col = "red")
shapiro.test(residuals) #normal

#Homoscedasticity(Residuals vs. Fitted)
library(sjPlot)
plot_model(modelPS_PC, type = "diag")

plot(fitted(modelPS_PC), residuals(modelPS_PC))
abline(h = 0, lty = 2)

#Random-Effects 
ranef(modelPS_PC)
library(lattice)
dotplot(ranef(modelPS_PC, condVar = TRUE))

#Multicollinearity
library(car)
vif(lm(amplitude ~ outcome * Perfekt_PSz * Perfekt_PCz, data = longdataRewP))

#If the average VIF is substantially greater than 1 then the regression may be biased
vif(modelPS_PC)
1/vif(modelPS_PC)
mean(vif(modelPS_PC))

#outliers
library(influence.ME)
infl <- influence(modelPS_PC, obs = TRUE)
plot(infl, which = "cook")
longdataRewP$cooks.distance<-cooks.distance(modelPS_PC) #all <1

#Overfitting
#Marginal & Conditional R²
#install.packages("performance")
library(performance)
#fixed effects (marginal R²), +random effects (conditional R²) explain ... % variance
r2(modelPS_PC)
