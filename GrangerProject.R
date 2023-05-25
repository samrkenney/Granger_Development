# This project intends to address the question of whether changes in one common indicator of economic development may predict changes in other
# economic indicators over time. World Bank time-series data were downloaded for every country in the world from 1980 to 2022
# covering several common indicators of economic development, including: Gross National Income per Capita (PPP-adjusted); Manufacturing,
# value added (% of GDP); and GDP growth (%); among others. After visualizing the data, the Granger Causality test is applied at different orders (i.e. "lag" periods,
# e.g. 1 year, 2 years) to each possible pairing of WB indicators for each country. The ADF and KPSS tests are applied to each  
# country-indicator series to ensure that Granger stationarity requirements are met. Data series that fail the stationarity
# tests are transformed using the difference method and retested for stationarity. Data series that pass both rounds of stationarity testing 
# are analyzed using the Granger Causality test from the lmtest library. The number of successful (p<0.05) Granger Causality tests
# among all countries will form the basis of determining which economic development indicators (if any) are broadly beneficial in predicting
# information about other economic development indicators.

install.packages("lmtest")
install.packages("dplyr")
install.packages("tseries")
library(tseries)
library(lmtest)
library(dplyr)

#===== load tables needed for data analysis

Input_Table <- read.csv("world_bank_indicators.csv", header = TRUE, sep = ",")
Country_List <- unique(Input_Table$COUNTRY)
Econ_Indicators <- c("GDP_GROWTH", "GNI_CAPITA_PPP", "AG_GDP", "INDUSTRY_GDP", "MANUFACTURING_GDP", "GROSS_CAPITAL")

#===== aggregate indicators from Input_Table

Q1 <- function(a.1) {
  quantile(a.1, probs = 0.25)
}

Q3 <- function(a.1) {
  quantile(a.1, probs = 0.75)
}

IQR1 <- function(a.1) {
  (Q3(a.1) - Q1(a.1))*1.5
}

Upper_Fence <- function(a.1) {
  if (Q3(a.1) + IQR1(a.1) > max(a.1)) {
    max(a.1)
  } else {
    Q3(a.1) + IQR1(a.1)
  }
}

Lower_Fence <- function(a.1) {
  if (Q1(a.1) - IQR1(a.1) < min(a.1)) {
    min(a.1)
  } else {
    Q1(a.1) - IQR1(a.1)
  } 
}

avgs_table <- cbind(aggregate(.~YEAR, Input_Table[, -c(1)], mean))
avgs = c("Avg_GDP", "Avg_GNI", "Avg_AG", "Avg_IND", "Avg_MANUF", "Avg_CAP")
for (i in 1:6) names(avgs_table)[names(avgs_table) == Econ_Indicators[i]] = avgs[i]

Q1_table <- cbind(aggregate(.~YEAR, Input_Table[, -c(1)], Q1))
Q1s = c("Q1_GDP", "Q1_GNI", "Q1_AG", "Q1_IND", "Q1_MANUF", "Q1_CAP")
for (i in 1:6) names(Q1_table)[names(Q1_table) == Econ_Indicators[i]] = Q1s[i]

Q3_table <- cbind(aggregate(.~YEAR, Input_Table[, -c(1)], Q3))
Q3s = c("Q3_GDP", "Q3_GNI", "Q3_AG", "Q3_IND", "Q3_MANUF", "Q3_CAP")
for (i in 1:6) names(Q3_table)[names(Q3_table) == Econ_Indicators[i]] = Q3s[i]

LF_table <- cbind(aggregate(.~YEAR, Input_Table[, -c(1)], Lower_Fence))
LFs = c("LF_GDP", "LF_GNI", "LF_AG", "LF_IND", "LF_MANUF", "LF_CAP")
for (i in 1:6) names(LF_table)[names(LF_table) == Econ_Indicators[i]] = LFs[i]

UF_table <- cbind(aggregate(.~YEAR, Input_Table[, -c(1)], Upper_Fence))
UFs = c("UF_GDP", "UF_GNI", "UF_AG", "UF_IND", "UF_MANUF", "UF_CAP")
for (i in 1:6) names(UF_table)[names(UF_table) == Econ_Indicators[i]] = UFs[i]

merge_list <- list(avgs_table, Q1_table, Q3_table, LF_table, UF_table)
viz_table <- Reduce(function(x, y) merge(x, y, all = TRUE), merge_list)

#===== visualize aggregated indicators: GDP

Avg_GDP_Growth <- viz_table$Avg_GDP
Q1_GDP_Growth <- viz_table$Q1_GDP
Q3_GDP_Growth <- viz_table$Q3_GDP
Lower_Fence_GDP_Growth <- viz_table$LF_GDP
Upper_Fence_GDP_Growth <- viz_table$UF_GDP

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_GDP_Growth, type = "l", col = "black", xlab = "Year", ylab = "GDP growth (annual %)", main = "GDP Growth", ylim = c(min(Lower_Fence_GDP_Growth)-1, max(Upper_Fence_GDP_Growth)+1))
lines(x_values, Upper_Fence_GDP_Growth, type = "l", lty = 2, col = "black")
lines(x_values, Q3_GDP_Growth, type = "l", lty = 2, col = "black")
lines(x_values, Q1_GDP_Growth, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_GDP_Growth, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_GDP_Growth, rev(Q1_GDP_Growth)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_GDP_Growth, rev(Q3_GDP_Growth)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_GDP_Growth, rev(Upper_Fence_GDP_Growth)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_GDP_Growth, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_GDP_Growth, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_GDP_Growth, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_GDP_Growth, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_GDP_Growth, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#===== visualize aggregated indicators: GNI Per Capita (PPP)

Avg_GNI <- viz_table$Avg_GNI
Q1_GNI <- viz_table$Q1_GNI
Q3_GNI <- viz_table$Q3_GNI
Lower_Fence_GNI <- viz_table$LF_GNI
Upper_Fence_GNI <- viz_table$UF_GNI

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_GNI, type = "l", col = "black", xlab = "Year", ylab = "GNI", main = "GNI Per Capita (PPP)", ylim = c(min(Lower_Fence_GNI)-1, max(Upper_Fence_GNI)+1))
lines(x_values, Upper_Fence_GNI, type = "l", lty = 2, col = "black")
lines(x_values, Q3_GNI, type = "l", lty = 2, col = "black")
lines(x_values, Q1_GNI, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_GNI, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_GNI, rev(Q1_GNI)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_GNI, rev(Q3_GNI)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_GNI, rev(Upper_Fence_GNI)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_GNI, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_GNI, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_GNI, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_GNI, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_GNI, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#===== visualize aggregated indicators: Agriculture (% of GDP)

Avg_AG <- viz_table$Avg_AG
Q1_AG <- viz_table$Q1_AG
Q3_AG <- viz_table$Q3_AG
Lower_Fence_AG <- viz_table$LF_AG
Upper_Fence_AG <- viz_table$UF_AG

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_AG, type = "l", col = "black", xlab = "Year", ylab = "Agriculture (% of GDP)", main = "Agriculture, forestry, and fishing, value added (% of GDP)", ylim = c(min(Lower_Fence_AG)-1, max(Upper_Fence_AG)+1))
lines(x_values, Upper_Fence_AG, type = "l", lty = 2, col = "black")
lines(x_values, Q3_AG, type = "l", lty = 2, col = "black")
lines(x_values, Q1_AG, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_AG, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_AG, rev(Q1_AG)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_AG, rev(Q3_AG)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_AG, rev(Upper_Fence_AG)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_AG, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_AG, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_AG, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_AG, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_AG, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#===== visualize aggregated indicators: Industry (% of GDP)

Avg_IND <- viz_table$Avg_IND
Q1_IND <- viz_table$Q1_IND
Q3_IND <- viz_table$Q3_IND
Lower_Fence_IND <- viz_table$LF_IND
Upper_Fence_IND <- viz_table$UF_IND

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_IND, type = "l", col = "black", xlab = "Year", ylab = "Industry (% of GDP)", main = "Industry (including construction), value added (% of GDP)", ylim = c(min(Lower_Fence_IND)-1, max(Upper_Fence_IND)+1))
lines(x_values, Upper_Fence_IND, type = "l", lty = 2, col = "black")
lines(x_values, Q3_IND, type = "l", lty = 2, col = "black")
lines(x_values, Q1_IND, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_IND, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_IND, rev(Q1_IND)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_IND, rev(Q3_IND)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_IND, rev(Upper_Fence_IND)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_IND, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_IND, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_IND, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_IND, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_IND, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#===== visualize aggregated indicators: Manufacturing (% of GDP)

Avg_MANUF <- viz_table$Avg_MANUF
Q1_MANUF <- viz_table$Q1_MANUF
Q3_MANUF <- viz_table$Q3_MANUF
Lower_Fence_MANUF <- viz_table$LF_MANUF
Upper_Fence_MANUF <- viz_table$UF_MANUF

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_MANUF, type = "l", col = "black", xlab = "Year", ylab = "Manufacturing (% of GDP)", main = "Manufacturing, value added (% of GDP)", ylim = c(min(Lower_Fence_MANUF)-1, max(Upper_Fence_MANUF)+1))
lines(x_values, Upper_Fence_MANUF, type = "l", lty = 2, col = "black")
lines(x_values, Q3_MANUF, type = "l", lty = 2, col = "black")
lines(x_values, Q1_MANUF, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_MANUF, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_MANUF, rev(Q1_MANUF)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_MANUF, rev(Q3_MANUF)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_MANUF, rev(Upper_Fence_MANUF)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_MANUF, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_MANUF, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_MANUF, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_MANUF, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_MANUF, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#===== visualize aggregated indicators: Gross capital formation (current US$)

Avg_CAP <- viz_table$Avg_CAP
Q1_CAP <- viz_table$Q1_CAP
Q3_CAP <- viz_table$Q3_CAP
Lower_Fence_CAP <- viz_table$LF_CAP
Upper_Fence_CAP <- viz_table$UF_CAP

x_values <- (1990:2021)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x_values, Avg_CAP, type = "l", col = "black", xlab = "Year", ylab = "Gross Capital Formation (USD)", main = "Gross capital formation (current US$)", ylim = c(min(Lower_Fence_CAP)-1, max(Upper_Fence_CAP)+1))
lines(x_values, Upper_Fence_CAP, type = "l", lty = 2, col = "black")
lines(x_values, Q3_CAP, type = "l", lty = 2, col = "black")
lines(x_values, Q1_CAP, type = "l", lty = 2, col = "black")
lines(x_values, Lower_Fence_CAP, type = "l", lty = 2, col = "black")

polygon(c(x_values, rev(x_values)), c(Lower_Fence_CAP, rev(Q1_CAP)), col = "#cbdef5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q1_CAP, rev(Q3_CAP)), col = "#89baf5", lty = 0)
polygon(c(x_values, rev(x_values)), c(Q3_CAP, rev(Upper_Fence_CAP)), col = "#cbdef5", lty = 0)

lines(x_values, Avg_CAP, type = "l", lwd = 3, col = "black")
lines(x_values, Q1_CAP, type = "l", lwd = 2, lty = 2, col = "black")
lines(x_values, Q3_CAP, type = "l", lty = 2, lwd = 2, col = "black")
lines(x_values, Lower_Fence_CAP, type = "l", lty = 2, col = "black")
lines(x_values, Upper_Fence_CAP, type = "l", lty = 2, col = "black")

legend("topright", inset=c(-0.2,0), cex = 0.75, legend = c("Upper Fence", "3rd Quartile", "Mean", "1st Quartile", "Lower Fence"), lty = c(3, 2, 1, 2, 3), lwd = c(1, 2, 3, 2, 1))

#=====Augmented Dickey-Fuller (ADF) test for Stationarity, a requirement for Granger Causality

# H0: time series is non-stationary or has a unit root
# HA: time series is stationary or trend stationary
# significance: p<0.05 to reject H0

ADF_df <- data.frame(COUNTRY = Country_List)

for (i in 3:8) {
  A <- NULL
  for (g in Country_List) {
    B <- na.omit(Input_Table[Input_Table$COUNTRY == g, c(i)])
    if (length(B) == 0 | length(B) == 1) {
      A <- append(A, 1)
    } else {
      A <- append(A, adf.test(B)$p.value)
    }
  }
  ADF_df <- cbind(ADF_df, A)
}

Econ_Indicators1 = append(Econ_Indicators, "x", 0)

for ( i in 2:7) {
  colnames(ADF_df)[i] = Econ_Indicators1[i]
}

#===== Kwiatkowski-Phillips-Schmidt-Shin (KPSS) tests for trend Stationarity, a requirement for performing the Granger Causality test
# H0: time series is trend stationary
# HA: time series is not trend stationary
# significance: p<0.05 to reject H0

KPSS_df <- data.frame(COUNTRY = Country_List)

for (i in 3:8) {
  A <- NULL
  for (g in Country_List) {
    B <- na.omit(Input_Table[Input_Table$COUNTRY == g, c(i)])
    if (length(B) == 0 | length(B) == 1) {
      A <- append(A, 0)
    } else {
      A <- append(A, kpss.test(B)$p.value)
    }
  }
  KPSS_df <- cbind(KPSS_df, A)
}

for (i in 2:7) {
  colnames(KPSS_df)[i] = Econ_Indicators1[i]
}

#===== Identifying country-indicator time series stationarity, comparison of ADP and KPSS
# Case 1: "Stationary" - ADF: p < 0.05 & KPSS: p > 0.05
# Case 2: "Difference Stationary" - ADF: p < 0.05 & KPSS: p < 0.05
# Case 3: "Trend Stationary" - ADF: p > 0.05 & KPSS: p > 0.05
# Case 4: "Unit Root" - ADF: p > 0.05 & KPSS: p < 0.05

ADF_KPSS_df <- data.frame(COUNTRY = Country_List)

Case_Test <- function(x.1, x.2) {
  if (x.1 < 0.05 && x.2 > 0.05) {
    "Case 1"
  } else if (x.1 < 0.05 && x.2 < 0.05) {
    "Case 2"
  } else if (x.1 == 1 | x.2 == 1) {
    "Case 4"
  } else if (x.1 > 0.05 && x.2 > 0.05) {
    "Case 3"
  } else if (x.1 > 0.05 && x.2 < 0.05) {
    "Case 4"
  }
}

for (i in 2:7) {
  X <- NULL
  C <- NULL
  for (g in Country_List) {
    A <- ADF_df[ADF_df$COUNTRY == g, c(i)]
    B <- KPSS_df[KPSS_df$COUNTRY == g, c(i)]
    C <- Case_Test(A, B)
    X <- append(X, C)
  }
  ADF_KPSS_df <- cbind(ADF_KPSS_df, X)
}

for (i in 2:7) {
  colnames(ADF_KPSS_df)[i] = Econ_Indicators1[i]
}

Case_1_count <- sum(ADF_KPSS_df == "Case 1")
Case_2_count <- sum(ADF_KPSS_df == "Case 2")
Case_3_count <- sum(ADF_KPSS_df == "Case 3")
Case_4_count <- sum(ADF_KPSS_df == "Case 4")

#===== Difference method on Case 2 & Case 3

GDP_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$GDP_GROWTH == "Case 4", c(1)]
GNI_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$GNI_CAPITA_PPP == "Case 4", c(1)]
AG_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$AG_GDP == "Case 4", c(1)]
IND_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$INDUSTRY_GDP == "Case 4", c(1)]
MANU_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$MANUFACTURING_GDP == "Case 4", c(1)]
CAP_Case_4 <- ADF_KPSS_df[ADF_KPSS_df$GROSS_CAPITAL == "Case 4", c(1)]

Transition_Table <- Input_Table

for (h in GDP_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(3)] = NA
}

for (h in GNI_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(4)] = NA
}

for (h in AG_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(5)] = NA
}

for (h in IND_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(6)] = NA
}

for (h in MANU_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(7)] = NA
}

for (h in CAP_Case_4) {
  Transition_Table[Transition_Table$COUNTRY == h, c(8)] = NA
}

GDP_Cases <- ADF_KPSS_df[ADF_KPSS_df$GDP_GROWTH == "Case 1" | ADF_KPSS_df$GDP_GROWTH == "Case 2" | ADF_KPSS_df$GDP_GROWTH == "Case 3", c(1)]
GNI_Cases <- ADF_KPSS_df[ADF_KPSS_df$GNI_CAPITA_PPP == "Case 1" | ADF_KPSS_df$GNI_CAPITA_PPP == "Case 2" | ADF_KPSS_df$GNI_CAPITA_PPP == "Case 3", c(1)]
AG_Cases <- ADF_KPSS_df[ADF_KPSS_df$AG_GDP == "Case 1" | ADF_KPSS_df$AG_GDP == "Case 2" | ADF_KPSS_df$AG_GDP == "Case 3", c(1)]
IND_Cases <- ADF_KPSS_df[ADF_KPSS_df$INDUSTRY_GDP == "Case 1" | ADF_KPSS_df$INDUSTRY_GDP == "Case 2" | ADF_KPSS_df$INDUSTRY_GDP == "Case 3", c(1)]
MANU_Cases <- ADF_KPSS_df[ADF_KPSS_df$MANUFACTURING_GDP == "Case 1" | ADF_KPSS_df$MANUFACTURING_GDP == "Case 2" | ADF_KPSS_df$MANUFACTURING_GDP == "Case 3", c(1)]
CAP_Cases <- ADF_KPSS_df[ADF_KPSS_df$GROSS_CAPITAL == "Case 1" | ADF_KPSS_df$GROSS_CAPITAL == "Case 2" | ADF_KPSS_df$GROSS_CAPITAL == "Case 3", c(1)]

for (h in GDP_Cases) {
    U <- Transition_Table[Transition_Table$COUNTRY == h, c(3)]
    A <- diff(U)
    A <- append(A, NA)
    Transition_Table[Transition_Table$COUNTRY == h, c(3)] = A
  }

for (h in GNI_Cases) {
  U <- Transition_Table[Transition_Table$COUNTRY == h, c(4)]
  A <- diff(U)
  A <- append(A, NA)
  Transition_Table[Transition_Table$COUNTRY == h, c(4)] = A
}

for (h in AG_Cases) {
  U <- Transition_Table[Transition_Table$COUNTRY == h, c(5)]
  A <- diff(U)
  A <- append(A, NA)
  Transition_Table[Transition_Table$COUNTRY == h, c(5)] = A
}

for (h in IND_Cases) {
  U <- Transition_Table[Transition_Table$COUNTRY == h, c(6)]
  A <- diff(U)
  A <- append(A, NA)
  Transition_Table[Transition_Table$COUNTRY == h, c(6)] = A
}

for (h in MANU_Cases) {
  U <- Transition_Table[Transition_Table$COUNTRY == h, c(7)]
  A <- diff(U)
  A <- append(A, NA)
  Transition_Table[Transition_Table$COUNTRY == h, c(7)] = A
}

for (h in CAP_Cases) {
  U <- Transition_Table[Transition_Table$COUNTRY == h, c(8)]
  A <- diff(U)
  A <- append(A, NA)
  Transition_Table[Transition_Table$COUNTRY == h, c(8)] = A
}

#===== Second ADF Test (Transition_Table)

ADF_df_2 <- data.frame(COUNTRY = Country_List)

for (i in 3:8) {
  A <- NULL
  for (g in Country_List) {
    B <- na.omit(Transition_Table[Transition_Table$COUNTRY == g, c(i)])
    if (length(B) == 0 | length(B) == 1) {
      A <- append(A, 1)
    } else {
      A <- append(A, adf.test(B)$p.value)
    }
  }
  ADF_df_2 <- cbind(ADF_df_2, A)
}

for ( i in 2:7) {
  colnames(ADF_df_2)[i] = Econ_Indicators1[i]
}

#===== Second KPSS test on (Transition_Table)

KPSS_df_2 <- data.frame(COUNTRY = Country_List)

for (i in 3:8) {
  A <- NULL
  for (g in Country_List) {
    B <- na.omit(Transition_Table[Transition_Table$COUNTRY == g, c(i)])
    if (length(B) == 0 | length(B) == 1) {
      A <- append(A, 0)
    } else {
      A <- append(A, kpss.test(B)$p.value)
    }
  }
  KPSS_df_2 <- cbind(KPSS_df_2, A)
}

for (i in 2:7) {
  colnames(KPSS_df_2)[i] = Econ_Indicators1[i]
}

#====Throw out all values that don't pass (ADF p < 0.05 & KPSS p > 0.05) from "Transition_Table"
# Resulting in "Transformed_Table"

ADF_KPSS_df_2 <- data.frame(COUNTRY = Country_List)

Case_Test_2 <- function(x.1, x.2) {
  if (x.1 < 0.05 && x.2 > 0.05) {
    "Case 1"
  } else "Case 4"
}

for (i in 2:7) {
  X <- NULL
  C <- NULL
  for (g in Country_List) {
    A <- ADF_df_2[ADF_df_2$COUNTRY == g, c(i)]
    B <- KPSS_df_2[KPSS_df_2$COUNTRY == g, c(i)]
    C <- Case_Test_2(A, B)
    X <- append(X, C)
  }
  ADF_KPSS_df_2 <- cbind(ADF_KPSS_df_2, X)
}

Transformed_Table <- Transition_Table

GDP_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$GDP_GROWTH == "Case 4", c(1)]
GNI_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$GNI_CAPITA_PPP == "Case 4", c(1)]
AG_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$AG_GDP == "Case 4", c(1)]
IND_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$INDUSTRY_GDP == "Case 4", c(1)]
MANU_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$MANUFACTURING_GDP == "Case 4", c(1)]
CAP_Case_4 <- ADF_KPSS_df_2[ADF_KPSS_df_2$GROSS_CAPITAL == "Case 4", c(1)]


for (h in GDP_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(3)] = NA
}

for (h in GNI_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(4)] = NA
}

for (h in AG_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(5)] = NA
}

for (h in IND_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(6)] = NA
}

for (h in MANU_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(7)] = NA
}

for (h in CAP_Case_4) {
  Transformed_Table[Transformed_Table$COUNTRY == h, c(8)] = NA
}

GDP_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$GDP_GROWTH == "Case 1", c(1)]
GNI_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$GNI_CAPITA_PPP == "Case 1", c(1)]
AG_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$AG_GDP == "Case 1", c(1)]
IND_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$INDUSTRY_GDP == "Case 1", c(1)]
MANU_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$MANUFACTURING_GDP == "Case 1", c(1)]
CAP_Cases <- ADF_KPSS_df_2[ADF_KPSS_df_2$GROSS_CAPITAL == "Case 1", c(1)]

for (h in GDP_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(3)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(3)] = A
}

for (h in GNI_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(4)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(4)] = A
}

for (h in AG_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(5)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(5)] = A
}

for (h in IND_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(6)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(6)] = A
}

for (h in MANU_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(7)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(7)] = A
}

for (h in CAP_Cases) {
  U <- Transformed_Table[Transformed_Table$COUNTRY == h, c(8)]
  A <- diff(U)
  A <- append(A, NA)
  Transformed_Table[Transformed_Table$COUNTRY == h, c(8)] = A
}

Trans_Table_NAs <- as.numeric(paste0(table(is.na(Transformed_Table)))[2])
Trans_Table <- as.numeric(paste0(table(is.na(Transformed_Table)))[1])

#===== Granger analysis on Transform_Table
Q <- NULL
COUNTRIES <- NULL
Lag_ <- NULL

for (q in Country_List) {
  Q <- c(q, q, q, q, q)
  COUNTRIES <- c(COUNTRIES, Q)
}

for (w in 1:length(Country_List)) {
  W <- c(1, 2, 3, 4, 5)
  Lag_ <- c(Lag_, W)
}

Output_Table <- data.frame(COUNTRIES, Lag_)

granger_test <- function(xx, yy, zz, XX) {
  tryCatch(
    {
      aa = grangertest(xx ~ yy, order = zz, data = XX)
      return(aa)
    },
    error = function(e) {
      return(1)
    }
  )
}

granger_fail <- NULL

for (i in 3:8) {
  J <- c(3:8)[-(i-2)]
  for (j in J) {
    A <- NULL
    B <- NULL
    C <- NULL
    granger_result = NULL
    for (x in Country_List) {
      O <- NULL
      A <- Transformed_Table[Transformed_Table$COUNTRY == x, c(i)]
      B <- Transformed_Table[Transformed_Table$COUNTRY == x, c(j)]
      if (length(na.omit(A)) == 0 | length(na.omit(B)) == 0) {
        O <- c(1, 1, 1, 1, 1)
        granger_fail <- append(granger_fail, 1)
      } else {
        L <- length(A)
        for (p in 1:L) {
          if (is.na(A[p]) | is.na(B[p])) {
            A <- A[c(-p)]
            B <- B[c(-p)]
          }
        }
        m <- matrix(c(A, B), ncol = 2)
        myts <- ts(m, start = 1, frequency = 1)
        for (k in 1:5) {
          granger_result <- granger_test(A, B, k, myts)
          if ("numeric" %in% class(granger_result)) {
            D <- 1
            granger_fail <- append(granger_fail, 1)
          } else {
            D <- granger_result["Pr(>F)"][2,]
          }
          O <- append(O, D)
        }
      }
      C <- c(C, O)
      C <- ifelse(is.nan(C), 1, C)
    }
    Output_Table <- cbind(Output_Table, C)
  }
}

granger_fail <- sum(granger_fail)

Columns <- NULL

for (x in 1:6) {
  Y <- c(1:6)[-x] 
  for (y in Y) {
    bb <- Econ_Indicators[x]
    qq <- Econ_Indicators[y]
    Columns <- append(Columns, paste(bb, "~", qq, sep = " "))
  }
}

for (i in 1:length(Columns)) {
  colnames(Output_Table)[i+2] = Columns[i]
}

# ==== Summarize Output_Table
# Order1 Table
Order1 <- Econ_Indicators
Table1 <- data.frame(Order1)
A <- NULL
B <- NULL
C <- NA

for (i in 3:7) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
}
Table1 <- cbind(Table1, C)

C <- NULL
for (i in 8:12) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 8) {
    C <- append(C, NA)
  }
}
Table1 <- cbind(Table1, C)

C <- NULL
for (i in 13:17) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 14) {
    C <- append(C, NA)
  }
}
Table1 <- cbind(Table1, C)

C <- NULL
for (i in 18:22) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 20) {
    C <- append(C, NA)
  }
}
Table1 <- cbind(Table1, C)

C <- NULL
for (i in 23:27) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 26) {
    C <- append(C, NA)
  }
}
Table1 <- cbind(Table1, C)

C <- NULL
for (i in 28:32) {
  A <- Output_Table[Output_Table$Lag_ == 1, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 32) {
    C <- append(C, NA)
  }
}
Table1 <- cbind(Table1, C)
for (i in 1:length(Econ_Indicators)) {
  colnames(Table1)[i+1] = Econ_Indicators[i]
}

# Order2 Table
Order2 <- Econ_Indicators
Table2 <- data.frame(Order2)
A <- NULL
B <- NULL
C <- NA

for (i in 3:7) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
}
Table2 <- cbind(Table2, C)

C <- NULL
for (i in 8:12) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 8) {
    C <- append(C, NA)
  }
}
Table2 <- cbind(Table2, C)

C <- NULL
for (i in 13:17) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 14) {
    C <- append(C, NA)
  }
}
Table2 <- cbind(Table2, C)

C <- NULL
for (i in 18:22) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 20) {
    C <- append(C, NA)
  }
}
Table2 <- cbind(Table2, C)

C <- NULL
for (i in 23:27) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 26) {
    C <- append(C, NA)
  }
}
Table2 <- cbind(Table2, C)

C <- NULL
for (i in 28:32) {
  A <- Output_Table[Output_Table$Lag_ == 2, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 32) {
    C <- append(C, NA)
  }
}
Table2 <- cbind(Table2, C)
for (i in 1:length(Econ_Indicators)) {
  colnames(Table2)[i+1] = Econ_Indicators[i]
}

# Order3 Table
Order3 <- Econ_Indicators
Table3 <- data.frame(Order3)
A <- NULL
B <- NULL
C <- NA

for (i in 3:7) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
}
Table3 <- cbind(Table3, C)

C <- NULL
for (i in 8:12) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 8) {
    C <- append(C, NA)
  }
}
Table3 <- cbind(Table3, C)

C <- NULL
for (i in 13:17) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 14) {
    C <- append(C, NA)
  }
}
Table3 <- cbind(Table3, C)

C <- NULL
for (i in 18:22) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 20) {
    C <- append(C, NA)
  }
}
Table3 <- cbind(Table3, C)

C <- NULL
for (i in 23:27) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 26) {
    C <- append(C, NA)
  }
}
Table3 <- cbind(Table3, C)

C <- NULL
for (i in 28:32) {
  A <- Output_Table[Output_Table$Lag_ == 3, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 32) {
    C <- append(C, NA)
  }
}
Table3 <- cbind(Table3, C)
for (i in 1:length(Econ_Indicators)) {
  colnames(Table3)[i+1] = Econ_Indicators[i]
}

# Order4 Table
Order4 <- Econ_Indicators
Table4 <- data.frame(Order4)
A <- NULL
B <- NULL
C <- NA

for (i in 3:7) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
}
Table4 <- cbind(Table4, C)

C <- NULL
for (i in 8:12) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 8) {
    C <- append(C, NA)
  }
}
Table4 <- cbind(Table4, C)

C <- NULL
for (i in 13:17) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 14) {
    C <- append(C, NA)
  }
}
Table4 <- cbind(Table4, C)

C <- NULL
for (i in 18:22) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 20) {
    C <- append(C, NA)
  }
}
Table4 <- cbind(Table4, C)

C <- NULL
for (i in 23:27) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 26) {
    C <- append(C, NA)
  }
}
Table4 <- cbind(Table4, C)

C <- NULL
for (i in 28:32) {
  A <- Output_Table[Output_Table$Lag_ == 4, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 32) {
    C <- append(C, NA)
  }
}
Table4 <- cbind(Table4, C)
for (i in 1:length(Econ_Indicators)) {
  colnames(Table4)[i+1] = Econ_Indicators[i]
}

# Order5 Table
Order5 <- Econ_Indicators
Table5 <- data.frame(Order5)
A <- NULL
B <- NULL
C <- NA

for (i in 3:7) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
}
Table5 <- cbind(Table5, C)

C <- NULL
for (i in 8:12) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 8) {
    C <- append(C, NA)
  }
}
Table5 <- cbind(Table5, C)

C <- NULL
for (i in 13:17) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 14) {
    C <- append(C, NA)
  }
}
Table5 <- cbind(Table5, C)

C <- NULL
for (i in 18:22) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 20) {
    C <- append(C, NA)
  }
}
Table5 <- cbind(Table5, C)

C <- NULL
for (i in 23:27) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 26) {
    C <- append(C, NA)
  }
}
Table5 <- cbind(Table5, C)

C <- NULL
for (i in 28:32) {
  A <- Output_Table[Output_Table$Lag_ == 5, c(i)]
  B <- sum(A <= 0.05)
  C <- append(C, B)
  if (i == 32) {
    C <- append(C, NA)
  }
}
Table5 <- cbind(Table5, C)
for (i in 1:length(Econ_Indicators)) {
  colnames(Table5)[i+1] = Econ_Indicators[i]
}

# Calculate total number of Granger successes

granger_sum <- NULL
for (i in 2:7) {
  granger_sum <- append(granger_sum, sum(na.omit(Table1[i])))
}

for (i in 2:7) {
  granger_sum <- append(granger_sum, sum(na.omit(Table2[i])))
}

for (i in 2:7) {
  granger_sum <- append(granger_sum, sum(na.omit(Table3[i])))
}

for (i in 2:7) {
  granger_sum <- append(granger_sum, sum(na.omit(Table4[i])))
}

for (i in 2:7) {
  granger_sum <- append(granger_sum, sum(na.omit(Table5[i])))
}

granger_sum <- sum(granger_sum)

# Print summary of Granger analyses

summary_funct <- function(aa, bb, cc, dd, ee, ff, gg, hh) {
  print("World Bank csv file download covered 217 countries, 6 economic development indicators, and 43 years (1980-2022).")
  print("Out of a total possible 55,986 data points, actions were taken to eliminate NULL/empty values, resulting in 199 countries")
  print("over 1990-2021: 1,194 data series or 37,554 data points, of which 3,757 (10%) are NULL/empty and 71 are imputed (0.1%).")
  print("See metadata & data cleaning Excel file for more detail.")
  cat("\n")
  print(paste0(aa, " data series passed ADF & KPSS tests. ", bb + cc, " partially passed the tests and require further transformation,"))
  print(paste0("and ", dd, " did not pass either test."))
  print(paste0("Following transformation via difference method and re-testing, the resulting dataframe included ", ee, " NAs and"))
  print(paste0(ff, " non-null values."))
  cat("\n")
  print(paste0("The Granger Test was executed ", 30845 - gg, " times, and skipped (due to data series failing to meet requirements) ", gg, " times"))
  print(paste0("Of the ", 30845 - gg, " Granger tests, ", hh, " (1.4%) were successful (p<0.05), as represented below by Order#."))
}

summary_funct(Case_1_count,
              Case_2_count,
              Case_3_count,
              Case_4_count,
              Trans_Table_NAs,
              Trans_Table,
              granger_fail,
              granger_sum)
Table1
Table2
Table3
Table4
Table5
