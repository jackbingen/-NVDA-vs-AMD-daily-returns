# Load required libraries
library(dplyr)
library(ggplot2)

# -------------------------
# 1. Load and clean NVIDIA data
nvda <- read.csv("C:/Users/jacki/Downloads/NVIDIA_Stock_Price_History.csv")
nvda$Date <- as.Date(nvda$Date, format="%m/%d/%Y")
nvda$DailyReturn <- c(NA, diff(nvda$Price)/head(nvda$Price,-1))
nvda$Ticker <- "NVIDIA"

# 2. Load and clean AMD data
amd <- read.csv("C:/Users/jacki/Downloads/AMD Stock Price History.csv")
amd$Date <- as.Date(amd$Date, format="%m/%d/%Y")
amd$DailyReturn <- c(NA, diff(amd$Price)/head(amd$Price,-1))
amd$Ticker <- "AMD"

# -------------------------
# 3. Merge datasets on common dates
stocks <- inner_join(
  nvda[, c("Date","Price","DailyReturn")],
  amd[, c("Date","Price","DailyReturn")],
  by="Date",
  suffix=c(".NVDA",".AMD")
)

# -------------------------
# 4. Plot stock prices together
ggplot() +
  geom_line(data=nvda, aes(x=Date, y=Price, color="NVIDIA")) +
  geom_line(data=amd, aes(x=Date, y=Price, color="AMD")) +
  labs(title="NVIDIA vs AMD Stock Prices",
       x="Date", y="Stock Price (USD)") +
  scale_color_manual(values=c("NVIDIA"="blue","AMD"="red")) +
  theme_minimal()

# -------------------------
# 5. Correlation of daily returns
correlation <- cor(stocks$DailyReturn.NVDA, stocks$DailyReturn.AMD, use="complete.obs")
cat("Correlation between NVIDIA and AMD daily returns:", correlation, "\n")

# -------------------------
# 6. Scatter plot of daily returns
ggplot(stocks, aes(x=DailyReturn.NVDA, y=DailyReturn.AMD)) +
  geom_point(alpha=0.5, color="purple") +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="NVIDIA vs AMD Daily Returns",
       x="NVIDIA Daily Return",
       y="AMD Daily Return") +
  theme_minimal()

# -------------------------
# 7. Histograms of daily returns
# Combine into a single dataframe for plotting
returns_combined <- rbind(
  nvda[, c("DailyReturn", "Ticker")],
  amd[, c("DailyReturn", "Ticker")]
)

# Plot histograms side by side
ggplot(returns_combined, aes(x=DailyReturn, fill=Ticker)) +
  geom_histogram(bins=50, alpha=0.6, position="identity", color="black") +
  labs(title="Histogram of Daily Returns: NVIDIA vs AMD",
       x="Daily Return",
       y="Frequency") +
  scale_fill_manual(values=c("NVIDIA"="blue","AMD"="red")) +
  theme_minimal()
