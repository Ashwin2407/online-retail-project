# ============================================
# E-COMMERCE SALES DATA ANALYSIS
# ============================================

# --- Load required libraries ---
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(readr)

# --- Load dataset ---
raw_data <- read.csv("OnlineRetail.csv")

# --- Initial exploration ---
head(raw_data)
str(raw_data)
glimpse(raw_data)
colnames(raw_data)
summary(raw_data)
colSums(is.na(raw_data))  # Check missing values

# ============================================
# DATA CLEANING
# ============================================

data_clean <- raw_data %>%
  filter(!is.na(CustomerID), !is.na(Description)) %>%
  filter(Quantity > 0, UnitPrice > 0) %>%
  mutate(
    InvoiceDate = dmy_hm(InvoiceDate),
    TotalPrice = Quantity * UnitPrice
  )

# --- Summary statistics ---
summary(data_clean$Quantity)
summary(data_clean$UnitPrice)
summary(data_clean$TotalPrice)

# --- Unique counts ---
n_distinct(data_clean$CustomerID)
n_distinct(data_clean$Description)
n_distinct(data_clean$InvoiceNo)
n_distinct(data_clean$Country)

# ============================================
# PRODUCT & SALES ANALYSIS
# ============================================

# --- Top 10 Most Frequently Sold Products ---
data_clean %>%
  group_by(Description) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalQuantity)) %>%
  head(10)

# --- Top 10 Products by Revenue ---
data_clean %>%
  group_by(Description) %>%
  summarise(Revenue = sum(TotalPrice)) %>%
  arrange(desc(Revenue)) %>%
  head(10)

# --- Top 10 Customers by Spending ---
data_clean %>%
  group_by(CustomerID) %>%
  summarise(TotalSpent = sum(TotalPrice)) %>%
  arrange(desc(TotalSpent)) %>%
  head(10)

# --- Invoice Date Range ---
min(data_clean$InvoiceDate)
max(data_clean$InvoiceDate)

# --- Bar Chart: Top 5 Products ---
data_clean %>%
  group_by(Description) %>%
  summarise(TotalSold = sum(Quantity)) %>%
  arrange(desc(TotalSold)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(Description, TotalSold), y = TotalSold)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 5 Selling Products", x = "Product", y = "Quantity Sold") +
  theme_minimal()
ggsave("top_5_products.png", width = 8, height = 6)

# --- Bar Chart: Revenue by Country (Top 10) ---
data_clean %>%
  group_by(Country) %>%
  summarise(Revenue = sum(TotalPrice)) %>%
  arrange(desc(Revenue)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Country, Revenue), y = Revenue)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Countries by Revenue", x = "Country", y = "Revenue") +
  theme_minimal()
ggsave("top_10_revenue_countries.png", width = 8, height = 6)

# ============================================
# TIME SERIES ANALYSIS
# ============================================

data_clean <- data_clean %>% mutate(Month = floor_date(InvoiceDate, "month"))

monthly_revenue <- data_clean %>%
  group_by(Month) %>%
  summarise(MonthlyRevenue = sum(TotalPrice, na.rm = TRUE))

ggplot(monthly_revenue, aes(x = Month, y = MonthlyRevenue)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "red") +
  scale_y_continuous(labels = comma) +
  labs(title = "Monthly Revenue Trend", x = "Month", y = "Revenue") +
  theme_minimal()
ggsave("monthly_revenue_trend.png", width = 8, height = 6)

# ============================================
# CUSTOMER ANALYSIS
# ============================================

top_customers <- data_clean %>%
  group_by(CustomerID) %>%
  summarise(TotalRevenue = sum(TotalPrice)) %>%
  arrange(desc(TotalRevenue)) %>%
  slice_max(TotalRevenue, n = 10)

ggplot(top_customers, aes(x = reorder(as.factor(CustomerID), TotalRevenue), y = TotalRevenue)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Customers by Revenue", x = "Customer ID", y = "Revenue (£)") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()
ggsave("top_10_customers_by_revenue.png", width = 8, height = 6)

# ============================================
# CANCELLATION ANALYSIS
# ============================================

cancelled_orders <- raw_data %>% filter(Quantity < 0)

cancel_summary <- cancelled_orders %>%
  mutate(LostRevenue = Quantity * UnitPrice) %>%
  summarise(
    CancelledOrders = n(),
    TotalLostRevenue = sum(LostRevenue)
  )
print(cancel_summary)

# --- Top 10 Countries with Cancelled Orders ---
cancel_by_country <- cancelled_orders %>%
  group_by(Country) %>%
  summarise(CancelledOrders = n()) %>%
  slice_max(order_by = CancelledOrders, n = 10)

ggplot(cancel_by_country, aes(x = reorder(Country, CancelledOrders), y = CancelledOrders)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Countries with Cancelled Orders", x = "Country", y = "Number of Cancellations") +
  theme_minimal()
ggsave("cancelled_orders_by_country.png", width = 8, height = 6)

# --- Pie Chart: Transaction Types ---
raw_data <- raw_data %>%
  mutate(TransactionType = ifelse(Quantity > 0, "Valid", "Cancelled"))

transaction_summary <- raw_data %>%
  group_by(TransactionType) %>%
  summarise(Count = n())

ggplot(transaction_summary, aes(x = "", y = Count, fill = TransactionType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Transaction Types", fill = "Transaction Type") +
  theme_void() +
  scale_fill_manual(values = c("Valid" = "seagreen3", "Cancelled" = "tomato"))
ggsave("transaction_type_pie_chart.png", width = 6, height = 6)

# --- Revenue Lost from Cancellations by Country ---
cancel_loss_by_country <- cancelled_orders %>%
  mutate(LostRevenue = abs(Quantity * UnitPrice)) %>%
  group_by(Country) %>%
  summarise(TotalLostRevenue = sum(LostRevenue)) %>%
  arrange(desc(TotalLostRevenue)) %>%
  slice_head(n = 10)

ggplot(cancel_loss_by_country, aes(x = reorder(Country, TotalLostRevenue), y = TotalLostRevenue)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top Countries with Most Revenue Lost from Cancellations", x = "Country", y = "Revenue Lost (£)") +
  theme_minimal()
ggsave("top_countries_revenue_lost_cancellations.png", width = 8, height = 6)

# ============================================
# ORDER VALUE DISTRIBUTION
# ============================================

order_values <- data_clean %>%
  group_by(InvoiceNo) %>%
  summarise(OrderValue = sum(TotalPrice))

ggplot(order_values, aes(x = "All Orders", y = OrderValue)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2, width = 0.3) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "darkblue") +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of Order Values", x = "", y = "Order Value (£)") +
  coord_flip() +
  theme_minimal()
ggsave("distribution_order_values_boxplot.png", width = 8, height = 6)

ggplot(data_clean, aes(x = TotalPrice)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_log10(labels = comma) +
  labs(title = "Distribution of Total Transaction Values (Log Scale)", x = "TotalPrice (£)", y = "Count") +
  theme_minimal()
ggsave("histogram_totalprice_log.png", width = 8, height = 6)

# ============================================
# DASHBOARD SUMMARY
# ============================================

total_orders    <- n_distinct(data_clean$InvoiceNo)
total_customers <- n_distinct(data_clean$CustomerID)
total_revenue   <- sum(data_clean$TotalPrice)
total_products  <- n_distinct(data_clean$Description)

cat("🧾 SUMMARY DASHBOARD\n")
cat("Total Orders       :", total_orders, "\n")
cat("Total Customers    :", total_customers, "\n")
cat("Total Revenue (£)  :", round(total_revenue, 2), "\n")
cat("Unique Products    :", total_products, "\n")