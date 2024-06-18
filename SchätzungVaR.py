import pandas as pd
import os
import openpyxl
import numpy as np
from scipy.optimize import minimize

# Get the current working directory
current_working_directory = os.getcwd()


# Load the Excel file
file_path = '/Users/minhanhle/Documents/MasterArbeit/Eurostoxx1.xlsx'
sheet_name = 'Tabelle1'  # Korrigierte Blattname

# Read the Excel file and skip the first 3 rows and row 5 (row 4 after skipping 3)
df = pd.read_excel(file_path, sheet_name=sheet_name, skiprows=[0, 1, 2, 4])

# Set the first row as column headers
#df.columns = df.iloc[0]
#df = df[0:]

# Remove any columns with missing values
df = df.dropna(axis=1)

# Convert the first column to datetime format and set it as the index
df.iloc[:, 0] = pd.to_datetime(df.iloc[:, 0])
df.set_index(df.columns[0], inplace=True)

# Filter the data to include only dates between 01.01.2018 and 01.01.2024
start_date = '2018-01-01'
end_date = '2024-01-01'
df = df[(df.index >= start_date) & (df.index <= end_date)]

# Define the public holidays in Germany from 2018 to 2024
holidays = [
    '2018-01-01', '2018-03-30', '2018-04-02', '2018-05-10', '2018-05-21', '2018-10-03', '2018-12-24', '2018-12-25', '2018-12-26', '2018-12-31',
    '2019-01-01', '2019-04-19', '2019-04-22', '2019-05-30', '2019-06-10', '2019-10-03', '2019-12-24', '2019-12-25', '2019-12-26', '2019-12-31',
    '2020-01-01', '2020-04-10', '2020-04-13', '2020-05-21', '2020-06-01', '2020-10-03', '2020-12-24', '2020-12-25', '2020-12-26', '2020-12-31',
    '2021-01-01', '2021-04-02', '2021-04-05', '2021-05-13', '2021-05-24', '2021-10-03', '2021-12-24', '2021-12-25', '2021-12-26', '2021-12-31',
    '2022-01-01', '2022-04-15', '2022-04-18', '2022-05-26', '2022-06-06', '2022-10-03', '2022-12-24', '2022-12-25', '2022-12-26', '2022-12-31',
    '2023-01-01', '2023-04-07', '2023-04-10', '2023-05-18', '2023-05-29', '2023-10-03', '2023-12-24', '2023-12-25', '2023-12-26', '2023-12-31',
    '2024-01-01'
]

# Convert holidays to datetime format and remove them from the DataFrame
holidays = pd.to_datetime(holidays)
df = df[~df.index.isin(holidays)]

# Save the cleaned data to a CSV file without row numbers
output_path = '/Users/minhanhle/Documents/MasterArbeit/Cleaned_Eurostoxx.csv'
df.to_csv(output_path, index=True)

# Display the cleaned DataFrame
#df.head(3)

# Calculate one-day returns for all stocks
returns = np.log(df / df.shift(1))

# Drop the first row as it will contain NaN values due to the shift
returns = returns.dropna()

#Stichproben-VKM 
# Calculate the rolling covariance matrix with a window of 252 days
rolling_cov = returns.rolling(window=252).cov()

# Extract the first complete covariance matrix (i.e., after 252 days)
first_cov_matrix = rolling_cov.loc[returns.index[251]]

# Calculate the weights of the portfolio (assuming equal weights for simplicity)
# num_stocks = len(df.columns)
# weights = np.ones(num_stocks) / num_stocks

# # Calculate the portfolio variance using the first covariance matrix
# portfolio_variance = np.dot(weights.T, np.dot(first_cov_matrix, weights))

# # Calculate the portfolio standard deviation (volatility)
# portfolio_volatility = np.sqrt(portfolio_variance)

# # Define the confidence level for VaR (e.g., 95%)
# confidence_level = 0.95
# z_score = abs(np.percentile(np.random.standard_normal(1000000), (1-confidence_level)*100))

# # Calculate the VaR using the delta-normal method
# VaR = z_score * portfolio_volatility

# # Print the results
# print(f"Portfolio Variance: {portfolio_variance}")
# print(f"Portfolio Volatility (Standard Deviation): {portfolio_volatility}")
# print(f"Value at Risk (VaR) at {confidence_level*100}% confidence level: {VaR}")

#### Skrinkage-VKM 
#Berechnung der Durchschnittskorrelation
correlation_matrix = returns.corr()
average_correlation = correlation_matrix.values[np.triu_indices_from(correlation_matrix, k=1)].mean()

#Erstellung der Konstante-Korrelationsmatrix
constant_correlation_matrix = np.full(correlation_matrix.shape, average_correlation)
np.fill_diagonal(constant_correlation_matrix, 1)

# Ermittlung der Standardabweichungen der Renditen
std_devs = returns.std()

# Erstellung der Konstante-Kovarianz-Matrix
constant_cov_matrix = np.outer(std_devs, std_devs) * constant_correlation_matrix

# Funktion zur Berechnung der Frobenius-Norm
def frobenius_norm(shrinkage_intensity, constant_cov_matrix, empirical_cov_matrix):
    shrunk_cov_matrix = shrinkage_intensity * constant_cov_matrix + (1 - shrinkage_intensity) * empirical_cov_matrix
    return np.linalg.norm(shrunk_cov_matrix - empirical_cov_matrix, 'fro')

# Optimierung der Shrinkage-Intensität 
#TODO: fix bug bei result 
result = minimize(frobenius_norm, 0.5, args=(constant_cov_matrix, first_cov_matrix), bounds=[(0, 1)])
optimal_shrinkage_intensity = result.x[0]

# Anwendung der optimalen Shrinkage-Intensität
shrunk_cov_matrix = optimal_shrinkage_intensity * constant_cov_matrix + (1 - optimal_shrinkage_intensity) * first_cov_matrix

# Berechnung der VaR gemäß der Shrinkage-Kovarianz-Matrix
# Annahme gleicher Gewichtung für alle Aktien im Portfolio
num_stocks = len(df.columns)
weights = np.ones(num_stocks) / num_stocks

# Berechnung der Portfolio-Varianz unter Verwendung der Shrinkage-Kovarianz-Matrix
portfolio_variance_shrunk = np.dot(weights.T, np.dot(shrunk_cov_matrix, weights))

# Berechnung der Portfolio-Volatilität
portfolio_volatility_shrunk = np.sqrt(portfolio_variance_shrunk)

# Definition des Konfidenzniveaus für VaR (z.B. 95%)
confidence_level = 0.95
z_score = abs(np.percentile(np.random.standard_normal(1000000), (1-confidence_level)*100))

# Berechnung des VaR gemäß der delta-normal Methode
VaR_shrunk = z_score * portfolio_volatility_shrunk

# Ergebnis ausgeben
print(f"Optimale Shrinkage-Intensität: {optimal_shrinkage_intensity}")
print(f"Portfolio Variance (Shrunk): {portfolio_variance_shrunk}")
print(f"Portfolio Volatility (Shrunk): {portfolio_volatility_shrunk}")
print(f"Value at Risk (VaR) with Shrinkage at {confidence_level*100}% confidence level: {VaR_shrunk}")

###Backtesting VKM-Shrinkage 
# Berechnung der durchschnittlichen Korrelation und der Konstante-Korrelationsmatrix
def calculate_constant_cov_matrix(returns):
    correlation_matrix = returns.corr()
    average_correlation = correlation_matrix.values[np.triu_indices_from(correlation_matrix, k=1)].mean()
    constant_correlation_matrix = np.full(correlation_matrix.shape, average_correlation)
    np.fill_diagonal(constant_correlation_matrix, 1)
    std_devs = returns.std()
    constant_cov_matrix = np.outer(std_devs, std_devs) * constant_correlation_matrix
    return constant_cov_matrix

# Funktion zur Berechnung der Frobenius-Norm
def frobenius_norm(shrinkage_intensity, constant_cov_matrix, empirical_cov_matrix):
    shrunk_cov_matrix = shrinkage_intensity * constant_cov_matrix + (1 - shrinkage_intensity) * empirical_cov_matrix
    return np.linalg.norm(shrunk_cov_matrix - empirical_cov_matrix, 'fro')

# Funktion zur Berechnung der optimalen Shrinkage-Intensität und der geschrumpften Kovarianzmatrix
def calculate_shrunk_cov_matrix(empirical_cov_matrix, returns):
    constant_cov_matrix = calculate_constant_cov_matrix(returns)
    result = minimize(frobenius_norm, 0.5, args=(constant_cov_matrix, empirical_cov_matrix), bounds=[(0, 1)])
    optimal_shrinkage_intensity = result.x[0]
    shrunk_cov_matrix = optimal_shrinkage_intensity * constant_cov_matrix + (1 - optimal_shrinkage_intensity) * empirical_cov_matrix
    return shrunk_cov_matrix

# Initialisierung
window_size = 252
num_days = len(returns)
VaR_results = []
VaR_dates = []

# Berechnung der VaR für jedes Rolling Window ab dem Tag 252
for i in range(window_size, num_days):
    window_returns = returns.iloc[i-window_size:i]
    empirical_cov_matrix = window_returns.cov()
    shrunk_cov_matrix = calculate_shrunk_cov_matrix(empirical_cov_matrix, window_returns)
    
    # Berechnung der Portfolio-Varianz und -Volatilität
    num_stocks = len(returns.columns)
    weights = np.ones(num_stocks) / num_stocks
    portfolio_variance_shrunk = np.dot(weights.T, np.dot(shrunk_cov_matrix, weights))
    portfolio_volatility_shrunk = np.sqrt(portfolio_variance_shrunk)
    
    # Berechnung des VaR gemäß der delta-normal Methode
    confidence_level = 0.95
    z_score = abs(np.percentile(np.random.standard_normal(1000000), (1-confidence_level)*100))
    VaR_shrunk = z_score * portfolio_volatility_shrunk
    
    # Speichern des VaR und des Datums
    VaR_results.append(VaR_shrunk)
    VaR_dates.append(returns.index[i])

# Umwandlung der VaR-Ergebnisse in ein DataFrame
VaR_df = pd.DataFrame(VaR_results, index=VaR_dates, columns=['VaR'])

# Backtesting der berechneten VaR gegen die tatsächlichen Renditen ab Tag 253
actual_returns = returns.iloc[window_size:]

# Erstellung des Backtesting-Berichts
backtesting_results = actual_returns.apply(lambda row: row.sum() < -VaR_df.loc[row.name, 'VaR'], axis=1)
num_breaches = backtesting_results.sum()
total_observations = len(backtesting_results)
breach_percentage = num_breaches / total_observations * 100

print(f"Anzahl der Verletzungen: {num_breaches}")
print(f"Gesamtanzahl der Beobachtungen: {total_observations}")
print(f"Prozentsatz der Verletzungen: {breach_percentage:.2f}%")

# Ergebnis anzeigen
print(VaR_df)






# %%
