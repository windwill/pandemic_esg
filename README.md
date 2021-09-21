# pandemic_esg
Scenario Generator for Pandemic Events

This program includes statistical analysis of historical pandemic and epidemic events and data-driven scenario generation. It is part of the research project "Scenario Generation for Pandemics" sponsored by the Canadian Institue of Actuaries. The program is intended for educational purpose.

It is coded in R, which is an open source statistical software.

The sample program has been tested using R4.0.4.

It is an ongoing project and expected to be finished by early 2022.

# File Structure 
**Root folder**

pandemic_data.csv contains historical pandemic and epidemic events including time, disease, case fatality rate, mortality rate, infection rate, location, and impact on age groups

econ_data.csv contains annual economic data from 1871 to 2020, where available

econ_data_quarterly.csv contains quarterly economic data from Q2 1947 to Q2 2021, where available

subfolder "esg_input" contains all the input files needed to simulate economic factors and capital market variables.

Script: pandemic_scrip_fullt.R that contains codes to perform data exploration, distribution fitting, correlation analysis, ESG simulations, etc. The script itself is documented.

# Run Program

