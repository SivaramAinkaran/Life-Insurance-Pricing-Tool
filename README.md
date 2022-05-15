# Life-Insurance-Pricing-Tool

**NOTE: This is a collaborative group project completed by 4 students**

Creating a Pricing tool (as a Shiny App) for a large range of Insurance Products (conducted in RStudio)

A pricing tool application comparing premiums, reserves and visualising the 
progress of reserves over time for different insurance products. This tool was 
created using the "shiny" package and outputs as a "shiny.app". Simply click 
"Run App" within the "pricing_tool.R" file to use the tool.

The included "lifetable.txt" file contains two mortality tables which shows the
number of people expected to be alive out of 1000 people born, using past mortality
rate data. This is used as the basis to conduct the sensitivity analysis. The
mortality table "y" starts at age 20 and is derived from table "x" with the
number of people alive at age y equal to the number of people alive at age y-3 on 
table "x" (ie. number of people alive at 20 on table "y" equals the number of 
people alive at age 17 on table "x".

