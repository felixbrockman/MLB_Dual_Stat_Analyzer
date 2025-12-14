# MLB Dual Stat Analyzer

An interactive R Shiny application for evaluating MLB players by visualizing their combined performance across two statistics simultaneously. The core feature is an X–Y percentile-based plot that allows users to identify balanced players, trade-offs, and outliers at a glance, with integrated searching, filtering, and exportable results.

## Overview

Single-stat leaderboards often hide important context about a player’s overall profile. This application addresses that gap by plotting players on a two-dimensional X–Y graph, where each axis represents a different statistic expressed as a percentile relative to peers.

By placing players directly into this joint statistical space, the tool enables users to assess overall player quality across multiple dimensions, identify players who excel in both metrics simultaneously, compare trade-offs between complementary or competing skills, and isolate meaningful subsets of players within the plot.

## Core Analytical Concept: Dual-Stat X–Y Visualization

At the center of the application is an interactive X–Y scatter plot. Each player is represented as a point, with the X-axis and Y-axis corresponding to two user-selected statistics. Values are shown as percentiles, allowing fair comparison across seasons.

A player’s position within the plot reflects combined ability rather than rank in a single metric. High–high quadrants highlight well-rounded performers, while asymmetries reveal specialization and potential development targets.

## Interactive Exploration and Export

The X–Y visualization is fully interactive and responds in real time to user input. Filters update the graph dynamically, and users can search within the plot to highlight specific players or subsets of interest. Synchronized tables reflect the players currently visible in the graph.

The application also allows users to export the current visualization to a PDF, preserving the selected statistics, filters, and player set. This supports sharing insights, reporting, and discussion.

## Key Features

- Dual-stat X–Y percentile visualization for combined player evaluation  
- Support for both hitters and pitchers  
- Dynamic filtering by year, role, and statistic selection  
- Player search within the plotted results  
- PDF export of filtered and customized graph outputs  
- Offline-first design using preprocessed datasets for fast performance  
- Modular architecture with separated data and application layers  

## Data and Architecture

The project follows a reproducible, modular structure.

The build_lahman_tables.R script processes raw Lahman data into analysis-ready datasets. The data_processed directory contains derived RDS files used directly by the Shiny app for speed and reproducibility. The app.R file contains the Shiny application logic and UI, while the www directory holds custom CSS and JavaScript used to enhance interactivity and presentation.

Including processed data allows the application to run immediately while still making the full data pipeline transparent and reproducible.

## Tech Stack

- R  
- Shiny  
- dplyr  
- Lahman Baseball Database  
- Custom CSS and JavaScript  

## Live Application

https://felixbrockman.shinyapps.io/mlb_dual_stat_analyzer/

