# Institution Matcher App

## Overview

Institution Matcher is an R Shiny application built to automate the matching of educational institutions from candidate CVs.

The app processes CVs in multiple formats, extracts institution names from the education section, and compares them against a master institution list using exact and fuzzy matching. The goal is to reduce manual review time and improve consistency in institution verification workflows.

## Objectives

### 1. Extract institution-related text from CVs
The app reads candidate CVs from PDF, DOCX, and image formats and extracts text relevant to the education section.

### 2. Match extracted institution names to a reference list
The app compares extracted institution names against a master list using exact matching and fuzzy matching.

### 3. Support efficient and privacy-aware administrative review
The app is designed to run locally, avoid storing personal data, and present results in a simple downloadable format for manual verification.

## Tools Used

- R
- Shiny
- pdftools
- docxtractr
- tesseract
- stringdist
- readxl
- writexl
- DT

## Key Features

- Supports PDF, DOCX, JPG, JPEG, and PNG files
- Extracts text from CVs using OCR where needed
- Focuses on the education section to improve matching quality
- Uses exact and fuzzy matching for institution detection
- Flags duplicate institutions for manual review
- Exports matched results to Excel
- Runs locally with privacy-focused design

## Project Structure

```text
institution-matcher-app/
│── README.md
│── app.R
│── requirements.txt
│
├── data/
├── images/
└── outputs/
