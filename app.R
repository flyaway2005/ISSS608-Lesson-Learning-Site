# Add new tab in UI
tabPanel("Tender Market Analysis",
  tenderMarketAnalysisUI("market_analysis")
),

# ... existing code ...

# Add in server
tenderMarketAnalysis("market_analysis", labeled_tender_data) 