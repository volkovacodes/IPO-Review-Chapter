# SDC-PULL-IPO
This directory explains in details how to extract IPO data from SDC and match this data with CRSP information.
These codes and the final data sample is used in Lowry, Michaely and Volkova (2017) IPO review chapter.

* Step 1. Pull request from SDC
- file Extraction data from SDC.docx has detailed information about the criteria used to pull SDC data
- SDC report.rpt contains the exact report used to pull information from SDC

* Step 2. Cleaning data
- Cleaning SDC request.R includes the code used to clean the data. This code excludes:
-- Non-common stock, REIT, units, ADRs, penny stocks, closed-end funds and stocks with CRSP record within a week

* Step 3. Summary tables and figure
- Code Tables with summary.R is used to create all tables and figure in Lowry, Michaely and Volkova (2017)
- Tables.xlsx and Figure.xlsx include the exact figures used in Lowry, Michaely and Volkova (2017)

* Step 4. Long-run IPO performance
- Long-run returns.R estiamtes buy-and-hold, market-adjusted, size-adjusted and size-and-book-to-market-adjusted 3-year returns
- ipo_FF_portfolios.R estimates \alpha in four factor Fama and French model for three year after the IPO
