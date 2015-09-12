# Swedish poll of polls

![Swedish poll of polls](sample.png?raw=true)

This R script fetches data from [MansMeg/SwedishPolls][swedishpolls] and uses that data to provide a weighted "poll of polls" of Swedish election polls.
The polls are expanded to cover their collection period, then weighted by number of respondents per survey day. Then, a centered simple moving median over 84 days is plotted.

The plot also includes a ribbon, which shows a rolling range of maximum and minimum polling results for each party, also with a window of 84 days.

[swedishpolls]: https://github.com/MansMeg/SwedishPolls
