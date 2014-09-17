# Swedish poll of polls

![Swedish poll of polls](sample.png?raw=true)

This R script fetches data from [MansMeg/SwedishPolls][swedishpolls] and uses that data to provide a weighted "poll of polls" of Swedish election polls.
The polls are expanded to cover their collection period, then weighted by number of respondents. Then, a rolling average over 84 days is plotted.

[swedishpolls]: https://github.com/MansMeg/SwedishPolls
