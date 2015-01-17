# Swedish poll of polls

![Swedish poll of polls](sample.png?raw=true)

This R script (`polls.r`) fetches data from [MansMeg/SwedishPolls][swedishpolls] and uses that data to provide a weighted "poll of polls" of Swedish election polls.
The polls are expanded to cover their collection period, then weighted by number of respondents per survey day. Then, a rolling average over 84 days is plotted.

There's also a variant of the script, `polls-no-uncertain.r`, which removes the data for uncertain voters and re-normalizes the result. It's probably not ver useful.

[swedishpolls]: https://github.com/MansMeg/SwedishPolls
