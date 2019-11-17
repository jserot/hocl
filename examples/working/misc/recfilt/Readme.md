This is the filter bank described in Lee and Parks [classical paper on dataflow process network](https://dl.acm.org/citation.cfm?id=567010) (Sec III-C, Fig 15 and 16).
The `fb` recursive function here implements the recursion described in the paper.
Note that this function could be made more generic by taking the `qmf`, `qmf'` and `f` functions
as arguments.
