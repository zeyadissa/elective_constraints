**BACKGROUND**

The elective wait list has been growing year-on-year for over a decade. With this growth turbocharged in the aftermath of COVID-19, the total wait list stood at roughly 8mn by the first quarter of 2024, nearly double its pre-COVID levels. This led to 43% of patients waiting longer than 18 weeks, down from a 2019 value of 16%, in contravention of the NHS constitution, which aims to meet the 18 week target for at least 92% of patients.

The situation has naturally generated political momentum towards a resolution. Pledges were made by major political parties to reduce the wait list to a more "manageable" size, including most recently by Labour to reduce wait times as part of their six "Key Pledges" ahead of the upcoming General Election.

The challenges, however, are substantial. Rishi Sunak's pledge in January 2023 to cut the wait list ran into difficulties, with the wait list growing by almost 10% since the promise was made. The reasons behind which are not immediately clear. Workforce shortages are often cited, but staff FTEs have increased year on year since 2019/20, whilst completed activity stagnated.

![](images/clipboard-1716506656.png){width="349"}

It would, however, be misleading to read simple causation as saying that workforce does not matter. It is likely there are underlying constraints that are confounding the results. This analysis thus seeks to identify these bottlenecks: Are there additional factors driving the limited growth in NHS providers' ability to treat patients?

To answer this question, this model conceptualises elective care as depending on:

1.   Workforce inputs (such as FTEs)
2.  Capital inputs (such as the number of available beds or operating theatres)
3.  Throughput metrics (such as the ratio of COVID-occupied beds)

A change in the number of nurses, for example, may not necessarily be as impact if the number of beds fell. Conversely, if the increase in FTEs was primarily driven by doctors without enough support staff, then the impact might be similarly muted.

Thus, in order to test how these variables interact with each other and to identify which factors have acted as a bottleneck to elective activity growth, this model utilises a simple linear mixed effects model with trust-level random effects.

**MODEL OUTPUTS**

| Metric               | Impact on completed pathways | Change since 2019/20 |
|----------------------|------------------------------|----------------------|
| Total FTEs           | \+                           | +30%                 |
| Nurses               | \+                           | +25%                 |
| Doctors              | \+                           | +28%                 |
| Doctors *and* Nurses | \+                           | --                   |
| Managers             | \-                           | +20%                 |
| Operating Theatres   | \+                           | +11%                 |
| Beds                 | \+                           | +12%                 |
| Covid-Occupied Beds  | \-                           | --                   |
| Maintenance Backlog  | \-                           | +24%                 |
| Post-Covid Shock     | \-                           | --                   |

: Impact of various metrics versus changes since 2019/20

The results reveal three notable trends:

1.  Although more Nurses and Doctors have a positive impact on completed activity, the mix matters. Indeed, the variables were not significant until their interaction (the increase of doctors concurrent with nurses) was considered. This suggests a bottleneck that may occur when doctors are added without enough nurses and vice-versa.
2.  A larger severe maintenance backlog had a negative impact on completed activity. Although factors such as more beds and operating theatres have an unsurprisingly positive impact, this underscores that poorly maintained infrastructure can hinder a providers' ability deliver care.
3.  We find that there is an unexplained shock post-Covid that has a large negative impact on completed pathways. Even when Covid-occupied beds were taken into account, this factor remains significant. It is possible that part of this could be a hidden productivity drop post-COVID, however further research is needed to make that determination.

Ultimately our results reaffirm the importance of both the volume and mix of staff to elective care, but identify potential bottlenecks through which this growth might be muted. Capital spend is integral not solely to open up more beds or operating theatres, but also to maintain existing infrastructure to allow staff to do their work. The policy implication of a post-COVID decline further suggests a potential 'productivity' drop that needs to be addressed. Additional cash, therefore, might not necessarily unblock the system if not appropriately targeted, suggesting politicians need to think carefully about where they invest, rather than by how much.
