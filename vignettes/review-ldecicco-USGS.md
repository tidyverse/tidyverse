> The paper "Welcome to the tidyverse" is a 3 page introduction to the tidyverse R package. The tidyverse R package includes 8 core packages and other non-core packages. I am familiar with these tools, and my initial reaction was "this paper is entirely too short". Many of my questions on "tidyverse"-ness however were addressed in the link https://principles.tidyverse.org. Perhaps it would be worth moving that link into the introduction.

I've added a sentence to the intro with more details on what we cover. I've left the link to <https://design.tidyverse.org> (formerly https://principles.tidyverse.org) in the last section, but have renamed it make more clear that you'll get to that at the end of the paper.

> The JOSS review is not just on the 3 page paper, it is on the entire software ecosystem. In this case, that includes a massive amount of useful resources. Each core package has adaquate to excellent documentation and links to external resources.

> My impression is this will be a useful paper for people to cite when they used tidyverse tools in their analysis. It is a short paper, but contains many useful links and references. I would recommend this paper to be published, but I offer the following impressions (they can be addressed or ignored):

> 1. Discussing the pros and cons of using the "tidyverse" package versus loading individual packages would be useful. It is stated that the "tidyverse" is intended for the general workflow of a data project. However, it might be worth mentioning that it probably is not ideal for using as a dependency in package development. I'm sure there are other examples and issues that should be considered.

We've added a sentence clarifying the advantages of `library(tidyverse)`, and another on why you should avoid importing in from another package.

> 2. Related to #1... I think it would be useful to have a sentence or two addressing using tidyverse as it relates to package development. The tidyverse is such a nice introduction to R....and then the new-tidyverse user becomes a little more confident and wants to make an R package...The first thing they do is wrap their tidyverse code into some functions. Alerting them to some of the considerations would be great. Perhaps a link to http://r-pkgs.had.co.nz and https://dplyr.tidyverse.org/articles/programming.html (there are probably other great resources...those are the 2 I've used over the years).

This feels a little out of scope for this paper, particularly since our advice is still in flux, and doesn't appear in a single good place. I've create an issue, <https://github.com/hadley/r-pkgs/issues/609>, to make sure that we don't forget about this; I think "R packages" is best home since it's where we concentrate our other package development advice.

> 3. Listing the full list of dependencies somehow would be useful (either directly in the text, or an obvious link). It might be worth addressing the fact that...yes...there are a lot of packages...but using the "tidyverse" package does simplify installation and helps prevent conflicts. Basically, having a reference to pass on to help ease the weary "IT admin" who has a knee jerk reaction to installing anything with too many dependencies would be great.

We've mentioned the total number, but I'd prefer to avoid too much discussion of dependencies as the issued are rather nuanced (e.g. https://www.tidyverse.org/blog/2019/05/itdepends/) and I'd rather not distract from the meat of the paper.

> 4. Consider adding a link to data.tables, fst, and feather in the sentence "Additional non-core packages".

Thanks for spotting that we missed feather; I've added a citation.

TODO: comparison sentence or paragraph?

> 5. This is a bit more far-fetched, and could easily be considered out of scope of this paper....but... This might be a good venue for laying out some of the lessons learned over the years from developing all these packages. For example, how and when does the tidyverse team decide to break backwards compatibility? It might be nice to hear more about how to make a package more human centered. The last paragraph lays out some general ideas, but are there specific tactics for package developers to coordinate all that feedback that you all recommend? (I can only imagine that project management is critical for all these packages to be coordinated so well....)

I think that's a book's worth of material :) Please stay tuned to <http://design.tidyverse.org/> where I'll be attempting to get all this knowledge out of our collective heads and into text in the next 2-3 years.

> Each individual repo for core tidyverse packages serves as great examples of how to manage a package in a human centered way....if there were a few specific examples for inspiring other package developers to follow, that would be great.
