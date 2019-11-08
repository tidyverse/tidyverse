> In my opinion, the _tidyverse R_ package provides two additional functionalities that are not discussed in the manuscript. Firstly, attaching the _tidyverse R_ package will ensure that tidyverse packages are attached in the correct order. This is not trivial for beginners (e.g. attaching the _plyr_ and _dplyr R_ packages in the incorrect order will cause issues). Secondly, attaching the _tidyverse R_ package will inform the user about namespace conflicts in a concise and informative manner. This is very useful when many tidyverse and non-tidyverse packages are attached. For instance, conflicts arise when the _tidyverse R_ package is attached following the _raster R_ package because both the _tidyr R_ package---a core tidyverse package---and the _raster R_ package contain `extract()` functions. I think the _tidyverse R_ package is exceptionally useful when evaluated on its standalone merits---that is, disregarding the usefulness of the other packages that comprise the tidyverse---and I think readers would benefit from knowing about these functionalities.

We've added a sentence mentioning the "report" given when attaching the tidyverse package. But generally the order of packages within the tidyverse isn't so important (especially as over time we deliberately work to remove conflicts).

> Perhaps it is worth providing a citation for R? I realize that many readers will likely already be familiar with R to some extent, but perhaps this be useful for readers that are less familiar with R?

We've added a citation in the new related work paragraph, which also includes a more thorough description of the relaitonship between the tidyverse and base R.

> This manuscript is about describing (i) the collection of packages referred to as the tidyverse (covered in the **Components** and **Philosophy** sections) and (ii) the _tidyverse R_ package (covered in the **Tidyverse package** section). The **Summary** section does a fantastic job of summarizing the tidyverse collection of packages but I feel like it could provide more information about the _tidyverse R_ package itself. Perhaps the purpose of the _tidyverse R_ package could be mentioned in the **Summary** section?

We now mention the primary purpose (the ability to install all the packages with a single command) in the Summary section.

> Is the _tidyverse R_ package not a core tidyverse package? It is not listed as a core package in Line 129. Maybe it would be useful to explicitly state if the _tidyverse R_ package is a core package or not?

Since the tidyverse is a package that contains all of the other packages (core or otherwise), it doesn't fit into the categories given to its component packages. We've tried to clarify this by adding language re. its nature as a "meta" package.

> I wonder if it is worth listing all of the non-core packages in the tidyverse? I suggest this for two reasons. Firstly, it is noted that one of the main purposes for the _tidyverse R_ package is that it provides a convenient method to install all of the packages in the tidyverse. Since there over ten non-core packages listed on the tidyverse package's REAMDE file, I suspect that listing all of these packages here would help the reader appreciate this functionality. Secondly, if one of the main aims of this manuscript is to describe the tidyverse, then I think it would be useful to list all of the packages that comprise the tidyverse. Although additional packages may be added to the tidyverse in the future, I think this would helpful to the reader.

Now added to the "Tidyverse package" section.

> Throughout manuscript: The **Reviewer Checklist** requires that "the authors describe how this software compares to other commonly-used packages". This manuscript does not make such comparisons. For instance, the tidyverse _readr R_ package provides functions for importing data similar to the _utils_ (`read.table()`) and _data.table R_ packages. If this manuscript were specifically about the _readr R_ package then I would expect such comparisons to be present, but I personally think that detailed comparisons for all tidyverse packages would distract from the main purpose of the manuscript. I raise this issue here because it is on the checklist.

We've added a paragraph briefly discussing how the tidyverse ecosystem compares to other software, but feel the comparisons for the component packages would be out of scope for this paper (though we make more explicit mention of the compoinent-package references, now).

> ", not to solve every possible problem". Perhaps a semicolon or em-dash would be more appropriate here than a comma?

Changed to a semicolon.

> Perhaps "downloading and installing" might be more appropriate than "downloading", since the following _R_ code uses `install.packages`?

Thanks, we've made this change.

> Perhaps "tidyverse version 1.2.0" to make it clear that this sentence is referring to a specific version of the _tidyverse R_ package?

We've added this, as suggested.

> Is the ordering of these packages significant? If not, perhaps these packages could be listed alphabetically?

Packages are now listed in alphabetical order.

> Perhaps "application programming interface (API)" instead of "API" for clarity?

We don't believe that spelling API out makes it any easier to understand.

> This is a bit nitpicky but I do not think that data are necessarily "stored" in an API? I would say that data are accessed through an API? Apologies if I'm just plain wrong.

We've tried to work around this by saying "behind a web API".

> My impression is that terms like "data frame" and "flat file" will be very useful for individuals with experience in programming _R_, _Python_, or _SQL_. If such individuals are the intended audience for this manuscript, then using such terminology is ideal. If a broader audience is intended, then perhaps using different terms (or clarifying these terms) will be useful (e.g. terms like "tabular" or "spreadsheet" may be more informative for a broader audience).

We agree that "tabular" is a bit than "flat" so we've changed this.

> It appears that the term "data" is being used to refer to a singular entity, though some would argue that the term "data" is a plural. I do not know the grammar rules to which this journal subscribes, so I thought I would raise this point just in case.

We've done our best to use the proper conjugations, but will defer to the editors, of course.

> Perhaps it might be useful to provide the full name of the format that corresponds to these file extensions (e.g. tab-separated values for tsv)?

We left this as is, since they're already in a parenthetical, and worry that "comma-separated values" might be more obscure than "csv."

> I don't want to wade into the debate regarding if data is a singular or plural term, but I think that it might be useful to be consistent throughout the manuscript. My interpretation of the sentence containing the phrase "[...] import data stored in other common formats or retrieve it directly [...]" uses the term "data" to refer to both a plural and a singular entity. Perhaps the sentence might be clearer if written as something like "[...] import data from other common formats or the web directly"?

Now changed to "make it possible to import data stored in other common formats or directly from the web."

> The manuscript contains many embedded links which mean that URLs are not shown in the text (e.g. [like this](https://google.com)). I am not familiar enough with the correct reference style for this journal, so I thought I would raise this point just in case.

Also well as links to individual websites, all packages also have full citations.

> Perhaps "core tidyr package provides" might be useful to be consistent with the rest of the manuscript (e.g. "the core readr package", line 139)?

So added.

> The reference for Bache & Wickham (2014) has a lower case "r" in the title when referring to the R programming language.

Good catch! Change has been made.
