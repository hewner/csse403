---
title: CSSE000
---

Welcome to the CSSE000!  All the materials are available from [the
course github repository]({{ site.github.repository_url }}).

# List of all rendered files

If you'd like to view a particular page in the repo full-page, without
all the associated Github history and info, here's direct links:

{% for item in site.pages %}
* [{{item.path}}]({{ site.baseurl }}{{item.url}})
{% endfor %}

