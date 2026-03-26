# MapDork Blog

A Quarto blog that provides "under the hood" technical details on maps published on the [Substack page](https://vanleuven.substack.com/). Posts show the R code and interactive maps behind each Substack post.

**Author:** Andrew J. Van Leuven

## Creating a New Post

1. Create a new directory under `posts/` named after the post topic (e.g., `posts/my-new-map/`)
2. Create `posts/my-new-map/index.qmd` with this frontmatter:

```yaml
---
title: "Post Title"
author: "Andrew J. Van Leuven"
date: "YYYY-MM-DD"
categories: [map]
format:
  html:
    theme: cosmo
    embed-resources: false
---
```

3. Add any data files the post needs into that same directory
4. Render and preview: `quarto preview` or `quarto render`
5. The `posts/_metadata.yml` applies to all posts automatically: freeze is enabled and title blocks use banner style

## Rules

- When I ask for your help in writing, I don't ever want standalone em dashes. I only like em dashes when there is a complete pair of them, and they are in the middle of a sentence. Also, do not surround em dashes with spaces. Let them touch the text directly. 