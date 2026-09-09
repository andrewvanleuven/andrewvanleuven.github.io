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
description: "One sentence, used as the listing card subtitle."
image: thumbnail.png
image-alt: "What the thumbnail shows."
format:
  html:
    theme: cosmo
    embed-resources: false
---
```

3. Add any data files the post needs into that same directory
4. Always declare an `image:` (and ideally an `image-alt:`) in the frontmatter. Every post should have one. See "Quarto preview litters post folders" below for why this is not merely cosmetic.
5. Render and preview: `quarto preview` or `quarto render`
6. The `posts/_metadata.yml` applies to all posts automatically: `freeze: auto` and banner-style title blocks

A post can be fully self-contained. The `09_2026_rent` post pulls its own ACS data, does the analysis, and draws every map inside `index.qmd`, with no data sidecars in the repo at all, because `freeze` caches the results. Prefer that when the data comes from an API.

## Rendering and freeze

`posts/_metadata.yml` sets `freeze: auto`, which re-executes a post when its source changes. Three things about this are easy to get wrong.

**`freeze: true` means never re-execute, not "re-execute on change."** That is `freeze: auto`. Under `true`, Quarto updates the hash in `_freeze/.../execute-results/html.json` but keeps the old results and figures, and reports a successful render. Code edits are silently ignored.

**Three posts are deliberately pinned to `freeze: true`** in their own frontmatter, which overrides the folder default: `03_2026_popest`, `07_2026_golf`, `09_2026_rent`. All three embed geometry in mapgl widgets, so every re-execution writes a multi-megabyte blob into git history. **To edit any of them, flip the pin to `auto` first, render, then flip it back and render again to confirm nothing re-executed.**

**Editing `posts/_metadata.yml` invalidates every post**, because it is merged into each post's frontmatter. A one-line change there re-executes the whole blog.

**`freeze` is only honored on a project-wide `quarto render` or `quarto preview`.** Rendering a single file (`quarto render posts/x/index.qmd`) always executes.

To confirm a render actually did the work: check that the figure PNG mtimes are newer than the `.qmd`, and that `_freeze/posts/<post>/index/execute-results/html.json` contains the new code under `result.markdown`.

## Publishing

`quarto publish gh-pages` builds the site, commits it to the local `gh-pages` branch, and pushes. **Its push fails on this repo** and has done so consistently once the payload passes roughly 17 MB:

```
send-pack: unexpected disconnect while reading sideband packet
fatal: the remote end hung up unexpectedly
```

SSH keepalives do not help. HTTPS with the `gh` credential helper works every time:

```bash
git -c credential.helper='!gh auth git-credential' push \
  https://github.com/andrewvanleuven/andrewvanleuven.github.io.git main:main

git -c credential.helper='!gh auth git-credential' push \
  https://github.com/andrewvanleuven/andrewvanleuven.github.io.git gh-pages:gh-pages
```

**The publish output is misleading when this happens.** It still prints the "deployments normally take a few minutes" note, so it reads as success. Always verify:

```bash
git -c credential.helper='!gh auth git-credential' fetch \
  https://github.com/andrewvanleuven/andrewvanleuven.github.io.git 'refs/heads/*:refs/remotes/origin/*'
git rev-list --count origin/gh-pages..gh-pages   # must be 0
```

Pushing by explicit URL does not update the `origin/*` tracking refs, which is why the fetch above is needed before the count means anything.

## Keeping the repo small

`.git` is around 270 MB and one file is most of it. `_freeze/posts/03_2026_popest/index/execute-results/html.json` is **52 MB**, over GitHub's 50 MB warning threshold, because that post embeds full county geometry in its mapgl widgets. The file is not growing, but every re-execution writes a *new* 52 MB blob into history permanently, since mapgl regenerates widget ids on each run. Hence the freeze pins above.

For new posts with interactive maps:

- **One source per widget.** `add_source(id = "counties", data = x)` once, then have every layer reference `source = "counties"`. Passing an sf object to each layer embeds a separate copy of the geometry. In `09_2026_rent` this made county outlines free instead of costing 1.4 MB per map.
- **Simplify the boundaries.** `rmapshaper::ms_simplify(keep = 0.35, keep_shapes = TRUE)` cuts roughly a third of the payload and preserves shared borders, so counties still tile with no slivers. Below about 0.35 the savings flatten out while coastlines like the Mississippi delta visibly polygonise. Keep full resolution for the analysis and any static maps; simplify only what the widgets embed.
- Static ggplot maps cost nothing by comparison, since only the PNG ships.

## Known Issues and Fixes

### Quarto preview litters post folders with 0-byte files

A running `quarto preview` writes empty placeholder files into post directories on every re-render, named `index (1).html`, `index (2).html` and upward. 93 of these accumulated in one session. They are 0 bytes, mode 600, and were never tracked or published, but they pile up indefinitely.

It only happens for posts where Quarto **cannot resolve a preview image for the listing card**. Posts that declare `image:` in the frontmatter, or that have a discoverable `<img>` in the body, are unaffected. Giving every post an `image:` is the actual fix; `.gitignore` carries `**/* (*).html` and `**/*_files (*)/` as a backstop.

If they reappear, look for a stray preview process and kill it:

```bash
ps aux | grep "quarto preview" | grep -v grep
```

Before deleting anything that looks like render output, check `git ls-files` on it. Supporting files have been committed into post folders by mistake before, so "obviously generated" is not the same as untracked.


### `mapgl` CDN dependency bug (revealjs only)

`mapgl` 0.4.5 has a bug where `compare()` and `mapboxgl()` use CDN URLs for some JS dependencies instead of local files. This causes a Quarto render error in revealjs output:

```
Error in FUN(X[[i]], ...) :
  Dependency maptiler-geocoding-control 2.1.7 is not disk-based
```

**Fix:** Edit the YAML file in the installed package to replace the CDN `href` with the local `src` path. The file to patch is:

```
/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/mapgl/htmlwidgets/maplibregl_compare.yaml
```

Find this block:
```yaml
  - name: maptiler-geocoding-control
    version: 2.1.7
    src:
      href: "https://cdn.maptiler.com/maptiler-geocoding-control/v2.1.7/"
```

Replace with:
```yaml
  - name: maptiler-geocoding-control
    version: 2.1.7
    src: "htmlwidgets/lib/maptiler-geocoding-control"
```

Also: `mapboxgl()` is entirely CDN-based (Mapbox licensing prevents bundling). Use `maplibre()` with `carto_style()` instead in revealjs presentations.

## Rules

- When I ask for your help in writing, I don't ever want standalone em dashes. I only like em dashes when there is a complete pair of them, and they are in the middle of a sentence. Also, do not surround em dashes with spaces. Let them touch the text directly. 