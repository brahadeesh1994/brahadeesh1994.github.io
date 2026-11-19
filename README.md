# Academic Homepage

Source code for my academic website, hosted at https://brahadeesh1994.github.io.

## Tools

- HTML and CSS (no build step)
- MathJax 3 (loaded from CDN, for LaTeX rendering)
- Google Fonts: Roboto Mono
- Latin Modern Roman (self-hosted in `fonts/`, under the GUST Font License)

## Structure

Lab pages (main navigation):

- `index.html` — Home: the CAT Lab, its research threads, the lab logo, upcoming events, announcements, recent updates
- `research.html` — Current and past research, with collaborators
- `publications.html` — Publications, with abstracts, errata and classification
- `team.html` — PI, current members, alumni, collaborators
- `photos.html` — Photographs from conferences, workshops and events, with a lightbox viewer
- `updates.html` — Record of past events, publications and milestones
- `join.html` — Joining the lab; contact details and map

Personal pages (sub-navigation):

- `about.html` — About the PI: position, academic history, profile links
- `about-my-name.html` — Notes on my name, its pronunciation and how to address me
- `resources.html` — My reviews for MathSciNet and zbMATH Open; hard-to-find papers
- `teaching.html` — Teaching record (instructor, TA, other teaching and mentoring)
- `activities.html` — Talks, events organized, outreach, professional service, conferences attended, grants, awards, institutional service
- `blog.html` — Blog index; individual posts in `blog/`

Folders and shared files:

- `courses/` — Course pages (e.g. `mal3040-2627-I.html`), with course files in a subfolder of the same name
- `tools/` — Interactive explorers (Tree Coloring Explorer, △-Convexity Hull Explorer)
- `assets/` — PDFs (CV, thesis, slides, posters, accepted manuscripts, reviews) and photos
- `fonts/` — Latin Modern Roman web fonts and license
- `style.css` — Single shared stylesheet
- `k7-torus-light.svg`, `k7-torus-dark.svg` — CAT Lab logo (light and dark mode)
- `k7-torus-favicon.svg` — Favicon
