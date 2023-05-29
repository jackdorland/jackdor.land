# ジャック・どろぁんど — Jack Dorland

## jackdor.land website & blog

A personal website and blog to house all of my pens, pencils, politics, dreams, desires, hopes, troubles, and much more. This website/blog is created with [***Hakyll***](https://jaspervdj.be/hakyll/), which I **highly** recommend for anyone looking to start their own blog in the future.

Posts will be periodically added to [posts/](posts/) and automatically reflected onto [**pens, pencils and politics**](https://jackdor.land/blog) after compilation.

My only contact method is [**jack@jackdor.land**](mailto:jack@jackdor.land). I do not use social media of any kind. Feel free to contact me in English or 日本語. Please refrain from sending me any emails vying for personal information or for marketing purposes. If you have a question about one of my interests, or this blog, I'll always be happy to answer!

## File Structure

This website follows a fairly standard Hakyll file structure.

- assets → visual assets and scripts
  - images → images & resources
  - stylesheets → cascading stylesheet resources
    left_handed_girlfriend.otf
    scripts.js
- posts → contains all blog posts
- templates → all page templates and partials
  - partials → partial Hakyll templates for post listings & metadata

Within [*site.hs*](site.hs) lies the Hakyll definitions and setup file which defines routes and compilers for said routes.

## Installation

Ensure you have the Haskell toolchain installed and simply clone this repo. Run `stack build && stack exec site watch` in the root repository directory to start a local server at `localhost:8000`.
