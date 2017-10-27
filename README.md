# Use a Markdown Editor (MDE) in an Shiny App 

### Proof of Concept: Can we store the values in MDE in a shiny.collection?

We use <a href="https://simplemde.com/" target="_blank">SimpleMDE -  Markdown Editor</a> a simple Markdown Editor to collect user input. 

This input is stored in  [shiny.collections](https://github.com/Appsilon/shiny.collections) `rethinkdb` (db) and could later be integrated into a Markdown Report.

## Solved so far
- Input from the editor is written into the db
- The last input in the db gets written into a template, which could be used in a .RMD file
- The app works fine for a single user -- but this is not what we want to build it for...


## Problem
Problems arise whe we have multiple clients (editors).
- As long as the input in the editor only comes from one and the same instance, all is fine and the changed text gets refreshed in all other editors
- If other clients start to change the input, the text from the multiple editors doesn't refresh and mixes up with 'old' text, that was saved some steps before (where is it captured...?)

My knowledge of shiny and JS is marginal, do I am not able to solve the problem(s) now. Which basically means, I'm giving up on this project.



