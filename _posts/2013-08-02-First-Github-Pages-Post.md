---
layout: post
published: true
---

## Github Pages

This is my first post using Github pages and [Prose](http://prose.io/ "Prose").  Below are some quick notes on how to get this going.

## Getting Started with Github Pages

First things first checkout the [documentation from Github]( https://help.github.com/categories/20/articles "Documentation").  Github uses Jekyll to generate html based on the Liquid templating engine.  This means you will have to be familiar with the liquid template syntax if you want to use the advanced features.

The easist way to get started is to [Fork this repository](https://github.com/tjchaplin/tjchaplin.github.io "My Github Page") into your own repository named: "YOUR-GITHUB-USER-NAME.github.io".  It has a barebones strucutre for using github pages as a blog. If you do this you won't need to install Jekyll on your own machine to get going.

Then, without understanding anymore about Jekyll or Liquid, start posting Markdown style blog posts with Prose.

## Getting Started With Prose.io

Prose is an online Markdown editor that interfaces with Github.  All you have to do is go to [Prose.io](http://prose.io/ "Prose") and follow the steps to authenticate into Github.  From there you will have access to all your repositories.  

If you forked my barebones github pages repository.  You will already be configured with prose.  This can be easily added by creating a _config.yml file with the following configuration:

```
prose:
  rooturl: '_posts'
```

The above tells prose that when you look at your github pages project open up the _posts folder.  The _posts folder is where all your blog entries are stored.  The naming convention for every post is "YEAR-MONTH-DAY-SOMEFILENAME".  This allows Jekyll to then organize each post by date.

Once you select your Github pages repository("YOUR-GITHUB-USER-NAME.github.io") Prose will open up the posts directory.  From there you can edit your old posts or simply click new and a new posts will be added.

## Get Going

Markdown is a quick way to get going with blogging and publishing content online.