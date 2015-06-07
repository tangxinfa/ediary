A diary publish system.

# What is ediary #

From [Wikipedia](http://en.wikipedia.org/wiki/Diary)

> A diary (also called journal) is a record (originally in handwritten format) with discrete entries arranged by date reporting on what has happened over the course of a day or other period. 

This project named __ediary__ is just for avoid name conflict with emacs's builtin package __diary__ and also have the same meaning as __diary__.

# Objectives #

- Easy to use

Write a piece of text, call publish command, then this piece of text is appeared as a article in your website.

- Easy to implement

Split to subprojects, use the right tool to do the right thing.

- Easy to customize

Change the config file or tweak the templates even fork this project, this project is very easy to understand so don't afraid to change it.

# Workflow #

- Edit source file

Support formats: [org](http://orgmode.org)

- Export to ediary file

A json file adherence to specification: (Ediary File Specification)

- Generate web pages

Generate HTML/XML/CSS/JS/IMAGES files ready for publish

- Publish web pages

Publish web pages to you site.

Such as __git push__ to you [github pages](https://pages.github.com/)

# Inspired by #

- [o-blog](https://github.com/renard/o-blog)

# Ediary File Specification #

It just a json file with the following structure:

    [
        {
            "title": "The article's title",
            "slug": "Use to generate url if title is not appropriate, this's not REQUIRED",
            "timestamp": "2015-06-07 11:05:44",
            "tags": ["ediary", "example"],
            "body": "The article body with html format"
        },
        {
            "title": "More article following",
            "slug": "",
            "timestamp": "2015-06-07 11:05:44",
            "tags": ["ediary", "example"],
            "body": "The article body with html format"
        }
    ]
