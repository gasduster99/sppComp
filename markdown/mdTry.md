---
title: Title
author: Nick Grunloh
date: Nov. 7, 2015
---

<!-- 
pandoc -o mdTry2.pdf mdTry.md --webtex
pandoc -o mdTry2.html mdTry.md --webtex
pandoc -o mdTry2.docx mdTry.md --webtex 
-->

#Header

[Markdown 1.0.1](http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip) (18 KB) -- 17 Dec 2004

##HHeader

Markdown is a text-to-HTML conversion tool for 
web writers. Markdown allows you to write using an easy-to-read, easy-to-write 
plain text format, then convert it to structurally valid XHTML (or HTML).

Thus, "Markdown" is two things: (1) a plain text formatting syntax; and (2) a 
software tool, written in Perl, that converts the plain text formatting to 
HTML. See the page for details pertaining to Markdown's formatting 
syntax. You can try it out, right now, using the online.

The overriding design goal for Markdown's formatting syntax is to make it as 
readable as possible. The idea is that a Markdown-formatted document should be 
publishable as-is, as plain text, without looking like it's been marked up 
with tags or formatting instructions. While Markdown's syntax has been 
influenced by several existing text-to-HTML filters, the single biggest source 
of inspiration for Markdown's syntax is the format of plain text email.

###HHHeader

<!--Comment???-->
bullet list:

1. foo fighter
    * Foo
        * Foo Bar [links](https://en.wikipedia.org/wiki/Foobar)
    - Fluff I

1. Lists in a list item:
    - Indented four spaces.
        * indented eight spaces.
            2. indent number
                - flooof
                    i. uses four spaces not tabs
    - Four spaces again.

####HHHHeader

~~~Markdown
bullet list:

* Foo
    * Foo Bar [links](https://en.wikipedia.org/wiki/Foobar)
    * Fluff I
~~~

```python 
{i**2 for i in xrange(10**6)}
```

#####HHHHHeader

$$f(x|\mu, \sigma)=x^2$$

This is an inline $f(x|\mu, \sigma)=x^2$ equation

![Caption](http://static1.squarespace.com/static/532602ede4b09e655908a4e6/53262d3ce4b0c5c3326b0ce6/5635400fe4b06c8363eca3c5/1446330385723/Hollental-full-art-1440.jpg )

*ITALICS*

**BOLD**

