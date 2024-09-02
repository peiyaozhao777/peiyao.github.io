# NARS Lab

## Build site

To build the website locally, clone the repo with:

```
git clone https://github.com/peiyaozhao777/peiyao.github.io.git
```

Then install necessary Ruby dependencies by running `bundle install` from within the `peiyao.github.io` directory.  After this, the site can be be built with:

```
bundle exec jekyll build
```

To view the site, run `bundle exec jekyll serve` and point a browser to `http://localhost:4000/`.  More information on Jekyll can be found [here](http://jekyllrb.com/).

To include projects, preprocessing scripts are necessary to clone project repos and update Jekyll metadata. This can be accomplished with:

```
ruby _scripts/update-and-preprocess.rb
```

Then `jekyll build` works as normal.

## Contribute

Blog posts just require YAML top matter that looks something like:

```
---
layout: post
title: A brief history of complexity science
author: Jimi Oke
link: URL
image: /images/blog/IMAGE.png
---
```

The `layout`, `title` and `author` tags are required, while `link` and `image` are optional.  Just save a Markdown file with this top matter as something like `blog/_posts/2020-09-14-history-complexity.md`, where `2020-09-14` is the date of the post and `history-complexity` is the short title.  This short title is used in the URL of the post, so this becomes `blog/history-complexity/`.
The short title should be therefore be unique to avoid conflicts.

## For more information

* See [format guide](https://narslab.org/guide/format/)
* See the [style guide](https://narslab.org/guide/style/)

## License

All source code in this repository, consisting of files with extensions `.html`, `.css`, `.less`, `.rb` or `.js`, is freely available under an MIT license, unless otherwise noted within a file.
Original code is gratefully attributed to [bedford.io](http://bedford.io).

**The MIT License (MIT)**

Copyright (c) 2020 Jimi Oke
Copyright (c) 2013-2019 Trevor Bedford

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
