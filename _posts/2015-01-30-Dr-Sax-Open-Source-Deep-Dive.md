---
layout: post
published: true
---
# An Open Source Deep Dive with Dr. SAX

The open source deep dive series explores a select group of interesting, trending projects.  The purpose is to highlight best practices and techniques in the not so underground world of open source software.

This article explores [Dr. SAX](https://github.com/toddself/dr-sax) 

# Rating

**Install and Star**

```
npm install dr-sax
```

[Dr. SAX](https://github.com/toddself/dr-sax) describes itself as an HTML to markdown converter.  It comes with a solid set of tests, documentation, demo page, has benchmarks, and uses streams.  Pretty much covers it!

Dr. Sax is written by:

![Todd Kennedy](https://avatars2.githubusercontent.com/u/193412?v=3&s=75)

[Todd Kennedy](https://github.com/toddself) 

[@whale_eat_squid](https://twitter.com/whale_eat_squid)

[Blog](https://tck.io/)

I would recomend installing it and having a look next time you need to convert html to Markdown.

# What stands out

Dr. Sax does a couple of things worth reviewing:
* The API
* Benchmarks
* Documentation
* Tests
* Creating objects without `new`

## The API

Dr. Sax comes with a straight forward way for converting html to markdown.  The project utilizes node's core with streams, but also shows how the tool can be used without it.  Having a simple API that can be used with and without streams opens the door for more users, because streams can be a little scary if you're just getting going with Node.  Here is a sample of the API:

No Streams

```javascript
var DrSax = require('dr-sax');
var drsax = new DrSax();
drsax.write('<h1>A header </h1>');
//-> # A Header
```

With Streams

```javascript
var drSax = require('dr-sax').stream();
fs.createReadStream('header.html').pipe(drSax).pipe(process.stdout));
```

## Benchmarks

Todd created some benchmarks tests to compare his solution with others.  I think it is awesome when developers take time to compare their products to others.  It is not so much about why this product is better, but it shows that the developer really cares about their product.  They care so much that they research other solutions and see where their product stands to others.  It is interesting to note that Dr. SAX is actually the **2nd** fastest.

The tool used for benchmarking is [Benchmark](https://github.com/bestiejs/benchmark.js) by [John Dalton](https://github.com/jdalton).  It is also worth taking a look at when benchmarking your next project.

## Documentation

The documentation for the project is simple and tells you everything you need to get started.  The other great thing is the project has a Demo page.  Demo pages can really allow users to get their feet wet with the application before downloading it.

## Tests

As this is a project about Markdown, the project has choosen to run tests using John Gruber's own [project](http://daringfireball.net/projects/markdown/).  The tests also uses tools to convert from Markdown back to html to verify.  Again, I think the dedication, interest, and knowledge that the author has on the subject really shows through with the testing of the application.

## Creation of object without `new`

In javascript you don't always have to call `new` to create a new object.  Dr. SAX uses a cool trick that I see often.  In some cases when pulling in a project you may have to do the following to use a module:

```javascript
var SomeThing = require('someThing');
var someThing = new SomeThing();
```

The above makes sense in certain cases but other times I don't want to have to write another line of code, I am lazy!  From a user perspective you just want to do the following:

```javascript
var someThing = require('someThing');
```

You can do this in several ways, but I like the following:

```javascript
'use strict'

module.exports = function SomeThing(){
  if(!(this instanceof SomeThing)){
    return new SomeThing();
  }
  //...
}
```

The above simply checks if this is an instance of the object.  If not it creates a new one for you!  Simple and provides a nice user API.


# What could be better

As with any code there is always room for improvement.  The main call out that could use some reworking is the fact that with the streams implementation the author is buffering the whole string.  With streams the intention is to process the data as it comes in, as oppossed to in one big chunk.  Here is what I mean:

```javascript
//Buffering of string
StreamingDrSax.prototype._transform = function(chunk, enc, cb){
  this._htmlString += chunk.toString();
  cb();
};

//_Flush is called once all data is consumed.
StreamingDrSax.prototype._flush = function(cb){
  //gets markdown for the buffered html string 
  var markdown = this.drSax.write(this._htmlString)

  //pushes the entire makrdown back onto the stream
  this.push(markdown);
  cb();
};
```

Here is the documentation reference for [transform streams](http://nodejs.org/api/stream.html#stream_class_stream_transform_1) and [transform stream flush](http://nodejs.org/api/stream.html#stream_transform_flush_callback)

