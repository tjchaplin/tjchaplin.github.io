---
layout: post
published: true
---
# The Erlang Diaries - Installing and creating a project with `rebar`

## Purpose

After starting out with the basics of Erlang, through the *Seven Languages In Seven Weeks*, I wanted to explore more of what Erlang has to offer.  The first part of that was looking at setting up an OTP application and structure.  This led me to [rebar](https://github.com/rebar/)

Rebar describes itself as follows:
> Erlang build tool that makes it easy to compile and test Erlang applications, port drivers and releases.

That sums it up nicely.  This addition of *The Erlang Diaries* talks about how to install and create an Erlang application with Rebar.

## Warming Up

You will need Erlang, if you don't already have it installed.  You can find the latest installations and instructions on the [Erlang website](http://www.erlang.org/download.html).

Once erlang is installed type in the following command, and confirm the output:

```
$ erl
EShell V6.3 
```

## Installing Rebar

1. Clone Rebar Github repository

```
git clone git://github.com/rebar/rebar.git
```

2. Build it

```
./bootstrap
```

3. Add rebar to your path

```
~/rebar
```

4. Open a new console window and confirm rebar

```
$ rebar
```

## Creating an application with Rebar

1. Create a new application using the default rebar template

```
$rebar create-app appid=poke
> writing src/poke.app.src
> writing src/poke_app.erl
> writing src/poke_sup.erl
```

2. Compile the new application

```
$ rebar compile
> ==> (compile)
> compiled src/poke_app.erl
> compiled src/poke_sup.erl
```

## Review

Rebar standarizes the creation of Erlang projects.  Out of the box, you can easily get going with your next awesomeo application! You don't have to think about the basics, and can get going with your implementation.

The default template provides the following components:
* poke.app.src => definintion of the application; description, version, dependencies, etc
* poke_app.erl => the entry point of the application
* poke_sup.erl => the application supervisor

Besides the basic project templates rebar can provide dependency management and more.  Take a look at the rebar site or stay tuned to the *Erlang Diaries* for more.

## See Also

* [Erlang Installation Guide](http://www.erlang.org/doc/installation_guide/INSTALL-WIN32.html)