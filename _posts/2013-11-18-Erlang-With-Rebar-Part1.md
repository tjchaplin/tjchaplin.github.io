---
published: true
---

## Purpose
After starting out with the basics of Erlang, through the *Seven Languages In Seven Weeks*, I wanted to explore more of what Erlang has to offer.  The first part of that was looking at setting up an OTP application and structure.  This led me to [rebar](https://github.com/rebar/)

Rebar describes itself as follows:
> Erlang build tool that makes it easy to compile and test Erlang applications, port drivers and releases.

That sums it up nicely.  Below is how to get the template application running

# The Code

1. Create a directory to put the new application

```
mkdir poke
```
2. CD into the new directory

```
cd poke
```
3. Create the application template using rebar

```
rebar create-app appid=poke
% The application stub has now been created. 
% *dir* the directory to see the files that have been created
```
4. Compile the created files

```
rebar compile
% Should compile and create an *ebin* directory with the BEAM files
```
5. Now create a release directory in the root to put the releases *rel*

```
mkdir rel
```
6. Create a rebar config file('rebar.config') in the root directory to reference the new release directory
[rebar.config]

```
{sub_dirs, ["rel"]}.
```
7. Change directory into the release directory and  create a release node 

```
rebar create-node nodeid=poke
% -> This creates items in the current directory(rel) to handle a release
```
8. Update the reltool.config file sys section to reference the files in the parent folder
From:

```
{sys, [
		%...
       {app, poke, [{mod_cond, app}, {incl_cond, include}]}
      ]}.
```
To:

```
{sys, [
		%...
       {app, poke, [{mod_cond, app}, {incl_cond, include}, {lib_dir, ".."}]}
      ]}.
```
9. cd up to root project directory and run the command to generate the release

```
rebar generate
% -> release has now been created and can be run.  Look under rel/poke
```
10. Now run your release

```
.\rel\poke\bin\poke console
% This will spin up a new console and run your release
```
# How it works

