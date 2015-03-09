---
layout: post
published: true
---
# The Erlang Diaries - Simple Unit Testing

## Purpose

This addition of *The Erlang Diaries* discusses simple unit testing with rebar and eunit.  Testing is fundamental to any development project.  Erlang provides `eunit` to perform unit testing.  

## Getting Started

I hope by reading this you already have Erlang installed, but if not you can find the latest installations and instructions on the [Erlang website](http://www.erlang.org/download.html).

You will also need `rebar` and an initial rebar project.  You can initialize a project with the following command:

```
$ rebar create-app appid=poke
```

For more information on how to install and setup a project in rebar check out this [diary entry](http://tjchaplin.github.io/2015/03/05/The-Erlang-Diaries---Installing-rebar.html).

## Testing First

1. Create a directory to hold the unit tests(test/unit):
	
	```
	$ mkdir test; mkdir test/unit
	```
2. Create a basic test file (`test/unit/poker_tests.erl`) with the following contents:

	```erl
	-module(poker_tests).
	
	-include_lib("eunit/include/eunit.hrl").
	```
3. Create an assertion to confirm a poke response: `when you call poke the response is hehe`:
	
	```erl
	when_calling_poke_should_respond_hehe_test() ->
		Result = poker:poke(),
		?assert(Result == <<"hehe">>).
	```

Now you have a failing test!  Go on to the next step and make it pass.

## Making it pass

1. Run the test with rebar

	```
	$rebar eunit
	> poker_tests: when_calling_poke_should_respond_hehe_test (module 'poker_tests')...*failed*
	```
2. Add missing object by creating a new poker module named poker.erl

	```erl
	-module(poker).
	```
3. Add a function called poke

	```erl
	-module(poker).
	
	poke() ->
		<<"hehe">>.
	```
4. Export the function

	```erl
	-module(poker).
	-export([poke/0]).
	
	poke() ->
		<<"hehe">>.
	```
5. Compile 
	
	```
	$ rebar compile
	```
6. Remove old tests

	```
	$ rm -rf .eunit
	```
7. Rerun the tests

	```
	$ rebar eunit
	> Compiled src/poke_sup.erl
	> Compiled src/poke_app.erl
	> Compiled src/poker.erl
	> Compiled test/unit/poker_tests.erl
	>	Test Passed.
	```

## Making it better

The build tasks needed to be wrapped up, so that we don't have to repeat ourselves each test run.  The best way to do 
this is to use a `Makefile`.

Here is a Makefile to accomplish the testing tasks:

```makefile
REBAR=rebar

.PHONY: compile test clean

compile:
	@$(REBAR) skip_deps=true compile

test: clean
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
``` 

To run the tests simply use make:

```
$ make test
```

* Note for windows you can install make with cygwin

## A Couple of Notes
* test modules must have suffix of `tests`; ie. myfile_tests
* Each unit test within a test file must have a suffix of `test`
	
	```erl
	When_Something_Should_Do_Something_test() -> 
		blah.
	```
* When running tests it is best to skip dependencies, so that dependent module tests don't get run.  Use the following command to only run test within your project:
	
	```
	$ rebar skip_deps=true eunit
	```
	
## Review

With the above example you should feel at home with testing.  It is not very different from other testing frameworks.  Rebar makes compiling and running the tests dead simple.  The nuances with testing are to do with the naming of the test module and test entries.


## See Also

* [Learn You Some Erlang for greater good! - eunit](http://learnyousomeerlang.com/eunit)
* [Erlang Eunit Guide](http://www.erlang.org/doc/apps/eunit/chapter.html)
