---
published: true
---

#Build vs Consume

Recently when working on [Scarlet](http://www.scarletjs.com/), I came across an issue in which I had to choose between using an existing framework or creating my own function to do the samething.  This is an issue developers often face.  

Here are my findings:

##The Problem

When method is called:

1. Create a new object with and instance variable and the paramaters
2. Given a list of methods, call each of them in sequence with the new object
3. Once all are complete call a final method

Here is a sample of what needed to be called

```javascript
function someApi(instance){
	var self = this;
    self.methodsToCall = [];
    self.instanceValue = instance;

    function sequenceCaller(param1, param2){
    
    	//1. Create the new object with an instance var and the parameters
    	var newObject = new Instance(self.instanceValue,param1, param2);
        
        //2. call each method in array of methods with the new object
	    methodsToCall[0].someMethod(newObject);
	    methodsToCall[1].someMethod(newObject);
        
        //3. call a final method 
	    newObject.finalMethod();
    }
}
```

##First Choice - Consume

Instead of reinventing the wheel I knew of several **Sequence** methods that exist in other frameworks.  Here are a few:
- [async](https://github.com/caolan/async)
- [chainsaw](https://github.com/substack/node-chainsaw)

As I have used async before, I decided to go with that.

For what I needed to do above, here was the code:

```javascript
var async = require('async');

...
	function sequenceCaller(param1, param2){
    
    	//1. Create the new object with an instance var and the parameters
    	var newObject = new Instance(self.instanceValue,param1, param2);
        
        var next = function(target, callback){
			target.someMethod.apply(self,newObject,[callback,newObject]);
        }
        
         //2. call each method in array of methods with the new object
		 async.mapSeries(self.methodsToCall,
         				
                        //2.1. method to call on each function
         				next.bind(self),
                        
				        //3. call a final method                         
                        function(err, result){
                    	    newObject.finalMethod();
						});
    }

```

##Consume - Review

Overall for the change I needed this was relatively straight forward.

###Pros
- No custom code
- Get to use tried and tested code

###Cons
- Performance
- Project bloat

The **cons** are relative to [scarlet]('www.scarletjs.com') and the implemenation I was developing.  A large part of the project is focused on creating a low level base framework that is quite performant.  

##Second Choice - Build

Due to the constraints of the project I felt I needed to explore a **build** option in order to better meet my requirements(See Consume - Review -> Cons for details).

Here is the build solution :

```javascript
var async = require('async');
...
	function sequenceCaller(param1, param2){
    
    	//1. Create the new object with an instance var and the parameters
    	var newObject = new Instance(self.instanceValue,param1, param2);
        
		//2. call each method in array of methods with the new object        
        var next = function(target, callback){
        
        	//3. call a final method 
            //when the nextTarget is >= to the number of methods to call
			if(self.currentTarget >= self.methodsToCall.length){
				self.currentTarget = 0;	
				return newObject.finalMethod();
			}

			var targetMethod = self.methodsToCall[self.currentTarget];
			self.currentTarget++;
			targetCall(targetMethod);
		};
        
        //2.1. method to call on each function
        var targetCall = function(target){
				target.someMethod.apply(self,[next,newObject]);
		};
    }

```

##Build - Review

Overall the solution is **ok**  it is not as clean as the framework, however it met the requirements for being more performant and didn't bloat the project

###Pros
- Faster than the **consume** solution
- Didn't require additional frameworks on the project

###Cons
- More code to maintain and break

##Reflection

In the above example, I have shown the steps I used to make the descion of **Build vs Consume**.  I think it is important to reflect on the requirements and project goals  when making these descions.  Also, it is good to:

1. Try the consume option( this is often easy to setup and get going)
2. If that doesn't work then create your own solution
3. If you created your own solution it is best to compare the two outcomes and make your final descion on what best meets the projects requirements and goals.