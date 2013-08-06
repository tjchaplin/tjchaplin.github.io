---
published: false
---

#Build vs Consume

Recently when working on [Scarlet](http://www.scarletjs.com/), I came across an issue in which I had to choose between using an existing framework or creating my own function to do the samething.  This is an issue developers often face.  

Here are my findings:

#The Problem

When method is called:

1. Create a new object with and instance variable and the paramaters
2. Given a list of methods, call each of them in sequence with the new object
3. Once all are complete call a final method

Here is a sample of what needed to be called

```javascript
function someApi(instance){
	var self = this;
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

#First Choice - Consume

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


