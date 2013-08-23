---
published: true
---

# Mongo Queries Reference

## Find Inner Documents

Given
```
{
	parentField1: "ABC",
	arrayField:[
    	{
    		field1: "abc"
        	field2: "def"
         },
    	{
    		field1: "ghi"
        	field2: "jkl"
         },
    ]
}
```

Find where field1 = abc
command:
```
db.MyCollection.find(
{ 
	"arrayField" : { 
		"$elemMatch" : { 
        		"field1" : "abc" 
         }
     } 
  }).limit(50);
```

# Find By Dates

Given 
```
{
	dateField1:"2013-08-05T00:00:00Z"
}
```

Mongo Query
```
db.MyCollection.find(
	{ 
    	"dateField1" : 
        	{ 
        		"$gt" : ISODate("2013-08-05T00:00:00Z") 
        	} 
     }).limit(200);
```