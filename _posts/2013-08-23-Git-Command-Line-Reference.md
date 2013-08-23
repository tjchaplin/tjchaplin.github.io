---
published: false
---

Using the shell/terminal is faster than selecting, right clicking, and using tools like [tourtise git](https://code.google.com/p/tortoisegit/).

This is a quick cheat cheat/reference for when you want to *type* and not *select*.

For the full reference and docs go to the [Git site](http://git-scm.com/docs).

## Stashing

Stashing allows your uncommited changes to be *stashed away*. This allows your current working branch to return to last committed changes.  you might want to:

* Create a new branch with your stashed changes
* Pull the latest source, but not commit your changes

Here is the command:

```
git stash
```

## Branching

Branching allows your code to continue to move forward without independently of other codes.

### Create New Branch

```
git branch MyFeatureBranch
```

### Checkout to new branch multiple commands

```
git branch MyFeatureBranch
git checkout MyFeatureBranch
```

### Checkout to new branch single command

```
git checkout -b MyFeatureBranch
```
