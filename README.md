Supstance
=========
A set of convinient routines which simplify child specifications generation and prevalidation.

Overview
--------

**Supstance** is actually a single module which is designed to aid with following:

 - make generation of valid supervisor childspecs clean and simple as much as possible;
 - simplify options passing to the components of your application;
 - provide childspecs prevalidation thus throw informative exceptions anytime something is wrong.
 
In fact the interface is very clean and you may consult module documentation at any time for the further details. 

Building and configuring
------------------------

In order to build a project you should execute something like that:
```
git clone git://github.com/keynslug/supstance.git
cd deep
make compile
```

Though that way this is useless so consider including it in your projects say as a rebar dependency:
```
{'deep_props', ".*", {git, "git://github.com/keynslug/supstance.git", "master"}}
```
