# org-to-jira.el
### Synchronizes the current org buffer to Jira worklogs

It searchs for level two entries as Jira issue key name, and reads the clocks (clock-in, clock-out) inside the entry for the Jira worklog.

For example, given the buffer:
```
* PROJECT-ONE

** PROJECT-ONE-1
   CLOCK: [2017-01-19 Thu 14:18]--[2017-01-19 Thu 14:41] =>  0:23
   
** PROJECT-ONE-240
   CLOCK: [2017-01-20 Fri 10:33]--[2017-01-20 Fri 11:02] =>  0:29
   CLOCK: [2017-01-19 Thu 18:44]--[2017-01-19 Thu 19:08] =>  0:24
   

* TOP-PROJECT

** TOP-PROJECT-333
   Some comment or thing that will not interfere with the tool
   CLOCK: [2017-01-23 Mon 18:44]--[2017-01-23 Mon 19:04] =>  0:20
*** SomeIgnoredSubThing: 
    hello!
      
```

The following vars must be setted in order to get this working
```
(setq jira-host "https://my-jira-host.com")
(setq jira-user "super-user")
(setq jira-pass "awfulpass123")
(require 'org-to-jira)
```


#### This is a work-in-progress and it has no intentions of become a massive tool. It only works in this particular scenario and fits my tracking needs for now. 

#### Please feel free of forking and pulling for new requests.
