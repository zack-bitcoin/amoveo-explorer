Amoveo Explorer
=====

A work in progress. 

The purpose of this explorer is to be an open source tool to look up info about the Amoveo blockchain. It maintains complicated databases that would be inconvenient to make necessary for all full nodes.

turn it on, then open this in a browser: http://localhost:8090/main.html


Turning it on and off
=======

First make sure you have an Amoveo node running, and that the keys are unlocked on that node.

```
sh start.sh
```

To connect to it, so you can give it commands:
```
sh attach.sh
```
If it says "Node is not running!", this means that the Amoveo p2p derivatives explorer is shut off and there is nothing for you to connect to. So try using start.sh to turn it on.

To disconnect, and allow it to run in the background, hold the CTRL key, and press D.

Then to turn it off, make sure you are attached, and run:

```
utils:off().
```
then run:
```
halt().
```

