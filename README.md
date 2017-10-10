# Gold Fever

<img src="https://raw.githubusercontent.com/inaka/gold_fever/c33f50e/priv/treasure.png" align="right" style="float:right" height="250px"/>

A Treasure Hunt for Erlangers

### Introduction
This repo provides the server for a distributed treasure hunt game using erlang nodes.
It was originally used for Inaka's Erlang Dojo 2015, but it can hopefully be used by anyone :)

### Instructions

#### Booting up the Server
To run the server just clone the repo and start the app in shell:

```bash
git clone https://github.com/inaka/gold_fever.git
cd gold_fever
make && make shell
```

Then let the players connect to it by giving them your node name and cookie (as a bonus you can give them a *clue* to get the cookie instead).

#### The Players
For the players the story begins when they boot up an erlang node and connect to the server. At that time, they'll receive the first clue that will let them move along. Once they deciphered that clue they'll have to perform another action that will lead them to the next clue, and so on so far until they eventually find the treasure.
The actual story depends on the server [configuration](#configuration). With the default configuration, the game is set up somewhere in the far west and a well known burglar has escaped with a treasure in his hands. It's the player's task to find him and retrieve the treasure to its rightful owner.

### Configuration
#### :warning: **SPOILERS SECTION** :warning:
_(don't click the links if you just want to play with the default configuration)_

The different game stages are configured by application environment. To help you with that a [sys.config.template](config/sys.config.template) is provided, along with the configuration we used at [inaka](config/inaka.config) and the [generic](config/generic.config) one that's used by default.
If you want to start the server using your own config, just do:

```bash
CONFIG="path/to/your.config" make shell
```

You can also set the IP part of your node name by doing:

```bash
NODEIP=192.168.48.33 make shell
```

### Contact Us
If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/galgo/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)
