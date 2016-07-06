# README #

### Prepare yourself for Charles the Penguin and the USSHeaven! ###

* USSHeaven is an RPG about a cyberpunk family living on a ship orbiting the earth.
* Gameplay, lore, and code structure are closely linked.
* Type enforced everything!  Despite a heavy use of (object) global state, USSHeavengine's level database search functions - and AI functions which rely on them - are carefully typed to prevent me from allowing AIs to make queries irrelevant to the concerns of their constituent condition-behavior objects.
* Highly generalized abilities and buffs/debuffs.  Layers of abstraction allow quick additions, but type-safety prevents non-sensical behavior.  Anything (not stupid) is possible!
* Lots of cute bishies . . . once I finish learning to draw >.>  Right now the game is purely text, but I'm hoping to make it into more of a visual novel.
* Space!  The game takes place on an abstract, undirected graph.  I was previously going for Cartesian spacial constraints, but if I make the rooms memorable enough, an abstract graph won't be too confusing, right?
* Behavior graph directed conversations.  Every node of speech branches conditional reply nodes.  If no condition is met, there is an awkward silence until the need to speak arises again.

### Running the Game ###
* As of 2/Jan/2015, USSHeaven does not build, though there are several lovely unconnected constituent parts.
* USSHeaven requires the Haskell Platform, as well as the package "netwire" (available on Cabal).  I will add my own Cabal stuff later.  Sorry about the inconvenience.
* When I do get things running, you will want to open Play.hs with GHCi.

### Reading the Code ###
* Try this order: 
* Stat
* UnitSheet
* Identifiers
* World
* Action
* View 
* UnitView
* UnitAction
* DmgCalc
* AI
* WorldAdvance
* Play
* Take a look at MathUtil for some fun combinator functions.