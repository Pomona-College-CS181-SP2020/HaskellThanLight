# BUG 2020-05-07 #3
Problem: crew not moving properly towards right and down. Working for left and up.

Who: Gabriel

2020-05-07 There was a mitake in the use of intClamp in the movetwards function, but fixing revealed that moving in any direction is broken.

2020-05-07 Fixed issue: Sustain'Move case was removing the movelist. Now it keeps the movelist and all movement works!

# BUG 2020-05-06 #3
Problem: crew movement missaligns if order is issued mid movement. Will attempt fix by forcing to finish current move; how implementation will proceed is still unknown.

Who: Gabriel

2020-05-07 Fixed by setting current tile as the tile we are moving to and including current tile as first move in move path.

# BUG 2020-05-05 #2
Problem: issue modifying crew stateful variables after introducing more than one crew member. Likely to create a wrapper around it.

Who: Gabriel

2020-05-07 Fixed by handling every update on the stepCrewState function and mapping it over the states of the crew members.

# BUG 2020-04-30 #1

Problem: Bug with setting up SDL libraries for linux: We currently have the game running successfully on windows machines, but ideally we want to be able to run it on ubuntu, both to make it more accessible and because TravisCI uses ubuntu VMs to test code. The game runs sucessfully on Gabe's ubuntu machine, but not on mine.

Who: Joe

2020-03-05  I have installed the sdl libraries through apt, but get a bug when trying to use sdl-image to load in png images. I have a feeling it might be caused by the sdl-image library not being installed correctly?