# BUG 2020-05-06 #3
Problem: crew movement missaligns if order is issued mid movement. Will attempt fix by forcing to finish current move; how implementation will proceed is still unknown.

# BUG 2020-05-05 #2
Problem: issue modifying crew stateful variables after introducing more than one crew member. Likely to create a wrapper around it.

# BUG 2020-04-30 #1

Problem: Bug with setting up SDL libraries for linux: We currently have the game running successfully on windows machines, but ideally we want to be able to run it on ubuntu, both to make it more accessible and because TravisCI uses ubuntu VMs to test code. The game runs sucessfully on Gabe's ubuntu machine, but not on mine.

Who: Joe

2020-03-05  I have installed the sdl libraries through apt, but get a bug when trying to use sdl-image to load in png images. I have a feeling it might be caused by the sdl-image library not being installed correctly?