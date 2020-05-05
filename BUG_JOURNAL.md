# BUG 2020-04-30 #1

Problem: Bug with setting up SDL libraries for linux: We currently have the game running successfully on windows machines, but ideally we want to be able to run it on ubuntu, both to make it more accessible and because TravisCI uses ubuntu VMs to test code. The game runs sucessfully on Gabe's ubuntu machine, but not on mine.

Who: Joe

2020-03-05  I have installed the sdl libraries through apt, but get a bug when trying to use sdl-image to load in png images. I have a feeling it might be caused by the sdl-image library not being installed correctly?