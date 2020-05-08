# HaskellThanLight

HaskellThanLight is an imitation of the game FTL: Faster Than Light by Subset Games, written in Haskell. In our simplified imitation of the game, you command a ship in a combat scenario. You can move your crew members from room to room, and you can damage the enemy ship by clicking your weapon box, then clicking the enemy ship.  

# Setup
This build has only been proven to work on windows, but may work on other systems.
To run the game on windows, clone the repository and then run

`stack exec -- pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-SDL2 mingw64/mingw-w64-x86_64-SDL2_ttf mingw64/mingw-w64-x86_64-SDL2_image mingw64/mingw-w64-x86_64-SDL2_mixer mingw64/mingw-w64-x86_64-libmikmod`

`stack build`

`stack exec HaskellThanLight-exe`
