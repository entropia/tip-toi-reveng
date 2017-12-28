playmus
=======

This file is taken from the `SDL_mixer` distribution. We use it on Windows,
where we could not find a free, simple, small command line tool to play `.wav`,
`.ogg` and `.mp3` files.

On Linux, the system-wide installed `sox` is used instead.

I use this command to compile `playmus.exe`

    wine gcc playmus.c -o playmus.exe -I'C:\SDL\include\SDL' -L'C:\SDL\lib' -lmingw32 -lSDLmain -lSDL  -mwindows -lSDL_mixer

This requires the development versions of `SDL-1.2` and `SDL_mixer-1.2` to be
installed accoring to [this guide](http://spin.atomicobject.com/2014/08/13/haskell-sdl-bindings-windows/).
