To build an MSYS2 binary package, open an MSYS2 terminal for your preferred environment (MINGW64, UCRT64 or CLANG64), navigate to this directory, and run *makepkg* as follows:

  makepkg-mingw -sCLf

To view the contents of the package, use *pacman* as follows:

  pacman -Qlp <filename>
