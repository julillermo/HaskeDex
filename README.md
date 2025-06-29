# HaskeDex

My attempt to recreate the "hcat" pager application described in the "Effective Haskell" book for learning purposes

Simply put, program works a lot like the `less` linux terminal command for displaying the contents of a text file.

The name was inspired by "[Pokedex](https://bulbapedia.bulbagarden.net/wiki/Pok%C3%A9dex)" because of the similarity of the program's intended purpose. A Pokedex is meant to show the details of a pokemon found in the wild while the HaskeDex is meant to show the details of files on a computer

# Notable deviations from the book

- The project doesn't include status line displaying file informaiton. It instead shows the file information when you pass `info` as an argument after the filepath.
  - For example: `cabal run HaskeDex HaskeDex.cabal info`
- The `groupsOf` function is modified as the original implementation somehow gave me infinite lists when I used it (likely a skill issue on my end).
- I avoided the use of the `ByteString` module as I couldn't get it to work (likely a skill issue on my end).
- I added another validation step for empty strings `""` for the `handleArgs` function
- I would occasionally use the `case .. of` style of pattern matching while following along with the orignal that uses a different style of pattern matching.
- I refactord the code to their `do` block equivalent as much as possible

# How to run the project

1. Make sure you have Haskell and Cabal installed. I installed and managed Haskell and Cabal verions via `ghcup` ([available by following the instructions here](https://www.haskell.org/ghcup/#))
2. Go to the root of the HaskeDex project folder and run `cabal build`
3. While at the root folder, run the following to display them in the terminal:
   - `cabal run HaskeDex HaskeDex.cabal` to display the contents of the `HaskeDex.cabal` file
   - `cabal run HaskeDex HaskeDex.cabal info` to display the file information of the file
