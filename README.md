# HaskeDex

My attempt to recreate the "hcal" pager application described in the "Effective Haskell" book for learning purposes

Simply put, program works a lot like `less` linux terminal command for displaying the contents of a text file.

# Notable deviations from the book

- The project doesn't include status line displaying file informaiton
- The `groupsOf` function is modified as the original implementation somehow gave me infinite lists when I used it (likely skill issue on my end).
- I avoided the use of the `ByteString` module as I couldn't get it to work (likely skill issue on my end).
- I added another validation step for empty strings `""` for the `handleArgs` function
- I would occasionally use the `case .. of` style of pattern matching while following along with the orignal that uses a different style of pattern matching.
- I created a separate file where the functions are translated as `do` blocks where possible

# How to run the project

1. Make sure you have Haskell and Cabal installed. I installed and managed Haskell and Cabal verions via `ghcup` ([available by following the instructions here](https://www.haskell.org/ghcup/#))
2. Go to the root of the HaskeDex project folder and run `cabal build`
3. While at the root folder, run `cabal run HaskeDex HaskeDex.cabal` to display the contents of the `HaskeDex.cabal` file in the terminal
