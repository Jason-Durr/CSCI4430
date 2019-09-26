Jason Durr
Alexander Kim

Bugs
- Beta Reduction seems to not fully reduce in many nested Apply expressions
- Eta reduction looks to be reducing when it shouldn't

Special Features
- Alpha Renaming Convention: 
    We renamed all the bound variables by concatenating the original Atom to itself
    Everytime we hit an apply statement we concatenated a L (Left) or a R (Right) to the variable being replaced depending on which expression we were renaming