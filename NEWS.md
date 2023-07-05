# rxnorm 0.2.1.9000

* Added a `NEWS.md` file to track changes to the package.
* Changed the 'relasource' for `get_atc` from 'ATC' to 'ATCPROD'. See https://lhncbc.nlm.nih.gov/RxNav/news/ATCPROD-202307.html for more information.
* Added the optional argumnet `prod` to `get_atc`. Setting to `FALSE` will return the ATCs associated with a drugs ingredient.
