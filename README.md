# Usage
At the root of this project run

`scalac src/main/scala/Spreadsheet.scala`

`cat src/test/resources/spreadsheet.txt | scala Spreadsheet`

### Assumptions
Basically i have tried to put in assertions (defensive programming) along the way but i don't think i checked absolutely everything
- input data is well formed. 
    - Number of col and rows coincides with the data that follows
    - First row is Int Int
- there are some points in the code where i access the HashMap using `apply` because at those points in the code it an established fact that the key, value pair *IS* in the map   
    
### Considerations
- http://docs.scala-lang.org/overviews/collections/performance-characteristics.html
- What is RPN? (another way of saying postfix notation) https://en.wikipedia.org/wiki/Reverse_Polish_notation
- To detect cycle in directed graph (i didn't consult the code, just wanted to see if there are better ways) http://www.geeksforgeeks.org/detect-cycle-in-a-graph/
        
### Todo
- I wish i had more time to write more tests
- `evalExpr` is non tail recursive. Will have to relook at the whole design to see if there is any other way to do this.     