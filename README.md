# Auction Algorithm

This is a repo contains several implementation of Bertsekas's Auction Algorithm. The code generates a random cost matrix and then solves the problem of optimaly assigning N objects to N people given the preferences specified in the cost matrix.

Implementations:

1.  **C++**: This is the only complete implementation. On my machine the code could solve a size 500 assignment problem in ~.2 seconds. I compiled the code using the command `g++-5 ./auction.cpp -Ofast -funroll-loops --std=c++14 -o auction`.
2. **Java**: Status: WIP (may be deleted)
3. **Scala**: Status: Not yet started, plan to use scalanlp/breeze for matrix operations
