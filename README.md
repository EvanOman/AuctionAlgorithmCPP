# Auction Algorithm

This is a repo contains two implementation of Bertsekas's Auction Algorithm. The algorithm solves the problem of optimaly assigning N objects to N people given the preferences specified in a given cost matrix.

Implementations:

1. **C++**: Complete implementation. On my machine the code could solve a size 500 assignment problem in ~.03 seconds. I compiled the code using the command `g++-5 ./auction.cpp -Ofast -std=c++14 -o auction`.
2. **Scala**: Complete implementation. After a bit of optimization, this implementation can solve a size 500 assignment problem in ~.125 seconds. I have also implemented a parallel version, which seems to outperform the sequential version once the problem size is over 5000. This implementation is still being optimized. 
