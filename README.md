# Auction Algorithm CPP

This is a repo contains a `C++` implementation of [Bertsekas's Auction Algorithm](http://dspace.mit.edu/bitstream/handle/1721.1/3233/P-2064-24690022.pdf?sequence=1). The algorithm solves the problem of optimally assigning N objects to N people given the preferences specified in a given cost matrix.

On my machine the code could solve a size 500 assignment problem in ~.03 seconds. I compiled the code using the command `g++-5 ./auction.cpp -Ofast -std=c++14 -o auction`.