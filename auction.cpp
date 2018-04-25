#include <iostream>
#include <fstream>
#include <vector>
#include <limits>
#include <stdlib.h>
#include <math.h>
#include <chrono>
#include <algorithm>
#include <string.h>
#include <stdio.h>
#include <tuple>
using namespace std;

#define INF numeric_limits<int>::max()
#define VERBOSE false

/* Pre-declare functions to allow arbitrary call ordering  */
void randAuction(int N);
void auction(int N, vector<int> C);
void auctionRound(vector<int>* assignment, vector<double>* prices, vector<int>* C, double epsilon);
vector<int> makeRandC(int size);
tuple<int, vector<int>> readMatrix(char* fName);
template <typename T> void printMatrix(vector<T>* mat, int size);
template <typename T> vector<int> getIndicesWithVal(vector<T>* v, T val);
template <typename T> void reset(vector<T>* v, T val);
template <typename T> void printVec(vector<T>* v);
void runRand();

int main(int argc, char* argv[])
{
	if (argc > 2 && strcmp(argv[1], "-fname") == 0)
	{
		/* Read in the file */
		cout << "Reading in file: " << argv[2] << "\n";
		tuple<int, vector<int>> matrixTuple = readMatrix(argv[2]);
		int probSize = get<0>(matrixTuple);
		vector<int> C = get<1>(matrixTuple);
		cout << "Running auction" << endl;
		auction(probSize, C);
	}
	else
	{
		/* Run random */
		runRand();
	}
	return 0;
}

void runRand()
{
	cout << "Enter 0 if 500x500 problem size, 1 for custom size: ";
	int isCustom;
	cin >> isCustom;
	if (isCustom)
	{
		cout << "Please enter a problem size: ";
		int probSize;
		cin >> probSize;
		randAuction(probSize);
	}
	else
	{
		/* Run on fixed size problem */
		randAuction(500);
	}
}

void randAuction(int N)
{
	vector<int> C = makeRandC(N);
	auction(N, C);
}

void auction(int N, vector<int> C)
{
	if (VERBOSE)
	{
		cout << "Cost matrix: " << endl;
		printMatrix(&C, N);
	}

	/* Begin Time */
	auto t1 = std::chrono::high_resolution_clock::now();
	clock_t start = clock();


	vector<int> assignment(N, INF);
	vector<double> prices(N, 1);
	double epsilon = 1.0;
	int iter = 1;

	while(epsilon > 1.0/N)
	{
		reset(&assignment, INF);
		while (find(assignment.begin(), assignment.end(), INF) != assignment.end())
		{
			iter++;
			auctionRound(&assignment, &prices, &C, epsilon);

		}
		epsilon = epsilon * .25;
	}


	clock_t end = clock();
	

	/* End Time */
	auto t2 = std::chrono::high_resolution_clock::now();
	double timing = std::chrono::duration_cast<std::chrono::milliseconds>(t2-t1).count();
	double time = (double) (end-start) / CLOCKS_PER_SEC * 1000.0;

	cout << "Num Iterations:\t" << iter << endl;
	cout << "Total time:\t" << timing / 1000.0 << endl;
	cout << "Total CPU time:\t" << time << endl;

	if (VERBOSE)
	{
		cout << endl << endl << "Solution: " << endl;
		for (int i = 0; i < assignment.size(); i++)
		{
			cout << "Person " << i << " gets object " << assignment[i] << endl;
		}
	}
}

void auctionRound(vector<int>* assignment, vector<double>* prices, vector<int>* C, double epsilon)
{

	/* Prints the assignment and price vectors */
	if (VERBOSE)
	{
		cout << endl << "Assignment: \t\t";
		printVec(assignment);
		cout << "prices: \t\t";
		printVec(prices);
		cout << endl;
	}

	int N = prices->size();

	/* 
		These are meant to be kept in correspondance such that bidded[i]  
		and bids[i] correspond to person i bidding for bidded[i] with bid bids[i]
	*/
	vector<int> tmpBidded;
	vector<double> tmpBids;
	vector<int> unAssig;

	/* Compute the bids of each unassigned individual and store them in temp */
	for (int i = 0; i < assignment->size(); i++)
	{
		if (assignment->at(i) == INF)
		{
			unAssig.push_back(i);

			/* 
				Need the best and second best value of each object to this person
				where value is calculated row_{j} - prices{j}
			*/
			double optValForI = -INF;
			double secOptValForI = -INF;
			int optObjForI, secOptObjForI;
			for (int j = 0; j < N; j++)
			{
				double curVal = C->at(j + i*N) - prices->at(j);
				if (curVal > optValForI)
				{
					secOptValForI = optValForI;
					secOptObjForI = optObjForI;
					optValForI = curVal;
					optObjForI = j;
				}
				else if (curVal > secOptValForI)
				{
					secOptValForI = curVal;
					secOptObjForI = j;
				}
			}

			/* Computes the highest reasonable bid for the best object for this person */
			double bidForI = optValForI - secOptValForI + epsilon;

			/* Stores the bidding info for future use */
			tmpBidded.push_back(optObjForI);
			tmpBids.push_back(bidForI);
		}
	}

	/* 
		Each object which has received a bid determines the highest bidder and 
		updates its price accordingly
	*/
	for (int j = 0; j < N; j++)
	{
		vector<int> indices = getIndicesWithVal(&tmpBidded, j);
		if (indices.size() != 0)
		{
			/* Need the highest bid for object j */
			double highestBidForJ = -INF;
			int i_j;
			for (int i = 0; i < indices.size(); i++)
			{
				double curVal = tmpBids.at(indices.at(i));
				if (curVal > highestBidForJ)
				{
					highestBidForJ = curVal;
					i_j = indices.at(i);
				}
			}

			/* Find the other person who has object j and make them unassigned */
			for (int i = 0; i < assignment->size(); i++)
			{
				if (assignment->at(i) == j)
				{
					if (VERBOSE)
						cout << "Person " << unAssig[i_j] << " was assigned object " << i << " but it will now be unassigned" << endl;
					assignment->at(i) = INF;
					break;
				}
			}
			if (VERBOSE)
				cout << "Assigning object " << j << " to person " << unAssig[i_j]<< endl;

			/* Assign object j to i_j and update the price vector */
			assignment->at(unAssig[i_j]) = j;
			prices->at(j) = prices->at(j) + highestBidForJ;
		}
	}
}


/*<--------------------------------------   Utility Functions   -------------------------------------->*/

vector<int> makeRandC(int size)
{
	srand (time(NULL));
	vector<int> mat(size * size, 2);
	for(int i = 0; i < size; i++)
	{
		for(int j = 0; j < size; j++)
		{
			mat[i + j * size] = rand() % size + 1;
		}
	}
	return mat;
}

tuple<int, vector<int>> readMatrix(char* fName)
{
	vector<string> lines;
	vector<int> ints;
	ifstream infile_buf(fName);
	string line;
	int counter = 0;
	while(getline(infile_buf, line))
	{
		counter++;
		lines.push_back(line);
		char * pch;
		char lineC[10240];
		strcpy(lineC, line.c_str());
		pch = strtok (lineC,"\t");
		while (pch != NULL)
		{
			ints.push_back(atoi(pch));
			pch = strtok (NULL, "\t");
		}
	}
	return make_tuple(counter, ints);
}

template <typename T> void printMatrix(vector<T>* mat, int size)
{
	for(int i = 0; i < size; i++)
	{
		for(int j = 0; j < size; j++)
		{
			cout << mat->at(i + j * size) << "\t";
		}
		cout << endl;
	}
}

template <typename T> void printVec(vector<T>* v)
{
	for(int i = 0; i < v->size(); i++)
	{
		if (v->at(i) == INF)
		{
			cout << "INF" << "\t";
		}
		else
		{
			cout << v->at(i) << "\t";
		}
	}
	cout << endl;
}

/* Returns a vector of indices from v which have the specified value val */
template <typename T> vector<int> getIndicesWithVal(vector<T>* v, T val)
{
	vector<int> out;
	for (int i = 0; i < v->size(); i++)
	{
		if (v->at(i) == val)
		{
			out.push_back(i);
		}
	}
	return out;
}

template <typename T> void reset(vector<T>* v, T val)
{
	for (int i = 0; i < v->size(); i++)
	{
		v->at(i) = val;
	}
}
