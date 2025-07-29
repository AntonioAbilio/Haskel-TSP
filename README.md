# PFL 2024/2025 – Practical Assignment 1

This project implements a variety of graph algorithms using Haskell, centered around the concept of cities and roads (edges with distances). It explores fundamental operations on graphs such as adjacency checks, depth-first and breadth-first search, and pathfinding, culminating in a dynamic programming solution for the Travelling Salesman Problem (TSP).

## Final Grade

**19.5 / 20**

## Features

### Graph Representation

* `RoadMap` is represented as a list of tuples `(City, City, Distance)`.
* Cities are represented as `String`.
* Paths are `[City]`.
* Distances are `Int`.

### Basic Graph Operations

* `cities`: Extracts all unique cities in the graph.
* `areAdjacent`: Checks if two cities are directly connected.
* `distance`: Gets the direct distance between two cities, if available.
* `adjacent`: Lists all directly connected cities and respective distances.
* `pathDistance`: Computes total distance of a given path.

### Graph Analysis

* `cityCount`: Computes the degree (number of direct connections) of each city.
* `rome`: Returns the cities with the highest degree.
* `isStronglyConnected`: Verifies if all cities in the graph are reachable from each other.

### Traversal Algorithms

* **DFS (Depth-First Search)**: Used for connectivity analysis.
* **BFS (Breadth-First Search)**: Used for finding all paths between two cities.
* `shortestPath`: Extracts shortest paths using BFS and distance evaluation.

### Travelling Salesman Problem (TSP)

* Efficient dynamic programming implementation using bitmasking and memoization.
* Includes helper structures such as `Set` (bitmask representation) and `Table` (memoization table).
* `travelSales`: Solves the TSP and returns the optimal path.
* Placeholder `tspBruteForce`: Reserved for brute-force TSP (for groups of 3 only).

## Data Structures Used

* Custom `Queue` type for BFS.
* Bitmask-based `Set` type for subset manipulation in TSP.
* Array-based `Table` type for memoization.

## Example Graphs

Three example graphs (`gTest1`, `gTest2`, `gTest3`) are provided for testing, including a disconnected graph.
