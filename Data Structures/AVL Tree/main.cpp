#include<map>
#include<set>
#include<queue>
#include<vector>
#include<forward_list>

#include"avl_tree.h"

using namespace std;

int main() {
	//priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;

	vector<int> data({ 12, 8, 3, 5, 4, 6, 13, 24 });
	
	///*
	AVLTree<int> myTree;
	myTree.insert(data.begin(), data.end());
	myTree.print(myTree.LEVEL);

	//myTree.emplaceHint(next(myTree.begin(), 0), 9);
	myTree.erase(myTree.begin());

	cout << "\n";
	myTree.print(myTree.LEVEL);
	//*/

	/*
	std::set<int> mySet(data.begin(), data.end());
	cout << "My set: ";
	for (const auto v : mySet) {
		cout << v << " ";
	}
	cout << "\n";

	for (auto i = prev(mySet.end(), 1);;) {
		cout << "Erase " << *i << "\n";
		i = mySet.erase(i);
		if (i == mySet.begin()) {
			break;
		}
		--i;
	}

	cout << "My set: ";
	for (const auto v : mySet) {
		cout << v << " ";
	}
	cout << "\n";
	*/

	/*
	std::map<int, bool> myMap; // is even values
	for (const auto v : data) {
		myMap.emplace(v, v % 2 == 0);
	}

	auto i = next(myMap.begin(), 4);
	cout << "I: " << i->first << " - " << (i->second ? "Even" : "Odd") << "\n";
	i->second = false; // OK
	// i->first = 99; // Non-const

	cout << "My set:\n";
	for (const auto p : myMap) {
		cout << p.first << ": " << (p.second ? "Even" : "Odd") << "\n";
	}
	cout << "\n";
	*/

	return 0;
}

// Update min max nodes

/*
Nodes:	5 4 8 3 6 13 12 24
Height: 4 2 3 1 1 2  1  1

*/