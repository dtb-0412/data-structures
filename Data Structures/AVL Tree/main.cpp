#include<map>
#include<set>
#include<queue>
#include<vector>
#include<forward_list>
#include<functional>

#include"avl_tree.h"
#include"type_traits.hpp"

using namespace std;

#define TYPE int

int main() {
	vector<TYPE> data({ 5, 4, 8, 3, 6, 13, 12, 24, });

	AVLTree<TYPE> tree;
	for (const auto& val : data) {
		tree.insert(val);
	}
	tree.print(Traversal::IN);

	const auto res = tree.find(24);
	if (res != tree.end()) {
		cout << "Found: " << *res << "\n";
	}
	else {
		cout << "Not found!\n";
	}

	cout << "\nPress Enter to exit...";
	cin.get();
	return 0;
}

// Update min max nodes

/*
Nodes:	5 4 8 3 6 13 12 24
Height: 4 2 3 1 1 2  1  1
*/