#include<set>
#include<vector>
#include<algorithm>

#include"avl_tree.h"

#include"input.hpp"
#include"utility.hpp"
#include"printer.hpp"

using namespace std;

class Int {
public:
	Int() = default;
	
	explicit Int(int val)
		: data(val) {}

	friend ostream& operator<<(ostream& os, const Int& obj) {
		return os << obj.data;
	}

	int data;
};

// Create a transparent comparator for Int
//struct IntCompare {
//	bool operator()(const Int& a, const Int& b) const {
//		return a.data < b.data;
//	}
//
//	[[nodiscard]] bool operator()(int a, const Int& b) const {
//		return a < b.data;
//	}
//
//	[[nodiscard]] bool operator()(const Int& a, int b) const {
//		return a.data < b;
//	}
//
//	using is_transparent	= void;
//	using IsTransparent		= void;
//};

//[[nodiscard]] bool operator<(const Int& a, const Int& b) {
//	return a.data < b.data;
//}
//
//[[nodiscard]] bool operator<(int a, const Int& b) {
//	return a < b.data;
//}
//
//[[nodiscard]] bool operator<(const Int& a, int b) {
//	return a.data < b;
//}

int main() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });

	AVLTree<int> tree;
	for (const auto& val : data) {
		tree.emplace(val);
	}

	//tree.print(TreeOrder::LEVEL_ORDER);
	//std::ranges::move(data.begin(), data.begin() + 1, data.end() + 2);

	cout << "\nPress Enter to exit...";
	cin.get();
	return 0;
}

// Update min max nodes

/*
Nodes:	5 4 8 3 6 13 12 24
Height: 4 2 3 1 1 2  1  1
Tree:
		5
	4		8
3		  6   13
			12  24
*/