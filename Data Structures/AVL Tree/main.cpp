#include<set>
#include<vector>
#include<string>
#include<forward_list>

#include"avl_tree.h"
#include"type_traits.hpp"

using std::cout;
using std::cin;

#define TYPE int

class IntWrapper {
public:
	IntWrapper() = default;
	
	explicit IntWrapper(int val)
		: data(val) {}

	friend std::ostream& operator<<(std::ostream& os, const IntWrapper& obj) {
		return os << obj.data;
	}

	int data;
};

// Create a transparent comparator for IntWrapper
//struct IntWrapperCompare {
//	bool operator()(const IntWrapper& a, const IntWrapper& b) const {
//		return a.data < b.data;
//	}
//
//	[[nodiscard]] bool operator()(int a, const IntWrapper& b) const {
//		return a < b.data;
//	}
//
//	[[nodiscard]] bool operator()(const IntWrapper& a, int b) const {
//		return a.data < b;
//	}
//
//	using is_transparent	= void;
//	using IsTransparent		= void;
//};

// Or overload operator< and use std::less<> since its also transparent by default
[[nodiscard]] bool operator<(const IntWrapper& a, const IntWrapper& b) {
	return a.data < b.data;
}

[[nodiscard]] bool operator<(int a, const IntWrapper& b) {
	return a < b.data;
}

[[nodiscard]] bool operator<(const IntWrapper& a, int b) {
	return a.data < b;
}

int main() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });

	AVLTree<TYPE> tree;
	for (const auto& val : data) {
		tree.emplace(val);
	}

	tree.print(TreeOrder::LEVEL_ORDER);

	cout << "\nPress Enter to exit...";
	cin.get();
	return 0;
}

// Update min max nodes

/*
Nodes:	5 4 8 3 6 13 12 24
Height: 4 2 3 1 1 2  1  1
Line:
5
4 8
3 6 13
12 24
*/