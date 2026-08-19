#include<forward_list>
#include<map>
#include<queue>
#include<set>
#include<vector>

#include"avl_tree.h"
#include"printer.hpp"

using namespace std;

class Int {
public:
	Int() = default;
	
	explicit Int(int val)
		: data(val) {
		std::cout << "Int(" << data << ")\n";
	}

	~Int() noexcept {
		std::cout << "~Int(" << data << ")\n";
	}

	friend ostream& operator<<(ostream& os, const Int& obj) {
		return os << obj.data;
	}

	int data;
};

// Create a transparent comparator for Int
struct IntCompare {
	bool operator()(const Int& a, const Int& b) const {
		return a.data < b.data;
	}

	[[nodiscard]] bool operator()(int a, const Int& b) const {
		return a < b.data;
	}

	[[nodiscard]] bool operator()(const Int& a, int b) const {
		return a.data < b;
	}

	using is_transparent = void;
};

int main() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> data2({ 7, 20, 10, 2, 9, 1 });

	{
		AVLTree<Int, Int, IntCompare> tree;
		for (const auto& val : data) {
			std::cout << "Inserting " << val << "\n";
			tree.emplace(val);
		}

		std::cout << tree.min() << " - " << tree.max() << "\n";
		if (std::next(tree.begin()) == tree.end()) {
			std::cout << "Correct\n";
		}

		AVLTree<Int, Int, IntCompare> tree2;
		tree2.insert(data2.begin(), data2.end());
		tree2.level_order();
		std::cout << "\n\n";

		tree.merge(tree2);

		printer::Printer printer;
		printer.sep(", ").alt("Empty\n");
		printer
			.prompt("Tree1: ")
			.print_range(tree.begin(), tree.end());

		printer
			.prompt("Tree2: ")
			.print_range(tree2.begin(), tree2.end());
		tree.level_order();
	}
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