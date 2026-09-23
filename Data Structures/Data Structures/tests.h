#pragma once

#include<deque>
#include<forward_list>
#include<list>
#include<map>
#include<queue>
#include<set>
#include<stack>
#include<unordered_map>
#include<vector>

#include"./include/avl_tree.h"
#include"./include/deque.h"
#include"./include/dynamic_array.h"
#include"./include/forward_list.h"
#include"./include/list.h"
#include"./include/map.h"
#include"./include/rb_tree.h"
#include"./include/set.h"
#include"./include/stack.h"
#include"common.h"
#include"printer.hpp"
#include"random.hpp"

#define TYPE int
#define COMP std::less<> //TypeCompare

void avl_tree_test() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> data2({ 7, 20, 10, 2, 9, 1 });

	{
		AVLTree<TYPE, COMP> tree;
		for (const auto& val : data) {
			tree.emplace_hint(tree.end(), TYPE(val));
		}

		AVLTree<TYPE, COMP> tree1(tree);

		AVLTree<TYPE, COMP> tree2;
		tree2.insert(data2.begin(), data2.end());
		tree2.level_order();
		std::cout << "\n\n";

		tree1.erase(8);
		//tree1.merge(tree2);

		printer::Printer printer;
		printer.sep(", ").alt("Empty\n");
		printer
			.prompt("Tree1: ")
			.print_range(tree1.begin(), tree1.end());

		printer
			.prompt("Tree2: ")
			.print_range(tree2.begin(), tree2.end());
		tree1.level_order();
	}

	std::cout << "\nPress any key to exit...";
	std::cin.get();

	/*
	Nodes:	5 4 8 3 6 13 12 24
	Height: 4 2 3 1 1 2  1  1
	Tree:
			5
		4		8
	3		  6   13
				12  24
	*/
}

void deque_test() {
#pragma pack(push, 1)
	struct _13BytesStruct {
		int*	_8Bytes;
		int		_4Bytes;
		bool	_1byte;
	};
#pragma pack(pop)
	Deque<_13BytesStruct> deque;

	//const Deque<int> deque({ 4, 3, 8, 0, 7, 5, 1, 9, 2 });
	//deque.assign({ 4, 3, 8, 0, 7, 5, 1, 9, 2 });
	//deque.resize(5);
	//deque.clear();

	printer::Printer printer;
	printer.sep(", ").alt("Empty\n");
	
	//std::cout << "Size: " << deque.size() << "\n";
	//deque.print_map();
	//
	//printer
	//	.prompt("\nMy deque: ")
	//	.print_range(deque.begin(), deque.end());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

void dynamic_array_test() {
	DynamicArray<int> arr;
	arr.reserve(20);

	for (auto i = 0; i < 10; ++i) {
		arr.emplace_back(i);
	}
	arr.insert(arr.end(), 5, 99);
	arr.emplace(arr.begin(), arr[5]);
	arr.erase(arr.begin(), arr.begin() + 5);
	//arr.assign(13, 22);

	printer::Printer printer;
	printer.sep(", ").alt("Empty\n");
	printer
		.prompt("Array: ")
		.print_range(arr.begin(), arr.end());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

void forward_list_test() {
	ForwardList<int> flist1({ 1, 5, 8, 4, 10 });
	ForwardList<int> flist2({ 3, 7, 2, 9, 6 });

	flist1.splice_after(std::next(flist1.begin(), flist1.size() - 1), flist2);
	flist1.sort_after(std::next(flist1.before_begin(), 5), flist1.end());
	std::cout << "Sort after range (" << 
		*std::next(flist1.before_begin(), 5) << ", " << 
		*std::next(flist1.begin(), flist1.size() - 1) << ")\n";
	//flist1.resize(20, 7);
	//flist1.remove_if_after(
	//	[&](const auto& val) -> bool { return val % 2 == 0; },
	//	std::next(flist1.begin(), flist1.size() / 2 - 1), flist1.end()
	//); // Remove all even elements from mid range to end
	//flist1.erase_after(std::next(flist1.begin(), 9), std::next(flist1.begin(), flist1.size() - 1));
	//flist1.assign({ 3, 2, 1 });
	//flist1.resize(12, 99);

	printer::Printer printer;
	printer.sep(printer::COMMA).alt("Empty\n");

	printer
		.prompt("Flist1: ")
		.print_range(flist1);
	printer
		.prompt("Flist2: ")
		.print_n(flist2.begin(), std::distance(flist2.begin(), flist2.end()));

	printer
		.sep("")
		.print_args("Size: ", flist1.size(), " - ", flist2.size());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

void list_test() {
	List<int> list1({ 1, 5, 8, 4, 10 });
	List<int> list2({ 3, 7, 2, 9, 6 });

	list1.splice(std::next(list1.begin(), list1.size()), list2);
	list1.sort();
	std::cout << "Sort after range [" <<
		*std::next(list1.begin(), 4) << ", " <<
		*std::prev(list1.end(), 1) << "]\n";
	//list1.remove_if(
	//	[&](const auto& val) -> bool { return val % 2 == 0; },
	//	std::next(list1.begin(), list1.size() / 2 - 1), list1.end()
	//); // Remove all even elements from mid range to end
	////list1.erase(std::next(list1.begin(), 9), std::next(list1.begin(), list1.size() - 1));
	//list1.assign({ 3, 2, 1 });
	//list1.resize(3);
	//list1.reverse(list1.begin(), list1.end());

	printer::Printer printer;
	printer.sep(printer::COMMA).alt("Empty\n");

	printer
		.prompt("Flist1: ")
		.print_range(list1.begin(), list1.end());
	printer
		.prompt("Flist2: ")
		.print_n(list2.begin(), std::distance(list2.begin(), list2.end()));

	printer
		.sep("")
		.print_args("Size: ", list1.size(), " - ", list2.size());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

struct MapDefaultPrint {
	template<concepts::printable T, concepts::printable U>
	void operator()(std::ostream& os, const std::pair<const T, U>& val) const {
		os << "[" << val.first << "]" << ": " << std::fixed << std::setprecision(1) << val.second;
	}
};

void map_test() {
	random::RandomGenerator<std::uniform_real_distribution<>> rng(0.0, 100.0);

	std::vector<int> key1({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> key2({ 7, 20, 10, 2, 9, 1 });

	std::vector<std::pair<TYPE, double>> data, data2;
	data.reserve(key1.size());
	data2.reserve(key2.size());
	for (const auto& key : key1) {
		data.emplace_back(key, rng.next());
	}
	for (const auto& key : key2) {
		data2.emplace_back(key, rng.next());
	}

	{
		Map<TYPE, double, COMP> map;
		for (const auto& val : data) {
			map.emplace(val);
		}

		Map<TYPE, double, COMP> map1(map);
		Map<TYPE, double, COMP> map2;
		map2.insert(data2.begin(), data2.end());
		map2.level_order();
		std::cout << "\n\n";

		map1.erase(8);
		map1.merge(map2);

		printer::Printer printer;
		printer.sep(", ").alt("Empty\n");
		printer
			.prompt("Map1: ")
			.print_range(map1.begin(), map1.end(), MapDefaultPrint{});
		printer
			.prompt("Map2: ")
			.print_range(map2.begin(), map2.end(), MapDefaultPrint{});
		map1.level_order();
	}
}

void rb_tree_test() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> data2({ 7, 20, 10, 2, 9, 1 });

	{
		RBTree<TYPE, COMP> tree;
		for (const auto& val : data) {
			tree.emplace_hint(tree.end(), TYPE(val));
		}

		RBTree<TYPE, COMP> tree1(tree);

		RBTree<TYPE, COMP> tree2;
		tree2.insert(data2.begin(), data2.end());
		tree2.level_order();
		std::cout << "\n\n";

		tree1.erase(8);
		//tree1.merge(tree2);

		printer::Printer printer;
		printer.sep(", ").alt("Empty\n");
		printer
			.prompt("Tree1: ")
			.print_range(tree1.begin(), tree1.end());

		printer
			.prompt("Tree2: ")
			.print_range(tree2.begin(), tree2.end());
		tree1.level_order();
	}

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

void set_test() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> data2({ 7, 20, 10, 2, 9, 1 });

	{
		Set<TYPE, COMP> set;
		for (const auto& val : data) {
			set.emplace(TYPE(val));
		}

		Set<TYPE, COMP> set2;
		set2.insert(data2.begin(), data2.end());
		set2.level_order();
		std::cout << "\n\n";

		set.merge(set2);

		printer::Printer printer;
		printer.sep(", ").alt("Empty\n");
		printer
			.prompt("set1: ")
			.print_range(set.begin(), set.end());

		printer
			.prompt("set2: ")
			.print_range(set2.begin(), set2.end());
		set.level_order();
	}

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}

void stack_test() {
	//Stack<int> arr;

	//for (auto i = 0; i < 10; ++i) {
	//	arr.push(i);
	//}

	//printer::Printer printer;
	//printer.sep(", ").alt("Empty\n");
	//printer
	//	.prompt("Array: ")
	//	.print_range(arr.begin(), arr.end());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
}