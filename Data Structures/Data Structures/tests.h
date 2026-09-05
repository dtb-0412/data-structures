#pragma once

#include<forward_list>
#include<map>
#include<set>
#include<stack>
#include<vector>

#include"./include/avl_tree.h"
#include"./include/dynamic_array.h"
#include"./include/forward_list.h"
#include"./include/map.h"
#include"./include/rb_tree.h"
#include"./include/set.h"
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

void dynamic_array_test() {
	DynamicArray<int> arr;
	arr.reserve(20);

	for (auto i = 0; i < 10; ++i) {
		arr.emplace_back(i);
	}
	arr.insert(arr.end(), 5, 99);
	arr.emplace(arr.begin(), arr[5]);

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
	flist1.sort();
	flist1.remove_if(
		[&](const auto& val) -> bool { return val % 2 == 0; },
		std::next(flist1.begin(), flist1.size() / 2 - 1), flist1.end()
	); // Remove all even elements from mid range to end

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