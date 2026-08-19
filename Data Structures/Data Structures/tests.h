#pragma once

#include<vector>

#include"./include/avl_tree.h"
#include"./include/dynamic_array.h"
#include"./include/forward_list.h"
#include"./include/set.h"
#include"common.h"
#include"printer.hpp"

void avl_tree_test() {
	std::vector<int> data({ 5, 4, 8, 3, 6, 13, 12, 24, });
	std::vector<int> data2({ 7, 20, 10, 2, 9, 1 });

	{
		AVLTree<Type<int>, TypeCompare> tree;
		for (const auto& val : data) {
			std::cout << "Inserting " << val << "\n";
			tree.emplace(val);
		}

		AVLTree<Type<int>, TypeCompare> tree2;
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

void set_test() {

}