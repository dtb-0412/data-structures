#include<vector>

#include"dynamic_array.h"
#include"printer.hpp"

int main() {
	DynamicArray<int> arr;
	arr.reserve(20);

	for (auto i = 0; i < 10; ++i) {
		arr.emplace_back(i);
	}
	arr.insert(arr.end(), 5, 99);
	arr.emplace(arr.begin(), arr[5]);

	printer::Printer p;
	p.sep(", ").alt("Empty\n");
	p.print_range(arr.begin(), arr.end());

	std::cout << "\nPress any key to exit...";
	std::cin.get();
	return 0;
}