#include<forward_list>

#include"forward_list.h"
#include"printer.hpp"
#include"random.hpp"

int main() {
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

	return 0;
}