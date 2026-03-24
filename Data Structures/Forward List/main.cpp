#include<forward_list>

#include"utility.hpp"
#include"forward_list.h"

using namespace std;
using namespace util;

int main() {
	ForwardList<int> flist1({ 1, 5, 8, 4, 10 });
	ForwardList<int> flist2({ 3, 7, 2, 9, 6 });

	flist1.splice_after(std::next(flist1.begin(), std::distance(flist1.begin(), flist1.end()) - 1), flist2);

	Printer flPrinter;
	flPrinter.sep(Printer::COMMA).alt("Empty\n");

	cout << "Flist1: ";
	flPrinter.print(flist1.begin(), flist1.end());
	cout << "Flist2: ";
	flPrinter.print(flist2.begin(), flist2.end());

	return 0;
}