#define SIZE 10
#define TYPE int

#include<forward_list>

#include"random.hpp"
#include"utility.hpp"
#include"forward_list.h"

using namespace std;

template<class FwdIter>
void printList(FwdIter first, const FwdIter last) {
	if (first == last) {
		cout << "Empty\n";
	} else {
		for (; first != last; ++first) {
			cout << *first << "  ";
		}
		cout << "\n";
	}
}

int main() {

	cout << "\nPress any key to exit...";
	std::cin.get();
	return 0;
}