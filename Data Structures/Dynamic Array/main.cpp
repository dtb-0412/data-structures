#include<vector>
#include<forward_list>
#include<iostream>

#include"random.hpp"
#include"dynamic_array.h"

using namespace std;

class Int {
public:
	Int()
		: val() {
		std::cout << "Int::Int()\n";
	};

	explicit Int(int val)
		: val(val) {
		std:: cout << "Int::Int(" << val << ")\n";
	}

	Int(const Int& other)
		: val(other.val) {
		std::cout << "Int::Int(const Int&)\n";
	}

	Int(Int&& other)
		: val(std::move(other.val)) {
		std::cout << "Int::Int(Int&&)\n";
	}

	Int& operator=(const Int& other) {
		std::cout << "Int::operator=(const Int&)\n";
		val = other.val;
		return *this;
	}

	Int& operator=(Int&& other) {
		std::cout << "Int::operator=(Int&&)\n";
		val = std::move(other.val);
		return *this;
	}

	~Int() {
		std::cout << "Int::~Int(" << val << ")\n";
	}

	friend std::ostream& operator<<(std::ostream& os, const Int& i) {
		os << i.val;
		return os;
	}

public:
	int val{};
};

bool operator==(const Int& lhs, const Int& rhs) {
	return lhs.val == rhs.val;
}

bool operator!=(const Int& lhs, const Int& rhs) {
	return !(lhs == rhs);
}

bool operator<(const Int& lhs, const Int& rhs) {
	return lhs.val < rhs.val;
}

bool operator>(const Int& lhs, const Int& rhs) {
	return rhs < lhs;
}

bool operator<=(const Int& lhs, const Int& rhs) {
	return !(rhs < lhs);
}

bool operator>=(const Int& lhs, const Int& rhs) {
	return !(lhs < rhs);
}

int main() {
	//forward_list<int> fl = { 1, 2, 3, 4, 5 };
	{
		DynamicArray<Int> da(5, 10);
	}

	cout << "\nPress any key to exit...";
	std::cin.get();
	return 0;
}