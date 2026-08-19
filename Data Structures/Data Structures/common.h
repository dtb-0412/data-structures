#pragma once

#include<iostream>

template<class T>
	requires 
		std::copy_constructible<T> &&
		std::equality_comparable<T> &&
		requires(std::ostream& os, const T& obj) { os << obj; }
class Type {
public:
	Type() = default;

	explicit Type(T val)
		: data(val) {
		std::cout << "Type(" << data << ")\n";
	}

	Type(const Type& other)
		: data(other.data) {
		std::cout << "Type(const Type(" << data << ")&)\n";
	}

	Type(Type&& other)
		: data(std::move(other.data)) {
		std::cout << "Type(Type(" << data << ")&&)\n";
	}

	Type& operator=(const Type& other) {
		if (this != std::addressof(other)) {
			std::cout << "Type& operator=(const Type(" << other.data << ")&)\n";
			data = other.data;
		}
		return *this;
	}

	Type& operator=(Type&& other) {
		if (this != std::addressof(other)) {
			std::cout << "Type& operator=(Type(" << other.data << ")&&)\n";
			data = std::move(other.data);
		}
		return *this;
	}

	~Type() noexcept {
		std::cout << "~Type(" << data << ")\n";
	}

	auto operator<=>(const Type&) const = default;

	friend std::ostream& operator<<(std::ostream& os, const Type& obj) {
		return os << obj.data;
	}

	T data{};
};

struct TypeCompare {
	using is_transparent = void;

	template<class T>
	bool operator()(const Type<T>& lhs, const Type<T>& rhs) const noexcept {
		return lhs.data < rhs.data;
	}

	template <class T, class U>
		requires std::equality_comparable_with<T, U>
	bool operator()(const Type<T>& obj, const U& val) const noexcept {
		return obj.data < val;
	}

	template <class T, class U>
		requires std::equality_comparable_with<T, U>
	bool operator()(const U& val, const Type<T>& obj) const noexcept {
		return val < obj.data;
	}
};