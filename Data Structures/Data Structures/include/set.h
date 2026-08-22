#pragma once
#ifndef SET_H
#define SET_H

#include"avl_tree.h"
#include"compare.hpp"

template<class T, class Comp = std::less<T>>
class Set : public _AVLTree<_TreeTraits<T, T, Comp, _AVLTreeNode, false>> {
private:
	using _BaseTree = _AVLTree< _TreeTraits<T, T, Comp, _AVLTreeNode, false>>;

public:
	using key_type		= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= _InsertReturnType<iterator, node_handle>;

public:
	Set()
		: _BaseTree(key_compare()) {}

	explicit Set(const key_compare& comp)
		: _BaseTree(comp) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	Set(It first, Se last)
		: _BaseTree(first, last, key_compare()) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	Set(It first, Se last, const key_compare& comp)
		: _BaseTree(first, last, comp) {}

	Set(std::initializer_list<value_type> initList)
		: _BaseTree(initList, key_compare()) {}

	Set(std::initializer_list<value_type> initList, const key_compare& comp)
		: _BaseTree(initList, comp) {}

	Set(const Set& other)
		: _BaseTree(other) {}

	Set(Set&& other)
		: _BaseTree(std::move(other)) {}

	Set& operator=(const Set& other) {
		_BaseTree::operator=(other);
		return *this;
	}

	Set& operator=(Set&& other) noexcept(std::is_nothrow_move_assignable_v<key_compare>) {
		_BaseTree::operator=(std::move(other));
		return *this;
	}

	Set& operator=(std::initializer_list<value_type> initList) {
		_BaseTree::operator=(initList);
		return *this;
	}

	void swap(Set& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(Set<T, Comp>& lhs, Set<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const Set<T, Comp>& lhs, const Set<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const Set<T, Comp>& lhs, const Set<T, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}

template<class T, class Comp = std::less<T>>
class MultiSet : public _AVLTree<_TreeTraits<T, T, Comp, _AVLTreeNode, true>> {
private:
	using _BaseTree = _AVLTree< _TreeTraits<T, T, Comp, _AVLTreeNode, true>>;

public:
	using key_type		= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= typename _BaseTree::value_type;
	using size_type			= typename _BaseTree::size_type;
	using difference_type	= typename _BaseTree::difference_type;
	using pointer			= typename _BaseTree::pointer;
	using const_pointer		= typename _BaseTree::const_pointer;
	using reference			= value_type&;
	using const_reference	= const value_type&;

	using iterator			= typename _BaseTree::iterator;
	using const_iterator	= typename _BaseTree::const_iterator;

	using reverse_iterator			= typename _BaseTree::reverse_iterator;
	using const_reverse_iterator	= typename _BaseTree::const_reverse_iterator;

	using node_handle			= typename _BaseTree::node_handle;
	using insert_return_type	= _InsertReturnType<iterator, node_handle>;

public:
	MultiSet()
		: _BaseTree(key_compare()) {
	}

	explicit MultiSet(const key_compare& comp)
		: _BaseTree(comp) {
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	MultiSet(It first, Se last)
		: _BaseTree(first, last, key_compare()) {
	}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	MultiSet(It first, Se last, const key_compare& comp)
		: _BaseTree(first, last, comp) {
	}

	MultiSet(std::initializer_list<value_type> initList)
		: _BaseTree(initList, key_compare()) {
	}

	MultiSet(std::initializer_list<value_type> initList, const key_compare& comp)
		: _BaseTree(initList, comp) {
	}

	MultiSet(const MultiSet& other)
		: _BaseTree(other) {
	}

	MultiSet(MultiSet&& other)
		: _BaseTree(std::move(other)) {
	}

	MultiSet& operator=(const MultiSet& other) {
		_BaseTree::operator=(other);
		return *this;
	}

	MultiSet& operator=(MultiSet&& other) noexcept(std::is_nothrow_move_assignable_v<key_compare>) {
		_BaseTree::operator=(std::move(other));
		return *this;
	}

	MultiSet& operator=(std::initializer_list<value_type> initList) {
		_BaseTree::operator=(initList);
		return *this;
	}

	void swap(MultiSet& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(MultiSet<T, Comp>& lhs, MultiSet<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const MultiSet<T, Comp>& lhs, const MultiSet<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const MultiSet<T, Comp>& lhs, const MultiSet<T, Comp>& rhs) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}
#endif // SET_H