#pragma once
#ifndef MAP_H
#define MAP_H

#include"avl_tree.h"
#include"compare.hpp"

template<class KeyT, class T, class Comp = std::less<T>>
class Map : public _AVLTree<_TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, false>> {
private:
	using _BaseTree = _AVLTree<_TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, false>>;

public:
	using key_type		= KeyT;
	using mapped_type	= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= std::pair<const key_type, mapped_type>;
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
	Map()
		: _BaseTree(key_compare()) {}

	explicit Map(const key_compare& comp)
		: _BaseTree(comp) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	Map(It first, Se last)
		: _BaseTree(first, last, key_compare()) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	Map(It first, Se last, const key_compare& comp)
		: _BaseTree(first, last, comp) {}

	Map(std::initializer_list<value_type> initList)
		: _BaseTree(initList, key_compare()) {}

	Map(std::initializer_list<value_type> initList, const key_compare& comp)
		: _BaseTree(initList, comp) {}

	Map(const Map& other)
		: _BaseTree(other) {}

	Map(Map&& other)
		: _BaseTree(std::move(other)) {}

	Map& operator=(const Map& other) {
		_BaseTree::operator=(other);
		return *this;
	}

	Map& operator=(Map&& other) noexcept(std::is_nothrow_move_assignable_v<key_compare>) {
		_BaseTree::operator=(std::move(other));
		return *this;
	}

	Map& operator=(std::initializer_list<value_type> initList) {
		_BaseTree::operator=(initList);
		return *this;
	}

	// ...

	void swap(Map& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

//template<class T, class Comp>
//void swap(Map<T, Comp>& lhs, Map<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
//	lhs.swap(rhs);
//}
//
//template<class T, class Comp>
//[[nodiscard]] bool operator==(const Map<T, Comp>& lhs, const Map<T, Comp>& rhs) {
//	return lhs.size() == rhs.size() &&
//		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
//}
//
//template<class T, class Comp>
//[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const Map<T, Comp>& lhs, const Map<T, Comp>& rhs) {
//	return std::lexicographical_compare_three_way(
//		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
//	);
//}

template<class KeyT, class T, class Comp = std::less<T>>
class MultiMap : public _AVLTree<_TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, true>> {
private:
	using _BaseTree = _AVLTree< _TreeMapTraits<KeyT, T, Comp, _AVLTreeNode, true>>;

public:
	using key_type		= KeyT;
	using mapped_type	= T;
	using key_compare	= Comp;
	using value_compare = typename _BaseTree::value_compare;

	using value_type		= std::pair<const key_type, mapped_type>;
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
	MultiMap()
		: _BaseTree(key_compare()) {}

	explicit MultiMap(const key_compare& comp)
		: _BaseTree(comp) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	MultiMap(It first, Se last)
		: _BaseTree(first, last, key_compare()) {}

	template<std::input_iterator It, std::sentinel_for<It> Se>
	MultiMap(It first, Se last, const key_compare& comp)
		: _BaseTree(first, last, comp) {}

	MultiMap(std::initializer_list<value_type> initList)
		: _BaseTree(initList, key_compare()) {}

	MultiMap(std::initializer_list<value_type> initList, const key_compare& comp)
		: _BaseTree(initList, comp) {}

	MultiMap(const MultiMap& other)
		: _BaseTree(other) {}

	MultiMap(MultiMap&& other)
		: _BaseTree(std::move(other)) {}

	MultiMap& operator=(const MultiMap& other) {
		_BaseTree::operator=(other);
		return *this;
	}

	MultiMap& operator=(MultiMap&& other) noexcept(std::is_nothrow_move_assignable_v<key_compare>) {
		_BaseTree::operator=(std::move(other));
		return *this;
	}

	MultiMap& operator=(std::initializer_list<value_type> initList) {
		_BaseTree::operator=(initList);
		return *this;
	}

	// ...

	void swap(MultiMap& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

//template<class T, class Comp>
//void swap(MultiMap<T, Comp>& lhs, MultiMap<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
//	lhs.swap(rhs);
//}
//
//template<class T, class Comp>
//[[nodiscard]] bool operator==(const MultiMap<T, Comp>& lhs, const MultiMap<T, Comp>& rhs) {
//	return lhs.size() == rhs.size() &&
//		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
//}
//
//template<class T, class Comp>
//[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(const MultiMap<T, Comp>& lhs, const MultiMap<T, Comp>& rhs) {
//	return std::lexicographical_compare_three_way(
//		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
//	);
//}
#endif // MAP_H