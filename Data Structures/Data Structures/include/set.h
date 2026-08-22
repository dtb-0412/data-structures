#pragma once
#ifndef SET_H
#define SET_H

#include"avl_tree.h"

template<class T, class Comp = std::less<T>>
class Set : public _BSTree<
	_BSTreeTraits<T, T, Comp, _AVLTreeNode, false>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _AVLTreeNode, false>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

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
class MultiSet : public _BSTree<
	_BSTreeTraits<T, T, Comp, _AVLTreeNode, true>,
	_AVLTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _AVLTreeNode, true>, _AVLTreeCore>;

public:
	using _BaseTree::_BaseTree;

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