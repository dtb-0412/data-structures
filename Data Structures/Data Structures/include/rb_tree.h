#pragma once
#ifndef RB_TREE_H
#define RB_TREE_H

#include"tree_core.h"

template<class ValueT>
struct _RBTreeNode : public _BSTreeNode<ValueT, _RBTreeNode<ValueT>> {
private:
	using _BaseNode = _BSTreeNode<ValueT, _RBTreeNode<ValueT>>;

public:
	using node_pointer	= typename _BaseNode::node_pointer;
	using value_type	= typename _BaseNode::value_type;

	_RBTreeNode() = default;

	_RBTreeNode(const _RBTreeNode&)				= delete;
	_RBTreeNode& operator=(const _RBTreeNode&)	= delete;
};

template<class ValueT, class SizeT, class DiffT, class Ptr, class ConstPtr, class NodeT>
class _RBTreeCore : public _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT> {
private:
	using _BaseVal = _BSTreeCore<ValueT, SizeT, DiffT, Ptr, ConstPtr, NodeT>;

public:
	using node_type		= typename _BaseVal::node_type;
	using node_pointer	= typename _BaseVal::node_pointer;

public:
	using _BaseVal::head;
	using _BaseVal::size;

	_RBTreeCore() = default;

	node_pointer insert(const _NodeLocation<node_pointer> location, node_pointer newNode) noexcept {
		// ...
	}

	node_pointer extract(_BSTreeConstIterator<_RBTreeCore> where) noexcept {
		// ...
	}
};

template<class T, class Comp = std::less<T>>
class RBTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _RBTreeNode, false>,
	_RBTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _RBTreeNode, false>, _RBTreeCore>;

public:
	using _BaseTree::_BaseTree;

	void swap(RBTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(RBTree<T, Comp>& lhs, RBTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const RBTree<T, Comp>& lhs, const RBTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const RBTree<T, Comp>& lhs, const RBTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}

template<class T, class Comp = std::less<T>>
class RBMultiTree : public _BSTree<
	_BSTreeTraits<T, T, Comp, _RBTreeNode, true>,
	_RBTreeCore
> {
private:
	using _BaseTree = _BSTree<_BSTreeTraits<T, T, Comp, _RBTreeNode, true>, _RBTreeCore>;

public:
	using _BaseTree::_BaseTree;

	void swap(RBMultiTree& other) noexcept(noexcept(_BaseTree::swap(other))) {
		_BaseTree::swap(other);
	}
};

template<class T, class Comp>
void swap(RBMultiTree<T, Comp>& lhs, RBMultiTree<T, Comp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template<class T, class Comp>
[[nodiscard]] bool operator==(const RBMultiTree<T, Comp>& lhs, const RBMultiTree<T, Comp>& rhs) {
	return lhs.size() == rhs.size() &&
		std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

template<class T, class Comp>
[[nodiscard]] compare::SynthThreeWayCompareResult<T> operator<=>(
	const RBMultiTree<T, Comp>& lhs, const RBMultiTree<T, Comp>& rhs
) {
	return std::lexicographical_compare_three_way(
		lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), compare::SynthThreeWayCompareResult<T>{}
	);
}
#endif // RB_TREE_H